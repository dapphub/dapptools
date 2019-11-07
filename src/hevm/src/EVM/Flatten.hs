module EVM.Flatten (flatten) where

-- This module concatenates all the imported dependencies
-- of a given source file, so that you can paste the result
-- into Remix or the Etherscan contract verification page.
--
-- The concatenated files are stripped of import directives
-- and compiler version pragmas are merged into just one.
--
-- This module is mostly independent from the rest of Hevm,
-- using only the source code metadata support modules.

import EVM.Dapp (DappInfo, dappSources)
import EVM.Solidity (sourceAsts)
import EVM.Demand (demand)

-- We query and alter the Solidity code using the compiler's AST.
-- The AST is a deep JSON structure, so we use Aeson and Lens.
import Control.Lens (preview, view, universe)
import Data.Aeson (Value (String))
import Data.Aeson.Lens (key, _String, _Array, _Integer)

-- We use the FGL graph library for the topological sort.
-- (We use four FGL functions and they're all in different modules!)
import qualified Data.Graph.Inductive.Graph as Fgl
import qualified Data.Graph.Inductive.PatriciaTree as Fgl
import qualified Data.Graph.Inductive.Query.BFS as Fgl
import qualified Data.Graph.Inductive.Query.DFS as Fgl

-- The Solidity version pragmas can be arbitrary SemVer ranges,
-- so we use this library to parse them.
import Data.SemVer (SemVerRange, parseSemVerRange)
import qualified Data.SemVer as SemVer

import Control.Monad (forM)
import Data.ByteString (ByteString)
import Data.Foldable (foldl', toList)
import Data.List (sort, nub)
import Data.Map (Map, (!))
import Data.Maybe (mapMaybe, isJust, catMaybes, fromJust)
import Data.Monoid ((<>))
import Data.Text (Text, unpack, pack, intercalate)
import Data.Text.Encoding (encodeUtf8)
import Text.Read (readMaybe)

import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.ByteString as BS

--- XXX: debugging
import Debug.Trace (trace)


-- Define an alias for FGL graphs with text nodes and unlabeled edges.
type FileGraph = Fgl.Gr Text ()

-- Given the AST of a source file, resolve all its imported paths.
importsFrom :: Value -> [Text]
importsFrom ast =
  let
    -- We use the astonishing `universe` function from Lens
    -- to get a lazy list of every node in the AST structure.
    allNodes :: [Value]
    allNodes = universe ast

    -- Given some subvalue in the AST, we check if it's an import,
    -- and if so, return its resolved import path.
    resolveImport :: Value -> Maybe Text
    resolveImport node =
      case preview (key "name") node of
        Just (String "ImportDirective") ->
          preview (key "attributes" . key "absolutePath" . _String) node
        _ ->
          Nothing

  -- Now we just try to resolve import paths at all subnodes.
  in mapMaybe resolveImport allNodes

flatten :: DappInfo -> Text -> IO ()
flatten dapp target = do
  let
    -- The nodes and edges are defined below.
    graph :: FileGraph
    graph = Fgl.mkGraph nodes edges

    -- The graph nodes are ints with source paths as labels.
    nodes :: [(Int, Text)]
    nodes = zip [1..] (Map.keys asts)

    -- The graph edges are defined by module imports.
    edges =
      [ (indices ! s, indices ! t, ()) -- Edge from S to T
      | (s, v) <- Map.toList asts      -- for every file S
      , t      <- importsFrom v ]      -- and every T imported by S.

    -- We can look up the node index for a source file path.
    indices :: Map Text Int
    indices = Map.fromList [(v, k) | (k, v) <- nodes]

    -- The JSON ASTs are indexed by source file path.
    asts :: Map Text Value
    asts = view (dappSources . sourceAsts) dapp

    topScopeIds :: [Integer]
    topScopeIds = mconcat $ fmap f $ Map.elems asts
      where
        id' = preview (key "id" . _Integer)
        f ast =
          [ fromJust' "no id for SourceUnit" $ id' node
          | node <- universe ast
          , nodeIs "SourceUnit" node
          ]

    contractsAndStructsToRename :: [Integer]
    contractsAndStructsToRename = mconcat $ fmap f $ Map.elems asts
      where
        scope = preview (key "attributes" . key "scope" . _Integer)
        id' = preview (key "id" . _Integer)
        p x = (nodeIs "ContractDefinition" x || nodeIs "StructDefinition" x)
          && (fromJust' "line:122 contract/struct scope" $ scope x) `elem` topScopeIds
        f ast =
          [ fromJust' "no id for top scoped contract or struct" $ id' node
          | node <- universe ast
          , p node
          ]

    contractStructs :: [(Integer, (Integer, Text))]
    contractStructs = mconcat $ fmap f $ Map.elems asts
      where
        scope = preview (key "attributes" . key "scope" . _Integer)
        cname = preview (key "attributes" . key "canonicalName" . _String)
        id' = preview (key "id" . _Integer)
        p x = (nodeIs "StructDefinition" x)
          && (fromJust' "line:137 nested struct" $ scope x) `elem` contractsAndStructsToRename
        f ast =
          [ let
              id'' = fromJust' "no id for nested struct" $ id' node
              cname' = fromJust'
                ("no canonical name of nested struct with id:" ++ show id'') $ cname node
              ref = fromJust'
                ("no scope of nested struct with id:" ++ show id'') $ scope node
            in
              (id'', (ref, cname'))
          | node <- universe ast
          , p node
          ]

  -- We use the target source file to make a relevant subgraph
  -- with only files transitively depended on from the target.
  case Map.lookup target indices of
    Nothing ->
      error "didn't find contract AST"
    Just root -> do
      let
        -- Restrict the graph to only the needed nodes,
        -- discovered via breadth-first search from the target.
        subgraph :: Fgl.Gr Text ()
        subgraph = Fgl.subgraph (Fgl.bfs root graph) graph

        -- Now put the source file paths in the right order
        -- by sorting topologically.
        ordered :: [Text]
        ordered = reverse (Fgl.topsort' subgraph)

        -- Take the highest Solidity version from all pragmas.
        pragma :: Text
        pragma = maximalPragma (Map.elems asts)

      -- Read the source files in order and strip unwanted directives.
      -- Also add an informative comment with the original source file path.
      sources <-
        forM ordered $ \path -> do
          src <- BS.readFile (unpack path)
          pure $ mconcat
            [ "////// ", encodeUtf8 path, "\n"
            -- Fold over a list of source transforms
            , fst
                (prefixContractAst
                  contractsAndStructsToRename
                  contractStructs
                  (stripImportsAndPragmas (src, 0) (asts ! path))
                  (asts ! path)), "\n"
            ]

      -- Force all evaluation before any printing happens, to avoid
      -- partial output.
      demand target; demand pragma; demand sources

      -- Finally print the whole concatenation.
      putStrLn $ "// hevm: flattened sources of " <> unpack target
      putStrLn (unpack pragma)
      BS.putStr (mconcat sources)

-- Construct a new Solidity version pragma for the highest mentioned version
-- given a list of source file ASTs.
maximalPragma :: [Value] -> Text
maximalPragma asts = (
    case mapMaybe versions asts of
      [] -> error "no Solidity version pragmas in any source files"
      xs ->
        "pragma solidity "
          <> pack (show (rangeIntersection xs))
          <> ";\n"
  )
  <> (
    mconcat . nub . sort . fmap (\ast ->
      mconcat $ fmap
        (\xs -> "pragma "
          <> intercalate " " [x | String x <- xs]
          <> ";\n")
        (otherPragmas ast)
    )
  ) asts


  where
    isVersionPragma :: [Value] -> Bool
    isVersionPragma =
      \case
        String "solidity" : _ -> True
        _ -> False

    pragmaComponents :: Value -> [[Value]]
    pragmaComponents ast = components
      where
        ps :: [Value]
        ps = filter (nodeIs "PragmaDirective") (universe ast)

        components :: [[Value]]
        components = catMaybes $ fmap
          ((fmap toList) . preview (key "attributes" . key "literals" . _Array))
          ps

    -- Simple way to combine many SemVer ranges.  We don't actually
    -- optimize these boolean expressions, so the resulting pragma
    -- might be redundant, like ">=0.4.23 >=0.5.0 <0.6.0".
    rangeIntersection :: [SemVerRange] -> SemVerRange
    rangeIntersection = foldr1 SemVer.And . nub . sort

    -- Get the semantic version range from a source file's pragma,
    -- or nothing if no pragma present.
    versions :: Value -> Maybe SemVerRange
    versions ast = fmap grok components
      where
        components :: Maybe [Value]
        components =
          case filter isVersionPragma (pragmaComponents ast) of
            [_:xs] -> Just xs
            []  -> Nothing
            _   -> error "multiple version pragmas"

        grok :: [Value] -> SemVerRange
        grok xs =
          let
            rangeText = mconcat [x | String x <- xs]
          in
            case parseSemVerRange rangeText of
              Right r -> r
              Left _ ->
                error ("failed to parse SemVer range " ++ show rangeText)

    otherPragmas :: Value -> [[Value]]
    otherPragmas = (filter (not . isVersionPragma)) . pragmaComponents

nodeIs :: Text -> Value -> Bool
nodeIs t x = isSourceNode && hasRightName
  where
    isSourceNode =
      isJust (preview (key "src") x)
    hasRightName =
      Just t == preview (key "name" . _String) x

stripImportsAndPragmas :: (ByteString, Int) -> Value -> (ByteString, Int)
stripImportsAndPragmas bso ast = stripAstNodes bso ast p
  where
    p x = nodeIs "ImportDirective" x || nodeIs "PragmaDirective" x

stripAstNodes :: (ByteString, Int)-> Value -> (Value -> Bool) -> (ByteString, Int)
stripAstNodes bso ast p =
  cutRanges [sourceRange node | node <- universe ast, p node]

  where
    -- Removes a set of non-overlapping ranges from a bytestring
    -- by commenting them out.
    cutRanges :: [(Int, Int)] -> (ByteString, Int)
    cutRanges (sort -> rs) = foldl' f bso rs
      where
        f (bs', n) (i, j) =
          ( cut bs' (i + n) (j + n)
          , n + length ("/*  */" :: String))

    -- Comments out the bytes between two indices from a bytestring.
    cut :: ByteString -> Int -> Int -> ByteString
    cut x i j =
      let (a, b) = BS.splitAt i x
      in a <> "/* " <> BS.take (j - i) b <> " */" <> BS.drop (j - i) b

readAs :: Read a => Text -> Maybe a
readAs = readMaybe . Text.unpack

prefixContractAst :: [Integer] -> [(Integer, (Integer, Text))] -> (ByteString, Int) -> Value -> (ByteString, Int)
prefixContractAst castr cs bso ast = prefixAstNodes
  where
    refDec = preview (key "attributes" . key "referencedDeclaration" . _Integer)
    name = preview (key "attributes" . key "name" . _String)
    id' = preview (key "id" . _Integer)

    p x = (nodeIs "ContractDefinition" x || nodeIs "StructDefinition" x)
      && (fromJust' "id of any" $ id' x) `elem` castr

    p' x =
      (nodeIs "Identifier" x || nodeIs "UserDefinedTypeName" x)
        && (fromJust' "refDec of ident/userdef" $ refDec x) `elem` castr

    p'' x =
      (nodeIs "Identifier" x || nodeIs "UserDefinedTypeName" x)
      && (let
          refs = fmap fst cs
          ref = fromJust' "refDec of ident/userdef" $ refDec x
          n = fromJust' "name of ident/userdef" $ name x
          cn = fromJust' "no match for lookup in nested structs" $ lookup ref cs
        in
          -- XXX: comparing canonical name with name of nested structs not super great
          ref `elem` refs && n == snd cn
      )

    p''' x = p x || p' x || p'' x

    prefixAstNodes :: (ByteString, Int)
    prefixAstNodes  =
      cutRanges [(sourceRange node, sourceId node) | node <- universe ast, p''' node]

    -- Parses the `id` and `attributes.referencedDeclaration` field of an AST node
    -- into a pair of byte indices.
    -- XXX: these manual offsets have to be replaced by looking at the bytes
    sourceId :: Value -> (Int, Integer)
    sourceId v =
      if (not $ p v || p' v) &&  p'' v then (
        let
          ref = fromJust' "refDec of nested struct ref" $ refDec v
          cn = fromJust' "no match for lookup in nested structs" $ lookup ref cs
        in
          (0, fst cn)
      ) else
        fromJust' "internal error: no id found for contract reference" x

      where
        x :: Maybe (Int, Integer)
        x = case preview (key "name") v of
          Just (String "ContractDefinition") ->
            fmap ((,) 9) $ id' v
          Just (String "StructDefinition") ->
            fmap ((,) 7) $ id' v
          Just (String t)
            | t `elem` ["UserDefinedTypeName", "Identifier"] ->
              fmap ((,) 0) $ refDec v
            | otherwise ->
              error "internal error: not a contract reference"
          _ ->
            error "internal error: not a contract reference"

    -- Prefix a set of non-overlapping ranges from a bytestring
    -- by commenting them out.
    cutRanges :: [((Int, Int), (Int, Integer))] -> (ByteString, Int)
    cutRanges (sort -> rs) = foldl' f bso rs
      where
        f (bs', n) ((i, _), (o, t)) =
          let
            t' = "_" <> (pack $ show t) <> "_"
          in
            ( prefix t' bs' (i + n + o)
            , n + Text.length t' )

    -- Comments out the bytes between two indices from a bytestring.
    prefix :: Text -> ByteString -> Int -> ByteString
    prefix t x i =
      let (a, b) = BS.splitAt i x
      in a <> encodeUtf8 t <> b

-- Parses the `src` field of an AST node into a pair of byte indices.
sourceRange :: Value -> (Int, Int)
sourceRange v =
  case preview (key "src" . _String) v of
    Just (Text.splitOn ":" -> [readAs -> Just i, readAs -> Just n, _]) ->
      (i, i + n)
    _ ->
      error "internal error: no source position for AST node"

fromJust' :: String -> Maybe a -> a
fromJust' msg = \case
  Just x -> x
  Nothing -> error $ "no match in fromJust' " ++ msg
