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

-- We query and alter the Solidity code using the compiler's AST.
-- The AST is a deep JSON structure, so we use Aeson and Lens.
import Control.Lens (preview, view, universe)
import Data.Aeson (Value (String))
import Data.Aeson.Lens (key, _String, _Array)

-- We use the FGL graph library for the topological sort.
-- (We use four FGL functions and they're all in different modules!)
import qualified Data.Graph.Inductive.Graph as Fgl
import qualified Data.Graph.Inductive.PatriciaTree as Fgl
import qualified Data.Graph.Inductive.Query.BFS as Fgl
import qualified Data.Graph.Inductive.Query.DFS as Fgl

import Control.Monad (forM)
import Data.ByteString (ByteString)
import Data.Foldable (foldl', toList)
import Data.List (sort)
import Data.Map (Map, (!))
import Data.Maybe (mapMaybe, isJust, fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text, unpack, pack, intercalate)
import Data.Text.Encoding (encodeUtf8)
import Text.Read (readMaybe)

import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.ByteString as BS

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
            , stripImportsAndPragmas src (asts ! path), "\n"
            ]

      -- Finally print the whole concatenation.
      putStrLn $ "// hevm: flattened sources of " <> unpack target
      putStrLn (unpack pragma)
      BS.putStr (mconcat sources)

-- Construct a new Solidity version pragma for the highest mentioned version
-- given a list of source file ASTs.
maximalPragma :: [Value] -> Text
maximalPragma asts =
  case mapMaybe versions asts of
    [] -> error "no Solidity version pragmas in any source files"
    xs ->
      "pragma solidity ^"
        <> intercalate "." (map (pack . show) (maximum xs))
        <> ";\n"

  where
    -- Get the version components from a source file's pragma,
    -- or nothing if no pragma present.
    versions :: Value -> Maybe [Int]
    versions ast = fmap grok components
      where
        pragma :: Maybe Value
        pragma =
          case filter (nodeIs "PragmaDirective") (universe ast) of
            [x] -> Just x
            []  -> Nothing
            _   -> error "multiple version pragmas"

        components :: Maybe [Value]
        components = fmap toList
          (pragma >>= preview (key "attributes" . key "literals" . _Array))

        grok :: [Value] -> [Int]
        grok = \case
          [String "solidity", String _prefix, String a, String b] ->
            map
              (fromMaybe
                 (error . Text.unpack $ "bad Solidity version: " <> a <> b)
                 . readAs)
              (Text.splitOn "." (a <> b))
          x ->
            error ("unrecognized pragma: " ++ show x)

nodeIs :: Text -> Value -> Bool
nodeIs t x = isSourceNode && hasRightName
  where
    isSourceNode =
      isJust (preview (key "src") x)
    hasRightName =
      Just t == preview (key "name" . _String) x

stripImportsAndPragmas :: ByteString -> Value -> ByteString
stripImportsAndPragmas bs ast = stripAstNodes bs ast p
  where
    p x = nodeIs "ImportDirective" x || nodeIs "PragmaDirective" x

stripAstNodes :: ByteString -> Value -> (Value -> Bool) -> ByteString
stripAstNodes bs ast p =
  cutRanges [sourceRange node | node <- universe ast, p node]

  where
    -- Parses the `src` field of an AST node into a pair of byte indices.
    sourceRange :: Value -> (Int, Int)
    sourceRange v =
      case preview (key "src" . _String) v of
        Just (Text.splitOn ":" -> [readAs -> Just i, readAs -> Just n, _]) ->
          (i, i + n)
        _ ->
          error "internal error: no source position for AST node"

    -- Removes a set of non-overlapping ranges from a bytestring
    -- by commenting them out.
    cutRanges :: [(Int, Int)] -> ByteString
    cutRanges (sort -> rs) = fst (foldl' f (bs, 0) rs)
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
