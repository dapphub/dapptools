module EVM.Flatten where

import EVM.Dapp
import EVM.Solidity

import Control.Lens hiding (indices)

import Data.Aeson
import Data.Aeson.Lens
import Data.Maybe (mapMaybe, isJust, catMaybes, fromMaybe)
import Data.List (sort)
import Data.Foldable (foldl', toList)
import Data.Monoid ((<>))
import Text.Read (readMaybe)

import qualified Data.Map as Map
import Data.Map ((!))

import qualified Data.Graph.Inductive.Graph as Fgl
import qualified Data.Graph.Inductive.PatriciaTree as Fgl
import qualified Data.Graph.Inductive.Query.BFS as Fgl
import qualified Data.Graph.Inductive.Query.DFS as Fgl

import Data.Text (Text)
import qualified Data.Text as Text

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8

flatten :: DappInfo -> Text -> IO ()
flatten dapp target = do
  let
    astMap  = view (dappSources . sourceAsts) dapp
    names   = Map.keys astMap
    indices = Map.fromList (zip names [1..])

    edges =
      [ (indices ! s, indices ! t, ()) -- edge from s to t
      | (s, v) <- Map.toList astMap    -- for every file s
      , t <- importsFrom v ]           -- and every t imported by s

    importsFrom ast =
      flip mapMaybe (universe ast) $ \x ->
        case preview (key "name") x of
          Just (String "ImportDirective") ->
            preview (key "attributes" . key "absolutePath" . _String) x
          _ ->
            Nothing

    g = Fgl.mkGraph (zip [1..] names) edges :: Fgl.Gr Text ()

  case Map.lookup target indices of
    Nothing ->
      error "didn't find contract"
    Just root -> do
      putStrLn $ "// hevm: flattened sources of " <> Text.unpack target
      putStrLn (Text.unpack (maximalPragma (Map.elems astMap)))
      mapM_
        (\x -> do
            src <- BS.readFile (Text.unpack x)
            putStrLn ("////// " <> Text.unpack x <> "\n")
            BS8.putStrLn (stripImportsAndPragmas src (astMap ! x)))
        (reverse (Fgl.topsort' (Fgl.subgraph (Fgl.bfs root g) g)))

maximalPragma :: [Value] -> Text
maximalPragma asts =
  case concatMap allPragmas asts of
    [] -> error "No Solidity version pragmas"
    xs -> "pragma solidity ^" <> Text.intercalate "." (map (Text.pack . show) (maximum xs)) <> ";\n"

allPragmas :: Value -> [[Int]]
allPragmas ast =
  let
    vs =
      map toList . catMaybes $ map
        (preview (key "attributes" . key "literals" . _Array))
        (filter (flip nodeIs "PragmaDirective") (universe ast))
    f = \case
      [String "solidity", String _, String a, String b] ->
        map
          (fromMaybe
             (error . Text.unpack $ "bad Solidity version: " <> a <> b)
             . readAs)
          (Text.splitOn "." (a <> b))
      x ->
        error ("oops: " ++ show x)
  in map f vs

nodeIs :: Value -> Text -> Bool
nodeIs x t =
  isJust (preview (key "src") x) && (preview (key "name" . _String) x == Just t)

stripImportsAndPragmas :: ByteString -> Value -> ByteString
stripImportsAndPragmas bs ast = stripAstNodes bs ast p
  where
    p x = nodeIs x "ImportDirective" || nodeIs x "PragmaDirective"

stripAstNodes :: ByteString -> Value -> (Value -> Bool) -> ByteString
stripAstNodes bs ast p =
  cutRanges bs [astNodeByteRange node | node <- universe ast, p node]

readAs :: Read a => Text -> Maybe a
readAs = readMaybe . Text.unpack

astNodeByteRange :: Value -> (Int, Int)
astNodeByteRange v =
  case preview (key "src" . _String) v of
    Just (Text.splitOn ":" -> [readAs -> Just i, readAs -> Just n, _]) ->
      (i, i + n)
    _ ->
      error "internal error: no source position for AST node"

-- | Removes a set of non-overlapping ranges from a bytestring
cutRanges :: ByteString -> [(Int, Int)] -> ByteString
cutRanges bs (sort -> rs) = fst (foldl' f (bs, 0) rs)
  where
    f (bs', n) (i, j) =
      ( cut bs' (i - n) (j - n)
      , n - length ("/*  */" :: String)
      )

-- | Removes the bytes between two indices from a bytestring
cut :: ByteString -> Int -> Int -> ByteString
cut x i j =
  let (a, b) = BS.splitAt i x
  in a <> "/* " <> BS.take (j - i) b <> " */" <> BS.drop (j - i) b
