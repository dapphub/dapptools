{-# Language LambdaCase #-}
{-# Language ViewPatterns #-}
{-# Language OverloadedStrings #-}

module Jays where

import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Foldable (toList)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Vector (snoc)

import qualified Data.ByteString.Lazy as BS
import qualified Data.HashMap.Lazy as Map

data Op
  = OpNewValue Value
  | OpInsert Text
  | OpType
  | OpKeys
  | OpUnstring
  | OpAcross [Op]
  | OpLength

parse :: [ByteString] -> Either Text [Op]
parse =
  \case
    [] ->
      pure []

    ("-n" : "{}" : xs) ->
      (OpNewValue (object []) :) <$> parse xs
    ("-n" : "object" : xs) ->
      (OpNewValue (object []) :) <$> parse xs

    ("-n" : "[]" : xs) ->
      (OpNewValue (toJSON ([] :: [()])) :) <$> parse xs
    ("-n" : "array" : xs) ->
      (OpNewValue (toJSON ([] :: [()])) :) <$> parse xs

    ("-n" : "t" : xs) ->
      (OpNewValue (Bool True) :) <$> parse xs
    ("-n" : "true" : xs) ->
      (OpNewValue (Bool True) :) <$> parse xs
    ("-n" : "f" : xs) ->
      (OpNewValue (Bool False) :) <$> parse xs
    ("-n" : "false" : xs) ->
      (OpNewValue (Bool False) :) <$> parse xs

    ("-n" : "null" : xs) ->
      (OpNewValue Null :) <$> parse xs
    ("-n" : "n" : xs) ->
      (OpNewValue Null :) <$> parse xs

    ("-n" : (decode -> Just v@(Number _)) : xs) ->
      (OpNewValue v :) <$> parse xs

    ("-n" : _) ->
      Left "unrecognized -n argument"

    ("-i" : (decodeUtf8 . toStrict -> t) : xs) ->
      (OpInsert t :) <$> parse xs

    ("-i" : _) ->
      Left "unrecognized -i argument"

    ("-s" : (decodeUtf8 . toStrict -> t) : xs) ->
      (OpNewValue (String t) :) <$> parse xs

    ("-s" : _) ->
      Left "invalid UTF-8 string"

    ("-t" : xs) ->
      (OpType :) <$> parse xs

    ("-k" : xs) ->
      (OpKeys :) <$> parse xs

    ("-u" : xs) ->
      (OpUnstring :) <$> parse xs

    -- Only allow at end, and treat as noop
    ("-j" : []) ->
      pure []

    ("-j" : _) ->
      Left "-j only allowed at end"

    ("-a" : xs) ->
      parse xs >>= \x -> pure [OpAcross x]

    ("-l" : xs) ->
      (OpLength :) <$> parse xs

    _ ->
      Left "unrecognized syntax"

jays :: ByteString -> [Text] -> (ByteString, Bool)
jays input args =
  let
    code  = filter (/= "-Q") args
    quiet = elem "-Q" args
    result = do
      ops <- parse (map (fromStrict . encodeUtf8) code)
      stk <-
        if BS.null input
        then pure []
        else
          case decode input of
            Nothing ->
              Left "invalid JSON input"
            Just x ->
              pure [x]
      work stk ops
  in
    case (result, quiet) of
      (Left _, True)  -> ("", False)
      (Left x, False) -> (fromStrict (encodeUtf8 x), False)
      (Right x, _)    -> (BS.intercalate "\n" x, True)

work :: [Value] -> [Op] -> Either Text [ByteString]
work stk ops =
  case (stk, ops) of
    ([], [])  -> pure []
    ([x], []) -> pure [encode x]
    (_, [])   -> Left "more than 1 stack element left"

    (xs, OpNewValue v : ops') -> work (v : xs) ops'

    (v : Object o : xs, OpInsert k : ops') ->
      work (Object (Map.insert k v o) : xs) ops'
    (v : Array o : xs, OpInsert "append" : ops') ->
      work (Array (snoc o v) : xs) ops'
    (_, OpInsert "append" : _) ->
      Left "error in -i append"
    (_, OpInsert _ : _) ->
      Left "error in -i"

    (v : xs, OpType : ops') ->
      let t = case v of
                Number _ -> "number"
                String _ -> "string"
                Bool _   -> "bool"
                Object _ -> "object"
                Array _  -> "array"
                Null     -> "null"
      in ([t] ++) <$> work xs ops'
    (_, OpType : _) ->
      Left "error in -t"

    (Object o : xs, OpKeys : ops') ->
      (map (fromStrict . encodeUtf8) (Map.keys o) ++) <$> work xs ops'
    (_, OpKeys : _) ->
      Left "error in -k"

    (String t : xs, OpUnstring : ops') ->
      (fromStrict (encodeUtf8 t) :) <$> work xs ops'
    (_, OpUnstring : _) ->
      Left "error in -u"

    (Array o : xs, OpAcross ops' : []) -> do
      rs <- mapM (\v -> work (v:xs) ops') (toList o)
      pure (concat rs)
    (_, OpAcross _ : []) ->
      Left "-a only for arrays"
    (_, OpAcross _ : _) ->
      Left "-a only at the end"

    (Array o : xs, OpLength : ops') ->
      work (Number (fromIntegral (length o)) : xs) ops'
    (_, OpLength : _) ->
      Left "error in -l"
