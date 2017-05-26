{-# Language TemplateHaskell #-}

module EVM.TTY where

import Brick
import Brick.BChan
import Brick.Focus
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Brick.Widgets.Edit
import Brick.Widgets.List

import EVM
import EVM.ABI
import EVM.Debug
import EVM.Exec
import EVM.Keccak
import EVM.Solidity
import EVM.Types
import EVM.UnitTest

import Control.Lens
import Control.Lens.TH
import Control.Monad
import Control.Monad.State.Strict hiding (state)

import Data.ByteString (ByteString)
import Data.List (sortBy)
import Data.Map (Map)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Format
import Data.Text.Lazy (toStrict)

import System.Directory (withCurrentDirectory)

import qualified Graphics.Vty as Vty
import qualified Data.Map as Map
import qualified Data.Vector as Vec
import qualified Data.Text as Text

data Name
  = ContractListPane
  | AbiPane
  | StackPane
  | BytecodePane
  | LogPane
  | TracePane
  | SolidityPane
  deriving (Eq, Show, Ord)

data UiVmState = UiVmState
  { _uiVm             :: VM
  , _uiVmStackList    :: List Name W256
  , _uiVmBytecodeList :: List Name (Int, Op)
  , _uiVmLogList      :: List Name Log
  , _uiVmTraceList    :: List Name Text
  , _uiVmSolidityList :: List Name ByteString
  }

data DappInfo = DappInfo
  { _dappRoot      :: FilePath
  , _dappContracts :: Map Text SolcContract
  , _dappSources   :: SourceCache
  , _dappUnitTests :: [(Text, [Text])]
  }

data UiState = UiState
  { _uiDapp         :: DappInfo
  , _uiFocusRing    :: FocusRing Name
  , _uiContractList :: List Name Text
  , _uiVmState      :: UiVmState
  }

makeLenses ''UiVmState
makeLenses ''DappInfo
makeLenses ''UiState

isUnitTestContract :: Text -> UiState -> Bool
isUnitTestContract name ui =
  elem name (map fst (view (uiDapp . dappUnitTests) ui))

example =
  withCurrentDirectory "/home/mbrock/dapphub/sai" $
    EVM.TTY.main
      "/home/mbrock/dapphub/sai"
      "/home/mbrock/dapphub/sai/out/tub.t.sol.json"

main :: FilePath -> FilePath -> IO ()
main dappRoot jsonFilePath = do
  readSolc jsonFilePath >>=
    \case
      Nothing ->
        error "Failed to read Solidity JSON"
      Just (contractMap, sourceCache) -> do
        let
          unitTests = findUnitTests (Map.elems contractMap)
          firstUnitTest = head unitTests
          Just testContract = view (at (fst firstUnitTest)) contractMap
          vm = initialUnitTestVm testContract (Map.elems contractMap)

          mkVty = do
            vty <- Vty.mkVty Vty.defaultConfig
            Vty.setMode (Vty.outputIface vty) Vty.BracketedPaste True
            return vty

          ui = UiState
            { _uiDapp = DappInfo
              { _dappRoot      = dappRoot
              , _dappUnitTests = unitTests
              , _dappContracts = contractMap
              , _dappSources   = sourceCache
              }
            , _uiContractList =
                list
                  ContractListPane
                  (Vec.fromList
                    . sortBy (comparing (\x -> (not (isUnitTestContract x ui), x)))
                    . Map.keys $ contractMap)
                  2
            , _uiFocusRing = focusRing
              [ ContractListPane
              , AbiPane
              ]
            , _uiVmState = UiVmState
              { _uiVm = vm
              , _uiVmStackList = list StackPane mempty 1
              , _uiVmBytecodeList =
                  list BytecodePane
                    (Vec.imap (,) (view codeOps (fromJust (currentContract vm))))
                    1
              , _uiVmLogList = list LogPane mempty 1
              , _uiVmTraceList = list TracePane mempty 1
              , _uiVmSolidityList =
                  list SolidityPane
                    (view (uiDapp . dappSources . sourceLines . ix 0) ui)
                    1
              }
            }

        _ <- customMain mkVty Nothing app ui
        return ()

app :: App UiState () Name
app = App
  { appDraw = drawVm
  , appChooseCursor = focusRingCursor (view uiFocusRing)
  , appHandleEvent = \s e ->
      case e of
        VtyEvent (Vty.EvKey Vty.KEsc []) -> halt s
        VtyEvent vtyE ->
          handleEventLensed s uiContractList handleListEvent vtyE >>= continue
        _ -> continue s
  , appStartEvent = return
  , appAttrMap = const (attrMap Vty.defAttr
                        [ (selectedAttr, Vty.defAttr `Vty.withStyle` Vty.standout)
                        , (dimAttr, Vty.defAttr `Vty.withStyle` Vty.dim)
                        , (borderAttr, Vty.defAttr `Vty.withStyle` Vty.dim)
                        , (wordAttr, fg Vty.yellow)
                        , (boldAttr, Vty.defAttr `Vty.withStyle` Vty.bold)
                        , (activeAttr, Vty.defAttr `Vty.withStyle` Vty.standout)
                        ])
  }

drawVm ui =
  [ vBox
    [ vLimit 20 $ hBox
      [ drawStackPane ui
      , drawBytecodePane ui
      , drawLogPane ui
      ]
    , hBox $
      [ hLimit 32 $ drawTracePane ui
      , drawSolidityPane ui
      ]
    ]
  ]

drawStackPane ui =
  borderWithLabel (txt "Stack") $
    renderList
      (\_ x -> str (show x))
      False
      (view (uiVmState . uiVmStackList) ui)

drawBytecodePane ui =
  borderWithLabel (txt "Bytecode") $
    renderList
      (\active x -> if not active
                    then withDefAttr dimAttr (opWidget x)
                    else withDefAttr boldAttr (opWidget x))
      False
      (view (uiVmState . uiVmBytecodeList) ui)

withHighlight False = withDefAttr dimAttr
withHighlight True  = withDefAttr boldAttr

opWidget (i, x) = hLimit 6 (padLeft Max (str (show i ++ " "))) <+> case x of
  OpStop -> txt "STOP"
  OpAdd -> txt "ADD"
  OpMul -> txt "MUL"
  OpSub -> txt "SUB"
  OpDiv -> txt "DIV"
  OpSdiv -> txt "SDIV"
  OpMod -> txt "MOD"
  OpSmod -> txt "SMOD"
  OpAddmod -> txt "ADDMOD"
  OpMulmod -> txt "MULMOD"
  OpExp -> txt "EXP"
  OpSignextend -> txt "SIGNEXTEND"
  OpLt -> txt "LT"
  OpGt -> txt "GT"
  OpSlt -> txt "SLT"
  OpSgt -> txt "SGT"
  OpEq -> txt "EQ"
  OpIszero -> txt "ISZERO"
  OpAnd -> txt "AND"
  OpOr -> txt "OR"
  OpXor -> txt "XOR"
  OpNot -> txt "NOT"
  OpByte -> txt "BYTE"
  OpSha3 -> txt "SHA3"
  OpAddress -> txt "ADDRESS"
  OpBalance -> txt "BALANCE"
  OpOrigin -> txt "ORIGIN"
  OpCaller -> txt "CALLER"
  OpCallvalue -> txt "CALLVALUE"
  OpCalldataload -> txt "CALLDATALOAD"
  OpCalldatasize -> txt "CALLDATASIZE"
  OpCalldatacopy -> txt "CALLDATACOPY"
  OpCodesize -> txt "CODESIZE"
  OpCodecopy -> txt "CODECOPY"
  OpGasprice -> txt "GASPRICE"
  OpExtcodesize -> txt "EXTCODESIZE"
  OpExtcodecopy -> txt "EXTCODECOPY"
  OpBlockhash -> txt "BLOCKHASH"
  OpCoinbase -> txt "COINBASE"
  OpTimestamp -> txt "TIMESTAMP"
  OpNumber -> txt "NUMBER"
  OpDifficulty -> txt "DIFFICULTY"
  OpGaslimit -> txt "GASLIMIT"
  OpPop -> txt "POP"
  OpMload -> txt "MLOAD"
  OpMstore -> txt "MSTORE"
  OpMstore8 -> txt "MSTORE8"
  OpSload -> txt "SLOAD"
  OpSstore -> txt "SSTORE"
  OpJump -> txt "JUMP"
  OpJumpi -> txt "JUMPI"
  OpPc -> txt "PC"
  OpMsize -> txt "MSIZE"
  OpGas -> txt "GAS"
  OpJumpdest -> txt "JUMPDEST"
  OpCreate -> txt "CREATE"
  OpCall -> txt "CALL"
  OpCallcode -> txt "CALLCODE"
  OpReturn -> txt "RETURN"
  OpDelegatecall -> txt "DELEGATECALL"
  OpSelfdestruct -> txt "SELFDESTRUCT"
  OpDup x -> txt "DUP" <+> str (show x)
  OpSwap x -> txt "SWAP" <+> str (show x)
  OpLog x -> txt "LOG" <+> str (show x)
  OpPush x -> txt "PUSH " <+> withDefAttr wordAttr (str (show x))
  OpUnknown x -> txt "UNKNOWN " <+> str (show x)

drawLogPane ui =
  borderWithLabel (txt "Logs") $
    renderList
      (\_ x -> str (show x))
      False
      (view (uiVmState . uiVmLogList) ui)

drawTracePane ui =
  borderWithLabel (txt "Trace") $
    padRight Max (txt "<test construction>")

drawSolidityPane ui =
  borderWithLabel (txt "Solidity") $
    renderList
      (\active x ->
         withHighlight active $
           txt (case decodeUtf8 x of
                  "" -> " "
                  y -> y))
      False
      (view (uiVmState . uiVmSolidityList) ui)

contractNamePart :: Text -> Text
contractNamePart x = Text.split (== ':') x !! 1

contractPathPart :: Text -> Text
contractPathPart x = Text.split (== ':') x !! 0

drawContractList ui =
  [ hLimit 42 $ vBox
    [ hBorderWithLabel (txt " Contracts ")
    , padLeft (Pad 1) $ renderList
        (\focused y ->
           let suffix = if isUnitTestContract y ui
                        then " (test)"
                        else ""
               name   = txt (contractNamePart y) <+> txt suffix
           in
             (if focused
              then withDefAttr selectedAttr name
              else name)
             <=> withDefAttr dimAttr (txt " " <+> txt (contractPathPart y)))
        (Just ContractListPane == focusGetCurrent (view uiFocusRing ui))
        (view uiContractList ui)
    ] <+> vBorder
  ]

selectedAttr :: AttrName; selectedAttr = "selected"
dimAttr :: AttrName; dimAttr = "dim"
wordAttr :: AttrName; wordAttr = "word"
boldAttr :: AttrName; boldAttr = "bold"
activeAttr :: AttrName; activeAttr = "active"
