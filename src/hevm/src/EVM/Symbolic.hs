{-# Language GADTs #-}
{-# Language NamedFieldPuns #-}
{-# Language DataKinds #-}

module EVM.Symbolic where

import Control.Lens
import EVM.Stepper
import EVM
import EVM.Exec
import EVM.Op
import EVM.TTY --todo: factor out stepping mechanisms properly
import Data.SBV.Trans.Control hiding (sat)
import Data.SBV.Trans
import Control.Monad.IO.Class
import Control.Monad.State.Class (MonadState)
import qualified Control.Monad.State.Class as State
import Control.Monad.State.Strict (runState, execState, unless)
import Data.Maybe (isJust)

-- execSymbolic :: VM -> IO ([VMResult])
-- execSymbolic vm = do
--   let branch = execState (execWhile isNotJump) vm
--   case view result branch of
--     Just x -> return [x]
--     Nothing -> do
--       let Just cond = branch^.state.stack ^? ix 1
--       runSMT $ do constrain $ cond ./= 0
--                   query $ do cs <- checkSat
--                              case cs of
--                                Unk   -> error "Solver said unknown!"
--                                Unsat -> _ --return Nothing -- no solution!
--                                Sat   -> set (state.stack) vm


execSymbolic :: VM -> IO [VMResult]
execSymbolic vm = do
  let branch = execState (execWhile isNotJump) vm
  case view result branch of
    Just x -> return [x]
    Nothing -> do
      print $ "possible branching point at pc: " <> show (view (state.pc) branch)
      let Just cond = branch^.state.stack ^? ix 1
          checkNonZero = do constrain $ cond ./= 0
                            query $ checkSat
      smtResult <- runSMT checkNonZero
      case smtResult of
        Unk   -> error "Solver said unknown!"
        Unsat -> 
                 -- the jump condition must be 0.
                 -- we change it to 0 on the stack;
                 -- step once and then recurse.
                 let vm1 = branch & state.stack.(ix 1) .~ 0
                     vm2 = execState exec1 vm1
                 in do print $ "but smt says jump condition is 0"
                       execSymbolic vm2
                          
        Sat   -> -- the jump condition cannot be 0!
                 -- we it to 1  on the stack;
                 -- step once and then recurse
                 let vm1 = branch & state.stack.(ix 1) .~ 1
                     vm2 = execState exec1 vm1
                 in do print $ "but smt says jump condition is non-zero"
                       execSymbolic vm2
--runSMT $ query $ do _



-- analyzeCond :: SWord 256 -> Symbolic (
-- analyzeCond s = do constrain s .== 0
--                    query $ do cs <- checkSat
--                               case cs of
--                                 Unk   -> error "Solver said unknown!"
--                               Unsat -> return Nothing -- no solution!
-- Sat   -> -- Query the values:
-- com

--  postvms <- head $ mapM (snd . runState (execWhile isNotJump)) vms
--  mapM (execWhile isNotJump)
--  vm <- head $ State.get
 --  case postvms^.result of
 --    Nothing -> let Just cond = postvms^.state.stack ^? ix 1
 --               in liftIO $ runSMT $ do canJump <- sat (cond .== 0)
 --                                       _
 --    Just x -> return [x]
 -- a <- State.state _-- State.state $ do v <- execWhile isNotJump
 --                                return v
                      
--   return $ view result vm

explore :: (MonadState [VM] m) => m [VMResult]
explore = error "hmm"

isNotJump :: VM -> Bool
isNotJump vm = vmOp vm /= Just OpJumpi ||
  case vm^.state.stack ^? ix 1 of
    Just c -> isConcrete c
    Nothing -> error "malformed stack"


finished :: VM -> Bool
finished vm = isJust (view result vm)
