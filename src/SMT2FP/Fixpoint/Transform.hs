{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module SMT2FP.Fixpoint.Transform where

import           Control.Exception
import           Control.Lens
import           Control.Monad.State.Lazy
import qualified Data.HashMap.Strict      as M

import           SMT2FP.SMT.Types
import           SMT2FP.Fixpoint.Types

data St = St { _invs          :: M.HashMap Id FQInv
             , _ufs           :: M.HashMap Id FQUF
             , _binds         :: M.HashMap Id FQBind
             , _constraints   :: [FQConstraint]
             , _wfConstraints :: [FQWFConstraint]
             , _n             :: Int
             } 
makeLenses ''St
  
emptySt = St { _invs          = M.empty
             , _ufs           = M.empty
             , _binds         = M.empty
             , _constraints   = []
             , _wfConstraints = []
             , _n             = 0
             }

type S = State St

smt2fp :: [Command] -> St
smt2fp cs = evalState comp emptySt
  where
    comp = do sequence_ (gather <$> cs)
              sequence_ (emitConstraints <$> cs)
              get

gather :: Command -> S ()
gather (DeclareFun{..}) =
  invs %= M.insert dfName (FQInv { invName  = dfName
                                 , invArity = length dfArgs
                                 })
gather (DeclareConst{..}) =
  ufs %= M.insert dcName (FQUF { ufName  = dcName
                               , ufArity = l dcArg
                               })
  where
    l (A ixs _) = length ixs
    l _         = throw $ PassError "uf type should be an array"
gather (Assert t)   = gatherTerm t
gather GetModel     = return ()    
gather CheckSat     = return ()    
gather (SetLogic _) = return ()    

gatherTerm :: Term -> S ()
gatherTerm (Forall{..}) = gatherTerm term
gatherTerm (BinOp{..})  = gatherTerm termL >> gatherTerm termR
gatherTerm (Ands ts)    = sequence_ $ gatherTerm <$> ts
gatherTerm (App{..})    = sequence_ $ gatherTerm <$> fArgs
gatherTerm (Ite{..})    = sequence_ $ gatherTerm <$> [termC, termL, termR]
gatherTerm (Var v)      = do
  n' <- use n; n += 1
  binds %= M.insertWith (\_new old -> old) v (FQBind { bindId   = n'
                                                     , bindName = v
                                                     , bindExpr = FQBoolean True
                                                     })
gatherTerm (Number _)   = return ()
gatherTerm (Boolean _)  = return ()

emitConstraints :: Command -> S ()
emitConstraints _ = return ()



