module SMT2FP.Fixpoint.Types where

import SMT2FP.SMT.Types

data FQInv = FQInv { invName  :: Id
                   , invArity :: Int
                   }

data FQUF = FQUF { ufName  :: Id
                 , ufArity :: Int
                 }

data FQBinOp = FQIMPLIES -- ==>
             | FQEQU     -- ==
             | FQPLUS    -- +
             | FQGE      -- >=

data FQExpr = FQBinOp   { fqExpOp :: FQBinOp
                        , fqExpL :: FQExpr
                        , fqExpR :: FQExpr
                        }
            | FQAnds    [FQExpr]
            | FQInvCall { callInv  :: FQInv 
                        , callArgs :: [Id]
                        }
            | FQUFCall  { callUF    :: FQUF
                        , callArgs  :: [Id]
                        }
            | FQVar     Id
            | FQNumber  Int
            | FQBoolean Bool

data FQBind = FQBind { bindId   :: Int
                     , bindName :: Id
                     }

data FQConstraint = FQConstraint { contraintId     :: Int 
                                 , constraintBinds :: [FQBind]
                                 , constraintLhs   :: FQExpr
                                 , constraintRhs   :: FQExpr
                                 }

data FQWFConstraint = FQWFConstraint { wfInv :: FQInv
                                     }
