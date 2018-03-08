{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module SMT2FP.Fixpoint.Transform where

import           Control.Lens
import           Control.Monad.State.Lazy
import qualified Data.HashMap.Strict      as M

import           SMT2FP.Fixpoint.Types

data St = St { _invs          :: [FQInv]
             , _ufs           :: [FQUF]
             , _binds         :: [FQBind]
             , _constraints   :: [FQConstraint]
             , _wfConstraints :: [FQWFConstraint]
             } 

makeLenses ''St

