module SMT2FP.Parser where

data Command = SetLogic String
             | DeclareFun { commandName :: String
                          , commandArgs :: [String]
                          , dfRet  :: String
                          }
             | DeclareConst { commandName :: String
                            , commandArgs :: [String]
                            }
             | Assert Term
             | CheckSat
             | GetModel

data SortedVar = SortedVar { svSymbol :: String
                           , svSort   :: String
                           }

data Term = Forall [SortedVar] Term
