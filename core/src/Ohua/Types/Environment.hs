{-# LANGUAGE TemplateHaskell #-}
module Ohua.Types.Environment where

import Universum

import Control.Lens.TH
import Data.Default.Class
import qualified Data.HashSet as HS
import qualified Data.Vector as V

import Ohua.Types.Make
import Ohua.Types.Reference
import Ohua.Types.Stage


declareLenses [d|
    data Options = Options
        { callEnvExpr :: !(Maybe QualifiedBinding)
        , callLocalFunction :: !(Maybe QualifiedBinding)
        , transformRecursiveFunctions :: Bool
        , stageHandling :: StageHandling
        , skipCtrlTransformation :: Bool
        , higherOrderFunctions :: HS.HashSet QualifiedBinding
        }
  |]

-- | Stateful name generator
declareLenses [d|
    data NameGenerator = NameGenerator
        { takenNames :: !(HS.HashSet Binding)
        , simpleNameList :: [Binding]
        }
  |]

-- | The read only compiler environment
declareLenses [d|
    newtype Environment = Environment { options :: Options }
  |]

-- | State of the ohua compiler monad.
declareLenses [d|
    data OhuaState envExpr = OhuaState
        { nameGenerator :: !NameGenerator
        , idCounter :: !FnId
        , envExpressions :: !(V.Vector envExpr)
        }
  |]


type instance SourceType NameGenerator = (HS.HashSet Binding, [Binding])
type instance SourceType (OhuaState envExpr) =
     (NameGenerator, FnId, V.Vector envExpr)


instance UnsafeMake NameGenerator where
    unsafeMake = uncurry NameGenerator


instance Make NameGenerator where
    make = pure . unsafeMake

instance Make (OhuaState envExpr) where
    make (ng, fnid, exprs) = pure $ OhuaState ng fnid exprs


instance Default Options where
    def =
        Options
            Nothing
            Nothing
            False -- for no we always disable this option
            (const (Don'tDump, False))
            False
            HS.empty

instance Default Environment where
    def = Environment def
