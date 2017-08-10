{-|
Module      : $Header$
Description : IR manipulation and Java conversion.
Copyright   : (c) Justus Adam 2017. All Rights Reserved.
License     : EPL-1.0
Maintainer  : dev@justus.science
Stability   : experimental
Portability : portable

This source code is licensed under the terms described in the associated LICENSE.TXT file.
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash  #-}
module Ohua.IR where

import           Data.Function       (on)
import qualified Data.HashMap.Strict as HM
import           Data.String
import           Lens.Micro
import           Ohua.ALang.Lang
import           Ohua.LensClasses
import           Ohua.Types
import           Ohua.Util

type IRFnId = FnId

unIRFnId :: IRFnId -> Int
unIRFnId = unFnId

class Consumes a where
    consumes :: Binding -> a -> Bool

class Returns a where
    returns :: Binding -> a -> Bool

class ToIRFnId a where
    toIRFnId :: a -> IRFnId


data Argument
    = ABinding !Binding
    | AEnv !HostExpr

_ABinding :: Prism' Argument Binding
_ABinding = prism' ABinding $ \case { ABinding b -> Just b; _ -> Nothing }

_AEnv :: Prism' Argument HostExpr
_AEnv = prism' AEnv $ \case { AEnv a -> Just a; _ -> Nothing }

instance Show Argument where
    show (ABinding b) = show b
    show (AEnv o)     = "fromEnv " ++ show o
instance Consumes Argument where
    consumes bnd (ABinding bnd0) = bnd == bnd0
    consumes _ _                 = False
instance ExtractBindings Argument where
    extractBindings (ABinding bnd) = [bnd]
    extractBindings _              = []


instance IsString Argument where fromString = ABinding . fromString


type Arguments = [Argument]

type FnReturn = Assignment

instance Returns FnReturn where
    returns bnd (Direct bnd2)   = bnd == bnd2
    returns bnd (Destructure l) = bnd `elem` l


data IRFn = IRFn
    { irfnIdField     :: !IRFnId
    , irfnName        :: !FnName
    , irfnArguments   :: !Arguments
    , irfnReturnField :: !FnReturn
    }

instance Show IRFn where
    show fn = unwords ["fn", show (fn^.name) ++ "(" ++ show (fn^.idField) ++ ")", show (fn^.arguments), show (fn^.returnField)]

instance HasIdField IRFn IRFnId where idField = lens irfnIdField (\s a -> s {irfnIdField=a})
instance HasName IRFn FnName where name = lens irfnName (\s a -> s {irfnName=a})
instance HasArguments IRFn Arguments where arguments = lens irfnArguments (\s a -> s {irfnArguments=a})
instance HasReturnField IRFn FnReturn where returnField = lens irfnReturnField (\s a -> s {irfnReturnField=a})

instance Returns IRFn where
    returns bnd fn = returns bnd (fn^.returnField)
instance Consumes IRFn where
    consumes bnd fn = any (consumes bnd) (fn^.arguments)
instance Eq IRFn where
    (==) = (==) `on` (^.idField)
instance ExtractBindings IRFn where extractBindings fn = extractBindings (fn^.arguments) ++ extractBindings (fn^.returnField)



type IRGraph = [IRFn]


type CtxtType = FnName

data CtxtFrame = CtxtFrame
    { ctxtFrameFrameType :: !CtxtType
    , ctxtFrameSourceFn  :: !IRFnId
    , ctxtFrameOutVar    :: !Int
    } deriving (Show)

instance HasFrameType CtxtFrame CtxtType where frameType = lens ctxtFrameFrameType (\s a -> s {ctxtFrameFrameType=a})
instance HasSourceFn CtxtFrame IRFnId where sourceFn = lens ctxtFrameSourceFn (\s a -> s {ctxtFrameSourceFn=a})
instance HasOutVar CtxtFrame Int where outVar = lens ctxtFrameOutVar (\s a -> s {ctxtFrameOutVar=a})


type CtxtStack = [CtxtFrame]


type CtxtMap = HM.HashMap IRFnId CtxtStack


instance ToIRFnId IRFnId where toIRFnId = id
instance ToIRFnId IRFn where toIRFnId = (^.idField)
