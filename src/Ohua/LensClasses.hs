-- |
-- Module      : $Header$
-- Description : Lens classes for ohua
-- Copyright   : (c) Justus Adam 2017. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : sebastian.ertel@gmail.com, dev@justus.science
-- Stability   : experimental
-- Portability : portable

-- This source code is licensed under the terms described in the associated LICENSE.TXT file
module Ohua.LensClasses where

import           Lens.Micro

class HasNameGenerator s a | s -> a where nameGenerator :: Lens' s a
class HasTakenNames s a | s -> a where takenNames :: Lens' s a
class HasSimpleNameList s a | s -> a where simpleNameList :: Lens' s a
class HasIdField s a | s -> a where idField :: Lens' s a
class HasName s a | s -> a where name :: Lens' s a
class HasArguments s a | s -> a where arguments :: Lens' s a
class HasReturnField s a | s -> a where returnField :: Lens' s a
class HasFrameType s a | s -> a where frameType :: Lens' s a
class HasSourceFn s a | s -> a where sourceFn :: Lens' s a
class HasOutVar s a | s -> a where outVar :: Lens' s a
class HasGraph s a | s -> a where graph :: Lens' s a
class HasBindingSupplier s a | s -> a where bindingSupplier :: Lens' s a
class HasIdSupplier s a | s -> a where idSupplier :: Lens' s a
class HasOther s a | s -> a where other :: Lens' s a
class HasNamespace s a | s -> a where namespace :: Lens' s a
class HasIdCounter s a | s -> a where idCounter :: Lens' s a
