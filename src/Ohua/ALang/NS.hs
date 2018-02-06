module Ohua.ALang.NS where


import qualified Data.HashMap.Strict as HM
import           Ohua.ALang.Lang
import           Ohua.Types

data FunAnn binding = FunAnn
  { argTypes :: [TyExpr binding]
  , retType  :: TyExpr binding
  }

-- | A namespace as defined by the ohua API. It has a name, a list of dependencies and aliasings,
-- defined expressions (currently constrained to lambdas/algos) and optionally ends with an executable expression.
data Namespace decl = Namespace
    { nsName        :: NSRef
    , nsAlgoImports :: [(NSRef, [Binding])]
    , nsSfImports   :: [(NSRef, [Binding])]
    , nsDecls       :: HM.HashMap Binding decl
    }
