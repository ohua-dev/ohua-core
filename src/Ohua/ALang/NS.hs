module Ohua.ALang.NS where


import qualified Data.HashMap.Strict as HM
import           Ohua.ALang.Lang
import           Ohua.Types


-- | A namespace as defined by the ohua API. It has a name, a list of dependencies and aliasings,
-- defined expressions (currently constrained to lambdas/algos) and optionally ends with an executable expression.
data Namespace sym = Namespace Binding [(Binding, [Binding])] (HM.HashMap Binding (Expr sym)) (Maybe (Expr sym))
