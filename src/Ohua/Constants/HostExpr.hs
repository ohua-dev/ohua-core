module Ohua.Constants.HostExpr (true, unit) where


import Ohua.Types



unit :: HostExpr
unit = unsafeMake (-1)

true :: HostExpr
true = unsafeMake (-2)
