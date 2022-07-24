
module Prog (module M) where

import Prog.AST as M
import Prog.Parse as M (parseProg)
import Prog.Infer as M (inferType)
