module Material where

import Linear
import BRDF

data Material = Mat BRDFs deriving Eq