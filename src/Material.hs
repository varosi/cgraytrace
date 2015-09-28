module Material where
import BRDF

data Material = Mat BRDFs deriving Eq