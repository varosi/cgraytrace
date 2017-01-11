module Material where
import BRDF

-- |Currently we support only materials with single BRDF
data Material = Mat BRDFs deriving (Eq, Show)