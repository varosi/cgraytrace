module Material where
import           BRDF

-- |Currently we support only materials with single BRDF
newtype Material = Mat BRDFs deriving (Eq, Show)
