module Main where

import           Control.Lens
import           Control.Monad.Except
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Map                   as M
import qualified Data.Vector                as V
import           Fusion
import           System.IO.Unsafe
import           Types

-- Could have use the ISO string aswell,
-- but we would have to project the inner 'property' object...
brusselFeatureId = 54094

main :: IO ()
main = do
  let monolith   = "../files/admin_level_6.geojson"
      absorbable = "../files/admin_level_4.geojson"
  merged <- runExceptT $ geoFusion aggregate monolith absorbable
  case merged of
    Right content -> BS.writeFile "merged.geojson" content
    Left msg      -> putStrLn $ "Unlucky boy: " ++ show msg
  where
    -- Folding behavior
    aggregate first second =
      let targetFeatures    = view features $ second
          isBrusselFeature  = (==) brusselFeatureId
          filterBrusselOnly = V.filter (isBrusselFeature . view featureId)
      in first & features %~ ((V.++) (filterBrusselOnly targetFeatures))
