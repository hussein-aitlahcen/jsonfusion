-- Fusion.hs ---

-- Copyright (C) 2018 Hussein Ait-Lahcen

-- Author: Hussein Ait-Lahcen <hussein.aitlahcen@gmail.com>

-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License
-- as published by the Free Software Foundation; either version 3
-- of the License, or (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program. If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}

module Fusion where

import           Control.Lens
import           Control.Monad.Except
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Vector                as V
import           Types

type MonadFusion = MonadError FusionError
type Content     = BS.ByteString
type Aggregate   = FeatureCollection -> FeatureCollection -> FeatureCollection

decodeE :: (MonadFusion m) => Content -> m FeatureCollection
decodeE bs = case eitherDecode bs of
               Right value -> pure value
               Left msg    -> throwError $ InvalidScheme msg

geoFusion ::
     (MonadIO m, MonadFusion m)
  => Aggregate
  -> FilePath
  -> FilePath
  -> m Content
geoFusion aggregate monolithPath absorbablePath = do
   monolithContent <- liftIO $ BS.readFile monolithPath
   targetContent <- liftIO $ BS.readFile absorbablePath
   monolith <- decodeE monolithContent
   absorbable <- decodeE targetContent
   let aggregat = aggregate monolith absorbable
   pure . encode $ aggregat
