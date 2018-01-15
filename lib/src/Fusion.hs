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
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Fusion where

import           Control.Lens
import           Control.Monad.Except
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Map                   as M
import qualified Data.Vector                as V
import           System.IO.Unsafe
import           Types

type Content = BS.ByteString

decodeE :: (MonadFusion m) => Content -> m FeatureCollection
decodeE bs = case eitherDecode bs of
               Right value -> pure value
               Left msg    -> throwError $ InvalidScheme msg

geoFusion ::
     (MonadIO m, MonadFusion m)
  => FilePath
  -> FilePath
  -> (FeatureCollection -> FeatureCollection -> FeatureCollection)
  -> m Content
geoFusion source target aggregate = do
   sourceContent <- liftIO $ BS.readFile source
   targetContent <- liftIO $ BS.readFile target
   monolith <- decodeE sourceContent
   absorbable <- decodeE targetContent
   let aggregat = aggregate monolith absorbable
   pure . encode $ aggregat
