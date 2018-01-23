-- JsonFusion.hs ---

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

{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module JsonFusion
  ( jsonFusion
  ) where

import           Control.Monad.Except       (MonadError, throwError)
import           Data.Aeson                 (FromJSON, ToJSON, eitherDecode,
                                             encode)
import qualified Data.ByteString.Lazy.Char8 as BS

data FusionError = InvalidScheme String deriving Show
type MonadFusion = MonadError FusionError
type Content     = BS.ByteString
type Aggregate a = a -> a -> a

decodeE :: (MonadFusion m, FromJSON a, ToJSON a) => Content -> m a
decodeE bs = case eitherDecode bs of
               Right value -> pure value
               Left msg    -> throwError $ InvalidScheme msg

jsonFusion :: (MonadFusion m, FromJSON a, ToJSON a)
  => Aggregate a
  -> Content
  -> Content
  -> m Content
jsonFusion aggregate monolithContent absorbableContent = do
  monolith   <- decodeE monolithContent
  absorbable <- decodeE absorbableContent
  let aggregat = aggregate monolith absorbable
  pure . encode $ aggregat
