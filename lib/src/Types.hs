
-- Types.hs ---

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

{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Types where

import           Control.Applicative
import           Control.Lens.TH
import           Control.Monad.Except
import           Data.Aeson
import           Data.ByteString
import           Data.Map
import           Data.Text
import           Data.Vector
import           GHC.Generics

type MonadFusion = MonadError FusionError

data FusionError = InvalidScheme String deriving Show

type ObjectType = Text
type Geocoding  = Value
type Features   = Vector Feature

type FeatureId   = Int
type OsmType     = Text
type FeatureName = Text

data FeatureCollection = FeatureCollection
  { _typeCollection :: ObjectType
  , _geocoding      :: Geocoding
  , _features       :: Features
  } deriving Show

data Feature = Feature
  { _featureId   :: FeatureId
  , _osmType     :: OsmType
  , _typeFeature :: ObjectType
  , _name        :: FeatureName
  , _properties  :: Map String String
  , _geometry    :: Value
  } deriving Show

instance ToJSON FeatureCollection where
  toJSON (FeatureCollection tc dc ft) =
    object ["features" .= ft, "geocoding" .= dc, "type" .= tc]

instance FromJSON FeatureCollection where
  parseJSON (Object o) =
    FeatureCollection <$> o .: "type" <*> o .: "geocoding" <*> o .: "features"

instance ToJSON Feature where
  toJSON (Feature fid osmt tf n props geom) =
    object
      [ "id" .= fid
      , "osm_type" .= osmt
      , "type" .= tf
      , "name" .= n
      , "properties" .= props
      , "geometry" .= geom
      ]

instance FromJSON Feature where
  parseJSON (Object o) =
      Feature <$> o .: "id"
              <*> o .: "osm_type"
              <*> o .: "type"
              <*> o .: "name"
              <*> o .: "properties"
              <*> o .: "geometry"

makeLenses ''FeatureCollection
makeLenses ''Feature
