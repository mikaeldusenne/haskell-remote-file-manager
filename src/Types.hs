{-# LANGUAGE OverloadedStrings, LambdaCase, DeriveGeneric #-}
module Types where

import Text.Blaze.Html5
import GHC.Generics
import Data.Aeson

import Hunix

type Token = String

data PageContent = PageContent{
  monitor :: Html,
  details :: Html,
  currentPath :: Html,
  token :: Token
  }

type FileSize = Integer

data FileDetail = FileDetail {
  path :: String,
  size :: FileSize,
  filetype :: FileType
  }
  deriving(Show, Generic)

data UploadFile = UPLD {
  file_data :: String,
  file :: String,
  file_type :: String,
  first_chunk :: Bool
  }
  deriving (Show, Generic)

instance ToJSON UploadFile where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON UploadFile

data UploadProgress = UplPrg { data_ :: String }
  deriving(Show, Generic)

instance ToJSON UploadProgress where
    toEncoding = genericToEncoding defaultOptions




data Data = Data {
  space :: Space,
  files :: [FileDetail],
  datapath :: String
  }
  deriving(Show, Generic)

instance ToJSON Space where
  toJSON p = Data.Aeson.object [
    "used" .= used p,
    "avail"  .= avail  p ]

instance ToJSON FileType where
  toJSON p = Data.Aeson.object [
    "type" .= show p]

instance ToJSON FileDetail where
    toEncoding = genericToEncoding defaultOptions


instance ToJSON Data where
    toEncoding = genericToEncoding defaultOptions
