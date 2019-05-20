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

data UploadProgress = UplPrg { data_ :: String }
  deriving(Show, Generic)

instance ToJSON UploadProgress where
    -- No need to provide a toJSON implementation.

    -- For efficiency, we write a simple toEncoding implementation, as
    -- the default version uses toJSON.
    toEncoding = genericToEncoding defaultOptions




data Data = Data {
  space :: Space,
  files :: [FileDetail]
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
