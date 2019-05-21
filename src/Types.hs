{-# LANGUAGE OverloadedStrings, LambdaCase, DeriveGeneric, TemplateHaskell #-}
module Types where

import Text.Blaze.Html5
import GHC.Generics
import Data.Aeson
import Control.Monad
import qualified Control.Applicative.Lift
import qualified Control.Applicative
import Hunix hiding (path)
import Data.Traversable
import Trees
import Control.Lens hiding (element, Empty, (.=))


type Token = String

data PageContent = PageContent{
  monitor :: Html,
  details :: Html,
  currentPath :: Html,
  token :: Token
  }

type FileSize = Integer


data FileDetail = FileDetail {
  _path :: String,
  _size :: FileSize,
  _filetype :: FileType
  }
  deriving(Show, Generic, Eq)
makeLenses ''FileDetail

data NewFolder = NewFolder{ folderpath :: String }
  deriving (Show, Generic)
instance FromJSON NewFolder

data UploadFile = UPLD {
  file_data :: String,
  file :: String,
  file_type :: String,
  first_chunk :: Bool
  }
  deriving (Show, Generic)
instance FromJSON UploadFile

instance ToJSON UploadFile where
    toEncoding = genericToEncoding defaultOptions


data UploadProgress = UplPrg { data_ :: String }
  deriving(Show, Generic)

instance ToJSON UploadProgress where
    toEncoding = genericToEncoding defaultOptions


-- data Tree a = Leaf a | Node a [Tree a]
--   deriving (Show, Generic, Eq)

-- treeValue (Leaf a) = a
-- treeValue (Node a _) = a

-- data CrazyTree a = CrazyLeaf a | CrazyNode a (CrazyTree (CrazyTree a))
--   deriving (Show, Generic, Eq)


-- instance Functor Tree where
--   fmap f (Leaf e) = Leaf (f e)
--   fmap f (Node e l) = Node (f e) $ fmap (fmap f) l

-- instance Foldable Tree where
--   foldMap f (Node a l) = f a `mappend` foldMap (foldMap f) l
--   foldMap f (Leaf a)   = f a

-- instance Traversable Tree where
--   traverse f (Node a l) = Node <$> f a <*> traverse (traverse f) l
--   traverse f (Leaf a)   = Leaf <$> f a

data Data = Data {
  space :: Space,
  files :: [FileDetail],
  datapath :: String
  }
  deriving(Show, Generic)

newtype StateApp = StateApp { runState :: Tree FileDetail }
  deriving(Show, Generic)

instance ToJSON Space where
  toJSON p = Data.Aeson.object [
    "used" .= used p,
    "avail"  .= avail  p ]

instance ToJSON FileType where
  toJSON p = Data.Aeson.object [
    "type" .= show p]

instance ToJSON FileDetail where
  toJSON (FileDetail pth sz ft) =
    Data.Aeson.object ["path" .= pth, "size" .= sz,
           "filetype" .= ft]


instance ToJSON StateApp where
    toEncoding = genericToEncoding defaultOptions

instance ToJSON Data where
    toEncoding = genericToEncoding defaultOptions
