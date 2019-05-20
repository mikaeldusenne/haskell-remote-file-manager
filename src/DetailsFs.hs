{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module DetailsFs where

import Web.Spock hiding (body)
import qualified Web.Spock as W
import Web.Spock.Config
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 hiding (main)
import qualified Text.Blaze.Html.Renderer.Text as R
import Text.Blaze.Html5.Attributes as A
import qualified Data.Text.Lazy as L
import Data.String (fromString, IsString)
import qualified Text.Blaze.Bootstrap as HB
import Control.Monad.IO.Class (liftIO)
import System.PosixCompat.Files

import Lib
import Maths
import Hunix
import List
import Params

import FileManager
import qualified Monitor 
import Types as T
import System.FilePath.Posix


detailsfs :: [T.FileDetail] -> FilePath -> Html
detailsfs l origin = HB.row $
                     ul ! A.style "width:100%;" ! class_ "list-group" $ mapM_ f $ l
  where dispForD e = (if T.filetype e == File
                       then ("list-group-item", "")
                       else ("list-group-item list-group-item-info", "/"))
        mkhref e = (toValue $ "?path=" <> (if (T.path e) == ".."
                                           then joinPath . init . splitPath $ origin
                                           else origin </> (T.path e)))
        f :: T.FileDetail -> Html
        f e = do
          let (cl, suff) = dispForD e
          li ! class_ (cl<>" d-flex justify-content-between align-items-center") $ do
            (H.a ! href (mkhref e)) . toHtml .
              (++suff) 
              $ T.path e
            H.div ! class_ "d-flex flex-row-reverse" $ do
              H.span ! class_ "badge badge-primary badge-pill" $ toHtml $
                prettyBytes $ (T.size e `Prelude.div` 1)
              if T.filetype e == File then mempty else H.span ! class_ "badge badge-secondary" $ do
              -- H.span ! class_ "glyphicon glyphicon-download" $ ""
                H.text "zip"
