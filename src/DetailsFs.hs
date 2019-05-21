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
import HtmlWebsite

inputNewFolder = do
  H.div ! A.id "div-new-folder" ! class_ "input-group " $ do
    div' "input-group-prepend" $
      H.span ! class_ "input-group-text" $ iconic "folder"
    input ! class_ "form-control" ! placeholder "Enter name here" 
      ! A.id "new-folder-field" ! name "new_folder" ! type_ "text" ! vmodel "new_folder_name"
    div' "input-group-append" $ 
      H.span ! class_ "input-group-text"! A.id "new-folder-submit" ! vclick "createFolder()" $ "Create"



detailsfs :: [T.FileDetail] -> FilePath -> Html
detailsfs l origin = HB.row $
                     ul ! A.style "width:100%;" ! class_ "list-group" $ do
  lii "list-group-item" ! A.id "li-new-folder" ! vif "show_new_folder" $ inputNewFolder
  -- mapM_ f $ l
  
  li `vfor` "file in filelist" ! class_ "list-group-item" $ do
    H.a ! href "#" ! ca "v-on:click" "fileAction(file)" $ "{{file.path}}"

  
  where lii cl = li ! class_ (cl<>" d-flex justify-content-between align-items-center")
        dispForD e = (if _filetype e == File
                       then ("list-group-item", "")
                       else ("list-group-item list-group-item-info", "/"))
        mkhref e = (toValue $ "?path=" <> (if (_path e) == ".."
                                           then joinPath . init . splitPath $ origin
                                           else origin </> (_path e)))
        f :: T.FileDetail -> Html
        f e = do
          let (cl, suff) = dispForD e
          lii cl $ do
            (H.a ! href (mkhref e)) . toHtml .
              (++suff) 
              $ _path e
            H.div ! class_ "d-flex flex-row-reverse" $ do
              H.span ! class_ "badge badge-primary badge-pill" $ toHtml $
                prettyBytes $ (_size e `Prelude.div` 1)
              if _filetype e == File then mempty else H.span ! class_ "badge badge-secondary" $ do
              -- H.span ! class_ "glyphicon glyphicon-download" $ ""
                H.text "zip"
