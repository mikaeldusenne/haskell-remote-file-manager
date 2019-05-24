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
import Control.Monad (when)

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
  li ! class_ "folder list-group-item" ! vif "currentPath.length > 0" $ do
    vlink "previousPath()" ".."

  li ! vfor' "file" "filelist" ! vclass "{selected: isSelected(file)}" !
    class_ "item list-group-item" !
    vclass "folderClass" !
    -- vclass "file.path==selecteditem?'selected':'not-selected'" !
    ca "@click.self" "selectItem(file.path)" $ do
    vifelse "(isDir(file))"
      (vlink "fileAction(file)" "{{file.path}}")
      (H.span "{{file.path}}")
      
    div' "float-right" $ do
      -- H.div ! vif "isSelected(file)" $ do
      let f click icon = H.span ! vclick click ! class_ "m-2" $
            iconic icon ! class_   "  " ! vif "isSelected(file)" 
      f "fileDownload(file)" "data-transfer-download"
      f "fileDelete(file)" "delete"
      H.span ! class_ " badge m-2 badge-secondary" $ "{{prettyBytes(file.size)}}"
      

  
  where lii cl = li ! class_ (cl<>" d-flex justify-content-between align-items-center")
        dispForD e = (if _filetype e == File
                       then ("list-group-item", "")
                       else ("list-group-item list-group-item-info", "/"))
        mkhref e = (toValue $ "?path=" <> (if (_path e) == ".."
                                           then dirname origin
                                           else origin </> (_path e)))
        f :: T.FileDetail -> Html
        f e = do
          let (cl, suff) = dispForD e
          lii cl $ do
            (H.a ! href (mkhref e)) . toHtml .
              (++suff) 
              $ _path e
            H.div ! class_ "d-flex flex-row-reverse" $ do
              H.span ! class_ "badge badge-secondary" $ toHtml $
                prettyBytes $ (_size e `Prelude.div` 1)
              if _filetype e == File then mempty else H.span ! class_ "badge badge-secondary" $ do
              -- H.span ! class_ "glyphicon glyphicon-download" $ ""
                H.text "zip"
