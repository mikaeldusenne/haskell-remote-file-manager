{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module ActionBar where

import Web.Spock hiding (body)
import qualified Web.Spock as W
import Web.Spock.Config
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 hiding (main, map)
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
import Control.Monad

-- statusBarCurrentPath path = div' "navbar-text" $ case path of
--   "" -> H.text "/"
--   path -> do  
--     let l = splitPath path
--     div' "navbar-text" $ 
--       mapM_ f (scanl (++) "" l)
--       where f e = do
--               H.text " / "
--               vlink "actionFolder(filetype.type)" (H.toHtml $ basename e)

statusBarCurrentPath = div' "navbar-text" ! A.id "curent-path" $
  H.span ! vfor "path in currentpathBar" $ do
  H.text " / "
  vlink "cd(path)" "{{prettybasename(path)}}"

actionbar :: Html
actionbar = do -- H.div ! class_ "row" $ do
  -- H.span ! class_ "navbar-text" ! A.id "upload-progress" $ do
  --   "Please select a file and click \"Upload\"."
  --
  H.form ! class_ "form-inline mb-1" ! method "POST" ! ca "onsubmit" "return start_upload();" $ do
  -- div' "row" $ do
    H.span ! customAttribute "v-if" "show_progress_upload" ! class_ "navbar-text" ! A.id "upload-progress" $ do
      "{{upload_status}}"
    H.div ! customAttribute "v-if" "! show_progress_upload" ! A.id "div-input-file" ! class_ "input-group " $ do
      div' "custom-file" $ do
        input ! vchange "set_upload_file($event)" ! class_ "custom-file-input" ! A.style "color:#eee;"
          ! A.id "file-upload-field" ! name "file_upload" ! type_ "file" 
        H.label ! class_ "custom-file-label" ! A.for "file-upload-field" $ "Choose a file"
      div' "input-group-append" $ 
        -- button ! class_ "form-control btn-sm btn btn-outline-secondary"! A.id "file-upload-submit" ! type_ "submit" ! class_ "btn btn-secondary mb-2" $ "Upload"
        -- H.span ! class_ "input-group-text"! A.id "file-upload-submit" $ "Upload"
        H.span ! class_ "input-group-text"! customAttribute "v-on:click" "start_upload()" $ "Upload"
    div' "btn btn-secondary mx-1" ! vclick "show_new_folder = ! show_new_folder" $
      iconic "folder"
      -- img ! src "open-iconic/svg/data-transfer-upload.svg"
        
    -- H.label ! for "filepath" ! class_ "sr-only" $ "File"
    -- H.div ! class_ "from-group" $ do

uploaderjs = do
  script ! src "js/uploader.js" $ mempty




