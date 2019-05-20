{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module ActionBar where

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


-- uploader :: Html
-- uploader = do -- H.div ! class_ "row" $ do
--   elemt H.form "" "" $ do
--     div' "from-row" $ do
--       div' "col-auto my-1" $
--        H.span ! class_ "navbar-text" ! A.id "upload-progress" $ do
--          "Please select a file and click \"Upload\"."
--       div' "col-auto my-1" $
--        input ! class_ "form-control-file "
--          ! A.id "file-upload-field" ! type_ "file" ! name "file_upload"
--       div' "col-auto my-1" $
--         button ! class_ "btn btn-light form-control btn-sm"! A.id "file-upload-submit" ! type_ "submit" $ "Upload"


-- uploaderjs = do
--   script ! src "js/uploader.js" $ mempty

-- uploader :: Html
-- uploader = do -- H.div ! class_ "row" $ do
--   elemt H.form "width:20%" "" $ do
--     div' "from-row align-items-center" $ do
--       div' "col-xs-3 my-1" $
--        H.span ! class_ "navbar-text" ! A.id "upload-progress" $ do
--          "Please select a file and click \"Upload\"."
--       div' "col-xs-3 my-1" $
--        input ! class_ "form-control-file "
--          ! A.id "file-upload-field" ! type_ "file" ! name "file_upload"
--       div' "col-xs-3 my-1" $
--         button ! class_ "form-control btn-sm"! A.id "file-upload-submit" ! type_ "submit" ! class_ "btn btn-secondary mb-2" $ "Upload"


-- uploaderjs = do
--   script ! src "js/uploader.js" $ mempty


actionbar :: Html
actionbar = do -- H.div ! class_ "row" $ do
  -- H.span ! class_ "navbar-text" ! A.id "upload-progress" $ do
  --   "Please select a file and click \"Upload\"."
  --
  H.form ! class_ "form-inline mb-1" $ do
  --div' "row" $ do
    H.span ! class_ "navbar-text" ! A.id "upload-progress" $ do
      "Please select a file and click \"Upload\"."
    H.div ! A.id "div-input-file" ! class_ "input-group " $ do
      div' "custom-file" $ do
        input ! class_ "custom-file-input" ! A.style "color:#eee;"
          ! A.id "file-upload-field" ! name "file_upload" ! type_ "file" 
        H.label ! class_ "custom-file-label" ! A.for "file-upload-field" $ "Choose a file"
      div' "input-group-append" $ 
        -- button ! class_ "form-control btn-sm btn btn-outline-secondary"! A.id "file-upload-submit" ! type_ "submit" ! class_ "btn btn-secondary mb-2" $ "Upload"
        H.span ! class_ "input-group-text"! A.id "file-upload-submit" $ "Upload"
    button ! class_ "btn btn-secondary mx-1" ! A.onclick "newfolder_show()" $
      iconic "folder"
      -- img ! src "open-iconic/svg/data-transfer-upload.svg"
        
    -- H.label ! for "filepath" ! class_ "sr-only" $ "File"
    -- H.div ! class_ "from-group" $ do

uploaderjs = do
  script ! src "js/uploader.js" $ mempty




