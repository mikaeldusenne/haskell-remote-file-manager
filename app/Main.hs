{-# LANGUAGE OverloadedStrings, LambdaCase, DeriveGeneric #-}
module Main where

import Control.Monad (forM_)

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
import Network.Wai.Middleware.Static
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Base64 as BS64
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC8

import GHC.Generics
import Data.Aeson

import List
import Lib
import Maths
import HtmlWebsite
import Hunix
import List
import Params
import Uploader
import FileManager
import qualified Monitor 
import Types as T
import System.FilePath.Posix
import DetailsFs

type AppM = SpockM AppDb AppSession AppState ()
type AppAction ctx a = SpockActionCtx ctx AppDb AppSession AppState a

type AppDb = ()
type AppState = ()
type AppSession = ()
type Server a = SpockM () () () a

genToken :: IO [Char]
genToken = return "42"

-- webpage :: [p] -> Html
-- webpage :: Foldable t => t a -> Html
-- webpage :: (Foldable t, ToMarkup a) => t a -> Html
webpage :: PageContent -> Html
webpage pc = docTypeHtml $ do
  H.head $ do
    bootstrapCss
    css "css/style.css"
    H.title "Hello"
  body ! A.style "padding-top: 70px;" $ do
    nav ! class_ "navbar navbar-dark bg-primary fixed-top " $ do
      -- H.div ! class_ "container" $ do
      T.monitor pc
      T.currentPath pc
      uploader
    H.div ! class_ "container" $ do
      -- hr
      -- HB.row $
        -- column12 $
      T.details pc
    jquery
    bootstrapJs
    uploaderjs
    script $ toHtml $ "var token='" <> token pc <> "';"
    script $ "document.addEventListener('DOMContentLoaded', init)"

    

-- appFs :: String -> Html
-- appFs dir = do
--   fs <- liftIO $ listFiles
--   detailsfs fs

securePath ee
  | beginWith "/" ee || (any (beginWith "..") $ splitPath ee) = ""
  | otherwise = ee

app :: AppM
app = do
  middleware $ staticPolicy (addBase "static")
  get ("/") $ do
    path <- (\case
                Just ee -> securePath ee
                Nothing -> ""
            ) <$> W.param "path"
    token <- liftIO genToken
    liftIO $ print $ "accessing <" <> path <> ">"
    spc <- liftIO free

    let action = do
          let fullpath = directory </> path
          isfile <- liftIO $ (isRegularFile <$> getFileStatus fullpath)
          if isfile
            then file "" fullpath
            else do
            fileListing <- (`detailsfs`path) <$> liftIO (FileManager.listFiles path)
            let pct = pcent spc
            let pc = T.PageContent{
                  monitor       = Monitor.monitor spc pct,
                  T.details     = fileListing,
                  T.currentPath = H.span ! class_ "navbar-text" $ toHtml $ f ("/" ++ path),
                  token         = token}
                  where f s = "/ " ++ (concatWith " / " $ splitOn (=='/') s)
  
            W.html . L.toStrict . R.renderHtml $ webpage $ pc
    action
  post ("/fileupload") $ do
    liftIO $ print "upload"

    filename <- W.param' "file"
    -- liftIO $ print $ filename

    ddd <- tail . dropWhile (/=',') <$> (W.param' "file_data" :: AppAction () String)
    let dd = BS64.decode . BSC8.pack $ ddd
    case dd of Right d -> liftIO $ BS.appendFile (directory </> filename) d
               Left error -> liftIO $ do
                 print $ ddd
                 print $ "ERROR     " ++ error ++ "     ERROR"
    W.json $ UplPrg "ok data received"


main :: IO ()
main = do
  spockCfg <- defaultSpockCfg () PCNoDatabase ()
  runSpock 8080 (spock spockCfg app)


