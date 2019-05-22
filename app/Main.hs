{-# LANGUAGE OverloadedStrings, LambdaCase, DeriveGeneric #-}
module Main where

import Control.Monad (forM_)

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
import Network.Wai.Middleware.Static
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Base64 as BS64
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC8
import System.Directory (createDirectory)
import qualified Data.Text as Txt
import GHC.Generics
import Data.Aeson
import Data.IORef
import Control.Lens hiding (element, Empty)

import Trees
import List
import Misc
import Lib
import Maths
import HtmlWebsite
import Hunix hiding (path)
import List
import Params
import ActionBar
import FileManager
import qualified Monitor 
import Types as T
import System.FilePath.Posix
import DetailsFs

type AppM = SpockM AppDb AppSession (IORef StateApp) ()
-- type AppM = SpockM AppDb AppSession () ()
type AppAction ctx a = SpockActionCtx ctx AppDb AppSession AppState a

type AppDb = ()
-- type AppState = ()
data AppState = AppState { myAppState :: StateApp }

type AppSession = ()
type Server a = SpockM () () () a

genToken :: IO [Char]
genToken = return "42"


csss = do
  bootstrapCss
  mapM css [
    "//unpkg.com/bootstrap-vue@latest/dist/bootstrap-vue.min.css",
    "css/style.css",
    "open-iconic/font/css/open-iconic-bootstrap.css"
    ]

jss = do
  jquery
  bootstrapJs
  mapM js [
    -- "https://cdnjs.cloudflare.com/ajax/libs/vue/2.6.10/vue.js",
    "https://unpkg.com/vue/dist/vue.js",
    "http://unpkg.com/portal-vue",
    "//unpkg.com/bootstrap-vue@latest/dist/bootstrap-vue.min.js",
    "https://cdnjs.cloudflare.com/ajax/libs/vue-resource/1.5.1/vue-resource.min.js",
    "https://unpkg.com/vue-router/dist/vue-router.js",
    "js/script.js"]
  uploaderjs
  script $ "document.addEventListener('DOMContentLoaded', init)"


-- webpage :: [p] -> Html
-- webpage :: Foldable t => t a -> Html
-- webpage :: (Foldable t, ToMarkup a) => t a -> Html
webpage :: PageContent -> Html
webpage pc = docTypeHtml $ do
  H.head $ do
    csss
    H.title ""
  body ! A.style "padding-top: 70px;" $ do
    H.div ! A.id "app" $ do
      nav ! class_ "navbar navbar-dark bg-primary fixed-top " $ do
        -- H.div ! class_ "container" $ do
        vifelse "refreshing"
          (div' "" "")
          ( do
              T.monitor pc
              T.currentPath pc
          )
        
        
        actionbar
         
         
      H.div ! class_ "container" $ do
        -- hr
        -- HB.row $
          -- column12 $
        T.details pc -- ! vif "! refreshing"
    jss
    script $ toHtml $ "var token='" <> token pc <> "';"

    

-- appFs :: String -> Html
-- appFs dir = do
--   fs <- liftIO $ listFiles
--   detailsfs fs

rootRoute = do
    path <- securePath . just_or_default "" <$> W.param "path"
    token <- liftIO genToken
    liftIO $ print $ "root route: accessing <" <> path <> ">"
    spc <- liftIO free

    let action = do
          let fullpath = directory </> path
          isfile <- liftIO $ (isRegularFile <$> getFileStatus fullpath)
          if isfile
            then W.setHeader "Content-disposition" (Txt.pack $ "attachment; filename="<>basename path) >> W.file "" fullpath
            else do
            -- fileListing <- (`detailsfs`path) <$> liftIO (FileManager.listFiles path)
            let pct = pcent spc
            let pc = T.PageContent{
                  monitor       = Monitor.monitor spc pct,
                  T.details     = detailsfs [] path,
                  T.currentPath = H.span ! class_ "navbar-text" $ "/ {{addpathspaces(currentPath)}}",
                  token         = token}
                  -- where f s = "/ " ++ (concatWith " / " $ splitOn (=='/') s)
  
            W.html . L.toStrict . R.renderHtml $ webpage $ pc
    action

newFolderRoute = do
    liftIO $ print "new folder"
    W.body >>= liftIO . print
    e <- securePath . folderpath <$> jsonBody'
    liftIO . print $ e
    liftIO $ createDirectory $ directory </> e
    W.json $ UplPrg "ok"

fileUploadRoute = do
    liftIO $ print "upload"
    -- W.param' "test" >>= liftIO . putStrLn
    -- W. >>= liftIO . print
    e <- jsonBody'
    liftIO $ print $ ("@@@@@@@@", T.file e, first_chunk e)
    
    -- W.files >>= liftIO . print
    -- W.body >>= liftIO . print
    -- W.paramsPost >>= liftIO . print
    -- W.paramsGet >>= liftIO . print
    -- W.params >>= liftIO . print
    -- filename <- W.param' "file"
    -- liftIO $ print $ filename

    -- ddd <- tail . dropWhile (/=',') <$> (W.param' "file_data" :: AppAction () String)
    let ddd = tail . dropWhile (/=',') $ file_data e
    let dd = BS64.decode . BSC8.pack $ ddd
        mode = if first_chunk e then BS.writeFile else BS.appendFile
    case dd of Right d -> liftIO $ mode (directory </> T.file e) d
               Left error -> liftIO $ do
                 print $ ddd
                 print $ "ERROR     " ++ error ++ "     ERROR"
    W.json $ UplPrg "ok data received"



allRoute = do
    p <- (\case
            Just ee -> securePath ee
            Nothing -> ""
        ) <$> W.param "path"
    liftIO $ print $ "api call path <" <> p <> ">"
    
    
    state <- getState >>= (liftIO . (runState <$>) . readIORef)
    let fs = findTreeByPath p state
        children = case fs of Nothing -> []
                              Just (Leaf _) -> []
                              Just (Node e l) -> map treeValue l
    -- fs <- liftIO $ FileManager.listFiles path

    
    spc <- liftIO free
    W.json $ Data {
      space    = spc,
      T.files  = map (path %~ basename) $ children, -- fmap (over (path . treeValue) basename) children,
      datapath = p
      }


app :: AppM
app = do
  middleware $ staticPolicy (addBase "static")
  get ("/") rootRoute
  post ("api/newfolder") $ newFolderRoute
  post ("api/fileupload") $ fileUploadRoute
  get "api/all" $ allRoute
    

main :: IO ()
main = do
  print "main"
  st <- listTree (Path directory Dir) >>= ((StateApp <$>) .  traverse computeState )
  -- print st
  let -- aaa :: Tree FileDetail
      -- aaa = over (T.path . treeValue) basename $ runState $ st
      bb :: FileDetail
      bb = nodeValue $ runState st
      cc = path .~ "o" $ bb

  -- print $ findTreeByPath "eeeeeeeeeeee" $ runState st
  ref <- newIORef $ st
  spockCfg <- defaultSpockCfg () PCNoDatabase ref
  let e = spc_initialState spockCfg
  runSpock 8080 (spock spockCfg app)


