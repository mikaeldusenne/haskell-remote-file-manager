{-# LANGUAGE OverloadedStrings, LambdaCase, DeriveGeneric #-}
module Main where

import Control.Monad (forM_)

import Web.Spock hiding (body)
import qualified Web.Spock as W
import Web.Spock.Config
-- import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 as H hiding (main, map, docType, docTypeHtml)
import qualified Text.Blaze.Html5 as H (docTypeHtml)
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
import Control.Monad
import Data.Aeson
import Data.IORef
import Control.Lens hiding (element, Empty, (<.>))
import System.IO.Temp
import Network.HTTP.Types.Status
import System.Directory

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

genToken :: IO String
genToken = return "42"

io ::  IO a -> ActionCtxT () (WebStateM AppDb AppSession (IORef StateApp)) a
io = liftIO


csss = do
  bootstrapCss
  mapM css [
    "//unpkg.com/bootstrap-vue@latest/dist/bootstrap-vue.min.css",
    "/open-iconic/font/css/open-iconic-bootstrap.css",
    "/css/style.css"
    ]

jss = do
  jquery
  bootstrapJs
  mapM js [
    -- "https://cdnjs.cloudflare.com/ajax/libs/vue/2.6.10/vue.js",
    "https://unpkg.com/vue@2.6.14",
    "http://unpkg.com/portal-vue",
    "//unpkg.com/bootstrap-vue@latest/dist/bootstrap-vue.min.js",
    "https://cdnjs.cloudflare.com/ajax/libs/vue-resource/1.5.1/vue-resource.min.js",
    "https://unpkg.com/vue-router@3.5.3",
    "/js/script.js"]
  -- uploaderjs
  -- script $ "document.addEventListener('DOMContentLoaded', init)"


-- webpage :: [p] -> Html
-- webpage :: Foldable t => t a -> Html
-- webpage :: (Foldable t, ToMarkup a) => t a -> Html
webpage :: PageContent -> Html
webpage pc = H.docTypeHtml $ do
  H.head $ do
    csss
    H.title ""
  body ! A.style "padding-top: 70px;" $ do
    H.div ! A.id "app" $ do
      nav ! class_ "navbar navbar-dark bg-primary fixed-top " $ do
        -- H.div ! class_ "container" $ do
        vifelse "refreshing"
          (do
              div' "" "loading..."
              div' "" "loading...")
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
--   fs <- io $ listFiles
--   detailsfs fs

updateState = do
  state <- getState
  io $ do
    newst <- genState
    atomicModifyIORef' state (\_ -> (newst,()))

rootRoute = do
    path <- securePath . just_or_default "" <$> W.param "path"
    token <- io genToken
    io $ print $ "root route: accessing <" <> path <> ">"
    spc <- io free
    
    let action = do
          let fullpath = directory </> path
          isfile <- io $ (isRegularFile <$> getFileStatus fullpath)
          if isfile
            then W.setHeader "Content-disposition" (Txt.pack $ "attachment; filename="<>basename path) >> W.file "" fullpath
            else do
            -- fileListing <- (`detailsfs`path) <$> io (FileManager.listFiles path)
            let pct = pcent spc
            let pc = T.PageContent{
                  monitor       = Monitor.monitor spc pct,
                  T.details     = detailsfs [] path,
                  T.currentPath = statusBarCurrentPath,
                  token         = token}
                  -- where f s = "/ " ++ (concatWith " / " $ splitOn (=='/') s)
  
            W.html . L.toStrict . R.renderHtml $ webpage $ pc
    action

newFolderRoute = do
    io $ print "new folder"
    W.body >>= io . print
    e <- securePath . folderpath <$> jsonBody'
    io . print $ e
    io . print $ directory </> e
    io $ createDirectory $ directory </> e
    updateState
    W.json $ UplPrg "ok"

  

fileUploadRoute = do
    io $ print "upload"
    e <- jsonBody'
    io $ print $ ("@@@@@@@@", T.file e, first_chunk e)

    let ddd = tail . dropWhile (/=',') $ file_data e
    let dd = BS64.decode . BSC8.pack $ ddd
        mode = if first_chunk e then BS.writeFile else BS.appendFile
    case dd of Right d -> io $ mode (directory </> T.file e) d
               Left error -> io $ do
                 print $ ddd
                 print $ "ERROR     " ++ error ++ "     ERROR"
    updateState
    W.json $ UplPrg "ok data received"

statusRoute = do
  ppp <- W.body
  io $ print $ "statusroute -->> " ++ show ppp
  p <- (\case
          Just ee -> securePath ee
          Nothing -> ""
      ) <$> W.param "path"
  io $ print $ "api call status for path <" <> p <> ">"
  spc <- io free
  W.json $ Data {
    space    = spc,
    T.files  = [],
    datapath = p
    }

filesRoute = do
  p <- (\case
          Just ee -> securePath ee
          Nothing -> ""
      ) <$> W.param "path"
  io $ print $ "api call files for path <" <> p <> ">"
  state <- getState >>= (io . (runState <$>) . readIORef)
  let fs = findTreeByPath p state
      children = case fs of Nothing -> []
                            Just (Leaf _) -> []
                            Just (Node e l) -> map treeValue l
  -- io $ print children
  W.json $ Data {
    space    = Space 0 0,
    T.files  = map (path %~ basename) $ children,
    datapath = p
    }

getPath ::
  ActionCtxT ctx (WebStateM AppDb AppSession (IORef StateApp)) String
getPath = securePath . just_or_default "" <$> W.param "path"

downloadRoute = do
  path <- getPath
  io $ print $ "Downloading <" <> path <> ">"
  let fullpath = directory </> path
  isfile <- io $ (isRegularFile <$> getFileStatus fullpath)
  
  W.setHeader "Content-disposition" (Txt.pack $ "attachment; filename="<>basename path<>
                                     if isfile then "" else ".zip")
  if isfile
    then W.file "" fullpath
    else
    (io $ createTempDirectory "/tmp" "haskellfs") >>=
    (\tmpdir -> do
        let zippath = tmpdir </> basename fullpath <.> "zip"
        io $ do
          (exec''' "zip" ["-r", zippath, path ] directory)
          print $ zippath
        W.file "" $ zippath)


deleteRoute = do
  W.body >>= io . print
  path <- getPath
  let fullpath = directory </> path
  io $ print $ "DeleteRoute <" <> fullpath <> ">"
  
  exists <- io $ doesPathExist fullpath
  if exists then (
    do
      io $ do
        print path
        exec "trash" ["--rootDir", directory </> "..",
                       directory </> path] >>= print
      updateState
      W.json $ UplPrg "ok file removed")
    else W.setStatus status400
    
    
    -- ((raise $ "No such file or directory: " ++ path) `when`)
  

  

app :: AppM
app = do
  middleware $ staticPolicy (addBase "static")
  post ("api/newfolder") $ newFolderRoute
  get ("api/status") $ statusRoute
  get ("api/files") $ filesRoute
  post ("api/fileupload") $ fileUploadRoute
  get ("api/download") $ downloadRoute
  get ("api/delete") $ deleteRoute
  get ("/") rootRoute
  hookAny GET (\_ -> rootRoute)

  -- get "api/all" $ allRoute
    
genState = listTree (Path directory Dir) >>= ((StateApp <$>) .  traverse computeState )

main :: IO ()
main = do
  print "main"
  st <- genState
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


