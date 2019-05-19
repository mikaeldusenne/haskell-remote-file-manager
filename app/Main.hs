{-# LANGUAGE OverloadedStrings #-}
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

import Lib
import Maths
import HtmlWebsite

import FileManager
import qualified Monitor 
import Types as T

type AppM = SpockM AppDb AppSession AppState ()
type AppAction ctx a = SpockActionCtx ctx AppDb AppSession AppState a

type AppDb = ()
type AppState = ()
type AppSession = ()



detailsfs l = HB.row $ 
            ul $ mapM_ (li . H.span . toHtml) $ l

-- webpage :: [p] -> Html
-- webpage :: Foldable t => t a -> Html
-- webpage :: (Foldable t, ToMarkup a) => t a -> Html
webpage pc = docTypeHtml $ do
  H.head $ do
    bootstrapCss
    H.title "Hello"
  body $ do
    HB.container $ do
      -- HB.row $ alertSuccess "ok!"
      T.monitor pc
      T.details pc
    jquery
    bootstrapJs
    
main :: IO ()
main = do
  spockCfg <- defaultSpockCfg () PCNoDatabase ()
  runSpock 8080 (spock spockCfg app)

app :: AppM
app = get root $ do
  fs <- liftIO $ listFiles
  spc <- liftIO free
  let pct = pcent spc
  let pc = T.PageContent{
        monitor = Monitor.monitor spc pct,
        T.details = detailsfs fs}
  
  W.html . L.toStrict $ R.renderHtml $ webpage $ pc

type Server a = SpockM () () () a





