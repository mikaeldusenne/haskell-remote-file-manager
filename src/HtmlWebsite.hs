{-# LANGUAGE OverloadedStrings #-}
module HtmlWebsite where

import Lib

import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 hiding (main)
import qualified Text.Blaze.Html.Renderer.Text as R
import Text.Blaze.Html5.Attributes as A
import qualified Data.Text.Lazy as L
import qualified Text.Blaze.Bootstrap as HB

alertBootstrap :: AttributeValue -> Html -> Html
alertBootstrap which = H.div H.! class_ ("alert alert-" <> which) H.! role "alert"

alertSuccess = alertBootstrap "success"
alertWarning = alertBootstrap "warning"
alertDanger = alertBootstrap "danger"
alertSecondary = alertBootstrap "secondary"


bootstrapCss = link ! rel "stylesheet" ! type_ "text/css" ! href "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css"

js srcurl = script ! src srcurl $ mempty

jquery = js "https://code.jquery.com/jquery-3.4.1.min.js"
bootstrapJs = js "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/js/bootstrap.bundle.min.js"

column1 = H.div ! class_ ("col-md-1")
column2 = H.div ! class_ ("col-md-2")
column3 = H.div ! class_ ("col-md-3")
column4 = H.div ! class_ ("col-md-4")
column5 = H.div ! class_ ("col-md-5")
column6 = H.div ! class_ ("col-md-6")
column7 = H.div ! class_ ("col-md-7")
column8 = H.div ! class_ ("col-md-8")
