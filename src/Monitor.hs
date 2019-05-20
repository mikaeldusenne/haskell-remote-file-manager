{-# LANGUAGE OverloadedStrings #-}
module Monitor where


import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 hiding (main)
import Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Bootstrap as HB

import Hunix

import HtmlWebsite
import Maths

round2 :: (RealFrac a) => a -> a
round2 = (/100.0) . fromIntegral . round . (*100)

monitor :: Space -> Double -> Html
monitor (Space{avail=a, used=u}) pct =  do
  let alertTypePct | pct < 10 = alertWarning
                   | pct > 10 = alertSuccess
      f alrt k = H.span ! class_ "navbar-text navbar-dark" $ k -- column12 .

  
  -- HB.row $ alertTypePct "ok!"
  f alertTypePct (H.toHtml $ "Available space: " <> prettyBytes a <> " / " <> prettyBytes (a+u) <> " ( " <> show (round2 pct) <> "% )")
  -- round2 pct
  -- let a' = (++"GB") . show . round . (/(1024*1024)) $ fromIntegral a
  -- f alertSecondary "available" $ prettyBytes a
  -- f alertSecondary "used" $ prettyBytes u
  
