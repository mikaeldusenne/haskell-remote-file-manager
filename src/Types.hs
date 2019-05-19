module Types where

import Text.Blaze.Html5


data PageContent = PageContent{
  monitor :: Html,
  details :: Html
  }
