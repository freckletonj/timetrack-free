{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Util.Misc where

import Network.Wai.Handler.Warp
import Network.HTTP.Media ((//), (/:))
import Network.HTTP.Simple hiding (Proxy)
import Data.String.Conversions (cs)
import Servant

-- | HTML Content Type for returning Strings as HTML
type HTML = String
instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")
instance MimeRender HTML String where
  mimeRender _ = cs

