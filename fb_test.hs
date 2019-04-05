{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Facebook
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class
import Data.Monoid ((<>))
import Data.ByteString.Char8 (pack)
import Data.Text hiding (pack)
import Data.Aeson
import qualified Data.Text.Encoding as TE

myCreds :: Credentials
myCreds =
  Credentials
  { appName = "Dynasty Baseball Helper"
  , appId = "187760695155139"
  , appSecret = ""
  }

main :: IO ()
main = do
  mgr <- newManager tlsManagerSettings
  let redirectUrl = "http://localhost/"
  runResourceT $
    runFacebookT myCreds mgr $
    do url1 <- getUserAccessTokenStep1 redirectUrl ["public_profile", "email"]
       liftIO $ print ("Paste the url in browser and get code: " <> url1)
       code <- liftIO $ getLine
       token <- getUserAccessTokenStep2 redirectUrl [("code", pack code)]
       liftIO $ print token
       user <- getUser "me" [("fields", "first_name,last_name")] (Just token)
       liftIO $ print user
       (picture :: Value) <- getObject "/310613432384064/comments" [] (Just token)
       liftIO $ print picture
