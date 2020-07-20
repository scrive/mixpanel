{-# LANGUAGE OverloadedStrings #-}
module Mixpanel.Engage (Property(..), set, add, MixpanelResult(..))

where

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Simple (getResponseStatusCode, getResponseBody, setRequestMethod)
import Text.JSON as J
import Text.JSON.Gen
import qualified Text.JSON.Gen as J
import Data.List
import Data.Maybe
import Control.Monad
import Data.Time.Format
import Data.ByteString.Base64 as B64
import Data.ByteString.UTF8 as B
import Data.ByteString.Lazy.UTF8 as BU

import Mixpanel.Result
import Mixpanel.Properties

isip :: Property -> Bool
isip (IP _) = True
isip _      = False

jvalue :: Monad m => Property -> JSONGenT m ()
jvalue (IP s)        = J.value "$ip" s
jvalue (Email s)     = J.value "$email" s
jvalue (FirstName s) = J.value "$first_name" s
jvalue (LastName s)  = J.value "$last_name" s
jvalue (Created t)   = J.value "$created" $ formatTime undefined "%Y-%m-%dT%H:%M:%S" t
jvalue (LastLogin t) = J.value "$last_login" $ formatTime undefined "%Y-%m-%dT%H:%M:%S" t
jvalue (Username s)  = J.value "$username" s
jvalue (FullName s)  = J.value "Full name" s
jvalue (CustomString k v)  = J.value k v
jvalue (CustomNumber k v) = J.value k v
jvalue (CustomTime k v) = J.value k $ formatTime undefined "%Y-%m-%dT%H:%M:%S" v
jvalue (CustomBool k v) = J.value k v
jvalue _ = return () -- ignore Time property

set :: String -> String -> [Property] -> IO MixpanelResult
set token distinctid properties = do
  let obj = runJSONGen $ do
        J.value "$token" token
        J.value "$distinct_id" distinctid
        -- ip must be set either to 0 or the IP supplied, otherwise Mixpanel
        -- thinks they live in Dublin, Ireland (AWS EU server location)
        jvalue $ fromMaybe (IP "0") (find isip properties)
        -- ip must be in outer json
        J.object "$set" $ forM_ (filter (not . isip) properties) jvalue
      jsstring = J.encode obj
      jsb64 = B.toString $ B64.encode $ B.fromString jsstring

  let url = "http://api.mixpanel.com/engage?data=" ++ jsb64

  reqManager <- newTlsManager
  let timeout = responseTimeoutMicro $ 10 {- secs -} * 1000000
      req = (setRequestMethod "POST") $
              (parseRequest_ url) { responseTimeout = timeout }

  resp <- httpLbs req reqManager
  if (getResponseStatusCode resp == 200)
    then do
      case (getResponseBody resp) of
        "0" -> return $ MixpanelError $ "Mixpanel response is 0"
        "1" -> return Success
        r -> return $ HTTPError $ "Don't understand response. Should be '0' or '1' but got: " ++ BU.toString r
    else return $ HTTPError $ show $ getResponseStatusCode resp


add :: String -> String -> [Property] -> IO MixpanelResult
add token distinctid properties = do
  let obj = runJSONGen $ do
        J.value "$token" token
        J.value "$distinct_id" distinctid
        -- ip must be in outer json
        maybe (return ()) jvalue $ find isip properties
        J.object "$add" $ forM_ (filter (not . isip) properties) jvalue
      jsstring = J.encode obj
      jsb64 = B.toString $ B64.encode $ B.fromString jsstring

  let url = "http://api.mixpanel.com/engage?data=" ++ jsb64
  reqManager <- newTlsManager
  let timeout = responseTimeoutMicro $ 10 {- secs -} * 1000000
      req = (setRequestMethod "POST") $
              (parseRequest_ url) { responseTimeout = timeout }

  resp <- httpLbs req reqManager
  if (getResponseStatusCode resp == 200)
    then do
      case (getResponseBody resp) of
        "0" -> return $ MixpanelError $ "Mixpanel response is 0"
        "1" -> return Success
        r -> return $ HTTPError $ "Don't understand response. Should be '0' or '1' but got: " ++ BU.toString r
    else return $ HTTPError $ show $ getResponseStatusCode resp
