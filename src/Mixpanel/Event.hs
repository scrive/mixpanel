{-# LANGUAGE OverloadedStrings #-}
module Mixpanel.Event (Property(..), track, MixpanelResult(..))

where

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Simple (getResponseStatusCode, getResponseBody, setRequestMethod)

import Text.JSON as J
import Text.JSON.Gen
import qualified Text.JSON.Gen as J
import Control.Monad
import Data.ByteString.Base64 as B64
import Data.ByteString.UTF8 as B
import Data.ByteString.Lazy.UTF8 as BU
import Data.Time.Clock.POSIX

import Mixpanel.Result
import Mixpanel.Properties

jvalue :: Monad m => Property -> JSONGenT m ()
jvalue (IP s)         = J.value "ip" s
jvalue (FullName s)   = J.value "mp_name_tag" s
jvalue (Time t)       = J.value "time" $ (round $ utcTimeToPOSIXSeconds t :: Int)
jvalue (CustomString k v)  = J.value k v
jvalue (CustomNumber k v) = J.value k v
jvalue (CustomTime k v) = J.value k $ (round $ utcTimeToPOSIXSeconds v :: Int)
jvalue (CustomBool k v) = J.value k v
jvalue _ = return () -- Ingore Email, FirstName, LastName, and Created

track :: String -> Maybe String -> String -> [Property] -> IO MixpanelResult
track token mdistinctid event properties = do
  let obj = runJSONGen $ do
        J.value "event" event
        J.object "properties" $ do
          J.value "token" token
          maybe (return ()) (J.value "distinct_id") mdistinctid
          forM_ properties jvalue
      jsstring = J.encode obj
      jsb64 = B.toString $ B64.encode $ B.fromString jsstring

  let url = "http://api.mixpanel.com/track?data=" ++ jsb64
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
