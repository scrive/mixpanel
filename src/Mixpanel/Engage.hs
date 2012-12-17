module Mixpanel.Engage (Property(..), set, add)

where

import Network.HTTP hiding (Custom)
import Text.JSON as J
import Text.JSON.Gen
import qualified Text.JSON.Gen as J
import Data.List
import Control.Monad
import Data.Time.Clock
import Data.Time.Format
import Data.ByteString.Base64 as B64
import Data.ByteString.UTF8 as B

data Property = IP String
              | Email String
              | FirstName String
              | LastName String
              | Created UTCTime
              | LastLogin UTCTime
              | Username String
              | CustomString String String
              | CustomNumber String Double
              | CustomTime String UTCTime
              | CustomBool String Bool
                
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
jvalue (CustomString k v)  = J.value k v
jvalue (CustomNumber k v) = J.value k v
jvalue (CustomTime k v) = J.value k $ formatTime undefined "%Y-%m-%dT%H:%M:%S" v
jvalue (CustomBool k v) = J.value k v
                
set :: String -> String -> [Property] -> IO (Maybe String)
set token distinctid properties = do
  let obj = runJSONGen $ do
        J.value "$token" token
        J.value "$distinct_id" distinctid
        -- $ip must be in outer json
        maybe (return ()) jvalue $ find isip properties
        J.object "$set" $ forM_ (filter (not . isip) properties) jvalue
      jsstring = J.encode obj
      jsb64 = B.toString $ B64.encode $ B.fromString jsstring

  let url = "http://api.mixpanel.com/engage?data=" ++ jsb64
  eres <- simpleHTTP (postRequest url)
  
  case eres of
    Left ce -> return $ Just $ show ce
    Right res -> case rspBody res of
      "0" -> return $ Just $ show $ rspReason res
      "1" -> return Nothing
      r -> return $ Just $ "Don't understand response: " ++ r

add :: String -> String -> [Property] -> IO (Maybe String)
add token distinctid properties = do
  let obj = runJSONGen $ do
        J.value "$token" token
        J.value "$distinct_id" distinctid
        -- $ip must be in outer json
        maybe (return ()) jvalue $ find isip properties
        J.object "$add" $ forM_ (filter (not . isip) properties) jvalue
      jsstring = J.encode obj
      jsb64 = B.toString $ B64.encode $ B.fromString jsstring

  let url = "http://api.mixpanel.com/engage?data=" ++ jsb64
  eres <- simpleHTTP (postRequest url)
  
  case eres of
    Left ce -> return $ Just $ show ce
    Right res -> case rspBody res of
      "0" -> return $ Just $ show $ rspReason res
      "1" -> return Nothing
      r -> return $ Just $ "Don't understand response: " ++ r
