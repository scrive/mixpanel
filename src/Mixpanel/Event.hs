module Mixpanel.Event (Property(..), track)

where

import Network.HTTP hiding (Custom)
import Text.JSON as J
import Text.JSON.Gen
import qualified Text.JSON.Gen as J
import Control.Monad
import Data.Time.Clock
import Data.ByteString.Base64 as B64
import Data.ByteString.UTF8 as B
import Data.Time.Clock.POSIX

data Property = IP String
              | DistinctID String
              | Name String
              | Time UTCTime
              | CustomString String String
              | CustomNumber String Double
              | CustomTime String UTCTime
              | CustomBool String Bool
                
jvalue :: Monad m => Property -> JSONGenT m ()
jvalue (IP s)         = J.value "ip" s
jvalue (DistinctID s) = J.value "distinct_id" s
jvalue (Name s)       = J.value "mp_name_tag" s
jvalue (Time t)       = J.value "time" $ (round $ utcTimeToPOSIXSeconds t :: Int)
jvalue (CustomString k v)  = J.value k v
jvalue (CustomNumber k v) = J.value k v
jvalue (CustomTime k v) = J.value k $ (round $ utcTimeToPOSIXSeconds v :: Int)
jvalue (CustomBool k v) = J.value k v
                
track :: String -> String -> [Property] -> IO (Maybe String)
track token event properties = do
  let obj = runJSONGen $ do
        J.value "event" event
        J.object "properties" $ do
          J.value "token" token
          forM_ properties jvalue
      jsstring = J.encode obj
      jsb64 = B.toString $ B64.encode $ B.fromString jsstring

  let url = "http://api.mixpanel.com/track?data=" ++ jsb64
  eres <- simpleHTTP (postRequest url)
  
  case eres of
    Left ce -> return $ Just $ show ce
    Right res -> case rspBody res of
      "0" -> return $ Just $ show $ rspReason res
      "1" -> return Nothing
      r -> return $ Just $ "Don't understand response: " ++ r
