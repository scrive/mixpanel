module Mixpanel.Properties (Property(..)) where

import Data.Time.Clock

data Property = IP String
              | Email String
              | FullName String
              | FirstName String
              | LastName String
              | Created UTCTime
              | LastLogin UTCTime
              | Username String
              | Time UTCTime
              | CustomString String String
              | CustomNumber String Double
              | CustomTime String UTCTime
              | CustomBool String Bool
