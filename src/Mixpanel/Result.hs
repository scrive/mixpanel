module Mixpanel.Result (MixpanelResult(..)) where

data MixpanelResult = Success | HTTPError String | MixpanelError String