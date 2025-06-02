module File where

import Data.Text (Text)

data File = File {path :: String, content :: Text}