module Ballast.Types
       ( Username(..)
       , Password(..)
       ) where

import           Data.ByteString

-- | Username type used for HTTP Basic authentication.
newtype Username = Username { username :: ByteString } deriving (Read, Show, Eq)

-- | Password type used for HTTP Basic authentication.
newtype Password = Password { password :: ByteString } deriving (Read, Show, Eq)
