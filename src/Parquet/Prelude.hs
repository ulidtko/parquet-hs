-- |
module Parquet.Prelude
  ( module X
  , showPrettyT
  )
where

------------------------------------------------------------------------------

import Codec.Serialise as X (Serialise)
import Data.Aeson as X (FromJSON (..), ToJSON (..))
import Data.Binary as X (Binary)
import Data.Bits as X (shiftL, shiftR, (.&.), (.|.))
import Data.Traversable as X (for)
import Parquet.InstanceOrphanage ()
import Relude as X hiding (Type)
import Safe as X (headMay)
import Safe.Exact as X (zipExactMay)

import Text.Pretty.Simple (pShow)

------------------------------------------------------------------------------

showPrettyT :: Show a => a -> Text
showPrettyT = toStrict . Text.Pretty.Simple.pShow
