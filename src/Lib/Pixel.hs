{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib.Pixel (Dimmer (..), Temp (..), Tint (..), Fader (..), RGBW (..), Strobe (..), CCTRGBWPx (..), cast, mapLo, rounded) where

import Data.Serialize (Serialize)
import GHC.Generics (Generic)

newtype Dimmer a = Dimmer a
  deriving newtype (Eq, Show, Num, Fractional, Serialize)
  deriving stock (Functor)

newtype Temp a = Temp a
  deriving newtype (Eq, Show, Num, Fractional, Serialize)
  deriving stock (Functor)

newtype Tint a = Tint a
  deriving newtype (Eq, Show, Num, Fractional, Serialize)
  deriving stock (Functor)

newtype Fader a = Fader a
  deriving newtype (Eq, Show, Num, Fractional, Serialize)
  deriving stock (Functor)

data RGBW a = RGBW a a a a
  deriving stock (Eq, Show, Functor, Generic)
  deriving anyclass (Serialize)

-- TODO - figure out how strobe works with multipiple pixels. One at end of all pixels?
newtype Strobe a = Strobe a
  deriving newtype (Eq, Show, Num, Fractional, Serialize)
  deriving stock (Functor)

-- Depending on what precision mode you are using,
-- the values will either be all 8-bit or a mix of 16-bit and 8-bit,
-- hence the need for "hi" precision and "low" precision type params.
data CCTRGBWPx lo hi = CCTRGBWPx (Dimmer hi) (Temp hi) (Tint hi) (Fader lo) (RGBW hi)
  deriving stock (Eq, Show, Functor, Generic)
  deriving anyclass (Serialize)

mapLo :: (lo -> lo2) -> CCTRGBWPx lo hi -> CCTRGBWPx lo2 hi
mapLo fun (CCTRGBWPx a b c d e) = CCTRGBWPx a b c (fmap fun d) e

cast :: forall i o. (RealFrac i, Integral o, Bounded o) => i -> o
cast x = round (x * fromIntegral (maxBound :: o))

rounded :: forall lo' hi' lo hi . (RealFrac lo, RealFrac hi, Integral lo', Bounded lo', Integral hi', Bounded hi') =>
    CCTRGBWPx lo hi -> CCTRGBWPx lo' hi'
rounded = fmap cast . mapLo cast
