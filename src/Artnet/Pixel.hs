module Artnet.Pixel (Dimmer (..), Temp (..), Tint (..), Fader (..), RGBW (..), Strobe (..), CCTRGBWPx (..), BrightnessTemperature (..), cast, mapLo, Roundable1, round1, Roundable2, round2) where

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

data BrightnessTemperature precision = BrightnessTemperature (Dimmer precision) (Temp precision)
  deriving stock (Eq, Show, Functor, Generic)
  deriving anyclass (Serialize)

mapLo :: (lo -> lo2) -> CCTRGBWPx lo hi -> CCTRGBWPx lo2 hi
mapLo fun (CCTRGBWPx a b c d e) = CCTRGBWPx a b c (fmap fun d) e

cast :: forall i o. (RealFrac i, Integral o, Bounded o) => i -> o
cast x = round (x * fromIntegral (maxBound :: o))

class Roundable1 f where
  round1 :: (RealFrac precision, Integral precision', Bounded precision') => f precision -> f precision'

class Roundable2 f where
  round2 :: (RealFrac precision, Integral precision', Bounded precision') => f precision a -> f precision' a

instance Roundable1 (CCTRGBWPx lo) where
  round1 = fmap cast

instance Roundable2 CCTRGBWPx where
  round2 = mapLo cast

instance Roundable1 BrightnessTemperature where
  round1 = fmap cast
