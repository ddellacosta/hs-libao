
module AOExample where

import AO 
import Foreign.C
import Data.Bits
import Data.Sequence as Seq
import Data.Foldable (toList)

sample :: Floating a => a -> a -> a
sample step rate = 0.75 * 32768.0 * (sin (2 * pi * 440.0 * (step / rate)))

fillSampleVector' :: CInt -> CInt -> Seq CChar-> Seq CChar 
fillSampleVector' n rate xs
  | n == rate = xs
  | otherwise = fillSampleVector' (succ n) rate ((((xs |> s1 ) |> s2) |> s1) |> s2)
                  where sample' = floor $ sample (fromIntegral n) (fromIntegral rate)
                        s1      = sample' .&. 0xff
                        s2      = (sample' `shiftR` 8) .&. 0xff

fillSampleVector :: CInt -> Seq CChar
fillSampleVector rate = fillSampleVector' 0 rate empty

main = do
    ao_initialize
    devicePtr <- aoOpenLive deviceId fmt
    aoPlay devicePtr fmt bufSize buffer
    return (ao_close devicePtr)
    ao_shutdown
  where deviceId = ao_default_driver_id
        fmt      = AOSampleFormat { bits = 16, channels = 2, rate = 44100, byte_format = 1 }
        bufSize  = (bits fmt) `div` 8 * (channels fmt) * (rate fmt)
        buffer   = toList $ fillSampleVector (rate fmt)
