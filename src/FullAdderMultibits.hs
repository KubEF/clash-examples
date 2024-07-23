{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module FullAdderMultibits where

import Clash.Annotations.TH
import Clash.Prelude
    ( KnownNat,
      (+>>),
      foldr,
      repeat,
      zip,
      TopEntity(t_output, t_name, t_inputs),
      type (:::),
      Bit,
      Vec )
import FullAdder (fullAdderBit)

-- Можно опять сделать ещё аккуратнее, но потраченного времени будет жаль
fullAdderMultiBits
  :: (KnownNat n)
  => Vec n Bit
  -> Vec n Bit
  -> Bit
  -> (Vec n Bit, Bit)
fullAdderMultiBits a b c_in = res
  where
    zero = repeat 0 :: Vec _ Bit
    res = foldr func (zero, c_in) (zip a b)
    func (fstBit, sndBit) (ansVec, prevCarry) = (resBit +>> ansVec, nextCarry)
      where
        (resBit, nextCarry) = fullAdderBit fstBit sndBit prevCarry

topEntity
  :: "a" ::: Vec 8 Bit
  -> "b" ::: Vec 8 Bit
  -> "c_in" ::: Bit
  -> ( "sum" ::: Vec 8 Bit
     , "c_out" ::: Bit
     )
topEntity = fullAdderMultiBits

makeTopEntity 'topEntity
