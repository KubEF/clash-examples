{-# LANGUAGE PartialTypeSignatures #-}

module FullAdder where

import Clash.Annotations.TH
import Clash.Prelude

-- | Полный сумматор без заморочек, он бы тоже смог синтезироваться
fullAdderBit :: (Bits b) => b -> b -> b -> (b, b)
fullAdderBit a b c_in = (res_sum, c_out)
  where
    a_b_xor = xor a b
    res_sum = xor a_b_xor c_in
    conj = (.&.) a b
    in_xor = (.&.) c_in a_b_xor
    c_out = (.|.) conj in_xor

{- | Здесь сделано всё "по книжке".
 По идее без проблем синтезируется и без @Signal dom@
-}
fullAdder
  :: forall dom
   . (KnownDomain dom)
  => Signal dom Bit
  -> Signal dom Bit
  -> Signal dom Bit
  -> Signal dom (Bit, Bit)
fullAdder a b c_in = bundle (res_sum, c_out)
  where
    a_b_xor = xor <$> a <*> b -- да, из-за Signal dom приходится протаскивать вот так, это аппликативный функтор.
    res_sum = xor <$> a_b_xor <*> c_in
    conj = (.&.) <$> a <*> b
    in_xor = (.&.) <$> c_in <*> a_b_xor
    c_out = (.|.) <$> conj <*> in_xor

-- | Верхний модуль
topEntity
  :: "a" ::: Signal System Bit
  -> "b" ::: Signal System Bit
  -> "c_in" ::: Signal System Bit
  -> Signal System ("sum" ::: Bit, "c_out" ::: Bit)
topEntity = fullAdder

-- Это для того, чтобы аннотации сработали, тут тоже привет TemplateHaskell :)
makeTopEntity 'topEntity
