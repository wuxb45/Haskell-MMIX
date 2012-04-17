-- Copyright 2012 Wu Xingbo <wuxb45@gmail.com>
-- ** OMem: Memory Device with Octa entries (IOUArray)

-- module exports {{{
module MMIX.OMem
  ( OMem(..)
  , newOMem
  , octaIx
  ) 
  where
-- }}}

-- imports {{{

import MMIX.Base
import Data.Bits (shiftR, shiftL)
import Data.Array.MArray (newArray, readArray, writeArray)
import Data.Array.IO (IOUArray)

-- }}}

-- OMem {{{
data OMem = OMem
  { omemBound :: (PAddr, PAddr)
  , omemData  :: IOUArray PAddr Octa
  }
-- }}}

-- makeOMem {{{

newOMem :: (PAddr, PAddr) -> IO OMem
newOMem (l,r) = do
  let lix = l `shiftR` 3
  let rix = r `shiftR` 3
  let l' = lix `shiftL` 3
  let r' = rix `shiftL` 3
  memdata <- newArray (lix, rix) 0
  return $ OMem (l', r') memdata

-- }}}

-- octaIx: get index in a Octa (0~7){{{

octaIx :: PAddr -> PAddr
octaIx addr = addr `shiftR` 3

-- }}}

-- instance of Device {{{

-- OMem is Device
instance Device OMem where
  devAddrOk omem addr = l <= addr && addr < r
    where (l, r) = omemBound omem

  devReadOcta omem paddr = do
    if devAddrOk omem paddr
    then do
      val <- readArray (omemData omem) (octaIx paddr)
      return $ return val
    else
      return Nothing

  devWriteOcta omem paddr val = do
    if devAddrOk omem paddr
    then do
      writeArray (omemData omem) (octaIx paddr) val
      return True
    else
      return False

-- }}}

-- vim:fdm=marker
