-- Copyright 2012 Wu Xingbo <wuxb45@gmail.com>

module MMIX.OMem
  ( OMem(..)
  , makeOMem
  , octaIx
  ) 
  where

-- imports {{{

import MMIX.Basics
import Data.Bits (shiftR, shiftL)
import Data.Array.MArray (newArray, readArray, writeArray)
import Data.Array.IO (IOUArray)

-- }}}

data OMem = OMem { omemBound :: (PAddr, PAddr)
                 , omemData :: IOUArray PAddr Octa
                 }

-- makeOMem {{{

makeOMem :: (PAddr, PAddr) -> IO OMem
makeOMem (l,r) = do
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
  devAddrOk omem addr = l <= addr && r > addr
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

{- deprecated
  -- read Octa
  devRead omem addr readtype =
    if devAddrOk omem addr
    then do
      oval <- devRawRead omem addr
      return $ case readtype of
        AReadOcta  -> AReadOkOcta  oval
        AReadTetra -> AReadOkTetra $ xot addr oval
        AReadWyde  -> AReadOkWyde  $ xow addr oval
        AReadByte  -> AReadOkByte  $ xob addr oval
    else return AReadFail

  -- Write Octa
  devWrite omem addr writetype =
    if devAddrOk omem addr
    then do
      origval <- devRawRead omem addr
      let newOcta = case writetype of
            AWriteOcta  oval -> oval
            AWriteTetra tval -> mot addr origval tval
            AWriteWyde  wval -> mow addr origval wval
            AWriteByte  bval -> mob addr origval bval
      devRawWrite omem addr newOcta
      return AWriteOk
    else return AWriteFail
-}

-- }}}

-- vim:fdm=marker
