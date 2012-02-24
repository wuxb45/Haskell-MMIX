-- Copyright 2012 Wu Xingbo <wuxb45@gmail.com>
-- Tips: `za` toggle folding; `zM` fold all; `zR` unfold all.

-- ghc options {{{

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}

-- }}}

-- module exports {{{

module MMIX.Basics (
-- common helpers {{{
  cast, hx,
-- }}}
-- machine types and fields {{{
  Octa, Tetra, Wyde, Byte,
  Addr, VAddr, PAddr, RAddr, BitIx, BitFld,
  Insn, OPCode, X, Y, Z, YZ, XYZ,
  PTE, PTP,
  FP,
  OctaS, TetraS, WydeS, ByteS,
-- }}}
-- general bytes convertion {{{
  cvot, cvtw, cvwb, cvow, cvtb, cvob,
  cvto, cvwt, cvbw, cvwo, cvbt, cvbo,
  xob, xow, xot, xtb, xtw, xwb,
  mob, mow, mot, mtb, mtw, mwb,
-- }}}
-- general bits convertion {{{
  bitMask, bitUMask, bitGet, bitSet,
  bitGetRaw, bitSetRaw, bitSet1, bitSet0,
  fldMask, fldUMask, fldGet, fldSet,
  fldGetRaw, fldSetRaw, fldSet1, fldSet0,
-- }}}
-- coerce type casting {{{
  castFtoO,  castOtoF,
  castFtoOS, castOStoF,
  castSFtoT, castTtoSF,
  castOtoOS, castOStoO,
  castTtoTS, castTStoT,
  castWtoWS, castWStoW,
  castBtoBS, castBStoB,
-- }}}
-- common registers {{{
  RIx, SRIx, GRIx, RD,
  rSet, rGet,
-- }}}
-- special registers {{{
  newSRD,
  rBIx, rDIx, rEIx, rHIx, rJIx, rMIx, rRIx, rBBIx,
  rCIx, rNIx, rOIx, rSIx, rIIx, rTIx, rTTIx, rKIx,
  rQIx, rUIx, rVIx, rGIx, rLIx, rAIx, rFIx, rPIx,
  rWIx, rXIx, rYIx, rZIx, rWWIx, rXXIx, rYYIx, rZZIx,

  rVFb1,rVFb2,rVFb3,rVFb4,rVFs,rVFr,rVFn,rVFf,

  rAFEnable,
  rAFEnableD,rAFEnableV,rAFEnableW,rAFEnableI,
  rAFEnableO,rAFEnableU,rAFEnableZ,rAFEnableX,
  rABEnableD,rABEnableV,rABEnableW,rABEnableI,
  rABEnableO,rABEnableU,rABEnableZ,rABEnableX,
  rAFEvent,
  rAFEventD, rAFEventV, rAFEventW, rAFEventI,
  rAFEventO, rAFEventU, rAFEventZ, rAFEventX,
  rABEventD, rABEventV, rABEventW, rABEventI,
  rABEventO, rABEventU, rABEventZ, rABEventX,
  rAFRoundMode,

  rQFlow, rQFprog, rQFhigh, rQFmachine,
  rQFr, rQFw, rQFx, rQFn, rQFk, rQFb, rQFs, rQFp,
  rQBr, rQBw, rQBx, rQBn, rQBk, rQBb, rQBs, rQBp,

  rKFlow, rKFprog, rKFhigh, rKFmachine,
  rKFr, rKFw, rKFx, rKFn, rKFk, rKFb, rKFs, rKFp,
  rKBr, rKBw, rKBx, rKBn, rKBk, rKBb, rKBs, rKBp,

  rXFsign, rXFinsn, rXFrop,

  rXXFsign, rXXFinsn, rXXFrop, rXXFprog,
  rXXFr, rXXFw, rXXFx, rXXFn, rXXFk, rXXFb, rXXFs, rXXFp,
  rXXBr, rXXBw, rXXBx, rXXBn, rXXBk, rXXBb, rXXBs, rXXBp,
-- }}}
-- general registers {{{
  newGRD,
-- }}}
-- instruction operation {{{
  iGetOPCode, iGetX, iGetY, iGetZ, iGetYZ, iGetXYZ,
  iGetXu, iGetYu, iGetZu, iGetYZu, iGetXYZu,
  iGetXs, iGetYs, iGetZs, iGetYZs, iGetXYZs,
-- }}}
-- opcode {{{
  InsnOP (..), deOPCode,
-- }}}
-- address interface {{{
  Device (..),
-- }}}
-- the MMIX machine {{{
  MMIX (..),
  mmixGetPC, mmixSetPC,
  mmixGetSR, mmixSetSR,
  mmixGetGR, mmixSetGR,
  mmixLdOcta, mmixLdTetra, mmixLdWyde, mmixLdByte,
  mmixLdOcta0, mmixLdTetra0, mmixLdWyde0, mmixLdByte0,
  mmixStOcta, mmixStTetra, mmixStWyde, mmixStByte,
  --newDummyMMIX,
-- }}}
-- MMIX helpers {{{
  mmixGetGRS, mmixSetGRS,
  mmixGetGRF, mmixSetGRF,
  mmixSetGRX, mmixSetGRSX, mmixSetGRFX,
  mmixGetGRX, mmixGetGRSX, mmixGetGRFX,
  mmixGetGRY, mmixGetGRSY, mmixGetGRFY,
  mmixGetGRZ, mmixGetGRSZ, mmixGetGRFZ,
-- }}}
-- virtual address mapping {{{
  vaddrFaddr, vaddrFseg, vaddrFsign,
  pteFx, pteFay, pteFn, pteFp,
  pteFpr, pteFpw, pteFpx, pteBpr, pteBpw, pteBpx,
  ptpFc, ptpFn, ptpFq, ptpFnq,
  --VF (..),
-- }}}
-- Trip and Trap {{{
  TRIP (..),
  tripEntry,
  tripAEBit,
  tripCheckD, tripCheckV, tripCheckW, tripCheckI,
  tripCheckO, tripCheckU, tripCheckZ, tripCheckX, 
  tripCheckList, tripCheck, tripCheckOne,
-- }}}
-- ArithEx {{{
  ArithEx (..), cvAEtoT, cvAEtoBit,
-- }}}
-- floating-point {{{
  fpBsign, fpFsign, fpFe, fpFf, fpFse, fpBnan, fpFpl,
  RoundMode (..), toRoundMode,
  FSign (..), toFSign, toFSignRaw, fpSetFSignRaw, sfpSetFSignRaw,
  NaNType (..), fpToNaN, fpToNaNRaw, sfpToNaNRaw, fpSetNaNRaw, fpSetNaNPayload,
  FPInfo (..),
  fpUnpack, fpPack, sfpUnpack, sfpPack,

  fpiIsNaN, fpiIsSNaN, fpiIsQNaN,
  fpiIsInf, fpiIsPInf, fpiIsNInf,
  fpiIsZero, fpiIsPZero, fpiIsNZero,
  fpiIsNormal, fpiIsSubNormal,

  fpIsNaN, fpIsSNaN, fpIsQNaN,
  fpIsInf, fpIsPInf, fpIsNInf,
  fpIsZero, fpIsPZero, fpIsNZero,
  fpIsNormal, fpIsSubNormal,

  fpSNaN, fpQNaN, fpPInf, fpNInf, fpPZero, fpNZero,
  fpUnordered,
  fpAdd, fpiAdd,
  fpSub, fpiSub,
  fpCompare, fpiCompare,
  fpEqual, fpiEqual,
  fpFix, fpiFix, fpFixu, fpiFixu,
  fpFloat, fpFloatu,
-- }}}
  ) where

-- }}}

-- imports {{{
--import System.IO.Unsafe (unsafePerformIO)
import Prelude
       ( Maybe (..), Bool (..), IO (..), Ord (..),
         Integral (..), Ordering (..),
         Double, Float,
         (.), ($), (+), (-), (*), (/), (||), (&&),
         (>=), (<=), (<), (>),
         fromIntegral, otherwise, fst, id,
       )
import Control.Applicative (Applicative (..), (<$>), (<*>))
import Control.Monad (Monad (..), Functor (..), (>>=),
                      fmap, return, when, mapM,)
import Data.Array.IO (IOUArray)
import Data.Array.MArray (MArray, newArray, getElems,
                          readArray, writeArray)
import Data.Bits (Bits (..), bit, shift, shiftR, shiftL,
                  (.&.), (.|.), complement)
import Data.Eq (Eq (..))
import Data.List (filter, map, head, zip, (++), nub)
import Data.Int (Int, Int64, Int32, Int16, Int8)
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Map as Map (Map, fromList, (!))
import Data.String (String (..))
import Data.Word (Word64, Word32, Word16, Word8)
import Data.IORef (IORef, readIORef, writeIORef, newIORef)
import Debug.Trace (trace, traceShow)
import Numeric (showHex)
import Text.Printf (printf)
import Text.Show (Show (..))
import Unsafe.Coerce (unsafeCoerce)
-- }}}

-- common helpers {{{

-- cast, Just make fromIntegral short
cast :: (Integral a, Integral b) => a -> b
cast = fromIntegral

-- hx, show in Hex format
hx :: (Integral a, Show a) => a -> String
hx v = showHex v ""

-- }}}

-- machine types and fields {{{

type Octa   = Word64  -- 64 bits
type Tetra  = Word32  -- 32 bits 
type Wyde   = Word16  -- 16 bits
type Byte   = Word8   --  8 bits

type Addr   = Octa    -- General address, 64 bits
type VAddr  = Addr    -- Virtual address
type PAddr  = Addr    -- Physical address

type RAddr  = Byte    -- Register Address, 8 bits
type BitIx  = Octa    -- Index of bits
type BitFld = (BitIx, BitIx) -- bits field, (left=63,right=0)

type Insn   = Tetra
type OPCode = Byte    -- Opcode
type X      = Byte    -- X field
type Y      = Byte    -- Y field
type Z      = Byte    -- Z field
type YZ     = (Y,Z)   -- YZ combined field
type XYZ    = (X,Y,Z) -- XYZ combined field

type PTE    = Octa    -- Page Table Entry
type PTP    = Octa    -- Page Table Pointer

type FP     = Double  -- the floating-point type (64-bit)
type SFP    = Float   -- short floating point (32-bit)

type OctaS  = Int64   -- signed Octa
type TetraS = Int32   -- signed Octa
type WydeS  = Int16   -- signed Octa
type ByteS  = Int8    -- signed Octa

-- }}}

-- general bytes convertion (Big Endian Targets) {{{

-- split cv** {{{

-- Octa -> Tetra *2
cvot :: Octa -> (Tetra, Tetra)
cvot o = (cast (o `shiftR` 32), cast o)

-- Tetra -> Wyde *2
cvtw :: Tetra -> (Wyde,Wyde)
cvtw t = (cast (t `shiftR` 16), cast t)

-- Wyde -> Byte *2
cvwb :: Wyde -> (Byte,Byte)
cvwb w = (cast (w `shiftR` 8), cast w)

-- Octa -> Wyde *4
cvow :: Octa -> (Wyde,Wyde,Wyde,Wyde)
cvow o =
  let (ht,lt) = cvot o
      (hhw,lhw) = cvtw ht
      (hlw,llw) = cvtw lt
  in  (hhw,lhw,hlw,llw)

-- Tetra -> Byte *4
cvtb :: Tetra -> (Byte,Byte,Byte,Byte)
cvtb t =
  let (hw,lw) = cvtw t
      (hhb,lhb) = cvwb hw
      (hlb,llb) = cvwb lw
  in  (hhb,lhb,hlb,llb)

-- Octa -> Byte *8
cvob :: Octa -> (Byte,Byte,Byte,Byte,Byte,Byte,Byte,Byte)
cvob o =
  let (ht,lt) = cvot o
      (h0,h1,h2,h3) = cvtb ht
      (l0,l1,l2,l3) = cvtb lt
  in  (h0,h1,h2,h3,l0,l1,l2,l3)

-- split }}}

-- merge cv** {{{

-- Tetra *2 -> Octa
cvto :: (Tetra, Tetra) -> Octa
cvto (ht, lt) = ((cast ht) `shiftL` 32) .|. (cast lt)

-- Wyde *2 -> Tetra
cvwt :: (Wyde, Wyde) -> Tetra
cvwt (hw, lw) = ((cast hw) `shiftL` 16) .|. (cast lw)

-- Byte *2 -> Wyde
cvbw :: (Byte, Byte) -> Wyde
cvbw (hb, lb) = ((cast hb) `shiftL` 8) .|. (cast lb)

-- Wyde *4 -> Octa
cvwo :: (Wyde,Wyde,Wyde,Wyde) -> Octa
cvwo (w0, w1, w2, w3) = cvto (cvwt (w0,w1), cvwt (w2, w3))

-- Byte *4 -> Tetra
cvbt :: (Byte,Byte,Byte,Byte) -> Tetra
cvbt (b0, b1, b2, b3) = cvwt (cvbw (b0, b1), cvbw (b2, b3))

-- Byte *8 -> Octa
cvbo :: (Byte,Byte,Byte,Byte,Byte,Byte,Byte,Byte) -> Octa
cvbo (b0, b1, b2, b3, b4, b5, b6, b7) =
  cvto (cvbt (b0, b1, b2, b3), cvbt (b4, b5, b6, b7))

-- merge }}}

-- extract x** {{{

-- extract Byte from Octa
xob :: Addr -> Octa -> Byte
xob addr val = cast $ val `shiftR` sh
  where sh = (7 - cast (addr .&. 0x7)) `shiftL` 3 :: Int

-- extract Wyde from Octa
xow :: Addr -> Octa -> Wyde
xow addr val = cast $ val `shiftR` sh
  where sh = (6 - cast (addr .&. 0x6)) `shiftL` 3 :: Int

-- extract Tetra from Octa
xot :: Addr -> Octa -> Tetra
xot addr val = cast $ val `shiftR` sh
  where sh = (4 - cast (addr .&. 0x4)) `shiftL` 3 :: Int

-- extract Byte from Tetra
xtb :: Addr -> Tetra -> Byte
xtb addr val = cast $ val `shiftR` sh
  where sh = (3 - cast (addr .&. 0x3)) `shiftL` 3 :: Int

-- extract Wyde from Tetra
xtw :: Addr -> Tetra -> Wyde
xtw addr val = cast $ val `shiftR` sh
  where sh = (2 - cast (addr .&. 0x2)) `shiftL` 3 :: Int

-- extract Byte from Wyde
xwb :: Addr -> Wyde -> Byte
xwb addr val = cast $ val `shiftR` sh
  where sh = (1 - cast (addr .&. 0x1)) `shiftL` 3 :: Int

-- extract }}}

-- modify m** {{{

-- modify Byte in Octa
mob :: Addr -> Octa -> Byte -> Octa
mob addr o b = (o .&. mask) .|. ((cast b) `shiftL` sh)
  where mask = complement $ 0xff `shiftL` sh :: Octa
        sh = (7 - cast (addr .&. 0x7)) `shiftL` 3 :: Int

-- modify Wyde in Octa
mow :: Addr -> Octa -> Wyde -> Octa
mow addr o w = (o .&. mask) .|. ((cast w) `shiftL` sh)
  where mask = complement $ 0xffff `shiftL` sh :: Octa
        sh = (6 - cast (addr .&. 0x6)) `shiftL` 3 :: Int

-- modify Tetra in Octa
mot :: Addr -> Octa -> Tetra -> Octa
mot addr o t = (o .&. mask) .|. ((cast t) `shiftL` sh)
  where mask = complement $ 0xffffffff `shiftL` sh :: Octa
        sh = (4 - cast (addr .&. 0x4)) `shiftL` 3 :: Int

-- modify Byte in Tetra
mtb :: Addr -> Tetra -> Byte -> Tetra
mtb addr t b = (t .&. mask) .|. ((cast b) `shiftL` sh)
  where mask = complement $ 0xff `shiftL` sh :: Tetra
        sh = (3 - cast (addr .&. 0x3)) `shiftL` 3 :: Int

-- modify Wyde in Tetra
mtw :: Addr -> Tetra -> Wyde -> Tetra
mtw addr t w = (t .&. mask) .|. ((cast w) `shiftL` sh)
  where mask = complement $ 0xffff `shiftL` sh :: Tetra
        sh = (2 - cast (addr .&. 0x2)) `shiftL` 3 :: Int

-- modify Byte in Wyde
mwb :: Addr -> Wyde -> Byte -> Wyde
mwb addr w b = (w .&. mask) .|. ((cast b) `shiftL` sh)
  where mask = complement $ 0xff `shiftL` sh :: Wyde
        sh = (1 - cast (addr .&. 0x1)) `shiftL` 3 :: Int

-- modify }}}

-- }}}

-- general bits convertion (Octa only) {{{

-- bit operation {{{

-- mask a bit
bitMask :: (Bits a) => BitIx -> a
bitMask = bit . cast

-- unmask a bit
bitUMask :: (Bits a) => BitIx -> a
bitUMask = complement . bit . cast

-- get a bit
bitGet :: (Bits a) => BitIx -> a -> a
bitGet i v = (v .&. (bitMask i)) `shiftR` (cast i)

-- set a bit
bitSet :: (Bits a) => BitIx -> a -> a -> a
bitSet i v orig =
  ((bitUMask i) .&. orig) .|. ((bitMask i) .&. (v `shiftL` (cast i)))

-- get a bit, no shift
bitGetRaw :: (Bits a) => BitIx -> a -> a
bitGetRaw i v = v .&. (bitMask i)

-- set a bit, no shift
bitSetRaw :: (Bits a) => BitIx -> a -> a -> a
bitSetRaw i v orig = ((bitUMask i) .&. orig) .|. ((bitMask i) .&. v)

-- bit set to 1
bitSet1 :: (Bits a) => BitIx -> a -> a
bitSet1 i v = v .|. (bitMask i)

-- bit set to 0
bitSet0 :: (Bits a) => BitIx -> a -> a
bitSet0 i v = v .&. (bitUMask i)

-- }}}

-- field operation {{{

-- mask a field
fldMask :: (Bits a) => BitFld -> a
fldMask (l,r) = lb .|. (lb - rb)
  where lb = bit $ cast l
        rb = bit $ cast r

-- unmask a field
fldUMask :: (Bits a) => BitFld -> a
fldUMask = complement . fldMask

-- get field
fldGet :: (Bits a) => BitFld -> a -> a
fldGet f@(_,r) v = (v .&. (fldMask f)) `shiftR` (cast r)

-- set field: field -> value -> orig
fldSet :: (Bits a) => BitFld -> a -> a -> a
fldSet f@(_,r) v orig =
  ((fldUMask f) .&. orig) .|. ((fldMask f) .&. (v `shiftL` (cast r)))

-- get field, no shift
fldGetRaw :: (Bits a) => BitFld -> a -> a
fldGetRaw f v = v .&. (fldMask f)

-- set field, no shift: field -> value -> orig
fldSetRaw :: (Bits a) => BitFld -> a -> a -> a
fldSetRaw f v orig = ((fldUMask f) .&. orig) .|. ((fldMask f) .&. v)

-- set field to 1
fldSet1 :: (Bits a) => BitFld -> a -> a
fldSet1 f v = v .|. (fldMask f)

-- set field to 0
fldSet0 :: (Bits a) => BitFld -> a -> a
fldSet0 f v = v .&. (fldUMask f)

-- }}}

-- sign extend {{{

signExt :: (Bits a) => BitIx -> a -> a
signExt b v = case bitGet b v of
  0 -> v
  1 -> fldSet (hiBit, max b hiBit) (-1) v
  where hiBit = cast $ (bitSize v) - 1 :: BitIx

signExtByte :: (Bits a) => a -> a
signExtByte = signExt 7

signExtWyde :: (Bits a) => a -> a
signExtWyde = signExt 15

signExtTetra :: (Bits a) => a -> a
signExtTetra = signExt 31

-- }}}

-- }}}

-- coerce type casting {{{

-- FP <-> Octa {{{

-- cast FP (64-bits floating-point) to Octa
castFtoO :: FP -> Octa
castFtoO = unsafeCoerce

-- cast Octa to FP
castOtoF :: Octa -> FP
castOtoF = unsafeCoerce

-- }}}

-- FP <-> OctaS {{{

-- cast FP (64-bits floating-point) to Octa
castFtoOS :: FP -> OctaS
castFtoOS = unsafeCoerce

-- cast Octa to FP
castOStoF :: OctaS -> FP
castOStoF = unsafeCoerce

-- }}}

-- SFP <-> Tetra {{{

-- cast SFP (32-bits floating-point) to Tetra
castSFtoT:: SFP -> Tetra
castSFtoT = unsafeCoerce

-- cast Octa to FP
castTtoSF :: Tetra -> SFP
castTtoSF = unsafeCoerce

-- }}}

-- Octa <-> OctaS {{{

castOtoOS :: Octa -> OctaS
castOtoOS = unsafeCoerce

castOStoO :: OctaS -> Octa
castOStoO = unsafeCoerce

-- }}}

-- Tetra <-> TetraS {{{

castTtoTS :: Tetra -> TetraS
castTtoTS = unsafeCoerce

castTStoT :: TetraS -> Tetra
castTStoT = unsafeCoerce

-- }}}

-- Wyde <-> WydeS {{{

castWtoWS :: Wyde -> WydeS
castWtoWS = unsafeCoerce

castWStoW :: WydeS -> Wyde
castWStoW = unsafeCoerce

-- }}}

-- Byte <-> ByteS {{{

castBtoBS :: Byte -> ByteS
castBtoBS = unsafeCoerce

castBStoB :: ByteS -> Byte
castBStoB = unsafeCoerce

-- }}}

-- }}}

-- common registers {{{

type RIx = RAddr      -- register Index
type SRIx = RIx       -- special register index
type GRIx = RIx       -- general register index

type RD = IOUArray RAddr Octa  -- register device

-- caution: unsafePerformIO, debug only, remove later
{-
instance Show RD where
  show a = show elist
    where elist = unsafePerformIO $ getElems a
-}

-- set register
rSet :: RD -> RIx -> Octa -> IO ()
rSet = writeArray

-- get register
rGet :: RD -> RIx -> IO Octa
rGet = readArray

-- }}}

-- special registers {{{

-- new special register device
newSRD :: IO RD
newSRD = newArray (0,31) 0

-- rB, rD, rE, rH, rJ, rM, rR, rBB --  0 ~  7 {{{

-- rB, bootstrap register (trip) [0];
rBIx  :: SRIx
rBIx  = 0
-- rD, dividend register [1];
rDIx  :: SRIx
rDIx  = 1
-- rE, epsilon register [2];
rEIx  :: SRIx
rEIx  = 2
-- rH, himult register [3];
rHIx  :: SRIx
rHIx  = 3
-- rJ, return-jump register [4];
rJIx  :: SRIx
rJIx  = 4
-- rM, multiplex mask register [5];
rMIx  :: SRIx
rMIx  = 5
-- rR, remainder register [6];
rRIx  :: SRIx
rRIx  = 6
-- rBB, bootstrap register (trap) [7];
rBBIx :: SRIx
rBBIx = 7

-- }}}

-- rC, rN, rO, rS, rI, rT, rTT,rK  --  8 ~ 15 {{{

-- rC, continuation register [8];
rCIx  :: SRIx
rCIx  = 8
-- rN, serial number [9];
rNIx  :: SRIx
rNIx  = 9
-- rO, register stack offset [10];
rOIx  :: SRIx
rOIx  = 10
-- rS, register stack pointer [11];
rSIx  :: SRIx
rSIx  = 11
-- rI, interval counter [12];
rIIx  :: SRIx
rIIx  = 12
-- rT, trap address register [13];
rTIx  :: SRIx
rTIx  = 13
-- rTT, dynamic trap address register [14];
rTTIx :: SRIx
rTTIx = 14
-- rK, interrupt mask register [15];
rKIx  :: SRIx
rKIx  = 15

-- }}}

-- rQ, rU, rV, rG, rL, rA, rF, rP  -- 16 ~ 23 {{{

-- rQ, interrupt request register [16];
rQIx  :: SRIx
rQIx  = 16
-- rU, usage counter [17];
rUIx  :: SRIx
rUIx  = 17
-- rV, virtual translation register [18];
rVIx  :: SRIx
rVIx  = 18
-- rG, global threshold register [19];
rGIx  :: SRIx
rGIx  = 19
-- rL, local threshold register [20];
rLIx  :: SRIx
rLIx  = 20
-- rA, arithmetic status register [21];
rAIx  :: SRIx
rAIx  = 21
-- rF, failure location register [22];
rFIx  :: SRIx
rFIx  = 22
-- rP, prediction register [23];
rPIx  :: SRIx
rPIx  = 23

-- }}}

-- rW, rX, rY, rZ, rWW,rXX,rYY,rZZ -- 24 ~ 31 {{{

-- rW, where-interrupted register (trip) [24];
rWIx  :: SRIx
rWIx  = 24
-- rX, execution register (trip) [25];
rXIx  :: SRIx
rXIx  = 25
-- rY, Y operand (trip) [26];
rYIx  :: SRIx
rYIx  = 26
-- rZ, Z operand (trip) [27];
rZIx  :: SRIx
rZIx  = 27
-- rWW, where-interrupted register (trap) [28];
rWWIx :: SRIx
rWWIx = 28
-- rXX, execution register (trap) [29];
rXXIx :: SRIx
rXXIx = 29
-- rYY, Y operand (trap) [30];
rYYIx :: SRIx
rYYIx = 30
-- rZZ, Z operand (trap) [31];
rZZIx :: SRIx
rZZIx = 31

-- }}}

-- field operation info {{{

-- *Get :: Octa -> Octa
--   OrigRegValue -> FieldValue

-- *Set :: Octa -> Octa -> Octa
--   OrigRegValue -> NewFieldValue -> NewRegValue

-- }}}

-- rK fields {{{

rKFlow :: BitFld
rKFlow = (63,40)

rKFprog :: BitFld
rKFprog = (39,32)

rKFhigh :: BitFld
rKFhigh = (31,8)

rKFmachine :: BitFld
rKFmachine = (7,0)

rKFr :: BitFld
rKFr = (39,39)

rKFw :: BitFld
rKFw = (38,38)

rKFx :: BitFld
rKFx = (37,37)

rKFn :: BitFld
rKFn = (36,36)

rKFk :: BitFld
rKFk = (35,35)

rKFb :: BitFld
rKFb = (34,34)

rKFs :: BitFld
rKFs = (33,33)

rKFp :: BitFld
rKFp = (32,32)

rKBr :: BitIx
rKBr = 39

rKBw :: BitIx
rKBw = 38

rKBx :: BitIx
rKBx = 37

rKBn :: BitIx
rKBn = 36

rKBk :: BitIx
rKBk = 35

rKBb :: BitIx
rKBb = 34

rKBs :: BitIx
rKBs = 33

rKBp :: BitIx
rKBp = 32

-- }}}

-- rQ fields {{{

rQFlow :: BitFld
rQFlow = (63,40)

rQFprog :: BitFld
rQFprog = (39,32)

rQFhigh :: BitFld
rQFhigh = (31,8)

rQFmachine :: BitFld
rQFmachine = (7,0)

rQFr :: BitFld
rQFr = (39,39)

rQFw :: BitFld
rQFw = (38,38)

rQFx :: BitFld
rQFx = (37,37)

rQFn :: BitFld
rQFn = (36,36)

rQFk :: BitFld
rQFk = (35,35)

rQFb :: BitFld
rQFb = (34,34)

rQFs :: BitFld
rQFs = (33,33)

rQFp :: BitFld
rQFp = (32,32)

rQBr :: BitIx
rQBr = 39

rQBw :: BitIx
rQBw = 38

rQBx :: BitIx
rQBx = 37

rQBn :: BitIx
rQBn = 36

rQBk :: BitIx
rQBk = 35

rQBb :: BitIx
rQBb = 34

rQBs :: BitIx
rQBs = 33

rQBp :: BitIx
rQBp = 32


-- }}}

-- rV fields {{{

-- bit   63      55      47      39      31      23      15      7
-- 63->0 ^-------^-------^-------^-------^-------^-------^-------^-------
-- rV:   {b1}{b2}{b3}{b4}{s.....}{r........................}{n.......}{f}

rVFb1 :: BitFld
rVFb1 = (63,60)

rVFb2 :: BitFld
rVFb2 = (59,56)

rVFb3 :: BitFld
rVFb3 = (55,52)

rVFb4 :: BitFld
rVFb4 = (51,48)

rVFs :: BitFld
rVFs =  (47,40)

rVFr :: BitFld
rVFr =  (39,13)

rVFn :: BitFld
rVFn =  (12,3)

rVFf :: BitFld
rVFf =  (2,0)

-- }}}

-- rG fields {{{

rGFg :: BitFld
rGFg = (7,0)

-- }}}

-- rL fields {{{

rLFl :: BitFld
rLFl = (7,0)

-- }}}

-- rA fields {{{

-- Enable bits *8 {{{

rAFEnable :: BitFld
rAFEnable = (15,8)

-- D: integer divide check
rAFEnableD :: BitFld
rAFEnableD = (15,15)

-- V: integer overflow
rAFEnableV :: BitFld
rAFEnableV = (14,14)

-- W: float-to-fix overflow
rAFEnableW :: BitFld
rAFEnableW = (13,13)

-- I: invalid operation
rAFEnableI :: BitFld
rAFEnableI = (12,12)

-- O: floating overflow
rAFEnableO :: BitFld
rAFEnableO = (11,11)

-- U: floating underflow
rAFEnableU :: BitFld
rAFEnableU = (10,10)

-- Z: floating division by zero
rAFEnableZ :: BitFld
rAFEnableZ = (9,9)

-- X: floating inexact
rAFEnableX :: BitFld
rAFEnableX = (8,8)

rABEnableD :: BitIx
rABEnableD = 15

rABEnableV :: BitIx
rABEnableV = 14

rABEnableW :: BitIx
rABEnableW = 13

rABEnableI :: BitIx
rABEnableI = 12

rABEnableO :: BitIx
rABEnableO = 11

rABEnableU :: BitIx
rABEnableU = 10

rABEnableZ :: BitIx
rABEnableZ = 9

rABEnableX :: BitIx
rABEnableX = 8


-- }}}

-- Event bits *8 {{{

rAFEvent :: BitFld
rAFEvent = (7,0)

rAFEventD :: BitFld
rAFEventD = (7,7)

rAFEventV :: BitFld
rAFEventV = (6,6)

rAFEventW :: BitFld
rAFEventW = (5,5)

rAFEventI :: BitFld
rAFEventI = (4,4)

rAFEventO :: BitFld
rAFEventO = (3,3)

rAFEventU :: BitFld
rAFEventU = (2,2)

rAFEventZ :: BitFld
rAFEventZ = (1,1)

rAFEventX :: BitFld
rAFEventX = (0,0)

rABEventD :: BitIx
rABEventD = 7

rABEventV :: BitIx
rABEventV = 6

rABEventW :: BitIx
rABEventW = 5

rABEventI :: BitIx
rABEventI = 4

rABEventO :: BitIx
rABEventO = 3

rABEventU :: BitIx
rABEventU = 2

rABEventZ :: BitIx
rABEventZ = 1

rABEventX :: BitIx
rABEventX = 0

-- }}}

-- Round Mode {{{

-- 00: round to nearest (default)
-- 01: round off (towards zero)
-- 10: round up (towards +inf)
-- 11: round down (towards -inf)

rAFRoundMode :: BitFld
rAFRoundMode = (17,16)

-- }}}

-- }}}

-- rX fields {{{

rXFsign :: BitFld
rXFsign = (63,63)

rXFinsn :: BitFld
rXFinsn = (31,0)

rXFrop :: BitFld
rXFrop = (62,56)

-- }}}

-- rXX fields {{{

rXXFsign :: BitFld
rXXFsign = (63,63)

rXXFinsn :: BitFld
rXXFinsn = (31,0)

rXXFrop :: BitFld
rXXFrop = (62,56)

rXXFprog :: BitFld
rXXFprog = (39,32)

rXXFr :: BitFld
rXXFr = rQFr

rXXFw :: BitFld
rXXFw = rQFw

rXXFx :: BitFld
rXXFx = rQFx

rXXFn :: BitFld
rXXFn = rQFn

rXXFk :: BitFld
rXXFk = rQFk

rXXFb :: BitFld
rXXFb = rQFb

rXXFs :: BitFld
rXXFs = rQFs

rXXFp :: BitFld
rXXFp = rQFp

rXXBr :: BitIx
rXXBr = rQBr

rXXBw :: BitIx
rXXBw = rQBw

rXXBx :: BitIx
rXXBx = rQBx

rXXBn :: BitIx
rXXBn = rQBn

rXXBk :: BitIx
rXXBk = rQBk

rXXBb :: BitIx
rXXBb = rQBb

rXXBs :: BitIx
rXXBs = rQBs

rXXBp :: BitIx
rXXBp = rQBp
-- }}}

-- }}}

-- general registers {{{

-- new general register device
newGRD :: IO RD
newGRD = newArray (0,255) 0

-- }}}

-- instruction field operations {{{

-- get insn fields {{{

-- get opcode byte
iGetOPCode :: Insn -> OPCode
iGetOPCode i = cast $ shiftR i 24

-- get X byte
iGetX :: Insn -> X
iGetX i = cast $ shiftR i 16

-- get Y byte
iGetY :: Insn -> Y
iGetY i = cast $ shiftR i 8

-- get Z byte
iGetZ :: Insn -> Z
iGetZ i = cast i

-- get YZ bytes
iGetYZ :: Insn -> YZ
iGetYZ i = ((cast $ shiftR i 8), cast i)

-- get XYZ bytes
iGetXYZ :: Insn -> XYZ
iGetXYZ i = ((cast $ shiftR i 16), (cast $ shiftR i 8), cast i)

-- }}}

-- get field unsigned {{{

-- get X value
iGetXu :: Insn -> Octa
iGetXu = cast . iGetX

-- get Y value
iGetYu :: Insn -> Octa
iGetYu = cast . iGetY

-- get Z value
iGetZu :: Insn -> Octa
iGetZu = cast . iGetZ

-- get YZ value
iGetYZu :: Insn -> Octa
iGetYZu i = cast $ i .&. 0xffff

-- get XYZ value
iGetXYZu :: Insn -> Octa
iGetXYZu i = cast $ i .&. 0xffffff

-- }}}

-- field field signed {{{

-- get X value sign-extended
iGetXs :: Insn -> Octa
iGetXs = signExtByte . iGetXu

-- get Y value sign-extended
iGetYs :: Insn -> Octa
iGetYs = signExtByte . iGetYu

-- get Z value sign-extended
iGetZs :: Insn -> Octa
iGetZs = signExtByte . iGetZu

-- get YZ value sign-extended
iGetYZs :: Insn -> Octa
iGetYZs = signExtWyde . iGetYZu

-- get XYZ value sign-extended
iGetXYZs :: Insn -> Octa
iGetXYZs = signExt 23 . iGetXYZu

-- }}}

-- }}}

-- opcode (InsnOP) and decode {{{

-- data InsnOP {{{

data InsnOP
  = TRAP      | FCMP      | FUN       | FEQL
  | FADD      | FIX       | FSUB      | FIXU
  | FLOT      | FLOTI     | FLOTU     | FLOTUI
  | SFLOT     | SFLOTI    | SFLOTU    | SFLOTUI
  | FMUL      | FCMPE     | FUNE      | FEQLE
  | FDIV      | FSQRT     | FREM      | FINT
  | MUL       | MULI      | MULU      | MULUI
  | DIV       | DIVI      | DIVU      | DIVUI
  | ADD       | ADDI      | ADDU      | ADDUI
  | SUB       | SUBI      | SUBU      | SUBUI
  | IIADDU    | IIADDUI   | IVADDU    | IVADDUI
  | VIIIADDU  | VIIIADDUI | XVIADDU   | XVIADDUI
  | CMP       | CMPI      | CMPU      | CMPUI
  | NEG       | NEGI      | NEGU      | NEGUI
  | SL        | SLI       | SLU       | SLUI
  | SR        | SRI       | SRU       | SRUI
  | BN        | BNB       | BZ        | BZB
  | BP        | BPB       | BOD       | BODB
  | BNN       | BNNB      | BNZ       | BNZB
  | BNP       | BNPB      | BEV       | BEVB
  | PBN       | PBNB      | PBZ       | PBZB
  | PBP       | PBPB      | PBOD      | PBODB
  | PBNN      | PBNNB     | PBNZ      | PBNZB
  | PBNP      | PBNPB     | PBEV      | PBEVB
  | CSN       | CSNI      | CSZ       | CSZI
  | CSP       | CSPI      | CSOD      | CSODI
  | CSNN      | CSNNI     | CSNZ      | CSNZI
  | CSNP      | CSNPI     | CSEV      | CSEVI
  | ZSN       | ZSNI      | ZSZ       | ZSZI
  | ZSP       | ZSPI      | ZSOD      | ZSODI
  | ZSNN      | ZSNNI     | ZSNZ      | ZSNZI
  | ZSNP      | ZSNPI     | ZSEV      | ZSEVI
  | LDB       | LDBI      | LDBU      | LDBUI
  | LDW       | LDWI      | LDWU      | LDWUI
  | LDT       | LDTI      | LDTU      | LDTUI
  | LDO       | LDOI      | LDOU      | LDOUI
  | LDSF      | LDSFI     | LDHT      | LDHTI
  | CSWAP     | CSWAPI    | LDUNC     | LDUNCI
  | LDVTS     | LDVTSI    | PRELD     | PRELDI
  | PREGO     | PREGOI    | GO        | GOI
  | STB       | STBI      | STBU      | STBUI
  | STW       | STWI      | STWU      | STWUI
  | STT       | STTI      | STTU      | STTUI
  | STO       | STOI      | STOU      | STOUI
  | STSF      | STSFI     | STHT      | STHTI
  | STCO      | STCOI     | STUNC     | STUNCI
  | SYNCD     | SYNCDI    | PREST     | PRESTI
  | SYNCID    | SYNCIDI   | PUSHGO    | PUSHGOI
  | OR        | ORI       | ORN       | ORNI
  | NOR       | NORI      | XOR       | XORI
  | AND       | ANDI      | ANDN      | ANDNI
  | NAND      | NANDI     | NXOR      | NXORI
  | BDIF      | BDIFI     | WDIF      | WDIFI
  | TDIF      | TDIFI     | ODIF      | ODIFI
  | MUX       | MUXI      | SADD      | SADDI
  | MOR       | MORI      | MXOR      | MXORI
  | SETH      | SETMH     | SETML     | SETL
  | INCH      | INCMH     | INCML     | INCL
  | ORH       | ORMH      | ORML      | ORL
  | ANDNH     | ANDNMH    | ANDNML    | ANDNL
  | JMP       | JMPB      | PUSHJ     | PUSHJB
  | GETA      | GETAB     | PUT       | PUTI
  | POP       | RESUME    | SAVE      | UNSAVE
  | SYNC      | SWYM      | GET       | TRIP
  deriving (Show, Eq)

-- }}}

-- opList, from index 0 to 255 {{{
opList :: [InsnOP]
opList =
  [ TRAP, FCMP, FUN, FEQL, FADD, FIX, FSUB, FIXU -- #00 ~ #07
  , FLOT, FLOTI, FLOTU, FLOTUI, SFLOT, SFLOTI, SFLOTU, SFLOTUI -- #08 ~ #0f
  , FMUL, FCMPE, FUNE, FEQLE, FDIV, FSQRT, FREM, FINT
  , MUL, MULI, MULU, MULUI, DIV, DIVI, DIVU, DIVUI

  , ADD, ADDI, ADDU, ADDUI, SUB, SUBI, SUBU, SUBUI -- #20 ~ #27
  , IIADDU, IIADDUI, IVADDU, IVADDUI               -- #28 ~ #2b
  , VIIIADDU, VIIIADDUI, XVIADDU, XVIADDUI         -- #2c ~ #2f
  , CMP, CMPI, CMPU, CMPUI, NEG, NEGI, NEGU, NEGUI
  , SL, SLI, SLU, SLUI, SR, SRI, SRU, SRUI

  , BN, BNB, BZ, BZB, BP, BPB, BOD, BODB           -- #40 ~ #47
  , BNN, BNNB, BNZ, BNZB, BNP, BNPB, BEV, BEVB
  , PBN, PBNB, PBZ, PBZB, PBP, PBPB, PBOD, PBODB
  , PBNN, PBNNB, PBNZ, PBNZB, PBNP, PBNPB, PBEV, PBEVB

  , CSN, CSNI, CSZ, CSZI, CSP, CSPI, CSOD, CSODI   -- #60 ~ #67
  , CSNN, CSNNI, CSNZ, CSNZI, CSNP, CSNPI, CSEV, CSEVI
  , ZSN, ZSNI, ZSZ, ZSZI, ZSP, ZSPI, ZSOD, ZSODI
  , ZSNN, ZSNNI, ZSNZ, ZSNZI, ZSNP, ZSNPI, ZSEV, ZSEVI

  , LDB, LDBI, LDBU, LDBUI, LDW, LDWI, LDWU, LDWUI -- #80 ~ #87
  , LDT, LDTI, LDTU, LDTUI, LDO, LDOI, LDOU, LDOUI
  , LDSF, LDSFI, LDHT, LDHTI, CSWAP, CSWAPI, LDUNC, LDUNCI
  , LDVTS, LDVTSI, PRELD, PRELDI, PREGO, PREGOI, GO, GOI

  , STB, STBI, STBU, STBUI, STW, STWI, STWU, STWUI -- #A0 ~ #A7
  , STT, STTI, STTU, STTUI, STO, STOI, STOU, STOUI
  , STSF, STSFI, STHT, STHTI, STCO, STCOI, STUNC, STUNCI
  , SYNCD, SYNCDI, PREST, PRESTI, SYNCID, SYNCIDI, PUSHGO, PUSHGOI

  , OR, ORI, ORN, ORNI, NOR, NORI, XOR, XORI       -- #C0 ~ #C7
  , AND, ANDI, ANDN, ANDNI, NAND, NANDI, NXOR, NXORI
  , BDIF, BDIFI, WDIF, WDIFI, TDIF, TDIFI, ODIF, ODIFI
  , MUX, MUXI, SADD, SADDI, MOR, MORI, MXOR, MXORI

  , SETH, SETMH, SETML, SETL, INCH, INCMH, INCML, INCL
  , ORH, ORMH, ORML, ORL, ANDNH, ANDNMH, ANDNML, ANDNL
  , JMP, JMPB, PUSHJ, PUSHJB, GETA, GETAB, PUT, PUTI
  , POP, RESUME, SAVE, UNSAVE, SYNC, SWYM, GET, TRIP
  ]

-- }}}

opIxList :: [OPCode]
opIxList = [0 .. 255]

opListMap :: [(OPCode, InsnOP)]
opListMap = zip opIxList opList

opMap :: Map.Map OPCode InsnOP
opMap = Map.fromList opListMap

deOPCode :: Insn -> InsnOP
deOPCode insn = opMap Map.! (iGetOPCode insn)

-- }}}

-- 64-bits physical address interface {{{

-- Read command type and response {{{
{-
data AReadType = AReadByte | AReadWyde | AReadTetra | AReadOcta
  deriving (Show, Eq)

data AReadResp
  = AReadOkByte   Byte
  | AReadOkWyde   Wyde
  | AReadOkTetra  Tetra
  | AReadOkOcta   Octa
  | AReadFail
  deriving (Show, Eq)
-}
-- }}}

-- Write command type and response {{{
{-
data AWriteType
  = AWriteByte   Byte
  | AWriteWyde   Wyde
  | AWriteTetra  Tetra
  | AWriteOcta   Octa
  deriving (Show, Eq)

data AWriteResp = AWriteOk | AWriteFail
  deriving (Show, Eq)
-}
-- }}}

-- Abstract physical device
-- Read/Write by PAddr (64-bits address)
class Device dev where
  devAddrOk     :: dev -> PAddr -> Bool
  devReadOcta   :: dev -> PAddr -> IO (Maybe Octa)
  devReadTetra  :: dev -> PAddr -> IO (Maybe Tetra)
  devReadWyde   :: dev -> PAddr -> IO (Maybe Wyde)
  devReadByte   :: dev -> PAddr -> IO (Maybe Byte)
  devWriteOcta  :: dev -> PAddr -> Octa -> IO Bool
  devWriteTetra :: dev -> PAddr -> Tetra -> IO Bool
  devWriteWyde  :: dev -> PAddr -> Wyde -> IO Bool
  devWriteByte  :: dev -> PAddr -> Byte -> IO Bool

  -- default implementations
  devReadTetra omem paddr = do
    maybeO <- devReadOcta omem paddr
    return $ maybeO >>= return . xot paddr

  devReadWyde omem paddr = do
    maybeO <- devReadOcta omem paddr
    return $ maybeO >>= return . xow paddr

  devReadByte omem paddr = do
    maybeO <- devReadOcta omem paddr
    return $ maybeO >>= return . xob paddr

  devWriteTetra omem paddr val = do
    maybeO <- devReadOcta omem paddr
    case maybeO of
      Just orig -> devWriteOcta omem paddr $ mot paddr orig val
      _ -> return False
  devWriteWyde omem paddr val = do
    maybeO <- devReadOcta omem paddr
    case maybeO of
      Just orig -> devWriteOcta omem paddr $ mow paddr orig val
      _ -> return False
  devWriteByte omem paddr val = do
    maybeO <- devReadOcta omem paddr
    case maybeO of
      Just orig -> devWriteOcta omem paddr $ mob paddr orig val
      _ -> return False

-- }}}

-- dummy zero dev, accept any address {{{

data ZDev = ZDev
--instance Device ZDev where

-- }}}

-- MMIX machine model {{{

-- the MMIX {{{
--   1 pc
--   64 spr
--   256 gpr
--   some memory-mapped devices.
data MMIX =
  forall dev.  (Device dev) =>
  MMIX
  { mmixPC   :: IORef VAddr
  , mmixSRD  :: RD
  , mmixGRD  :: RD
  , mmixDev  :: dev
  }
-- }}}

-- PC {{{
mmixGetPC :: MMIX -> IO VAddr
mmixGetPC = readIORef . mmixPC

mmixSetPC :: MMIX -> VAddr -> IO ()
mmixSetPC mmix = writeIORef (mmixPC mmix)
-- }}}

-- SR {{{
mmixGetSR :: MMIX -> SRIx -> IO Octa
mmixGetSR mmix = rGet $ mmixSRD mmix

mmixSetSR :: MMIX -> SRIx -> Octa -> IO ()
mmixSetSR mmix = rSet $ mmixSRD mmix
-- }}}

-- GR {{{
mmixGetGR :: MMIX -> GRIx -> IO Octa
mmixGetGR mmix ix = do
  l <- xob 7 `fmap` mmixGetSR mmix rLIx
  g <- xob 7 `fmap` mmixGetSR mmix rGIx
  if ix < l || ix >= g
  then rGet (mmixGRD mmix) ix
  else return 0

mmixSetGR :: MMIX -> GRIx -> Octa -> IO ()
mmixSetGR mmix ix v = do
  rL <- mmixGetSR mmix rLIx
  rG <- mmixGetSR mmix rGIx
  let l = cast rL :: GRIx
  let g = cast rG :: GRIx
  when (l <= ix && ix < g) $ do
    mapM (\i -> rSet (mmixGRD mmix) i 0) [l..(ix - 1)]
    mmixSetSR mmix rLIx $ cast (ix + 1)
  rSet (mmixGRD mmix) ix v

-- }}}

-- memory-mapped devices {{{
mmixLdOcta :: MMIX -> PAddr -> IO (Maybe Octa)
mmixLdOcta (MMIX _ _ _ dev) = devReadOcta dev

mmixLdTetra :: MMIX -> PAddr -> IO (Maybe Tetra)
mmixLdTetra (MMIX _ _ _ dev) = devReadTetra dev

mmixLdWyde :: MMIX -> PAddr -> IO (Maybe Wyde)
mmixLdWyde (MMIX _ _ _ dev) = devReadWyde dev

mmixLdByte :: MMIX -> PAddr -> IO (Maybe Byte)
mmixLdByte (MMIX _ _ _ dev) = devReadByte dev

mmixLdOcta0 :: MMIX -> PAddr -> IO Octa
mmixLdOcta0 mmix paddr = fromMaybe 0 `fmap` mmixLdOcta mmix paddr

mmixLdTetra0 :: MMIX -> PAddr -> IO Tetra
mmixLdTetra0 mmix paddr = fromMaybe 0 `fmap` mmixLdTetra mmix paddr

mmixLdWyde0 :: MMIX -> PAddr -> IO Wyde
mmixLdWyde0 mmix paddr = fromMaybe 0 `fmap` mmixLdWyde mmix paddr

mmixLdByte0 :: MMIX -> PAddr -> IO Byte
mmixLdByte0 mmix paddr = fromMaybe 0 `fmap` mmixLdByte mmix paddr

mmixStOcta :: MMIX -> PAddr -> Octa -> IO Bool
mmixStOcta (MMIX _ _ _ dev) = devWriteOcta dev

mmixStTetra :: MMIX -> PAddr -> Tetra -> IO Bool
mmixStTetra (MMIX _ _ _ dev) = devWriteTetra dev

mmixStWyde :: MMIX -> PAddr -> Wyde -> IO Bool
mmixStWyde (MMIX _ _ _ dev) = devWriteWyde dev

mmixStByte :: MMIX -> PAddr -> Byte -> IO Bool
mmixStByte (MMIX _ _ _ dev) = devWriteByte dev
-- }}}

--newDummyMMIX :: IO MMIX
--newDummyMMIX = MMIX <$> newIORef 0 <*> newSRD <*> newGRD <*> return ZDev

-- }}}

-- MMIX helpers {{{

-- GR operation for OctaS {{{
mmixGetGRS :: MMIX -> GRIx -> IO OctaS
mmixGetGRS mmix ix = castOtoOS `fmap` mmixGetGR mmix ix

mmixSetGRS :: MMIX -> GRIx -> OctaS -> IO ()
mmixSetGRS mmix ix v = mmixSetGR mmix ix $ castOStoO v
-- }}}

-- GR operation for FP {{{
mmixGetGRF :: MMIX -> GRIx -> IO FP
mmixGetGRF mmix ix = castOtoF `fmap` mmixGetGR mmix ix

mmixSetGRF :: MMIX -> GRIx -> FP -> IO ()
mmixSetGRF mmix ix v = mmixSetGR mmix ix $ castFtoO v
-- }}}

-- set $X by insn {{{
mmixSetGRX :: MMIX -> Insn -> Octa -> IO ()
mmixSetGRX mmix insn v = mmixSetGR mmix (iGetX insn) v

mmixSetGRSX :: MMIX -> Insn -> OctaS -> IO ()
mmixSetGRSX mmix insn v = mmixSetGRS mmix (iGetX insn) v

mmixSetGRFX :: MMIX -> Insn -> FP -> IO ()
mmixSetGRFX mmix insn v = mmixSetGRF mmix (iGetX insn) v
-- }}}

-- get $X by insn {{{
mmixGetGRX :: MMIX -> Insn -> IO Octa
mmixGetGRX mmix insn = mmixGetGR mmix $ iGetX insn

mmixGetGRSX :: MMIX -> Insn -> IO OctaS
mmixGetGRSX mmix insn = mmixGetGRS mmix $ iGetX insn

mmixGetGRFX :: MMIX -> Insn -> IO FP
mmixGetGRFX mmix insn = mmixGetGRF mmix $ iGetX insn
-- }}}

-- get $Y by insn {{{
mmixGetGRY :: MMIX -> Insn -> IO Octa
mmixGetGRY mmix insn = mmixGetGR mmix $ iGetY insn

mmixGetGRSY :: MMIX -> Insn -> IO OctaS
mmixGetGRSY mmix insn = mmixGetGRS mmix $ iGetY insn

mmixGetGRFY :: MMIX -> Insn -> IO FP
mmixGetGRFY mmix insn = mmixGetGRF mmix $ iGetY insn
-- }}}

-- get $Z by insn {{{
mmixGetGRZ :: MMIX -> Insn -> IO Octa
mmixGetGRZ mmix insn = mmixGetGR mmix $ iGetZ insn

mmixGetGRSZ :: MMIX -> Insn -> IO OctaS
mmixGetGRSZ mmix insn = mmixGetGRS mmix $ iGetZ insn

mmixGetGRFZ :: MMIX -> Insn -> IO FP
mmixGetGRFZ mmix insn = mmixGetGRF mmix $ iGetZ insn
-- }}}

-- get $Y and $Z by insn {{{
mmixGetGRYZ :: MMIX -> Insn -> IO (Octa, Octa)
mmixGetGRYZ mmix insn = do
  y <- mmixGetGR mmix $ iGetY insn
  z <- mmixGetGR mmix $ iGetZ insn
  return (y, z)

mmixGetGRSYZ :: MMIX -> Insn -> IO (OctaS, OctaS)
mmixGetGRSYZ mmix insn = do
  y <- mmixGetGRS mmix $ iGetY insn
  z <- mmixGetGRS mmix $ iGetZ insn
  return (y, z)

mmixGetGRFYZ :: MMIX -> Insn -> IO (FP, FP)
mmixGetGRFYZ mmix insn = do
  y <- mmixGetGRF mmix $ iGetY insn
  z <- mmixGetGRF mmix $ iGetZ insn
  return (y, z)
-- }}}

-- }}}

-- Virtual address mapping {{{

-- virtual address fields {{{

vaddrFaddr :: BitFld
vaddrFaddr = (60,0)

vaddrFseg :: BitFld
vaddrFseg = (62,61)

vaddrFsign :: BitFld
vaddrFsign = (63,63)

-- }}}

-- page table entry {{{

pteFx :: BitFld
pteFx = (63,48)

pteFay :: BitFld
pteFay = (47,13)

pteFn :: BitFld
pteFn = (12,3)

pteFp :: BitFld
pteFp = (2,0)

pteFpr :: BitFld
pteFpr = (2,2)

pteFpw :: BitFld
pteFpw = (1,1)

pteFpx :: BitFld
pteFpx = (0,0)

pteBpr :: BitIx
pteBpr = 2

pteBpw :: BitIx
pteBpw = 1

pteBpx :: BitIx
pteBpx = 0

-- }}}

-- page table pointer {{{

ptpFc :: BitFld
ptpFc = (62,13)

ptpFn :: BitFld
ptpFn = (12,3)

ptpFq :: BitFld
ptpFq = (2,0)

ptpFnq :: BitFld
ptpFnq = (12,0)

-- }}}

-- virtual address function {{{

-- read/write/execute
--data VF = VFR | VFW | VFX

-- }}}

-- }}}

-- Trip/Trap type and entry point {{{

-- TRIP {{{
data TRIP
  = ITRIP  -- trip insn
  | ATRIPD -- integer divide check
  | ATRIPV -- integer overflow
  | ATRIPW -- float-to-fix overflow
  | ATRIPI -- invalid operation
  | ATRIPO -- floating overflow
  | ATRIPU -- floating underflow
  | ATRIPZ -- floating division by zero
  | ATRIPX -- floating inexact
-- }}}

-- tripEntry: TRIP -> entry-point {{{
tripEntry :: TRIP -> VAddr
tripEntry ITRIP  = 0x00
tripEntry ATRIPD = 0x10
tripEntry ATRIPV = 0x20
tripEntry ATRIPW = 0x30
tripEntry ATRIPI = 0x40
tripEntry ATRIPO = 0x50
tripEntry ATRIPU = 0x60
tripEntry ATRIPZ = 0x70
tripEntry ATRIPX = 0x80
-- }}}

-- tripAEBit: trip to arith-ex. bit {{{
tripAEBit :: TRIP -> Octa
tripAEBit ATRIPD = fldMask rAFEventD
tripAEBit ATRIPV = fldMask rAFEventV
tripAEBit ATRIPW = fldMask rAFEventW
tripAEBit ATRIPI = fldMask rAFEventI
tripAEBit ATRIPO = fldMask rAFEventO
tripAEBit ATRIPU = fldMask rAFEventU
tripAEBit ATRIPZ = fldMask rAFEventZ
tripAEBit ATRIPX = fldMask rAFEventX
tripAEBit ITRIP  = 0
-- }}}

-- tripCheck: checkTrip by rA {{{

-- check two bits is 1 (helper function) {{{
tripCheckDouble1 :: BitIx -> BitIx -> Octa -> Bool
tripCheckDouble1 b1 b2 rA = bitGet b1 rA == 1 && bitGet b2 rA == 1
-- }}}

-- check DVWIOUZX {{{

tripCheckD :: Octa -> Maybe TRIP
tripCheckD rA =
  if tripCheckDouble1 rABEventD rABEnableD rA
  then Just ATRIPD else Nothing

tripCheckV :: Octa -> Maybe TRIP
tripCheckV rA =
  if tripCheckDouble1 rABEventV rABEnableV rA
  then Just ATRIPV else Nothing

tripCheckW :: Octa -> Maybe TRIP
tripCheckW rA =
  if tripCheckDouble1 rABEventW rABEnableW rA
  then Just ATRIPW else Nothing

tripCheckI :: Octa -> Maybe TRIP
tripCheckI rA =
  if tripCheckDouble1 rABEventI rABEnableI rA
  then Just ATRIPI else Nothing

tripCheckO :: Octa -> Maybe TRIP
tripCheckO rA =
  if tripCheckDouble1 rABEventO rABEnableO rA
  then Just ATRIPO else Nothing

tripCheckU :: Octa -> Maybe TRIP
tripCheckU rA =
  if tripCheckDouble1 rABEventU rABEnableU rA
  then Just ATRIPU else Nothing

tripCheckZ :: Octa -> Maybe TRIP
tripCheckZ rA =
  if tripCheckDouble1 rABEventZ rABEnableZ rA
  then Just ATRIPZ else Nothing

tripCheckX :: Octa -> Maybe TRIP
tripCheckX rA =
  if tripCheckDouble1 rABEventX rABEnableX rA
  then Just ATRIPX else Nothing

-- }}}

-- a function list that checks all trips {{{ 
tripCheckList :: [(Octa -> Maybe TRIP)]
tripCheckList =
  [ tripCheckD
  , tripCheckV
  , tripCheckW
  , tripCheckI
  , tripCheckO
  , tripCheckU
  , tripCheckZ
  , tripCheckX
  ]
-- }}}

-- check for all trips {{{
tripCheck :: Octa -> [TRIP]
tripCheck rA = catMaybes $ map (\f -> f rA) tripCheckList
-- }}}

-- check for the most ungent trip {{{
tripCheckOne :: Octa -> Maybe TRIP
tripCheckOne rA = case tripCheck rA of
  [] -> Nothing
  (t:_) -> Just t
-- }}}

-- }}}

-- }}}

-- arithmetic basis {{{

-- ArithEx: ARITHmetic EXception {{{
data ArithEx = AED | AEV | AEW | AEI | AEO | AEU | AEZ | AEX deriving (Eq)
instance Show ArithEx where
  show AED = "\"integer devide check\""
  show AEV = "\"integer overflow\""
  show AEW = "\"float-to-fix overflow\""
  show AEI = "\"invalid operation\""
  show AEO = "\"floating overflow\""
  show AEU = "\"floating underflow\""
  show AEZ = "\"floating division by zero\""
  show AEX = "\"floating inexact\""
-- }}}

-- ArithRx: Arithmetic Result {{{
data ArithRx a = ArithRx
  { arithGetEx :: [ArithEx]
  , arithGetRx :: a
  }

instance (Show a) => Show (ArithRx a) where
  show (ArithRx [] rx) = show rx
  show (ArithRx ex rx) = show rx ++ "{" ++ show ex ++ "}"

instance Functor ArithRx where
  fmap f (ArithRx exa va) = ArithRx exa $ f va

instance Applicative ArithRx where
  pure a = ArithRx [] a
  ArithRx exf f <*> ArithRx ex v = ArithRx (ex ++ exf) $ f v

instance Monad ArithRx where
  return a = ArithRx [] a
  ArithRx exa va >>= f = ArithRx (exa ++ exb) vb
    where (ArithRx exb vb) = f va
-- }}}

-- cvAEtoT: convert arith-ex. to trip-type {{{
cvAEtoT :: ArithEx -> TRIP
cvAEtoT AED = ATRIPD
cvAEtoT AEV = ATRIPV
cvAEtoT AEW = ATRIPW
cvAEtoT AEI = ATRIPI
cvAEtoT AEO = ATRIPO
cvAEtoT AEU = ATRIPU
cvAEtoT AEZ = ATRIPZ
cvAEtoT AEX = ATRIPX
-- }}}

-- cvAEtoBit: convert arith-ex. to event bit {{{
cvAEtoBit :: ArithEx -> Octa
cvAEtoBit = tripAEBit . cvAEtoT
-- }}}

-- }}}

-- floating-point {{{

-- bit field {{{

-- FP {{{
fpBsign :: BitIx
fpBsign = 63

fpFsign :: BitFld
fpFsign = (63,63)

fpFe :: BitFld
fpFe = (62,52)

fpFf :: BitFld
fpFf = (51,0)

fpFse :: BitFld
fpFse = (63,52)

fpBnan :: BitIx
fpBnan = 51

fpFpl :: BitFld
fpFpl = (50,0)
-- }}}

-- SFP {{{
sfpBsign :: BitIx
sfpBsign = 31

sfpFsign :: BitFld
sfpFsign = (31,31)

sfpFe :: BitFld
sfpFe = (30,23)

sfpFf :: BitFld
sfpFf = (22,0)

sfpFse :: BitFld
sfpFse = (31,23)

sfpBnan :: BitIx
sfpBnan = 20

sfpFpl :: BitFld
sfpFpl = (19,0)
-- }}}

-- }}}

-- RoundMode {{{

data RoundMode
  = RNear
  | RZero
  | RUp
  | RDown
  deriving (Eq, Show)

-- convert bits to RoundMode {{{

toRoundMode :: Octa -> RoundMode
toRoundMode 0 = RNear
toRoundMode 1 = RZero
toRoundMode 2 = RUp
toRoundMode 3 = RDown

-- }}}

-- }}}

-- Sign {{{

data FSign = FPositive | FNegative deriving (Eq)

instance Show FSign where
  show FPositive = "+"
  show FNegative = "-"

instance Ord FSign where
  compare FPositive FNegative = GT
  compare FNegative FPositive = LT
  compare _ _ = EQ

-- convert bit to FSign {{{

-- from 0 or 1
toFSign :: (Bits a) => a -> FSign
toFSign sbit = if sbit == 0 then FPositive else FNegative

-- from 0x8000000000000000 or 0x0
toFSignRaw :: Octa -> FSign
toFSignRaw val = if bitGet fpBsign val == 0 then FPositive else FNegative

-- set FSign for a raw value
fpSetFSign :: FSign -> FP -> FP
fpSetFSign FPositive v = castOtoF $ bitSet fpBsign 0 $ castFtoO v
fpSetFSign FNegative v = castOtoF $ bitSet fpBsign 1 $ castFtoO v

-- set FSign for a raw value
fpSetFSignRaw :: FSign -> Octa -> Octa
fpSetFSignRaw FPositive v = bitSet fpBsign 0 v
fpSetFSignRaw FNegative v = bitSet fpBsign 1 v

-- set FSign for a raw short value
sfpSetFSignRaw :: FSign -> Tetra -> Tetra
sfpSetFSignRaw FPositive v = bitSet sfpBsign 0 v
sfpSetFSignRaw FNegative v = bitSet sfpBsign 1 v

-- }}}

-- flipFSign {{{
flipFSign :: FSign -> FSign
flipFSign FPositive = FNegative
flipFSign FNegative = FPositive
-- }}}

-- }}}

-- NaN Type {{{

data NaNType = SignalNaN | QuietNaN deriving (Eq)
instance Show NaNType where
  show SignalNaN = "sNaN"
  show QuietNaN = "qNaN"

-- convert bit to NaNType {{{
fpToNaN :: Octa -> NaNType
fpToNaN b = if b == 0 then SignalNaN else QuietNaN

fpToNaNRaw :: Octa -> NaNType
fpToNaNRaw val = if bitGet fpBnan val == 0 then SignalNaN else QuietNaN

sfpToNaNRaw :: Octa -> NaNType
sfpToNaNRaw val = if bitGet sfpBnan val == 0 then SignalNaN else QuietNaN

fpSetNaNRaw :: NaNType -> Octa -> Octa
fpSetNaNRaw SignalNaN val = bitSet fpBnan 0 val
fpSetNaNRaw QuietNaN val = bitSet fpBnan 1 val

-- payload -> orig -> newval
fpSetNaNPayload :: Octa -> FP -> FP
fpSetNaNPayload pl orig = castOtoF $ fldSet fpFpl pl $ castFtoO orig

-- payload -> orig -> newval
fpSetNaNPayloadRaw :: Octa -> Octa -> Octa
fpSetNaNPayloadRaw pl orig = fldSet fpFpl pl orig

-- payload -> orig -> newval
sfpSetNaNPayloadRaw :: Tetra -> Tetra -> Tetra
sfpSetNaNPayloadRaw pl orig = fldSet sfpFpl pl orig
-- }}}

-- }}}

-- FPInfo {{{

data FPInfo
  = Number FSign OctaS Octa    -- sign exp frac : f*(2^(e-1076))
  | NaN    FSign NaNType Octa  -- sign signal/quiet payload
  | Inf    FSign               -- +/-
  | Zero   FSign               -- +/-

instance Show FPInfo where
  show (Number s e f) =
      show s ++ show f ++ "*2^" ++ show (e - 1076)
      ++ " (e=" ++ show e
      ++ " f=#" ++ hx f ++ ")"
  show (NaN s ty p) = show s ++ show ty ++ " #" ++ hx p
  show (Inf s) = show s ++ "Inf"
  show (Zero s) = show s ++ "0.0"

-- }}}

-- fpUnpack {{{

fpUnpack :: FP -> FPInfo
fpUnpack fp = fpUnpacksef s e f
  where
    fp' = castFtoO fp
    s = bitGet fpBsign fp'
    e = fldGet fpFe fp'
    f = fldGet fpFf fp'

-- fpUnpacksef (helper for fpUnpack) {{{

fpUnpacksef :: Octa -> Octa -> Octa -> FPInfo
-- zero (e == 0 && f == 0)
fpUnpacksef s 0 0 = Zero $ toFSign s
-- subnormal (e == 0 && f > 0)
fpUnpacksef s 0 f = traceShow f $ Number s' e' f'
  where
    s' = toFSign s
    (e', f') = fpAlignRaw (0, f `shiftL` 2)
-- inf (e == 2047 && f == 0)
fpUnpacksef s 2047 0 = Inf $ toFSign s
-- nan (e == 2047 && f > 0)
fpUnpacksef s 2047 f = NaN (toFSign s) (fpToNaNRaw f) $ fldGet fpFpl f
-- normal (0 < e < 2047)
fpUnpacksef s e f = Number s' e' f'
  where
    s' = toFSign s
    e' = (castOtoOS e) - 1
    f' = bitSet1 54 $ f `shiftL` 2

-- }}}

-- }}}

-- sfpUnpack {{{

sfpUnpack :: SFP -> FPInfo
sfpUnpack sfp = sfpUnpacksef s e f
  where
    sfp' = mot 4 0 $ castSFtoT sfp
    s = bitGet sfpBsign sfp'
    e = fldGet sfpFe sfp'
    f = fldGet sfpFf sfp'

-- sfpUnpacksef (helper for sfpUnpack) {{{

sfpUnpacksef :: Octa -> Octa -> Octa -> FPInfo
-- zero (e == 0 && f == 0)
sfpUnpacksef s 0 0 = Zero $ toFSign s
-- sub-normal
sfpUnpacksef s 0 f = Number s' e' f'
  where
    s' = toFSign s
    (e', f') = fpAlignRaw (0, f `shiftL` 2)
-- inf (e == 255 && f == 0)
sfpUnpacksef s 255 0 = Inf $ toFSign s
-- nan (e == 255 && f > 0)
sfpUnpacksef s 255 f = NaN (toFSign s) (sfpToNaNRaw f) $ fldGet sfpFpl f
-- normal (0 < e < 255)
sfpUnpacksef s e f = Number s' e' f'
  where
    s' = toFSign s
    e' = (castOtoOS e) + 895
    f' = bitSet1 54 $ f `shiftL` 31
-- }}}
    
-- }}}

-- fpPack {{{

-- packup values, report exceptions
fpPack :: RoundMode -> FPInfo -> ArithRx FP
fpPack r fpi = castOtoF <$> fpPack' r fpi

fpPack' :: RoundMode -> FPInfo -> ArithRx Octa
fpPack' r (Number s e f) = ArithRx exList comb'
  where
    e' = castOStoO $ if e > 2045 then 2047 else max e 0
    f' | e > 2045 = 0
       | e < (-54) = 1
       | e < 0 = if fl == 0 then fh else fh .|. 1
       | otherwise = f
       where --fs = fldSet0 (castOStoO (-e - 1), 0) f
         fl = fldGet (castOStoO $ min (-e - 1) 63, 0) f
         fh = fldGet (63, castOStoO $ min (-e) 63) f
    f'' = fpRoundRaw r s f'
    comb  = (f'' `shiftR` 2) + (e' `shiftL` 52)
    comb' = if s == FNegative then bitSet1 63 comb else comb
    exList = nub $ inexactP ++ oflowP ++ uflowP
    inexactP = if (fldGet (1,0) f') == 0 then [] else [AEX]
    oflowP = if (fldGet fpFse comb) >= 0x7ff then [AEO,AEX] else []
    uflowP = if (fldGet fpFse comb) == 0 then [AEU] else []
fpPack' _ (Zero s) = return $ fpSetFSignRaw s 0
fpPack' _ (Inf s) = return $ fldSet fpFe 2047 $ fpSetFSignRaw s 0
fpPack' _ (NaN s nantype pl) = ArithRx ex nan
  where
    nan = fpSetFSignRaw s $ fpSetNaNPayloadRaw pl $ castFtoO fpQNaN
    ex = if nantype == SignalNaN then [AEI] else []

-- for DEBUG
fpTester :: FP -> Bool
fpTester fp = fp == fp'
  where (ArithRx _ fp') = fpPack RZero . fpUnpack $ fp

-- }}}

-- sfpPack {{{
-- packup values, report exceptions
sfpPack :: RoundMode -> FPInfo -> ArithRx SFP
sfpPack r fpi = castTtoSF <$> sfpPack' r fpi

sfpPack' :: RoundMode -> FPInfo -> ArithRx Tetra
sfpPack' r (Number s e f) = ArithRx exList comb'
  where
    e' = if e > 1149 then 255 else max 0 (e - 896)
    f' | e > 1149 = 0
       | e < 871 = 1
       | e < 896 = if fl == 0 then fh else fh .|. 1
       | otherwise = sf
       where
         sf = (fldGet (60,29) f) .|. (if fldGet (28,0) f == 0 then 0 else 1)
         fl = fldGet (castOStoO $ min (896 - e) 63, 0) sf
         fh = fldGet (63, castOStoO $ min (897 - e) 63) sf
    f'' = xot 4 $ fpRoundRaw r s f'
    comb  = (f'' `shiftR` 2) + (cast e' `shiftL` 23)
    comb' = sfpSetFSignRaw s comb
    exList = nub $ inexactP ++ oflowP ++ uflowP
    inexactP = if (fldGet (1,0) f') == 0 then [] else [AEX]
    oflowP = if (fldGet sfpFse comb) >= 0xff then [AEO,AEX] else []
    uflowP = if (fldGet (31,20) comb) == 0  then [AEU] else []
sfpPack' _ (Zero s) = return $ sfpSetFSignRaw s 0
sfpPack' _ (Inf s) = return $ fldSet sfpFe 255 $ sfpSetFSignRaw s 0
sfpPack' _ (NaN s nantype pl) = ArithRx ex nan
  where
    nan = sfpSetFSignRaw s $ sfpSetNaNPayloadRaw (cast pl) $ castSFtoT sfpQNaN
    ex = if nantype == SignalNaN then [AEI] else []

-- }}}

-- predicate {{{

-- FPInfo predicate {{{

-- test NaN {{{
fpiIsNaN :: FPInfo -> Bool
fpiIsNaN (NaN _ _ _) = True
fpiIsNaN _ = False

fpiIsSNaN :: FPInfo -> Bool
fpiIsSNaN (NaN _ SignalNaN _) = True
fpiIsSNaN _ = False

fpiIsQNaN :: FPInfo -> Bool
fpiIsQNaN (NaN _ QuietNaN _) = True
fpiIsQNaN _ = False
-- }}}

-- test Inf {{{
fpiIsInf :: FPInfo -> Bool
fpiIsInf (Inf _) = True
fpiIsInf _ = False

fpiIsPInf :: FPInfo -> Bool
fpiIsPInf (Inf FPositive) = True
fpiIsPInf _ = False

fpiIsNInf :: FPInfo -> Bool
fpiIsNInf (Inf FNegative) = True
fpiIsNInf _ = False
-- }}}

-- test Zero {{{
fpiIsZero :: FPInfo -> Bool
fpiIsZero (Zero _) = True
fpiIsZero _ = False

fpiIsPZero :: FPInfo -> Bool
fpiIsPZero (Zero FPositive) = True
fpiIsPZero _ = False

fpiIsNZero :: FPInfo -> Bool
fpiIsNZero (Zero FNegative) = True
fpiIsNZero _ = False
-- }}}

-- test Normal {{{
fpiIsNormal :: FPInfo -> Bool
fpiIsNormal (Number _ e _) = e > 0
fpiIsNormal _ = False

fpiIsSubNormal :: FPInfo -> Bool
fpiIsSubNormal (Number _ e _) = e == 0
fpiIsSubNormal _ = False
-- }}}

-- }}}

-- FP predicate {{{

-- test NaN {{{
fpIsNaN :: FP -> Bool
fpIsNaN = fpiIsNaN . fpUnpack

fpIsSNaN :: FP -> Bool
fpIsSNaN = fpiIsSNaN . fpUnpack

fpIsQNaN :: FP -> Bool
fpIsQNaN = fpiIsQNaN . fpUnpack
-- }}}

-- test Inf {{{
fpIsInf :: FP -> Bool
fpIsInf = fpiIsInf . fpUnpack

fpIsPInf :: FP -> Bool
fpIsPInf = fpiIsPInf . fpUnpack

fpIsNInf :: FP -> Bool
fpIsNInf = fpiIsNInf . fpUnpack
-- }}}

-- test Zero {{{
fpIsZero :: FP -> Bool
fpIsZero = fpiIsZero . fpUnpack

fpIsPZero :: FP -> Bool
fpIsPZero = fpiIsPZero . fpUnpack

fpIsNZero :: FP -> Bool
fpIsNZero = fpiIsNZero . fpUnpack
-- }}}

-- test Normal {{{
fpIsNormal :: FP -> Bool
fpIsNormal = fpiIsNormal . fpUnpack

fpIsSubNormal :: FP -> Bool
fpIsSubNormal = fpiIsSubNormal . fpUnpack
-- }}}

-- }}}

-- }}}

-- special values {{{

-- FP values {{{
fpSNaN :: FP
fpSNaN = castOtoF 0x7ff0000000000001

fpQNaN :: FP
fpQNaN = castOtoF 0x7ff8000000000000

fpPInf :: FP
fpPInf = castOtoF 0x7ff0000000000000

fpNInf :: FP
fpNInf = castOtoF 0xfff0000000000000

fpPZero :: FP
fpPZero = castOtoF 0

fpNZero :: FP
fpNZero = castOtoF 0x8000000000000000
-- }}}

-- SFP values {{{
sfpSNaN :: SFP
sfpSNaN = castTtoSF 0x7f800001

sfpQNaN :: SFP
sfpQNaN = castTtoSF 0x7fc00000

sfpPInf :: SFP
sfpPInf = castTtoSF 0x7f800000

sfpNInf :: SFP
sfpNInf = castTtoSF 0xff800000

sfpPZero :: SFP
sfpPZero = castTtoSF 0

sfpNZero :: SFP
sfpNZero = castTtoSF 0x80000000
-- }}}

-- }}}

-- fpAlignRaw {{{
fpAlignRaw :: (OctaS, Octa) -> (OctaS, Octa)
fpAlignRaw (e, f)
  | f < l  = fpAlignRaw (e - 1, f `shiftL` 1)
  | f >= h = fpAlignRaw (e + 1, (f .&. 1) .|. (f `shiftR` 1))
  | otherwise = (e, f)
  where h = bitMask 55 -- 0x0080000000000000
        l = bitMask 54 -- 0x0040000000000000
-- }}}

-- fpRoundRaw {{{

-- r s f -> f'
fpRoundRaw :: RoundMode -> FSign -> Octa -> Octa
fpRoundRaw RNear _ f = if bitGet 2 f == 1 then f + 2 else f + 1
fpRoundRaw RZero _ f = f
fpRoundRaw RDown s f = if s == FPositive then f else f + 3
fpRoundRaw RUp   s f = if s == FPositive then f + 3 else f

-- }}}

-- fpUnorderd {{{

fpUnordered :: FP -> FP -> Bool
fpUnordered a b = fpIsNaN a || fpIsNaN b

-- }}}

-- fpAdd {{{

-- fpAdd {{{
fpAdd :: RoundMode -> FP -> FP -> ArithRx FP
fpAdd r a b = fpiAdd r (fpUnpack a) (fpUnpack b)
-- }}}

-- fpiAdd {{{
fpiAdd :: RoundMode -> FPInfo -> FPInfo -> ArithRx FP
-- Number + Number
fpiAdd r a@(Number as ae af) b@(Number bs be bf) =
  if as /= bs && ae == be && af == bf -- zero?
  then fpiAdd r (Zero as) (Zero bs)
  else fpAddRaw r (as, ae, af) (bs, be, bf)
-- Zero + Number
fpiAdd _ a@(Zero _) b@(Number _ _ _) = fpPack RZero b
fpiAdd _ a@(Number _ _ _) b@(Zero _) = fpPack RZero a
-- Zero + Zero
fpiAdd r a@(Zero as) b@(Zero bs) =
  if as == bs then fpPack r a
  else return $ if r == RDown then fpNZero else fpPZero
-- any Inf -> Inf; +Inf-Inf=NaN
fpiAdd r a@(Inf as) b@(Inf bs) =
  if as /= bs then fpPack r $ NaN bs SignalNaN 0
  else fpPack r b
fpiAdd r a@(Inf as) _ = fpPack r a
fpiAdd r _ b@(Inf bs) = fpPack r b
-- any NaN -> NaN
fpiAdd r a@(NaN _ _ _) b@(NaN _ _ _) =
  fpPack r $ if fpiIsSNaN a then a else b
fpiAdd r a@(NaN _ _ _) _ = fpPack r a
fpiAdd r _ b@(NaN _ _ _) = fpPack r b

-- fpAddRaw: helper for num + num {{{
fpAddRaw :: RoundMode
         -> (FSign, OctaS, Octa)
         -> (FSign, OctaS, Octa)
         -> ArithRx FP
fpAddRaw r a@(as, ae, af) b@(bs, be, bf) =
  if ae < be || (ae == be && af < bf)
  then fpAddRaw r b a
  else addAligned r (as, af') (bs, bf') ae'
  where
    d = ae - be
    d' = ae' - be
    (ae', af') =
      if 3 <= d && d <= 53 
      then (ae - 1, af `shiftL` 1)
      else (ae, af)
    bf' = if d > 53 then 1
          else (bf `shiftR` cast d')
               .|. (if fldGet (cast d',0) bf == 0 then 0 else 1)
-- }}}

-- addAligned: another helper for num + num {{{
addAligned :: RoundMode
           -> (FSign, Octa)
           -> (FSign, Octa)
           -> OctaS
           -> ArithRx FP
addAligned r (as, af) (bs, bf) e = fpPack r (Number as e' f')
  where
    f = if as == bs then af + bf else af - bf
    (e', f') = fpAlignRaw (e, f)
-- }}}
  
-- }}}

-- }}}

-- fpSub {{{

-- fpSub {{{
fpSub :: RoundMode -> FP -> FP -> ArithRx FP
fpSub r a b = fpiSub r (fpUnpack a) (fpUnpack b)
-- }}}

-- fpiSub {{{
fpiSub :: RoundMode -> FPInfo -> FPInfo -> ArithRx FP
fpiSub r a b = case b of
  Number s e f -> fpiAdd r a $ Number (flipFSign s) e f
  Zero s -> fpiAdd r a $ Zero (flipFSign s)
  Inf s -> fpiAdd r a $ Inf (flipFSign s)
  NaN _ _ _ -> fpiAdd r a b
-- }}}

-- }}}

-- fpCompare {{{

-- fpCompare {{{
fpCompare:: FP -> FP -> (Ordering, [ArithEx])
fpCompare a b = fpiCompare (fpUnpack a) (fpUnpack b)
-- }}}

-- fpiCompare {{{
fpiCompare :: FPInfo -> FPInfo -> (Ordering, [ArithEx])
-- normal numbers
fpiCompare (Number as ae af) (Number bs be bf) =
  case compare as bs of
    EQ -> case compare ae be of
            EQ -> (compare af bf, [])
            eOrd -> (eOrd, [])
    sOrd -> (sOrd, [])
fpiCompare (Number as ae af) (Zero _) =
  case as of
    FPositive -> (GT, [])
    FNegative -> (LT, [])
fpiCompare (Zero _) (Number bs be bf) =
  case bs of
    FPositive -> (LT, [])
    FNegative -> (GT, [])
fpiCompare (Zero _) (Zero _) = (EQ, [])
-- use wildcard for NaN
fpiCompare (NaN _ _ _) _ = (EQ, [AEI])
fpiCompare _ (NaN _ _ _) = (EQ, [AEI])
-- use wildcard for Inf
fpiCompare (Inf FPositive) (Inf FPositive) = (EQ, [])
fpiCompare (Inf FNegative) (Inf FNegative) = (EQ, [])
fpiCompare (Inf FPositive) _ = (GT, [])
fpiCompare (Inf FNegative) _ = (LT, [])
fpiCompare _ (Inf FPositive) = (LT, [])
fpiCompare _ (Inf FNegative) = (GT, [])
-- }}}

-- }}}

-- fpEqual {{{

-- fpEqual {{{
fpEqual :: FP -> FP -> Bool
fpEqual a b = fpiEqual (fpUnpack a) (fpUnpack b)
-- }}}

-- fpiEqual {{{
fpiEqual (NaN _ _ _) _ = False
fpiEqual _ (NaN _ _ _) = False
fpiEqual a b = fst (fpiCompare a b) == EQ
-- }}}

-- }}}

-- fpInt: round to Integer value {{{

-- fpInt {{{
fpInt :: RoundMode -> FP -> ArithRx FP
fpInt r a = fpiInt r $ fpUnpack a
-- }}}

-- fpiInt {{{
fpiInt :: RoundMode -> FPInfo -> ArithRx FP
fpiInt r a@(Number s e f)
  | e >= 1074 = fpPack RZero a -- this line just make it fast
  | e >= 1022 = fpPack RZero (Number s e (fldSet (63, eSh) xf' 0))
  | xf' /= 0  = fpPack RZero (Number s 1023 0) -- 1
  | otherwise = fpPack RZero (Zero s)
  where
    -- 1 .. 53 .. more
    eSh = castOStoO $ max 0 (1074 - e) -- eShift >=0
    xf' = fldSet0 (1,0) . fpRoundRaw r s $
            if e <= 1020 then 1 else fldGet (63, eSh) f .|. stiky
    stiky = if fldGet (eSh - 1, 0) f == 0 then 0 else 1
fpiInt r a = fpPack RZero a
-- }}}

-- }}}

-- fpFix: FP -> Octa {{{

-- fpFix {{{
fpFix :: RoundMode -> FP -> ArithRx Octa
fpFix r a = fpiFix r $ fpUnpack a
-- }}}

-- fpiFix {{{
fpiFix :: RoundMode -> FPInfo -> ArithRx Octa
fpiFix r a@(NaN _ _ _) = (ArithRx [AEI] castFtoO) <*> (fpPack r a)
fpiFix r a@(Inf _) = (ArithRx [AEI] castFtoO) <*> (fpPack r a)
fpiFix r (Zero _) = return 0
fpiFix r a = case a' of
  (Zero _) -> return 0
  (Number s e f) -> fpFixRaw s e f
  where
    intf = arithGetRx $ fpiInt r a
    a' = fpUnpack intf
-- }}}

-- fpFixRaw {{{
fpFixRaw :: FSign -> OctaS -> Octa -> ArithRx Octa
fpFixRaw s e f = ArithRx ex $ setSignO s $ f `shift` eSh
  where
    eSh = cast $ max (-64) $ min 64 $ e - 1076
    setSignO FPositive o = o
    setSignO FNegative o = (-o)
    ex | e > 1085 = [AEW]
       | e < 1085 = []
       | f > 0x40000000000000 = [AEW]
       | f == 0x40000000000000 && s == FPositive = [AEW]
       | otherwise = []
-- }}}

-- }}}

-- fpFixu: FP -> Octa {{{

-- fpFixu {{{
fpFixu :: RoundMode -> FP -> ArithRx Octa
fpFixu r a = fpiFixu r $ fpUnpack a
-- }}}

-- fpiFixu {{{
fpiFixu :: RoundMode -> FPInfo -> ArithRx Octa
fpiFixu r a = ArithRx ex' rx
  where
    (ArithRx ex rx) = fpiFix r a
    ex' = filter (/= AEW) ex
-- }}}

-- }}}

-- fpFloat: Octa -> FP (Octa as signed) {{{

fpFloat :: RoundMode -> Octa -> ArithRx FP
fpFloat r 0 = return fpPZero
fpFloat r o = fpPack r (Number s e' f')
  where
    s = if bitGet 63 o == 0 then FPositive else FNegative
    f = if s == FPositive then o else (-o)
    (e', f') = fpAlignRaw (1076, f)

-- }}}

-- fpFloatu: Octa -> FP (Octa as unsigned) {{{

fpFloatu :: RoundMode -> Octa -> ArithRx FP
fpFloatu r 0 = return fpPZero
fpFloatu r o = fpPack r (Number s e f)
  where
    s = FPositive
    (e, f) = fpAlignRaw (1076, o)

-- }}}

-- fpSFloat: Octa -> SFP (Octa as signed) {{{



-- }}}

-- }}}

-- vim: fdm=marker

