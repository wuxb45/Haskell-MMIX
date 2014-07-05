-- Copyright 2012 Wu Xingbo <wuxb45@gmail.com>
-- ** Basic MMIX architecture.

-- ghc options {{{
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
-- }}}

-- module exports {{{

module MMIX.Base (
-- common helpers {{{
  cast, hx, traceShowIO,
  mapT2, mapT4, mapT8,
  zipT2, zipT4, zipT8,
-- }}}
-- machine types and fields {{{
  Octa, Tetra, Wyde, Byte,
  Addr, VAddr, PAddr, RAddr, BitIx, BitFld,
  Insn, OPCode, X, Y, Z, YZ, XYZ,
  PTE, PTP, TCKey, TCVal,
  FP, SFP,
  OctaS, TetraS, WydeS, ByteS,
-- }}}
-- general bytes convertion {{{
  cvot, cvtw, cvwb, cvow, cvtb, cvob,
  cvto, cvwt, cvbw, cvwo, cvbt, cvbo,
  xob, xow, xot, xtb, xtw, xwb,
  mob, mow, mot, mtb, mtw, mwb,
  mapot, mapow, mapob, maptw, maptb, mapwb,
  zipot, zipow, zipob, ziptw, ziptb, zipwb,
-- }}}
-- general bits convertion {{{
  bitMask, bitUMask, bitGet, bitSet,
  bitGetRaw, bitSetRaw, bitSet1, bitSet0,
  fldMask, fldUMask, fldGet, fldSet,
  fldGetRaw, fldSetRaw, fldSet1, fldSet0,
  signExt, signExtByte, signExtWyde, signExtTetra,
  signExtByteOcta, signExtWydeOcta, signExtTetraOcta,
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
  rSet, rGet, showR,
-- }}}
-- special registers {{{
  newSRD, showSRD,
  rBIx, rDIx, rEIx, rHIx, rJIx, rMIx, rRIx, rBBIx,
  rCIx, rNIx, rOIx, rSIx, rIIx, rTIx, rTTIx, rKIx,
  rQIx, rUIx, rVIx, rGIx, rLIx, rAIx, rFIx, rPIx,
  rWIx, rXIx, rYIx, rZIx, rWWIx, rXXIx, rYYIx, rZZIx,

  rQQIx, rINSIx, rTRIPIx, rTRAPIx,
  rRXIx, rRYIx, rRZIx, rTPIIx, rTPAIx,

  rKFlow, rKFprog, rKFhigh, rKFmachine,
  rKBr, rKBw, rKBx, rKBn, rKBk, rKBb, rKBs, rKBp,

  rQFlow, rQFprog, rQFhigh, rQFmachine,
  rQBr, rQBw, rQBx, rQBn, rQBk, rQBb, rQBs, rQBp,

  rVFb1,rVFb2,rVFb3,rVFb4,rVFs,rVFr,rVFn,rVFf,

  rGFg, rGFZero,
  rLFl, rLFZero,

  rAFEnable,
  rABEnableD,rABEnableV,rABEnableW,rABEnableI,
  rABEnableO,rABEnableU,rABEnableZ,rABEnableX,
  rAFEvent,
  rABEventD, rABEventV, rABEventW, rABEventI,
  rABEventO, rABEventU, rABEventZ, rABEventX,
  rAFRoundMode, rAFZero,

  rXBsign, rXFinsn, rXFrop, rXFae,

  rXXBsign, rXXFinsn, rXXFrop, rXXFprog,
  rXXBr, rXXBw, rXXBx, rXXBn, rXXBk, rXXBb, rXXBs, rXXBp,
-- }}}
-- general registers {{{
  newGRD,
-- }}}
-- instruction operation {{{
  iGetOPCode, iGetX, iGetY, iGetZ, iGetYZ, iGetXYZ,
  iGetXu, iGetYu, iGetZu, iGetYZu, iGetXYu, iGetXYZu,
  iGetXs, iGetYs, iGetZs, iGetYZs, iGetXYZs,
-- }}}
-- opcode {{{
  InsnOP (..), deOPCode, insnOpList,
-- }}}
-- address interface {{{
  Device (..),
-- }}}
-- virtual address mapping {{{
  vaddrFaddr, vaddrFseg, vaddrFsign, vaddrFtc,
  pteFx, pteFay, pteFn, pteFp,
  pteFpr, pteFpw, pteFpx, pteBpr, pteBpw, pteBpx,
  ptpFc, ptpFn, ptpFq, ptpFnq,

  vMapGetPTED0, vMapGetPTEI0,
  vMapGetPTED, vMapGetPTEI,
  vMapGetPAddr, vMapGetPTE,

  TC, TCInfo (..), MMIXTC (..),

  newMMIXTC, tcNewEntry, tcLookupPTE, tcKVToPTE,
  mmixTCAddEntryD, mmixTCAddEntryI,
  mmixTCDelEntryD, mmixTCDelEntryI,
  mmixTCFlushD, mmixTCFlushI,
-- }}}
-- MMIX machine model {{{
  MMIX (..), showMMIX,
  mmixGetPC, mmixSetPC,
  mmixGetSR, mmixSetSR,
  mmixGetGR, mmixSetGR,
  mmixGetITC, mmixSetITC,
  mmixGetDTC, mmixSetDTC,
  mmixLdInsn,
  mmixLdOcta, mmixLdTetra, mmixLdWyde, mmixLdByte,
  mmixLdOcta0, mmixLdTetra0, mmixLdWyde0, mmixLdByte0,
  mmixStOcta, mmixStTetra, mmixStWyde, mmixStByte,
  newDummyMMIX,
-- }}}
-- MMIX helpers {{{
  mmixGetGRS, mmixSetGRS,
  mmixGetGRF, mmixSetGRF,
  mmixGetSRF, mmixSetSRF,
  mmixSetGRX, mmixSetGRSX, mmixSetGRFX,
  mmixGetGRX, mmixGetGRSX, mmixGetGRFX,
  mmixGetGRY, mmixGetGRSY, mmixGetGRFY,
  mmixGetGRZ, mmixGetGRSZ, mmixGetGRFZ,
  mmixGetGRYZ, mmixGetGRSYZ, mmixGetGRFYZ,
  mmixGetGRXY, mmixGetGRXYZ,
-- }}}
-- Trip and Trap {{{
  TRIP (..),
  tripEntry,
  tripAEBit,
  tripCheckD, tripCheckV, tripCheckW, tripCheckI,
  tripCheckO, tripCheckU, tripCheckZ, tripCheckX, 
  tripCheckList, tripCheck, tripCheckOne,
-- }}}
-- arithmetic basis {{{
  ArithEx (..), cvAEtoT, cvAEtoBit,
  ArithRx (..),
  Hexadeca (..),
  fromOrdering, fromBool,
-- }}}
-- integral arithmetic {{{
  intSignBit,
  intMul, intMulu,
  intDivide, intDivideu,
  intAdd, intAddu, int2Addu, int4Addu, int8Addu, int16Addu,
  intSub, intSubu,
  intCompare, intCompareu,
  testN, testZ, testP, testODD,
  testNN, testNZ, testNP, testEVEN,
-- }}}
-- logic arithmetic {{{
  (.|.), (.&.), complement, xor,
  bitSL, bitSLu, bitSR, bitSRu,
  diffByte, diffWyde, diffTetra, diffOcta,
  bitMux, bitSAdd, bitTrans, bitMultiple, bitMOr, bitMXor,
-- }}}
-- floating-point {{{
  fpBsign, fpFsign, fpFe, fpFf, fpFse, fpBnan, fpFpl,
  sfpBsign, sfpFsign, sfpFe, sfpFf, sfpFse, sfpBnan, sfpFpl,

  RoundMode (..), toRoundModeRaw, toRoundMode, toRoundModeImm,

  FSign (..), toFSign, toFSignRaw, fpSetFSignRaw,
  sfpSetFSignRaw, flipFSign, mdFSign,

  NaNType (..), fpToNaN, fpToNaNRaw, sfpToNaNRaw,
  fpSetNaNRaw, fpSetNaNPl,
  fpSetNaNPlRaw, sfpSetNaNPlRaw,

  FPInfo (..),
  fpUnpack, fpPack, fpPackRaw, sfpUnpack, sfpPack,

  fpiIsNaN, fpiIsSNaN, fpiIsQNaN,
  fpiIsInf, fpiIsPInf, fpiIsNInf,
  fpiIsZero, fpiIsPZero, fpiIsNZero,
  fpiIsNormal, fpiIsSubNormal,

  fpIsNaN, fpIsSNaN, fpIsQNaN,
  fpIsInf, fpIsPInf, fpIsNInf,
  fpIsZero, fpIsPZero, fpIsNZero,
  fpIsNormal, fpIsSubNormal,

  fpSNaN, fpQNaN, fpPInf, fpNInf, fpPZero, fpNZero,
  sfpSNaN, sfpQNaN, sfpPInf, sfpNInf, sfpPZero, sfpNZero,

  fpUnordered,
  fpUnorderedEps,
  fpAdd, fpiAdd,
  fpSub, fpiSub,
  fpMult, fpiMult,
  fpDivide, fpiDivide,
  fpRem, fpiRem,
  fpSqrt, fpiSqrt,
  fpCompare, fpiCompare,
  fpEqual, fpiEqual,
  fpCompareEps, fpiCompareEps,
  fpEqualEps, fpiEqualEps,
  fpInt, fpiInt,
  fpFix, fpiFix, fpFixu, fpiFixu,
  fpFloat, fpFloatu, fpSFloat, fpSFloatu,
-- }}}
  ) where

-- }}}

-- imports {{{
import Prelude
       ( Maybe (..), Bool (..), IO (..), Ord (..),
         Integral (..), Ordering (..), Num (..), Real (..),
         Enum (..), Bounded (..),
         Double, Float,
         (.), ($), (+), (-), (*), (/), (||), (&&),
         (>=), (<=), (<), (>),
         fromIntegral, otherwise, fst, snd,
         id, error, undefined,
       )
import Control.Applicative (Applicative (..), (<$>), (<*>))
import Control.Monad (Monad (..), Functor (..), (>>=),
                      fmap, return, when, mapM, sequence)
import Data.Array.IO (IOUArray)
import Data.Array.MArray (MArray, newArray, getElems,
                          readArray, writeArray)
import Data.Bits (Bits (..), FiniteBits(..))
import Data.Eq (Eq (..))
import Data.List (filter, map, head, zip, zipWith, take,
                  (++), nub, foldl1', concat, concatMap,
                  reverse, length)
import Data.Int (Int, Int64, Int32, Int16, Int8)
import Data.Maybe (catMaybes, fromMaybe, isJust, fromJust,)
import qualified Data.Map as Map
       (Map, fromList, (!), lookup, empty, insert, delete,)
import Data.Ratio ((%))
import Data.String (String (..))
import Data.Word (Word64, Word32, Word16, Word8)
import Data.IORef (IORef, readIORef, writeIORef, newIORef)
import Debug.Trace (trace, traceShow, traceIO)
import Numeric (showHex)
import System.IO (putStrLn)
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

-- traceShowIO, traceShow on IO
traceShowIO :: (Show a) => a -> IO ()
traceShowIO a = traceIO $ show a

-- map on tuple {{{
mapT2 :: (a -> b)
      -> (a, a)
      -> (b, b)
mapT2 f (a1, a2) = (f a1, f a2)

mapT4 :: (a -> b)
      -> (a, a, a, a)
      -> (b, b, b, b)
mapT4 f (a1, a2, a3, a4) = (f a1, f a2, f a3, f a4)

mapT8 :: (a -> b)
      -> (a, a, a, a, a, a, a, a)
      -> (b, b, b, b, b, b, b, b)
mapT8 f (a1, a2, a3, a4, a5, a6, a7, a8) =
  (f a1, f a2, f a3, f a4, f a5, f a6, f a7, f a8)
-- }}}

-- zip on tuple {{{
zipT2 :: (a -> b -> c)
      -> (a, a)
      -> (b, b)
      -> (c, c)
zipT2 f (a1, a2) (b1, b2) = (f a1 b1, f a2 b2)

zipT4 :: (a -> b -> c)
      -> (a, a, a, a)
      -> (b, b, b, b)
      -> (c, c, c, c)
zipT4 f (a1, a2, a3, a4) (b1, b2, b3, b4) =
  (f a1 b1, f a2 b2, f a3 b3, f a4 b4)

zipT8 :: (a -> b -> c)
      -> (a, a, a, a, a, a, a, a)
      -> (b, b, b, b, b, b, b, b)
      -> (c, c, c, c, c, c, c, c)
zipT8 f (a1, a2, a3, a4, a5, a6, a7, a8)
        (b1, b2, b3, b4, b5, b6, b7, b8) =
  (f a1 b1, f a2 b2, f a3 b3, f a4 b4,
   f a5 b5, f a6 b6, f a7 b7, f a8 b8)
  
-- }}}

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
type TCKey  = Octa    -- Translation Cache Key
type TCVal  = Octa    -- Translation Cache Value

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

-- map on byte fields {{{
mapot :: (Tetra -> Tetra) -> Octa -> Octa
mapot f o = cvto . mapT2 f . cvot $ o

mapow :: (Wyde -> Wyde) -> Octa -> Octa
mapow f o = cvwo . mapT4 f . cvow $ o

mapob :: (Byte -> Byte) -> Octa -> Octa
mapob f o = cvbo . mapT8 f . cvob $ o

maptw :: (Wyde -> Wyde) -> Tetra -> Tetra
maptw f t = cvwt . mapT2 f . cvtw $ t

maptb :: (Byte -> Byte) -> Tetra -> Tetra
maptb f t = cvbt . mapT4 f . cvtb $ t

mapwb :: (Byte -> Byte) -> Wyde -> Wyde
mapwb f w = cvbw . mapT2 f . cvwb $ w
-- }}}

-- zip on byte fields {{{
zipot :: (Tetra -> Tetra -> Tetra) -> Octa -> Octa -> Octa
zipot f a b = cvto $ zipT2 f (cvot a) (cvot b)

zipow :: (Wyde -> Wyde -> Wyde) -> Octa -> Octa -> Octa
zipow f a b = cvwo $ zipT4 f (cvow a) (cvow b)

zipob :: (Byte -> Byte -> Byte) -> Octa -> Octa -> Octa
zipob f a b = cvbo $ zipT8 f (cvob a) (cvob b)

ziptw :: (Wyde -> Wyde -> Wyde) -> Tetra -> Tetra -> Tetra
ziptw f a b = cvwt $ zipT2 f (cvtw a) (cvtw b)

ziptb :: (Byte -> Byte -> Byte) -> Tetra -> Tetra -> Tetra
ziptb f a b = cvbt $ zipT4 f (cvtb a) (cvtb b)

zipwb :: (Byte -> Byte -> Byte) -> Wyde -> Wyde -> Wyde
zipwb f a b = cvbw $ zipT2 f (cvwb a) (cvwb b)

-- }}}

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
fldMask (l,r) = foldl1' (.|.) $ map (bit . cast) [r..l]

-- unmask a field
fldUMask :: (Bits a) => BitFld -> a
fldUMask = complement . fldMask

-- get field
fldGet :: (Bits a) => BitFld -> a -> a
fldGet f@(_,r) v = (v .&. (fldMask f)) `shiftR` (cast r)

-- set field: field -> value -> orig -> result
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

-- signExt: sign bit -> value -> extended
signExt :: (FiniteBits a) => BitIx -> a -> a
signExt b v = if testBit v (cast b) then fldSet fld allOne v else v
  where
    fld = (hiBit, min b hiBit)
    hiBit = cast $ (finiteBitSize v) - 1
    allOne = complement zeroBits

signExtByte :: (FiniteBits a) => a -> a
signExtByte = signExt 7

signExtWyde :: (FiniteBits a) => a -> a
signExtWyde = signExt 15

signExtTetra :: (FiniteBits a) => a -> a
signExtTetra = signExt 31

signExtByteOcta :: Byte -> Octa
signExtByteOcta v = signExtByte $ cast v

signExtWydeOcta :: Wyde -> Octa
signExtWydeOcta v = signExtWyde $ cast v

signExtTetraOcta :: Tetra -> Octa
signExtTetraOcta v = signExtTetra $ cast v

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

-- set register
rSet :: RD -> RIx -> Octa -> IO ()
rSet = writeArray

-- get register
rGet :: RD -> RIx -> IO Octa
rGet = readArray

-- GR: has 2 device: (L, G)
type GRD = (RD, RD)

-- showR: show register
-- value -> name -> String
showR :: String -> Octa -> String
showR name v = printf "[%4s #%016x (%20u)]\n" name v v
-- }}}

-- special registers {{{

-- new special register device
newSRD :: IO RD
newSRD = do
  arr <- newArray (0,255) 0
  rSet arr rGIx 255
  return arr

-- showSRD {{{
srNameList :: [String]
srNameList =
  [ "rB", "rD", "rE", "rH", "rJ", "rM", "rR", "rBB"
  , "rC", "rN", "rO", "rS", "rI", "rT", "rTT", "rK"
  , "rQ", "rU", "rV", "rG", "rL", "rA", "rF", "rP"
  , "rW", "rX", "rY", "rZ", "rWW","rXX","rYY","rZZ" ]

showSRD :: RD -> IO String
showSRD srd = do
  rvList <- mapM (rGet srd) [0 .. 31]
  return $ concat $ zipWith showR srNameList rvList
-- }}}

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

-- hidden Special Registers {{{
-- rQQ: keep rQ bits after previous GET
rQQIx :: SRIx
rQQIx = 32

-- rRSM: when exec inserted insn, set to 1
rINSIx :: SRIx
rINSIx = 33

-- rTRIP: signal force-trip, one bit
rTRIPIx :: SRIx
rTRIPIx = 34

-- rTRAP: signal force-trap, one bit
rTRAPIx :: SRIx
rTRAPIx = 35

-- rRX: rX or rXX
rRXIx :: SRIx
rRXIx = 36

-- rRY: rX or rYY
rRYIx :: SRIx
rRYIx = 37

-- rRZ: rZ or rZZ
rRZIx :: SRIx
rRZIx = 38

-- rTPI: trxpped insn
rTPIIx :: SRIx
rTPIIx = 39

-- rTPA: trxpped vaddr
rTPAIx :: SRIx
rTPAIx = 40

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

rGFZero :: BitFld
rGFZero = (63,8)

-- }}}

-- rL fields {{{

rLFl :: BitFld
rLFl = (7,0)

rLFZero :: BitFld
rLFZero = (63,8)

-- }}}

-- rA fields {{{

-- Enable bits *8 {{{

-- D: integer divide check
-- V: integer overflow
-- W: float-to-fix overflow
-- I: invalid operation
-- O: floating overflow
-- U: floating underflow
-- Z: floating division by zero
-- X: floating inexact

rAFEnable :: BitFld
rAFEnable = (15,8)

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

-- Always Zero {{{

rAFZero :: BitFld
rAFZero = (63,18)

-- }}}

-- }}}

-- rX fields {{{

rXBsign :: BitIx
rXBsign = 63

rXFinsn :: BitFld
rXFinsn = (31,0)

rXFrop :: BitFld
rXFrop = (62,56)

rXFae :: BitFld
rXFae = (47,40)

-- }}}

-- rXX fields {{{

rXXBsign :: BitIx
rXXBsign = 63

rXXFinsn :: BitFld
rXXFinsn = (31,0)

rXXFrop :: BitFld
rXXFrop = (62,56)

rXXFprog :: BitFld
rXXFprog = (39,32)

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
newGRD :: IO GRD
newGRD = do
  lr <- newArray (0,255) 0
  gr <- newArray (0,255) 0
  return (lr, gr)
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

-- get XY value
iGetXYu :: Insn -> Octa
iGetXYu = (`shiftR` 8) . iGetXYZu

-- get XYZ value
iGetXYZu :: Insn -> Octa
iGetXYZu i = cast $ i .&. 0xffffff

-- }}}

-- get field signed {{{

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
insnOpList :: [InsnOP]
insnOpList =
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
opListMap = zip opIxList insnOpList

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

-- Virtual address mapping {{{

-- VAddr (virtual address) fields {{{

vaddrFaddr :: BitFld
vaddrFaddr = (60, 0)

vaddrFseg :: BitFld
vaddrFseg = (62, 61)

vaddrFsign :: BitFld
vaddrFsign = (63, 63)

vaddrFtc :: BitFld
vaddrFtc = (63, 13)

-- }}}

-- PTE (page table entry) {{{

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

-- PTP (page table pointer) {{{

ptpFc :: BitFld
ptpFc = (62,13)

ptpFn :: BitFld
ptpFn = (12,3)

ptpFq :: BitFld
ptpFq = (2,0)

ptpFnq :: BitFld
ptpFnq = (12,0)

-- }}}

-- PTE operation {{{

-- vMapGetPTED0: lookup PTE, on fail returns 0 {{{
vMapGetPTED0 :: MMIX -> VAddr -> IO PTE
vMapGetPTED0 mmix vaddr = fromMaybe 0 <$> vMapGetPTED mmix vaddr
-- }}}

-- vMapGetPTEI0: lookup PTE, on fail returns 0 {{{
vMapGetPTEI0 :: MMIX -> VAddr -> IO PTE
vMapGetPTEI0 mmix vaddr = fromMaybe 0 <$> vMapGetPTEI mmix vaddr
-- }}}

-- vMapGetPTED: MMIX -> VAddr -> IO (Maybe PTE) {{{
vMapGetPTED :: MMIX -> VAddr -> IO (Maybe PTE)
vMapGetPTED mmix vaddr = do
  rV <- mmixGetSR mmix rVIx
  dtc <- mmixGetDTC mmix
  let mbInTC = tcLookupPTE dtc rV vaddr
  if isJust mbInTC
  then return mbInTC
  else do
    mbPTE <- vMapGetPTE mmix vaddr
    when (isJust mbPTE) $ mmixTCAddEntryD mmix vaddr $ fromJust mbPTE
    return mbPTE
-- }}}

-- vMapGetPTEI: MMIX -> VAddr -> IO (Maybe PTE) {{{
vMapGetPTEI :: MMIX -> VAddr -> IO (Maybe PTE)
vMapGetPTEI mmix vaddr = do
  rV <- mmixGetSR mmix rVIx
  itc <- mmixGetDTC mmix
  let mbInTC = tcLookupPTE itc rV vaddr
  if isJust mbInTC
  then return mbInTC
  else do
    mbPTE <- vMapGetPTE mmix vaddr
    when (isJust mbPTE) $ mmixTCAddEntryI mmix vaddr $ fromJust mbPTE
    return mbPTE
-- }}}

-- vMapGetPTE: lookup PTE in memory {{{
vMapGetPTE :: MMIX -> VAddr -> IO (Maybe PTE)
vMapGetPTE mmix vaddr = do
  rV <- mmixGetSR mmix rVIx -- no fail
  let mbVMapParam = vMapPrepare vaddr rV
  let mbStartPoint = mbVMapParam >>= vMapStartPoint
  case mbStartPoint of
    Just point -> vMapLookup mmix point
    Nothing -> return Nothing
-- }}}

-- vMapGetPAddr: rV -> VAddr -> PTE -> PAddr {{{
vMapGetPAddr :: Octa -> Octa -> Octa -> PAddr
vMapGetPAddr rV vaddr pte = a .|. offset
  -- make sure 13 <= s <= 48
  where
    s = fldGet rVFs rV
    s' = max 13 $ min 48 s
    a = fldGetRaw (48, s') pte
    offset = fldGetRaw (s' - 1, 0) vaddr
-- }}}

-- helper functions {{{

-- type VMapParam: (base, limit, pn, nval) {{{
type VMapParam = (Octa, Octa, Octa, Octa)
-- }}}

-- type VStartPoint: (nval, len, indexList) {{{
type VStartPoint = (Octa, Octa, [Octa])
-- }}}

-- vMapChecks (check s field) {{{
-- helper function, called by vMapPrepare
vMapChecks :: Octa -> Bool
vMapChecks rV = (s >= 13) && (s <= 48)
  where s = fldGet rVFs rV
-- }}}

-- prepare parameters for recursive find PTE {{{
-- helper function, called by vMapGetPTE
vMapPrepare :: VAddr -> Octa -> Maybe VMapParam
vMapPrepare vaddr rV = 
  if vMapChecks rV
  then Just (base, limit, pn, nval)
  else Nothing
  where
    seg = fldGet vaddrFseg vaddr
    (b,bp) = case seg of
      0 -> (0, fldGet rVFb1 rV)
      1 -> (fldGet rVFb1 rV, fldGet rVFb2 rV)
      2 -> (fldGet rVFb2 rV, fldGet rVFb3 rV)
      3 -> (fldGet rVFb3 rV, fldGet rVFb4 rV)
    raddr  = bitSet1 63 $ fldGetRaw rVFr rV
    base   = (b `shiftL` 13) + raddr
    virt   = fldSet0 (63,61) vaddr
    s      = fldGet rVFs rV
    pn     = virt `shiftR` (cast s)
    limit  = (bp `shiftL` 13) + raddr + (if pn == 0 then 1 else 0)
    nval   = bitSet1 63 $ fldGetRaw rVFn rV
-- }}}

-- guard for check n field match ptp {{{
-- helper function, called by vMapLookup
vMapGuardPTP :: Octa -> PTP -> Maybe PTP
vMapGuardPTP nval ptp =
  if (ptp `xor` nval) .&. ptpmask == 0
  then return ptp
  else Nothing
  -- check bit 63 and <n>
  where ptpmask = fldMask (63,63) .|. fldMask rVFn
-- }}}

-- guard for check n field match pte {{{
-- helper function, called by vMapLookup
vMapGuardPTE :: Octa -> PTE -> Maybe PTE
vMapGuardPTE nval pte =
  if (pte `xor` nval) .&. ptemask == 0
  then return pte
  else Nothing
  -- check <n>
  where ptemask = fldMask rVFn
-- }}}

-- guard for base < limit {{{
-- helper function, called by vMapStartPoint
vMapGuardLimit :: Octa -> Octa -> a -> Maybe a
vMapGuardLimit base limit a =
  if base < limit then return a else Nothing
-- }}}

-- convert page-number to page-index list {{{
-- helper function, called by vMapStartPoint
vMapGetPIxList :: Octa -> [Octa]
vMapGetPIxList = reverse . splitPn
  where
    splitPn 0 = []
    splitPn pn = pnIx:(splitPn pnShift)
      where
        pnShift = fldGet (63,10) pn
        pnIx = fldGet (9,0) pn
-- }}}

-- get the start point of lookup: [base, idx-list] {{{
-- helper function, called by vMapGetPTE
vMapStartPoint :: VMapParam -> Maybe VStartPoint
vMapStartPoint (base, limit, pn, nval) =
  if len < 1 || len > 5
  then Nothing
  else Just (nval, base', pageIxList) >>= vMapGuardLimit base' limit
  where
    pageIxList = vMapGetPIxList pn
    len = cast $ length pageIxList :: Octa
    base' = base + ((len - 1) * 0x2000)
-- }}}

-- load PTX (PTE/PTP) from Memory {{{
-- helper function, called by vMapLookup
vMapLoadPTX :: MMIX -> PAddr -> PAddr -> IO (Maybe Octa)
vMapLoadPTX mmix base index = mmixLdOcta mmix $ base + (index `shiftL` 3)
-- }}}

-- load (valid) PTE (maybe plus some PTP) {{{
-- helper function, called by vMapGetPTE
vMapLookup :: MMIX -> VStartPoint -> IO (Maybe PTE)
vMapLookup mmix (nval, base, [ix]) = do
  mbPTE <- vMapLoadPTX mmix base ix
  return $ mbPTE >>= vMapGuardPTE nval

vMapLookup mmix (nval, base, ix:ixs) = do
  mbPTP <- vMapLoadPTX mmix base ix
  case mbPTP >>= vMapGuardPTP nval >>= Just . fldSet0 ptpFnq of
    Just ptp -> vMapLookup mmix (nval, ptp, ixs)
    Nothing -> return Nothing
-- }}}

-- }}}

-- }}}

-- TCKey fields {{{
tcKeyfi :: BitFld
tcKeyfi = (62, 61)

tcKeyfv :: BitFld
tcKeyfv = (60, 13)

tcKeyfn :: BitFld
tcKeyfn = (12, 3)

tcKeyva :: BitFld
tcKeyva = (63, 13)
-- }}}

-- TCVal fields {{{
tcValfa :: BitFld
tcValfa = (37, 3)

tcValfp :: BitFld
tcValfp = (2, 0)
-- }}}

-- TC, TCInfo, MMIXTC {{{

type TC = Map.Map TCKey TCInfo

data TCInfo = TCInfo
  { tciVal :: Octa
  , tciTS  :: Octa
  , tciCT  :: Octa }

data MMIXTC = MMIXTC
  { tcInsn :: IORef TC
  , tcData :: IORef TC }

-- }}}

-- TC operation {{{

-- newMMIXTC {{{
newMMIXTC :: IO MMIXTC
newMMIXTC = do
  iTC <- newIORef $ Map.empty
  dTC <- newIORef $ Map.empty
  return $ MMIXTC iTC dTC
-- }}}

-- tcNewEntry: make tc entry from {time-stamp, rV, VAddr, PTE} {{{
tcNewEntry :: Octa -> Octa -> VAddr -> PTE -> (TCKey, TCInfo)
tcNewEntry ts rV vaddr pte = (key, val)
  where
    s = fldGet rVFs rV
    n = fldGet rVFn rV
    p = fldGet pteFp pte
    ay = fldGet pteFay pte
    key = (fldGetRaw (63, s) vaddr) .|. (fldGetRaw (12,3) rV)
    val = TCInfo trans ts 0
    trans = fldSet tcValfa ay p
-- }}}

-- tcLookupPTE: loopup in Insn-TC {{{
tcLookupPTE :: TC -> Octa -> VAddr -> Maybe PTE
tcLookupPTE tc rV vaddr = tcKVToPTE key <$> mbVal
  where
    n = fldGet rVFn rV
    vaRaw = fldGetRaw vaddrFtc vaddr
    key = fldSet tcKeyfn n vaRaw
    mbVal = tciVal <$> Map.lookup key tc
-- }}}

-- tcValToPTE: convert tcVal to PTE {{{
tcKVToPTE :: TCKey -> TCVal -> PTE
tcKVToPTE k v = fldSet pteFay ay $ fldSet pteFn n p
  where
    ay = fldGet tcValfa v
    p = fldGet tcValfp v
    n = fldGet tcKeyfn k
-- }}}

-- mmixTCAddEntryD {{{
mmixTCAddEntryD :: MMIX -> VAddr -> PTE -> IO ()
mmixTCAddEntryD mmix vaddr pte = do
  ts <- mmixGetSR mmix rIIx
  rV <- mmixGetSR mmix rVIx
  let (k,i) = tcNewEntry ts rV vaddr pte
  dtc <- mmixGetDTC mmix
  let dtc' = Map.insert k i dtc
  mmixSetDTC mmix dtc'
-- }}}

-- mmixTCAddEntryI {{{
mmixTCAddEntryI :: MMIX -> VAddr -> PTE -> IO ()
mmixTCAddEntryI mmix vaddr pte = do
  ts <- mmixGetSR mmix rIIx
  rV <- mmixGetSR mmix rVIx
  let (k,i) = tcNewEntry ts rV vaddr pte
  itc <- mmixGetITC mmix
  let itc' = Map.insert k i itc
  mmixSetITC mmix itc'
-- }}}

-- mmixTCDelEntryD {{{
mmixTCDelEntryD :: MMIX -> TCKey -> IO ()
mmixTCDelEntryD mmix key = do
  dtc <- mmixGetDTC mmix
  let dtc' = Map.delete key dtc
  mmixSetDTC mmix dtc'
-- }}}

-- mmixTCDelEntryI {{{
mmixTCDelEntryI :: MMIX -> TCKey -> IO ()
mmixTCDelEntryI mmix key = do
  itc <- mmixGetITC mmix
  let itc' = Map.delete key itc
  mmixSetITC mmix itc'
-- }}}

-- mmixTCFlushD {{{
mmixTCFlushD :: MMIX -> IO ()
mmixTCFlushD mmix = mmixSetDTC mmix Map.empty
-- }}}

-- mmixTCFlushI {{{
mmixTCFlushI :: MMIX -> IO ()
mmixTCFlushI mmix = mmixSetITC mmix Map.empty
-- }}}

-- }}}

-- }}}

-- MMIX machine model {{{

-- the MMIX {{{
--   1 pc
--   32 + n SR
--   256 GR
--   some memory-mapped devices.
data MMIX =
  forall dev. (Device dev) =>
  MMIX
  { mmixPC  :: IORef VAddr
  , mmixSRD :: RD
  , mmixGRD :: GRD
  , mmixTC  :: MMIXTC
  , mmixDev :: dev
  }

showMMIX :: MMIX -> IO ()
showMMIX mmix = do
  pc <- mmixGetPC mmix
  printf "PC: #%16x\n" pc
  srText <- showSRD $ mmixSRD mmix
  putStrLn srText
  showMMIXGRD mmix
  putStrLn "end\n"

showMMIXGRD :: MMIX -> IO ()
showMMIXGRD mmix = do
  l <- cast <$> mmixGetSR mmix rLIx
  g <- cast <$> mmixGetSR mmix rGIx
  let rIdList = take (cast l) [0 ..] ++ [g .. 255]
  rVList <- mapM (mmixGetGR mmix) rIdList
  putStrLn $ concat $ zipWith (showR) (map show rIdList) rVList

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
mmixGRDL :: MMIX -> RD
mmixGRDL = fst . mmixGRD

mmixGRDG :: MMIX -> RD
mmixGRDG = snd . mmixGRD

mmixGetGR :: MMIX -> GRIx -> IO Octa
mmixGetGR mmix ix = do
  l <- cast <$> mmixGetSR mmix rLIx
  g <- cast <$> mmixGetSR mmix rGIx
  o <- cast <$> mmixGetSR mmix rOIx
  if ix < l
  then rGet (mmixGRDL mmix) (o + ix)
  else
    if ix >= g
    then rGet (mmixGRDG mmix) (ix)
    else return 0

mmixSetGR :: MMIX -> GRIx -> Octa -> IO ()
mmixSetGR mmix ix v = do
  l <- cast <$> mmixGetSR mmix rLIx
  g <- cast <$> mmixGetSR mmix rGIx
  o <- cast <$> mmixGetSR mmix rOIx
  when (l <= ix && ix < g) $ do -- fill 0, inc rL
    mapM (\i -> rSet (mmixGRDL mmix) (o + i) 0) [l .. (ix - 1)]
    mmixSetSR mmix rLIx $ cast (ix + 1)
  if ix < g
  then rSet (mmixGRDL mmix) (o + ix) v
  else rSet (mmixGRDG mmix) ix v

-- }}}

-- TC {{{
mmixGetDTC :: MMIX -> IO TC
mmixGetDTC = readIORef . tcData . mmixTC

mmixGetITC :: MMIX -> IO TC
mmixGetITC = readIORef . tcInsn . mmixTC

mmixSetDTC :: MMIX -> TC -> IO ()
mmixSetDTC = writeIORef . tcData . mmixTC

mmixSetITC :: MMIX -> TC -> IO ()
mmixSetITC = writeIORef . tcInsn . mmixTC

-- }}}

-- memory-mapped devices {{{
-- TODO: can we avoid pattern-matching?
mmixLdInsn :: MMIX -> PAddr -> IO (Maybe Tetra)
mmixLdInsn = mmixLdTetra

mmixLdOcta :: MMIX -> PAddr -> IO (Maybe Octa)
mmixLdOcta MMIX { mmixDev = dev } = devReadOcta dev

mmixLdTetra :: MMIX -> PAddr -> IO (Maybe Tetra)
mmixLdTetra MMIX { mmixDev = dev } = devReadTetra dev

mmixLdWyde :: MMIX -> PAddr -> IO (Maybe Wyde)
mmixLdWyde MMIX { mmixDev = dev } = devReadWyde dev

mmixLdByte :: MMIX -> PAddr -> IO (Maybe Byte)
mmixLdByte MMIX { mmixDev = dev } = devReadByte dev

mmixLdOcta0 :: MMIX -> PAddr -> IO Octa
mmixLdOcta0 mmix paddr = fromMaybe 0 `fmap` mmixLdOcta mmix paddr

mmixLdTetra0 :: MMIX -> PAddr -> IO Tetra
mmixLdTetra0 mmix paddr = fromMaybe 0 `fmap` mmixLdTetra mmix paddr

mmixLdWyde0 :: MMIX -> PAddr -> IO Wyde
mmixLdWyde0 mmix paddr = fromMaybe 0 `fmap` mmixLdWyde mmix paddr

mmixLdByte0 :: MMIX -> PAddr -> IO Byte
mmixLdByte0 mmix paddr = fromMaybe 0 `fmap` mmixLdByte mmix paddr

mmixStOcta :: MMIX -> PAddr -> Octa -> IO Bool
mmixStOcta MMIX { mmixDev = dev } = devWriteOcta dev

mmixStTetra :: MMIX -> PAddr -> Tetra -> IO Bool
mmixStTetra MMIX { mmixDev = dev } = devWriteTetra dev

mmixStWyde :: MMIX -> PAddr -> Wyde -> IO Bool
mmixStWyde MMIX { mmixDev = dev } = devWriteWyde dev

mmixStByte :: MMIX -> PAddr -> Byte -> IO Bool
mmixStByte MMIX { mmixDev = dev } = devWriteByte dev
-- }}}

-- minimum MMIX with ZDev {{{

-- dummy zero dev, accept any address {{{
data ZDev = ZDev

instance Device ZDev where
  devAddrOk _ _ = True
  devReadOcta _ _ = return $ Just 0
  devWriteOcta _ _ _ = return True
-- }}}

newDummyMMIX :: IO MMIX
newDummyMMIX = MMIX
  <$> newIORef (1 `shiftL` 63)
  <*> newSRD
  <*> newGRD
  <*> newMMIXTC
  <*> return ZDev

-- }}}

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
mmixGetGRF mmix ix = castOtoF <$> mmixGetGR mmix ix

mmixSetGRF :: MMIX -> GRIx -> FP -> IO ()
mmixSetGRF mmix ix v = mmixSetGR mmix ix $ castFtoO v
-- }}}

{-
-- move GR {{{
mmixCopyGR :: MMIX -> GRIx -> GRIx -> IO ()
mmixCopyGR mmix from to = do
  r <- mmixGetGR mmix from
  mmixSetGR mmix to r
-- }}}
-}
-- SR operation for FP {{{
mmixGetSRF :: MMIX -> SRIx -> IO FP
mmixGetSRF mmix ix = castOtoF <$> mmixGetSR mmix ix

mmixSetSRF :: MMIX -> SRIx -> FP -> IO ()
mmixSetSRF mmix ix v = mmixSetSR mmix ix $ castFtoO v
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

-- get $X,$Y,$Z by insn {{{
mmixGetGRXY :: MMIX -> Insn -> IO (Octa, Octa)
mmixGetGRXY mmix insn = do
  x <- mmixGetGR mmix $ iGetX insn
  y <- mmixGetGR mmix $ iGetY insn
  return (x, y)
-- }}}

-- get $X,$Y,$Z by insn {{{
mmixGetGRXYZ :: MMIX -> Insn -> IO (Octa, Octa, Octa)
mmixGetGRXYZ mmix insn = do
  x <- mmixGetGR mmix $ iGetX insn
  y <- mmixGetGR mmix $ iGetY insn
  z <- mmixGetGR mmix $ iGetZ insn
  return (x, y, z)
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
tripAEBit ATRIPD = bit $ cast rABEventD
tripAEBit ATRIPV = bit $ cast rABEventV
tripAEBit ATRIPW = bit $ cast rABEventW
tripAEBit ATRIPI = bit $ cast rABEventI
tripAEBit ATRIPO = bit $ cast rABEventO
tripAEBit ATRIPU = bit $ cast rABEventU
tripAEBit ATRIPZ = bit $ cast rABEventZ
tripAEBit ATRIPX = bit $ cast rABEventX
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

-- common arithmetic {{{

-- ArithEx: ARITHmetic EXception {{{
data ArithEx
  = AED -- integer devide check
  | AEV -- integer overflow
  | AEW -- float-to-fix overflow
  | AEI -- invalid operation
  | AEO -- floating overflow
  | AEU -- floating underflow
  | AEZ -- floating division by zero
  | AEX -- floating inexact
  deriving (Eq)

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

-- instance: Show {{{
instance (Show a) => Show (ArithRx a) where
  show (ArithRx [] rx) = show rx
  show (ArithRx ex rx) = show rx ++ "{" ++ show ex ++ "}"
-- }}}

-- instance: Functor {{{
instance Functor ArithRx where
  fmap f (ArithRx exa va) = ArithRx exa $ f va
-- }}}

-- instance: Applicative {{{
instance Applicative ArithRx where
  pure a = ArithRx [] a
  ArithRx exf f <*> ArithRx ex v = ArithRx (ex ++ exf) $ f v
-- }}}

-- instance: Monad {{{
instance Monad ArithRx where
  return a = ArithRx [] a
  ArithRx exa va >>= f = ArithRx (exa ++ exb) vb
    where (ArithRx exb vb) = f va
-- }}}

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

-- Hexadeca: 128-bit unsigned integer {{{

-- HD (high-64 bits) (low-64 bits)
data Hexadeca = HD Octa Octa deriving (Eq, Ord)

-- instance: Bounded {{{
instance Bounded Hexadeca where
  minBound = HD 0 0
  maxBound = HD (-1) (-1)
-- }}}

-- instance: Enum {{{
instance Enum Hexadeca where
  succ (HD (-1) (-1)) = error "Hexadeca.succ maxBound: bad argument"
  succ hd = hd + (HD 0 1)

  pred (HD 0 0) = error "Hexadeca.pred minBound: bad argument"
  pred hd = hd - (HD 0 1)

  toEnum n =
    if n > 0
    then HD 0 (cast n)
    else error "Hexadeca.toEnum: bad argument"
  fromEnum (HD h l) =
    if h == 0 && l `shiftR` 63 == 0
    then cast l
    else error "Hexadeca.fromEnum: bad argument"
-- }}}

-- instance: Num ((+), (-), (*), abs, fromInteger) {{{
instance Num Hexadeca where
  (HD ah al) + (HD bh bl) = HD h l
    where
      l = al + bl
      h = if l < al then ah + bh + 1 else ah + bh

  (HD ah al) - (HD bh bl) = HD h l
    where
      l = al - bl
      h = if l > al then ah - bh - 1 else ah - bh

  a * b = fromInteger $ toInteger a * toInteger b

-- XXX: very slow mult {{{
-- XXX: keep this shit (it works!)
--          ######m7
--        ######m6
--      ######m5
--    ######m4
--  ######m3
--  ####m2
--  ##m1
{- Too slow with div
  (HD ah al) * (HD bh bl) = HD h l
    where
      cast2 (t1,t2) = (cast t1 :: Octa, cast t2 :: Octa)
      (a1,a2) = cast2 $ cvot ah
      (a3,a4) = cast2 $ cvot al
      (b1,b2) = cast2 $ cvot bh
      (b3,b4) = cast2 $ cvot bl
      m67 = HD 0 (a4 * b4) + (m6 `shift` 16)
      m6  = HD 0 (a4 * b3) + HD 0 (a3 * b4)
      m45 = HD 0 (a4 * b2) + HD 0 (a3 * b3)
          + HD 0 (a2 * b4) + (m4 `shift` 16)
      m4  = HD 0 (a4 * b1) + HD 0 (a3 * b2) 
          + HD 0 (a2 * b3) + HD 0 (a1 * b4)
      m23 = HD 0 (a3 * b1) + HD 0 (a2 * b2)
          + HD 0 (a1 * b3) + (m2 `shift` 16)
      m2  = HD 0 (a2 * b1) + HD 0 (a1 * b2)
      m01  = HD 0 (a1 * b1)
      (HD _ l) = m67 + (m45 `shiftL` 32)
      (HD h _) = (m01 `shiftL` 32) + m23
               + (m45 `shiftR` 32) + (m67 `shiftR` 64)
-}
-- }}}

  abs = id
  signum (HD 0 0) = 0
  signum _ = 1
  fromInteger i = HD (cast $ i `shiftR` 64) (cast i)
-- }}}

-- instance: Real {{{
instance Real Hexadeca where
  toRational hd = toInteger hd % 1
-- }}}

-- instance: Bits {{{
instance Bits Hexadeca where
  (HD ah al) .&. (HD bh bl) = HD (ah .&. bh) (al .&. bl)
  (HD ah al) .|. (HD bh bl) = HD (ah .|. bh) (al .|. bl)
  (HD ah al) `xor` (HD bh bl) = HD (ah `xor` bh) (al `xor` bl)
  complement (HD h l) = HD (complement h) (complement l)
  (HD h l) `shift` b = HD h' l'
    where
      h' | b >= 0 = (h `shift` b) .|. (l `shift` (b - 64))
         | otherwise = (h `shift` b)
      l' | b >= 0 = (l `shift` b)
         | otherwise = (h `shift` (b + 64)) .|. (l `shift` b)
  hd `rotate` b = (hd `shift` b0) .|. (hd `shift` b1)
    where b0 = b `rem` 128
          b1 | b >= 0 = b0 - 128
             | b < 0 = b0 + 128
  bitSize _ = 128
  bitSizeMaybe _ = Just 128
  testBit v n = (bit n .&. v) /= zeroBits
  zeroBits = HD 0 0
  bit n = (HD 0 1) `shift` n
  isSigned _ = False
  popCount (HD ah al) = popCount ah + popCount al
-- }}}

-- instance: FiniteBits {{{
instance FiniteBits Hexadeca where
  finiteBitSize _ = 128
-- }}}

-- instance: Integral (quotRem) {{{
instance Integral Hexadeca where
  quotRem a b = (fromInteger q, fromInteger r)
    where (q, r) = quotRem (toInteger a) (toInteger b)

-- XXX: very slow quot/rem (div) {{{
-- XXX: keep this shit.
{-
  quotRem a (HD 0 0) = error "Hexadeca.quotRem: bad argument"
  quotRem a b = qrIter a
    where
      qrIter r = if r < b then (HD 0 0, r) else (dq + q', r')
        where (dq, dr) = maxExp r (HD 0 1) b
              (q', r') = qrIter (r - dr)
      maxExp ra x r = if ra < r2 then (x, r) else maxExp ra x2 r2
        where x2 = x * (HD 0 2)
              r2 = r * (HD 0 2)
-}
-- }}}

  toInteger (HD h l) = ((toInteger h) `shiftL` 64) .|. (toInteger l)
-- }}}

-- instance: Show {{{
instance Show Hexadeca where
  show hd = "0x" ++ (hx $ toInteger hd)
-- }}}

-- }}}

-- fromOrdering: Ordering -> Number {{{
fromOrdering :: Ordering -> Octa
fromOrdering LT = -1
fromOrdering EQ = 0
fromOrdering GT = 1
-- }}}

-- fromBool: Bool -> Number {{{
fromBool :: Bool -> Octa
fromBool True = 1
fromBool False = 0
-- }}}

-- }}}

-- integral arithmetic {{{

-- integer sign bit {{{

intSignBit :: Octa -> Octa
intSignBit a = bitGet 63 a

-- }}}

-- intMul: signed/overflow/64-bits result {{{
intMul :: Octa -> Octa -> ArithRx Octa
intMul a b = ArithRx ex l
  where
    aa = (signExt 63 $ HD 0 a)
    bb = (signExt 63 $ HD 0 b)
    r@(HD h l) = aa * bb
    ss = fldGet (127,63) r -- 65 high bits should be same
    ex = if ss == 0 || ss == HD 1 (-1) then [] else [AEV]
-- }}}

-- intMulu: unsigned/128-bits result {{{
intMulu :: Octa -> Octa -> ArithRx Hexadeca
intMulu a b = return $ (HD 0 a) * (HD 0 b)
-- }}}

-- intDivide: signed/overflow(one case)/(q,r) {{{
intDivide :: Octa -> Octa -> ArithRx (Octa, Octa)
intDivide a b = if b == 0
  then ArithRx [AED] (0, a)
  else ArithRx ex qr
  where
    (q,r) = (castOtoOS a) `quotRem` (castOtoOS b)
    vCase = (0x8000000000000000, 0xffffffffffffffff)
    overflow = (a, b) == vCase
    ex = if overflow then [AEV] else []
    qr = if overflow then (a, 0) else (castOStoO q, castOStoO r)
-- }}}

-- intDivideu: unsigned/no exception {{{
intDivideu :: Hexadeca -> Octa -> ArithRx (Octa, Octa)
intDivideu a@(HD ah al) b = return $
  if ah >= b then (ah, al)
  else (ql, rl)
  where
    (HD qh ql, HD rh rl) = quotRem a (HD 0 b)
-- }}}

-- intAdd: signed/overflow {{{
intAdd :: Octa -> Octa -> ArithRx Octa
intAdd a b = ArithRx ex sum
  where
    sum = a + b
    ex = if intSignBit a == intSignBit b 
         && intSignBit a /= intSignBit sum
         then [AEV] else []
-- }}}

-- intAddu: unsigned {{{
intAddu :: Octa -> Octa -> ArithRx Octa
intAddu a b = return $ a + b
-- }}}

-- int2Addu: a * 2 + b {{{
int2Addu :: Octa -> Octa -> ArithRx Octa
int2Addu a b = return $ a + a + b
-- }}}

-- int4Addu: a * 2 + b {{{
int4Addu :: Octa -> Octa -> ArithRx Octa
int4Addu a b = return $ (a `shiftL` 2) + b
-- }}}

-- int8Addu: a * 2 + b {{{
int8Addu :: Octa -> Octa -> ArithRx Octa
int8Addu a b = return $ (a `shiftL` 3) + b
-- }}}

-- int16Addu: a * 2 + b {{{
int16Addu :: Octa -> Octa -> ArithRx Octa
int16Addu a b = return $ (a `shiftL` 4) + b
-- }}}

-- intSub {{{
intSub :: Octa -> Octa -> ArithRx Octa
intSub a b = ArithRx ex dif
  where
    dif = a - b
    ex = if intSignBit a /= intSignBit b
         && intSignBit a /= intSignBit dif
         then [AEV] else []
-- }}}

-- intSubu {{{
intSubu :: Octa -> Octa -> ArithRx Octa
intSubu a b = return $ a - b
-- }}}

-- intCompare: signed compare {{{
intCompare :: Octa -> Octa -> ArithRx Ordering
intCompare a b = return $ compare (castOtoOS a) (castOtoOS b)
-- }}}

-- intCompareu: unsigned compare {{{
intCompareu :: Octa -> Octa -> ArithRx Ordering
intCompareu a b = return $ compare a b
-- }}}

-- test (N/Z/P/OD/NN/NZ/NP/EV) {{{

-- testN (Negative)
testN :: Octa -> Bool
testN a = bitGet 63 a == 1

-- testZ (Zero)
testZ :: Octa -> Bool
testZ a = a == 0

-- testP (Positive)
testP :: Octa -> Bool
testP a = bitGet 63 a == 0 && a /= 0

-- testODD (Odd:1,3,5..)
testODD :: Octa -> Bool
testODD a = bitGet 0 a == 1

-- testNN (Non-Negative)
testNN :: Octa -> Bool
testNN a = bitGet 63 a == 0

-- testNZ (Non-Zero)
testNZ :: Octa -> Bool
testNZ a = a /= 0

-- testNP (Non-Positive)
testNP :: Octa -> Bool
testNP a = bitGet 63 a == 1 || a == 0

-- testEVEN (Even:2,4,6..)
testEVEN :: Octa -> Bool
testEVEN a = bitGet 0 a == 0

-- }}}

-- }}}

-- logic arithmetic {{{

-- just use (.&.), (.|.), xor, complement

-- shift {{{

-- bitSL: shift left signed with exception {{{
bitSL :: Octa -> Octa -> ArithRx Octa
bitSL a s = ArithRx ex re
  where
    is = if s > 64 then 64 else cast s
    ex = if ((castOtoOS re) `shiftR` is) /= (castOtoOS a)
         then [AEV] else []
    re = a `shiftL` is
-- }}}

-- bitSLu: shift left unsigned {{{
bitSLu :: Octa -> Octa -> ArithRx Octa
bitSLu a s = return $ a `shiftL` is
  where
    is = if s > 64 then 64 else cast s
-- }}}

-- bitSR: shift right signed no exception {{{
bitSR :: Octa -> Octa -> ArithRx Octa
bitSR a s = return $ castOStoO $ (castOtoOS a) `shiftR` is
  where
    is = if s > 64 then 64 else cast s
-- }}}

-- bitSRu: shift right unsigned {{{
bitSRu :: Octa -> Octa -> ArithRx Octa
bitSRu a s = return $ a `shiftR` is
  where
    is = if s > 64 then 64 else cast s
-- }}}

-- }}}

-- diff: y > z ? y - z : 0 {{{

-- byte diff {{{
diffByte :: Octa -> Octa -> Octa
diffByte a b = zipob diff a b
  where diff a b = if a > b then a - b else 0
-- }}}

-- wyde diff {{{
diffWyde :: Octa -> Octa -> Octa
diffWyde a b = zipow diff a b
  where diff a b = if a > b then a - b else 0
-- }}}

-- tetra diff {{{
diffTetra :: Octa -> Octa -> Octa
diffTetra a b = zipot diff a b
  where diff a b = if a > b then a - b else 0
-- }}}

-- octa diff {{{
diffOcta :: Octa -> Octa -> Octa
diffOcta a b = if a > b then a - b else 0
-- }}}

-- }}}

-- mux: bitwise multiplex {{{
bitMux :: Octa -> Octa -> Octa -> Octa
bitMux m a b = (m .&. a) .|. (mc .&. b)
  where mc = complement m
-- }}}

-- sadd: sideways add {{{
bitSAdd :: Octa -> Octa -> Octa
bitSAdd y z = cast $ popCount $ y .&. (complement z)
-- }}}

-- how to put 8 bits togather: {{{
--------------------------------------------------------------------
--  xxxxxxxxiiiiiiiixxxxxxxxiiiiiiiixxxxxxxxiiiiiiiixxxxxxxxiiiiiiii
--  and with 0x0101010101010101 ->
--  _______a_______b_______c_______d_______e_______f_______g_______h
--  ********************************^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
--  shift high part down 28 ->
--    ___a_______b_______c_______d____ (shift 28)
--  | _______e_______f_______g_______h
--  = ___a___e___b___f___c___g___d___h
--    ********^^^^^^^^********^^^^^^^^
--    ___a___e___b___f___c___g_ (shift 7)
--  | ____b___f___c___g___d___h
--  = ___ab__ef__xx__xx__cd__gh
--    *********^^^^^^^^^^^^^^^^
--    ___ab__ef__ (shift 14)
--  | _xx__cd__gh
--  = _xxabcdefgh
--------------------------------------------------------------------
-- }}}

-- bitTransposition {{{
bitTrans :: Octa -> Octa
bitTrans a = cvto (high, low)
  where
    high = cvbt $ mapT4 (cast . squeeze . skip8) (7,6,5,4)
    low  = cvbt $ mapT4 (cast . squeeze . skip8) (3,2,1,0)
    skip8 i = (a `shiftR` i) .&. 0x0101010101010101
    squeeze v = v3 .&. 0xff
      where
        v1 = (v `shiftR` 28) .|. v
        v2 = (v1 `shiftR` 7) .|. v1
        v3 = (v2 `shiftR` 14) .|. v2
-- }}}

-- bitMultiple: do the dirty work (XXX: slow!!!) {{{
bitMultiple :: (Octa -> Octa -> Octa) -> Octa -> Octa -> Octa
bitMultiple f a b = bitTrans $ foldl1' (.|.) $ map row [0..7]
  where
    yT = bitTrans a
    row i = f8 i $ yT .&. (dup8 i b)
    dup8 i v = d8
      where
        d1 = xob i v
        d2 = cvbw (d1, d1)
        d4 = cvwt (d2, d2)
        d8 = cvto (d4, d4)
    f8 ix v8 = v `shiftR` (cast ix)
      where
        v4 = f v8 $ shiftL (v8 .&. 0x0f0f0f0f0f0f0f0f) 4
        v2 = f v4 $ shiftL (v4 .&. 0x3030303030303030) 2
        v1 = f v2 $ shiftL (v2 .&. 0x4040404040404040) 1
        v = v1 .&. 0x8080808080808080
-- }}}

-- mor: multiple or  {{{
bitMOr :: Octa -> Octa -> Octa
bitMOr = bitMultiple (.|.)
-- }}}

-- mxor: multiple xor {{{
bitMXor :: Octa -> Octa -> Octa
bitMXor = bitMultiple xor
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

-- get RoundMode from SR value {{{
toRoundModeRaw :: Octa -> RoundMode
toRoundModeRaw = toRoundMode . fldGet rAFRoundMode
-- }}}

-- convert bits to RoundMode {{{
toRoundMode :: Octa -> RoundMode
toRoundMode 0 = RNear
toRoundMode 1 = RZero
toRoundMode 2 = RUp
toRoundMode 3 = RDown
-- }}}

-- convert immediate value to RoundMode {{{
toRoundModeImm :: Octa -> Maybe RoundMode
toRoundModeImm 1 = Just $ toRoundMode 1
toRoundModeImm 2 = Just $ toRoundMode 2
toRoundModeImm 3 = Just $ toRoundMode 3
toRoundModeImm 4 = Just $ toRoundMode 0
toRoundModeImm _ = Nothing
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
toFSign sbit = if sbit == zeroBits then FPositive else FNegative

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

-- mul/div sign {{{
mdFSign :: FSign -> FSign -> FSign
mdFSign as bs = if as == bs then FPositive else FNegative
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
fpSetNaNPl :: Octa -> FP -> FP
fpSetNaNPl pl orig = castOtoF $ fldSet fpFpl pl $ castFtoO orig

-- payload -> orig -> newval
fpSetNaNPlRaw :: Octa -> Octa -> Octa
fpSetNaNPlRaw pl orig = fldSet fpFpl pl orig

-- payload -> orig -> newval
sfpSetNaNPlRaw :: Tetra -> Tetra -> Tetra
sfpSetNaNPlRaw pl orig = fldSet sfpFpl pl orig
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
fpUnpacksef s 0 f = Number s' e' f'
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
    (e', f') = fpAlignRaw (925, f `shiftL` 2)
-- inf (e == 255 && f == 0)
sfpUnpacksef s 255 0 = Inf $ toFSign s
-- nan (e == 255 && f > 0)
sfpUnpacksef s 255 f = NaN (toFSign s)
                           (sfpToNaNRaw f)
                           (fldGet sfpFpl f `shiftL` 29)
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
    nan = fpSetFSignRaw s $ fpSetNaNPlRaw pl $ castFtoO fpQNaN
    ex = if nantype == SignalNaN then [AEI] else []

-- for DEBUG
fpTester :: FP -> Bool
fpTester fp = fp == fp'
  where (ArithRx _ fp') = fpPack RZero . fpUnpack $ fp

-- }}}

-- fpPackRaw {{{
fpPackRaw :: RoundMode -> FPInfo -> FP
fpPackRaw _ (NaN s SignalNaN 0) = fpSetFSign s $ fpSNaN
fpPackRaw _ (NaN s SignalNaN pl) = fpSetFSign s $ fpSetNaNPl pl fpSNaN
fpPackRaw r fpi = arithGetRx $ fpPack r fpi
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
    nan = sfpSetFSignRaw s $
            sfpSetNaNPlRaw (cast pl `shiftR` 29) $
            castSFtoT sfpQNaN
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

-- fpiAlign {{{
fpiAlign :: FPInfo -> FPInfo
fpiAlign (Number s e f) = Number s e' f'
  where (e',f') = fpAlignRaw (e,f)
fpiAlign x = x
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

-- fpUnordered {{{

fpUnordered :: FP -> FP -> Bool
fpUnordered a b = fpIsNaN a || fpIsNaN b

-- }}}

-- fpUnorderedEps {{{

fpUnorderedEps :: FP -> FP -> FP -> Bool
fpUnorderedEps e a b = 
  (bitGet fpBsign (castFtoO e) == 1) || fpIsNaN e || fpIsNaN a || fpIsNaN b

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
-- any NaN -> NaN
fpiAdd r a@(NaN _ _ _) b@(NaN _ _ _) =
  fpPack r $ if fpiIsSNaN a then a else b
fpiAdd r a@(NaN _ _ _) _ = fpPack r a
fpiAdd r _ b@(NaN _ _ _) = fpPack r b
-- any Inf -> Inf; +Inf-Inf=NaN
fpiAdd r a@(Inf as) b@(Inf bs) =
  if as /= bs then fpPack r $ NaN bs SignalNaN 0
  else fpPack r b
fpiAdd r a@(Inf as) _ = fpPack r a
fpiAdd r _ b@(Inf bs) = fpPack r b

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

-- fpMult {{{

-- fpMult {{{
fpMult :: RoundMode -> FP -> FP -> ArithRx FP
fpMult r a b = fpiMult r (fpUnpack a) (fpUnpack b)
-- }}}

-- fpiMult {{{
fpiMult :: RoundMode -> FPInfo -> FPInfo -> ArithRx FP
-- Number
fpiMult r (Number as ae af) (Number bs be bf) = fpPack r (Number s e f)
  where
    s = mdFSign as bs
    (HD h l) = (HD 0 af) * (HD 0 bf `shiftL` 9)
    (e, f) =
      if fldGet (63,54) h == 0
      then (ae + be - 1022, (h `shiftL` 1) .|. tbit)
      else (ae + be - 1021, h .|. tbit)
    tbit = if l == 0 then 0 else 1
-- Number * Zero = Zero
fpiMult r (Number as _ _) (Zero bs) = fpPack r (Zero s)
  where s = mdFSign as bs
fpiMult r (Zero as) (Number bs _ _) = fpPack r (Zero s)
  where s = mdFSign as bs
fpiMult r (Zero as) (Zero bs) = fpPack r (Zero s)
  where s = mdFSign as bs
-- Inf * not Zero -> Inf
fpiMult r (Inf as) (Number bs _ _) = fpPack r (Inf s)
  where s = mdFSign as bs
fpiMult r (Number as _ _) (Inf bs) = fpPack r (Inf s)
  where s = mdFSign as bs
fpiMult r (Inf as) (Inf bs) = fpPack r (Inf s)
  where s = mdFSign as bs
-- Inf * Zero -> NaN
fpiMult r (Inf as) (Zero bs) = fpPack r (NaN s SignalNaN 0)
  where s = mdFSign as bs
fpiMult r (Zero as) (Inf bs) = fpPack r (NaN s SignalNaN 0)
  where s = mdFSign as bs
-- NaN -> NaN
fpiMult r a@(NaN _ _ _) b@(NaN _ _ _) =
  fpPack r $ if fpiIsSNaN a then a else b
fpiMult r a@(NaN _ _ _) _ = fpPack r a
fpiMult r _ b@(NaN _ _ _) = fpPack r b
-- }}}

-- }}}

-- fpDivide {{{

-- fpDivide {{{
fpDivide :: RoundMode -> FP -> FP -> ArithRx FP
fpDivide r a b = fpiDivide r (fpUnpack a) (fpUnpack b)
-- }}}

-- fpiDivide {{{
fpiDivide :: RoundMode -> FPInfo -> FPInfo -> ArithRx FP
-- x / y = z
fpiDivide r (Number as ae af) (Number bs be bf) =
  fpPack r (Number qs qe qf)
  where
    (q,rem) = HD af 0 `quotRem` HD 0 (bf `shiftL` 9)
    qBig = fldGet (63, 55) q /= 0
    qLow = if rem /= 0 || (qBig && bitGet 0 q == 1) then 1 else 0
    qe = ae - be + 1021 + (if qBig then 1 else 0)
    qf = cast $ qLow .|. (if qBig then q `shiftR` 1 else q)
    qs = mdFSign as bs
-- x / 0 = 0 exception Z
fpiDivide r (Number as _ _) (Zero bs) =
  ArithRx [AEZ] () >> fpPack r (Zero s)
  where s = mdFSign as bs
-- x / inf = 0
fpiDivide r (Number as _ _) (Inf bs) = fpPack r (Zero s)
  where s = mdFSign as bs
-- 0 / x = 0
fpiDivide r (Zero as) (Number bs _ _) = fpPack r (Zero s)
  where s = mdFSign as bs
-- 0 / Inf = 0
fpiDivide r (Zero as) (Inf bs) = fpPack r (Zero s)
  where s = mdFSign as bs
-- 0 / 0 = NaN
fpiDivide r (Zero as) (Zero bs) = fpPack r (NaN s SignalNaN 0)
  where s = mdFSign as bs
-- Inf / Inf = NaN
fpiDivide r (Inf as) (Inf bs) = fpPack r (NaN s SignalNaN 0)
  where s = mdFSign as bs
-- Inf * not Zero -> Inf
fpiDivide r (Inf as) (Number bs _ _) = fpPack r (Inf s)
  where s = mdFSign as bs
-- Inf * Zero -> Inf
fpiDivide r (Inf as) (Zero bs) = fpPack r (Inf s)
  where s = mdFSign as bs
-- NaN -> NaN
fpiDivide r a@(NaN _ _ _) b@(NaN _ _ _) =
  fpPack r $ if fpiIsSNaN a then a else b
fpiDivide r a@(NaN _ _ _) _ = fpPack r a
fpiDivide r _ b@(NaN _ _ _) = fpPack r b
-- }}}

-- }}}

-- fpRem {{{

-- fpRem {{{
fpRem :: FP -> FP -> ArithRx FP
fpRem a b = fpiRem (fpUnpack a) (fpUnpack b)
-- }}}

-- fpiRem {{{
fpiRem :: FPInfo -> FPInfo -> ArithRx FP
-- number % number
fpiRem a@(Number as ae af) b@(Number bs be bf) =
  fpRemRaw as (ae, af) (be, bf) False
-- zero % x, num % inf -> orig a
fpiRem a@(Zero _) (Number _ _ _) = fpPack RZero a
fpiRem a@(Zero _) (Inf _) = fpPack RZero a
fpiRem a@(Number _ _ _) (Inf _) = fpPack RZero a
-- NaN -> NaN
fpiRem a@(NaN _ _ _) b@(NaN _ _ _) =
  fpPack RZero $ if fpiIsSNaN a then a else b
fpiRem a@(NaN _ _ _) _ = fpPack RZero a
fpiRem _ b@(NaN _ _ _) = fpPack RZero b
-- others: signal nan
fpiRem (Inf s) _ = fpPack RZero $ NaN s SignalNaN 0
fpiRem (Zero s) _ = fpPack RZero $ NaN s SignalNaN 0
fpiRem (Number s _ _) _ = fpPack RZero $ NaN s SignalNaN 0
-- }}}

-- fpRemRaw {{{
fpRemRaw :: FSign -> (OctaS, Octa) -> (OctaS, Octa) -> Bool -> ArithRx FP
fpRemRaw s a@(ae, af) b@(be, bf) odd
  | (ae >= be && af == bf) = fpPack RZero $ Zero s
  | (ae >= be && af < bf && ae == be) = fpPack RZero $ slctMin af
  | (ae >= be) = fpRemRaw s (fpAlignRaw a') b odd'
  | (ae < be - 1) = fpPack RZero $ fpiAlign $ Number s ae af
  | otherwise = fpPack RZero $ slctMin (af `shiftR` 1)
  where
    a' = if af < bf then (ae - 1, af + af - bf) else (ae, af - bf)
    odd' = odd || (if af < bf then ae - 1 == be else ae == be)
    slctMin f = fpiAlign $
      if cf > f || (cf == f && odd == False)
      then Number s be f
      else Number (flipFSign s) be cf
      where cf = bf - f
-- }}}

-- }}}

-- fpSqrt {{{

-- fpSqrt {{{
fpSqrt :: RoundMode -> FP -> ArithRx FP
fpSqrt r a = fpiSqrt r (fpUnpack a)
-- }}}

-- fpiSqrt {{{
fpiSqrt :: RoundMode -> FPInfo -> ArithRx FP
-- Number: + ok, - NaN
fpiSqrt r (Number FNegative _ _) =
  fpPack RNear $ NaN FNegative SignalNaN 0
fpiSqrt r (Number s e f) = fpPack r $ Number s xe xf'
  where
    (xe, xf) = ((e + 1022) `shiftR` 1, 2)
    f0 = if bitGet 0 e == 1 then f `shiftL` 1 else f
    rf = (fldGet (63,54) f0) - 1
    xf' = iter (53, rf, xf)
    iter (0,r,x) = if r /= 0 then x + 1 else x
    iter (k,r,x) = iter (k - 1, r''', x'')
      where
        (r',x') = (r `shiftL` 2, x `shiftL` 1)
        k2 = k `shiftL` 1
        fld | k >= 27 = (k2 - 53, k2 - 54)
            | otherwise = undefined
        r'' = if k >= 27 then r' + (fldGet fld f0) else r'
        (r''', x'') =
          if r'' > x'
          then (r'' - x' - 1, x' + 2)
          else (r'',x')
-- Zero -> orig
fpiSqrt r a@(Zero _) = fpPack r a
-- -NaN -> Signal NaN
fpiSqrt r (NaN FNegative _ _) = fpPack r $ NaN FNegative SignalNaN 0
-- -Inf -> Signal NaN
fpiSqrt r (Inf FNegative) = fpPack r $ NaN FNegative SignalNaN 0
-- +NaN/+Inf, pack it back
fpiSqrt r a = fpPack r a
-- }}}

-- }}}

-- fpCompare {{{

-- fpCompare {{{
fpCompare:: FP -> FP -> ArithRx Ordering
fpCompare a b = fpiCompare (fpUnpack a) (fpUnpack b)
-- }}}

-- fpiCompare {{{
fpiCompare :: FPInfo -> FPInfo -> ArithRx Ordering
-- normal numbers
fpiCompare (Number as ae af) (Number bs be bf) =
  case compare as bs of
    EQ -> case compare ae be of
            EQ -> return $ compare af bf
            eOrd -> return eOrd
    sOrd -> return sOrd
fpiCompare (Number as ae af) (Zero _) =
  case as of
    FPositive -> return GT
    FNegative -> return LT
fpiCompare (Zero _) (Number bs be bf) =
  case bs of
    FPositive -> return LT
    FNegative -> return GT
fpiCompare (Zero _) (Zero _) = return EQ
-- use wildcard for NaN
fpiCompare (NaN _ _ _) _ = ArithRx [AEI] EQ
fpiCompare _ (NaN _ _ _) = ArithRx [AEI] EQ
-- use wildcard for Inf
fpiCompare (Inf FPositive) (Inf FPositive) = return EQ
fpiCompare (Inf FNegative) (Inf FNegative) = return EQ
fpiCompare (Inf FPositive) _ = return GT
fpiCompare (Inf FNegative) _ = return LT
fpiCompare _ (Inf FPositive) = return LT
fpiCompare _ (Inf FNegative) = return GT
-- }}}

-- }}}

-- fpCompareEps {{{

-- fpCompareEps: eps -> a -> b -> >/</= (weak equal) {{{
fpCompareEps :: FP -> FP -> FP -> ArithRx Ordering
fpCompareEps e a b = fpiCompareEps (fpUnpack e) (fpUnpack a) (fpUnpack b)
-- }}}

-- fpiCompareEps {{{
fpiCompareEps :: FPInfo -> FPInfo -> FPInfo -> ArithRx Ordering
-- any NaN -> exception
fpiCompareEps (NaN _ _ _) _ _ = ArithRx [AEI] EQ
fpiCompareEps _ (NaN _ _ _) _ = ArithRx [AEI] EQ
fpiCompareEps _ _ (NaN _ _ _) = ArithRx [AEI] EQ
-- e < 0 -> exception
fpiCompareEps (Number FNegative _ _) _ _ = ArithRx [AEI] EQ
-- normal numbers
fpiCompareEps (Number _ ee ef) a@(Number as ae af) b@(Number bs be bf) =
  if fpWeakEqualEpsRaw (ee,ef) (as,ae,af) (bs,be,bf)
  then return EQ else fpiCompare a b
fpiCompareEps (Number _ ee ef) a@(Zero as) b@(Number bs be bf) =
  if fpWeakEqualEpsRaw (ee, ef) (as, -1000, 0) (bs, be, bf)
  then return EQ else fpiCompare a b
fpiCompareEps (Number _ ee ef) a@(Number as ae af) b@(Zero bs) =
  if fpWeakEqualEpsRaw (ee, ef) (as, ae, af) (bs, -1000, 0)
  then return EQ else fpiCompare a b
fpiCompareEps (Number _ _ _) (Zero _) (Zero _) = return EQ
fpiCompareEps (Number _ ee _) a@(Inf as) b@(Inf bs) =
  if as == bs || ee >= 1023 then return EQ else fpiCompare a b
fpiCompareEps (Number _ ee _) a@(Inf _) b =
  if ee >= 1022 then return EQ else fpiCompare a b
fpiCompareEps (Number _ ee _) a b@(Inf _) =
  if ee >= 1022 then return EQ else fpiCompare a b
fpiCompareEps (Zero _) a b = fpiCompare a b
fpiCompareEps (Inf _) _ _ = return EQ
-- }}}

-- fpWeakEqualEpsRaw {{{
fpWeakEqualEpsRaw
  :: (OctaS, Octa) -- eps
  -> (FSign, OctaS, Octa)
  -> (FSign, OctaS, Octa)
  -> Bool
fpWeakEqualEpsRaw e a@(_, ae, af) b@(_, be, bf) =
  fpEqualEpsRaw e l r
  where
    (ar, br) = (e0 a, e0 b)
    (l, r) =
      if ae < be || (ae == be && af < bf)
      then (br, ar)
      else (ar, br)
    e0 orig@(s, e, f) =
      if e < 0
      then (s, 0, f `shiftR` cast (-e))
      else orig
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
fpiEqual a b = arithGetRx (fpiCompare a b) == EQ
-- }}}

-- }}}

-- fpEqualEpsRaw {{{
fpEqualEpsRaw
  :: (OctaS, Octa) -- eps
  -> (FSign, OctaS, Octa) -- larger
  -> (FSign, OctaS, Octa) -- smaller
  -> Bool
fpEqualEpsRaw (ee,ef) (as,ae,af) (bs,be,bf)
  | ee >= 1023 = True
  | bl /= 0 && ee < 1020 = False
  | bh'' == 0 = True
  | ee < 968 = False
  | otherwise = bh'' <= ef'
  where
    d = ae - be
    (bh,bl)
      | d > 54 = (0, bf)
      | d > 0 = (fldGet (63, cast d) bf, fldGet (cast (d - 1), 0) bf)
      | otherwise = (bf, 0)
    bh' = if as /= bs && bl /= 0 then bh + 1 else bh
    bh'' = if as == bs then af - bh' else af + bh'
    ef' = if ee >= 1021
      then ef `shiftL` cast (ee - 1021)
      else ef `shiftR` cast (1021 - ee)
-- }}}

-- fpEqualEps {{{

-- fpEqualEps {{{
fpEqualEps :: FP -> FP -> FP -> ArithRx Bool
fpEqualEps e a b = fpiEqualEps (fpUnpack e) (fpUnpack a) (fpUnpack b)
-- }}}

-- fpiEqualEps {{{
fpiEqualEps :: FPInfo -> FPInfo -> FPInfo -> ArithRx Bool
-- any NaN -> exception
fpiEqualEps (NaN _ _ _) _ _ = ArithRx [AEI] False
fpiEqualEps _ (NaN _ _ _) _ = ArithRx [AEI] False
fpiEqualEps _ _ (NaN _ _ _) = ArithRx [AEI] False
-- e < 0 -> exception
fpiEqualEps (Number FNegative _ _) _ _ = ArithRx [AEI] False
-- normal numbers
fpiEqualEps (Number _ ee ef) (Number as ae af) (Number bs be bf) =
  return $ fpStrongEqualEpsRaw (ee,ef) (as,ae,af) (bs,be,bf)
fpiEqualEps (Number _ _ _) (Zero _) (Number _ _ _) = return False
fpiEqualEps (Number _ _ _) (Number _ _ _) (Zero _) = return False
fpiEqualEps (Number _ _ _) (Zero _) (Zero _) = return True
fpiEqualEps (Number _ ee _) (Inf as) (Inf bs) =
  return $ as == bs || ee >= 1023
fpiEqualEps (Number _ _ _) (Inf _) _ = return False
fpiEqualEps (Number _ _ _) _ (Inf _) = return False
fpiEqualEps (Zero _) a b = return $ fpiEqual a b
fpiEqualEps (Inf _) _ _ = return True
-- }}}

-- fpStrongEqualEpsRaw {{{
fpStrongEqualEpsRaw 
  :: (OctaS, Octa) -- eps
  -> (FSign, OctaS, Octa)
  -> (FSign, OctaS, Octa)
  -> Bool
fpStrongEqualEpsRaw e@(ee,ef) a@(_, ae, af) b@(_, be, bf) =
  fpEqualEpsRaw e' l r
  where
    (ar, br) = (e0 a, e0 b)
    (l@(_,le,_), r@(_,re,_)) =
      if ae < be || (ae == be && af < bf)
      then (br, ar)
      else (ar, br)
    e0 orig@(s, e, f) =
      if e < 0
      then (s, 0, f `shiftR` cast (-e))
      else orig
    e' = (ee - (le - re), ef)
-- }}}

-- }}}

-- fpInt: FP -> Octa {{{

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
fpFloat r o = fpPack r (Number s e f)
  where
    s = if bitGet 63 o == 0 then FPositive else FNegative
    u = if s == FPositive then o else (-o)
    (e, f) = fpAlignRaw (1076, u)

-- }}}

-- fpFloatu: Octa -> FP (Octa as unsigned) {{{

fpFloatu :: RoundMode -> Octa -> ArithRx FP
fpFloatu r 0 = return fpPZero
fpFloatu r o = fpPack r (Number s e f)
  where
    s = FPositive
    (e, f) = fpAlignRaw (1076, o)

-- }}}

-- fpSFloat: Octa -> FP (notice: not SFP) {{{
fpSFloat :: RoundMode -> Octa -> ArithRx FP
fpSFloat r 0 = return fpPZero
fpSFloat r o = sfpPack r (Number s e f) >>= fpPack r . sfpUnpack
  where
    s = if bitGet 63 o == 0 then FPositive else FNegative
    u = if s == FPositive then o else (-o)
    (e, f) = fpAlignRaw (1076, u)

-- }}}

-- fpSFloatu: Octa -> FP (notice: not SFP) {{{
fpSFloatu :: RoundMode -> Octa -> ArithRx FP
fpSFloatu r 0 = return fpPZero
fpSFloatu r o = sfpPack r (Number s e f) >>= fpPack r . sfpUnpack
  where
    s = FPositive
    (e, f) = fpAlignRaw (1076, o)

-- }}}

-- }}}

-- Tips: `za` toggle folding; `zM` fold all; `zR` unfold all.
-- vim: fdm=marker
