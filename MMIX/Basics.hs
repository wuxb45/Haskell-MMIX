-- Copyright 2012 Wu Xingbo <wuxb45@gmail.com>

-- ghc options {{{

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}

-- }}}

-- module exports {{{

module MMIX.Basics (
  fI,
-- machine types and fields {{{
  Octa, Tetra, Wyde, Byte,
  Addr, VAddr, PAddr, RAddr, BitIx, BitFld,
  Insn, OPCode, X, Y, Z, YZ, XYZ,
  PTE, PTP,
-- }}}
-- general bytes convertion {{{
  cvot, cvtw, cvwb, cvow, cvtb, cvob,
  cvto, cvwt, cvbw, cvwo, cvbt, cvbo,
  xob, xow, xot, xtb, xtw, xwb,
  mob, mow, mot, mtb, mtw, mwb,
-- }}}
-- general bits convertion {{{
  fldMask, fldUMask, fldGet, fldSet,
  fldGetRaw, fldSetRaw, fldSet1, fldSet0,
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
  rAFEventD, rAFEventV, rAFEventW, rAFEventI,
  rAFEventO, rAFEventU, rAFEventZ, rAFEventX,
  rAFEnableD,rAFEnableV,rAFEnableW,rAFEnableI,
  rAFEnableO,rAFEnableU,rAFEnableZ,rAFEnableX,
  rAFRoundMode,
-- }}}
-- general registers {{{
  newGRD,
-- }}}
-- address interface {{{
  Device (..),
-- }}}
-- instruction operation {{{
  iGetOByte, iGetX, iGetY, iGetZ,
  iGetYZ, iGetYZv, iGetXYZ, iGetXYZv,
-- }}}
-- opcode type {{{
  OPType (..), opMap,
-- }}}
-- the MMIX machine {{{
  MMIX (..),
  mmixGetPC, mmixSetPC,
  mmixGetSR, mmixSetSR,
  mmixGetGR, mmixSetGR,
  mmixLdOcta, mmixLdTetra, mmixLdWyde, mmixLdByte,
  mmixStOcta, mmixStTetra, mmixStWyde, mmixStByte,
  --newDummyMMIX,
-- }}}
-- virtual address mapping {{{
  vaddrFaddr, vaddrFseg, vaddrF63,
  pteFx, pteFay, pteFn, pteFp,
  ptpFc, ptpFn, ptpFq, ptpFnq,
-- }}}
  ) where

-- }}}

-- imports {{{
import System.IO.Unsafe (unsafePerformIO)
import Data.Word (Word64, Word32, Word16, Word8)
import Data.Bits (bit, shiftR, shiftL, (.&.), (.|.), complement)
import Data.Array.MArray (MArray, newArray, getElems,
                          readArray, writeArray)
import Data.Array.IO (IOUArray)
import Data.IORef (IORef, readIORef, writeIORef, newIORef)
import Control.Applicative ((<$>), (<*>))
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

type Insn   = Octa
type OPCode = Byte    -- Opcode
type X      = Byte    -- X field
type Y      = Byte    -- Y field
type Z      = Byte    -- Z field
type YZ     = (Y,Z)   -- YZ combined field
type XYZ    = (X,Y,Z) -- XYZ combined field

type PTE = Octa
type PTP = Octa

-- Just make it short
fI :: (Integral a, Integral b) => a -> b
fI = fromIntegral

-- }}}

-- general bytes convertion (Big Endian Targets) {{{

-- split cv** {{{

-- Octa -> Tetra *2
cvot :: Octa -> (Tetra, Tetra)
cvot o = (fI (o `shiftR` 32), fI o)

-- Tetra -> Wyde *2
cvtw :: Tetra -> (Wyde,Wyde)
cvtw t = (fI (t `shiftR` 16), fI t)

-- Wyde -> Byte *2
cvwb :: Wyde -> (Byte,Byte)
cvwb w = (fI (w `shiftR` 8), fI w)

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
cvto (ht, lt) = ((fI ht) `shiftL` 32) .|. (fI lt)

-- Wyde *2 -> Tetra
cvwt :: (Wyde, Wyde) -> Tetra
cvwt (hw, lw) = ((fI hw) `shiftL` 16) .|. (fI lw)

-- Byte *2 -> Wyde
cvbw :: (Byte, Byte) -> Wyde
cvbw (hb, lb) = ((fI hb) `shiftL` 8) .|. (fI lb)

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
xob addr val = fI $ val `shiftR` sh
  where sh = (7 - fI (addr .&. 0x7)) `shiftL` 3 :: Int

-- extract Wyde from Octa
xow :: Addr -> Octa -> Wyde
xow addr val = fI $ val `shiftR` sh
  where sh = (6 - fI (addr .&. 0x6)) `shiftL` 3 :: Int

-- extract Tetra from Octa
xot :: Addr -> Octa -> Tetra
xot addr val = fI $ val `shiftR` sh
  where sh = (4 - fI (addr .&. 0x4)) `shiftL` 3 :: Int

-- extract Byte from Tetra
xtb :: Addr -> Tetra -> Byte
xtb addr val = fI $ val `shiftR` sh
  where sh = (3 - fI (addr .&. 0x3)) `shiftL` 3 :: Int

-- extract Wyde from Tetra
xtw :: Addr -> Tetra -> Wyde
xtw addr val = fI $ val `shiftR` sh
  where sh = (2 - fI (addr .&. 0x2)) `shiftL` 3 :: Int

-- extract Byte from Wyde
xwb :: Addr -> Wyde -> Byte
xwb addr val = fI $ val `shiftR` sh
  where sh = (1 - fI (addr .&. 0x1)) `shiftL` 3 :: Int

-- extract }}}

-- modify m** {{{

-- modify Byte in Octa
mob :: Addr -> Octa -> Byte -> Octa
mob addr o b = (o .&. mask) .|. ((fI b) `shiftL` sh)
  where mask = complement $ 0xff `shiftL` sh :: Octa
        sh = (7 - fI (addr .&. 0x7)) `shiftL` 3 :: Int

-- modify Wyde in Octa
mow :: Addr -> Octa -> Wyde -> Octa
mow addr o w = (o .&. mask) .|. ((fI w) `shiftL` sh)
  where mask = complement $ 0xffff `shiftL` sh :: Octa
        sh = (6 - fI (addr .&. 0x6)) `shiftL` 3 :: Int

-- modify Tetra in Octa
mot :: Addr -> Octa -> Tetra -> Octa
mot addr o t = (o .&. mask) .|. ((fI t) `shiftL` sh)
  where mask = complement $ 0xffffffff `shiftL` sh :: Octa
        sh = (4 - fI (addr .&. 0x4)) `shiftL` 3 :: Int

-- modify Byte in Tetra
mtb :: Addr -> Tetra -> Byte -> Tetra
mtb addr t b = (t .&. mask) .|. ((fI b) `shiftL` sh)
  where mask = complement $ 0xff `shiftL` sh :: Tetra
        sh = (3 - fI (addr .&. 0x3)) `shiftL` 3 :: Int

-- modify Wyde in Tetra
mtw :: Addr -> Tetra -> Wyde -> Tetra
mtw addr t w = (t .&. mask) .|. ((fI w) `shiftL` sh)
  where mask = complement $ 0xffff `shiftL` sh :: Tetra
        sh = (2 - fI (addr .&. 0x2)) `shiftL` 3 :: Int

-- modify Byte in Wyde
mwb :: Addr -> Wyde -> Byte -> Wyde
mwb addr w b = (w .&. mask) .|. ((fI b) `shiftL` sh)
  where mask = complement $ 0xff `shiftL` sh :: Wyde
        sh = (1 - fI (addr .&. 0x1)) `shiftL` 3 :: Int

-- modify }}}

-- }}}

-- general bits convertion (Octa only) {{{

-- mask a field
fldMask :: BitFld -> Octa
fldMask (l,r) = lb .|. (lb - rb)
  where lb = bit $ fI l
        rb = bit $ fI r

-- unmask a field
fldUMask :: BitFld -> Octa
fldUMask = complement . fldMask

-- get field
fldGet :: BitFld -> Octa -> Octa
fldGet f@(_,r) v = (v .&. (fldMask f)) `shiftR` (fI r)

-- set field
fldSet :: BitFld -> Octa -> Octa -> Octa
fldSet f@(_,r) v fv =
  ((fldUMask f) .&. v) .|. ((fldMask f) .&. (fv `shiftL` (fI r)))

-- get field, no shift
fldGetRaw :: BitFld -> Octa -> Octa
fldGetRaw f v = v .&. (fldMask f)

-- set field, no shift
fldSetRaw :: BitFld -> Octa -> Octa -> Octa
fldSetRaw f v fv = ((fldUMask f) .&. v) .|. ((fldMask f) .&. fv)

-- set field to 1
fldSet1 :: BitFld -> Octa -> Octa
fldSet1 f v = v .|. (fldMask f)

-- set field to 0
fldSet0 :: BitFld -> Octa -> Octa
fldSet0 f v = v .&. (fldUMask f)

-- }}}

-- common registers {{{

type RIx = RAddr      -- register Index
type SRIx = RIx       -- special register index
type GRIx = RIx       -- general register index

type RD = IOUArray RAddr Octa  -- register device

-- caution: unsafePerformIO
instance Show RD where
  show a = show elist
    where elist = unsafePerformIO $ getElems a

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

-- rB, rD, rE, rH, rJ, rM, rR, rBB     -- 0 ~ 7 {{{

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

-- rC, rN, rO, rS, rI, rT, rTT, rK     -- 8 ~ 15 {{{

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

-- rQ, rU, rV, rG, rL, rA, rF, rP      -- 16 ~ 23 {{{

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

-- rW, rX, rY, rZ, rWW, rXX, rYY, rZZ  -- 24 ~ 31 {{{

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

-- rA fields operation {{{

-- Enable bits *8 {{{
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

-- }}}

-- Event bits *8 {{{
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

-- }}}

-- general registers {{{

-- new general register device
newGRD :: IO RD
newGRD = newArray (0,255) 0

-- }}}

-- instruction field operations {{{

-- get opcode byte
iGetOByte :: Tetra -> OPCode
iGetOByte t = fI $ shiftR t 24

-- get X byte
iGetX :: Tetra -> X
iGetX t = fI $ (shiftR t 16)

-- get Y byte
iGetY :: Tetra -> Y
iGetY t = fI $ (shiftR t 8)

-- get Z byte
iGetZ :: Tetra -> Z
iGetZ t = fI t

-- get YZ bytes
iGetYZ :: Tetra -> YZ
iGetYZ t = ((fI $ shiftR t 8), fI t)

-- get YZ value
iGetYZv :: Tetra -> Octa
iGetYZv t = fI $ (shiftR t 8) .&. 0xffff

-- get XYZ bytes
iGetXYZ :: Tetra -> XYZ
iGetXYZ t = ((fI $ shiftR t 16), (fI $ shiftR t 8), fI t)

-- get XYZ value
iGetXYZv :: Tetra -> Octa
iGetXYZv t = fI $ t .&. 0xffffff

-- }}}

-- opcode type {{{

-- OPType {{{

data OPType =
  TRAP|FCMP|FUN|FEQL|FADD|FIX|FSUB|FIXU|
  FLOT|FLOTI|FLOTU|FLOTUI|SFLOT|SFLOTI|SFLOTU|SFLOTUI|
  FMUL|FCMPE|FUNE|FEQLE|FDIV|FSQRT|FREM|FINT|
  MUL|MULI|MULU|MULUI|DIV|DIVI|DIVU|DIVUI|
  ADD|ADDI|ADDU|ADDUI|SUB|SUBI|SUBU|SUBUI|
  IIADDU|IIADDUI|IVADDU|IVADDUI|
  VIIIADDU|VIIIADDUI|XVIADDU|XVIADDUI|
  CMP|CMPI|CMPU|CMPUI|NEG|NEGI|NEGU|NEGUI|
  SL|SLI|SLU|SLUI|SR|SRI|SRU|SRUI|
  BN|BNB|BZ|BZB|BP|BPB|BOD|BODB|
  BNN|BNNB|BNZ|BNZB|BNP|BNPB|BEV|BEVB|
  PBN|PBNB|PBZ|PBZB|PBP|PBPB|PBOD|PBODB|
  PBNN|PBNNB|PBNZ|PBNZB|PBNP|PBNPB|PBEV|PBEVB|
  CSN|CSNI|CSZ|CSZI|CSP|CSPI|CSOD|CSODI|
  CSNN|CSNNI|CSNZ|CSNZI|CSNP|CSNPI|CSEV|CSEVI|
  ZSN|ZSNI|ZSZ|ZSZI|ZSP|ZSPI|ZSOD|ZSODI|
  ZSNN|ZSNNI|ZSNZ|ZSNZI|ZSNP|ZSNPI|ZSEV|ZSEVI|
  LDB|LDBI|LDBU|LDBUI|LDW|LDWI|LDWU|LDWUI|
  LDT|LDTI|LDTU|LDTUI|LDO|LDOI|LDOU|LDOUI|
  LDSF|LDSFI|LDHT|LDHTI|CSWAP|CSWAPI|LDUNC|LDUNCI|
  LDVTS|LDVTSI|PRELD|PRELDI|PREGO|PREGOI|GO|GOI|
  STB|STBI|STBU|STBUI|STW|STWI|STWU|STWUI|
  STT|STTI|STTU|STTUI|STO|STOI|STOU|STOUI|
  STSF|STSFI|STHT|STHTI|STCO|STCOI|STUNC|STUNCI|
  SYNCD|SYNCDI|PREST|PRESTI|SYNCID|SYNCIDI|PUSHGO|PUSHGOI|
  OR|ORI|ORN|ORNI|NOR|NORI|XOR|XORI|
  AND|ANDI|ANDN|ANDNI|NAND|NANDI|NXOR|NXORI|
  BDIF|BDIFI|WDIF|WDIFI|TDIF|TDIFI|ODIF|ODIFI|
  MUX|MUXI|SADD|SADDI|MOR|MORI|MXOR|MXORI|
  SETH|SETMH|SETML|SETL|INCH|INCMH|INCML|INCL|
  ORH|ORMH|ORML|ORL|ANDNH|ANDNMH|ANDNML|ANDNL|
  JMP|JMPB|PUSHJ|PUSHJB|GETA|GETAB|PUT|PUTI|
  POP|RESUME|SAVE|UNSAVE|SYNC|SWYM|GET|TRIP
  deriving (Show, Eq)

-- }}}

-- a list of opcode, from index 0 to 255 {{{
optypeList :: [OPType]
optypeList =
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

opcodeList :: [OPCode]
opcodeList = [0 .. 255]

opMap :: [(OPCode, OPType)]
opMap = zip opcodeList optypeList

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

-- a MMIX contains 1 pc; 64 spr; 256 gpr; and some address-devices.
data MMIX =
  forall dev.  (Device dev) =>
  MMIX
  { mmixPC   :: IORef VAddr
  , mmixSRD  :: RD
  , mmixGRD  :: RD
  , mmixDev  :: dev
  }

mmixGetPC :: MMIX -> IO VAddr
mmixGetPC = readIORef . mmixPC

mmixSetPC :: MMIX -> VAddr -> IO ()
mmixSetPC mmix = writeIORef (mmixPC mmix)

mmixGetSR :: MMIX -> SRIx -> IO Octa
mmixGetSR mmix = rGet $ mmixSRD mmix

mmixSetSR :: MMIX -> SRIx -> Octa -> IO ()
mmixSetSR mmix = rSet $ mmixSRD mmix

mmixGetGR :: MMIX -> GRIx -> IO Octa
mmixGetGR mmix = rGet $ mmixGRD mmix

mmixSetGR :: MMIX -> GRIx -> Octa -> IO ()
mmixSetGR mmix = rSet $ mmixGRD mmix

mmixLdOcta :: MMIX -> PAddr -> IO (Maybe Octa)
mmixLdOcta (MMIX _ _ _ dev) = devReadOcta dev

mmixLdTetra :: MMIX -> PAddr -> IO (Maybe Tetra)
mmixLdTetra (MMIX _ _ _ dev) = devReadTetra dev

mmixLdWyde :: MMIX -> PAddr -> IO (Maybe Wyde)
mmixLdWyde (MMIX _ _ _ dev) = devReadWyde dev

mmixLdByte :: MMIX -> PAddr -> IO (Maybe Byte)
mmixLdByte (MMIX _ _ _ dev) = devReadByte dev

mmixStOcta :: MMIX -> PAddr -> Octa -> IO Bool
mmixStOcta (MMIX _ _ _ dev) = devWriteOcta dev

mmixStTetra :: MMIX -> PAddr -> Tetra -> IO Bool
mmixStTetra (MMIX _ _ _ dev) = devWriteTetra dev

mmixStWyde :: MMIX -> PAddr -> Wyde -> IO Bool
mmixStWyde (MMIX _ _ _ dev) = devWriteWyde dev

mmixStByte :: MMIX -> PAddr -> Byte -> IO Bool
mmixStByte (MMIX _ _ _ dev) = devWriteByte dev

--newDummyMMIX :: IO MMIX
--newDummyMMIX = MMIX <$> newIORef 0 <*> newSRD <*> newGRD <*> return ZDev

-- }}}

-- Virtual address mapping {{{

-- virtual address fields {{{

vaddrFaddr :: BitFld
vaddrFaddr = (60,0)

vaddrFseg :: BitFld
vaddrFseg = (62,61)

vaddrF63 :: BitFld
vaddrF63 = (63,63)

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

-- }}}

-- vim: fdm=marker

