-- Copyright 2012 Wu Xingbo <wuxb45@gmail.com>
-- vim: fdm=marker

-- module exports {{{
module MMIX.Run (
  vMapGetPTE,
  doTrip,
  doFTrap,
  doDTrap,
  ) where
-- }}}

-- imports {{{

import Data.Maybe (fromMaybe)
import Data.Bits (complement, xor, shiftL, shiftR, (.|.), (.&.))
import Control.Monad (when, unless, void)
import Data.Functor ((<$>))
import Debug.Trace (trace, traceShow)
import Data.Array.IArray (Array (..), array, (!))
import MMIX.Basics
import MMIX.OMem

-- }}}

-- PTE operation {{{

-- type VMapParam: (base, limit, pn, nval) {{{
type VMapParam = (Octa, Octa, Octa, Octa)
-- }}}

-- type VStartPoint: (nval, len, indexList) {{{
type VStartPoint = (Octa, Octa, [Octa])
-- }}}

-- vMapGetPTE0: MMIX -> VAddr -> IO PTE, on fail returns 0 {{{
vMapGetPTE0 :: MMIX -> VAddr -> IO PTE
vMapGetPTE0 mmix vaddr = do
  mbPTE <- vMapGetPTE mmix vaddr
  case mbPTE of
    Just pte -> return pte
    Nothing -> return 0

-- }}}

-- vMapGetPTE: MMIX -> VAddr -> IO (Maybe PTE) {{{
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

-- convert VAddr to PAddr {{{

-- isVirtual: simply test if need to translate {{{
isVirtual :: VAddr -> Bool
isVirtual vaddr = (fldGet vaddrFsign vaddr) == 0
-- }}}

-- toPAddr: VAddr -> PAddr for a MMIX {{{
toPAddr :: MMIX -> VAddr -> IO (Maybe PAddr)
toPAddr mmix vaddr =
  if isVirtual vaddr
  then toPAddrVMap mmix vaddr
  else return $ Just $ bitSet0 63 vaddr
-- }}}

-- toPAddrVMap: PAddr -> VAddr by page-tables {{{
toPAddrVMap :: MMIX -> VAddr -> IO (Maybe PAddr)
toPAddrVMap mmix vaddr = do
  mbPTE <- vMapGetPTE mmix vaddr
  case mbPTE of
    Just pte -> do
      rV <- mmixGetSR mmix rVIx
      return $ return $ vMapGetPAddr rV vaddr pte
    Nothing -> return Nothing
-- }}}
      
-- }}}

-- load/store {{{

-- store (byte/wyde/tetra/octa; overflow/nothing) {{{

-- store Octa {{{
mmixVStOcta :: MMIX -> VAddr -> Octa -> IO ()
mmixVStOcta mmix vaddr v = do
  mbPAddr <- toPAddr mmix vaddr
  case mbPAddr of
    Just paddr -> void $ mmixStOcta mmix paddr v
    _ -> return ()
-- }}}

-- store Tetra unsigned {{{
mmixVStTetrau :: MMIX -> VAddr -> Octa -> IO ()
mmixVStTetrau mmix vaddr v = do
  mbPAddr <- toPAddr mmix vaddr
  case mbPAddr of
    Just paddr -> void $ mmixStTetra mmix paddr $ cast v
    _ -> return ()
-- }}}

-- store Wyde unsigned {{{
mmixVStWydeu :: MMIX -> VAddr -> Octa -> IO ()
mmixVStWydeu mmix vaddr v = do
  mbPAddr <- toPAddr mmix vaddr
  case mbPAddr of
    Just paddr -> void $ mmixStWyde mmix paddr $ cast v
    _ -> return ()
-- }}}

-- store Byte unsigned {{{
mmixVStByteu :: MMIX -> VAddr -> Octa -> IO ()
mmixVStByteu mmix vaddr v = do
  mbPAddr <- toPAddr mmix vaddr
  case mbPAddr of
    Just paddr -> void $ mmixStByte mmix paddr $ cast v
    _ -> return ()
-- }}}

-- store Tetra signed {{{
mmixVStTetra :: MMIX -> VAddr -> Octa -> IO ()
mmixVStTetra mmix vaddr v = do
  let high = fldGet (63,31) v
  let highSame = high == 0 || high == 0x1ffffffff
  unless highSame $ issueATrip mmix AEV
  mmixVStTetrau mmix vaddr v
-- }}}

-- store Wyde signed {{{
mmixVStWyde :: MMIX -> VAddr -> Octa -> IO ()
mmixVStWyde mmix vaddr v = do
  let high = fldGet (63,15) v
  let highSame = high == 0 || high == 0x1ffffffffffff
  unless highSame $ issueATrip mmix AEV
  mmixVStWydeu mmix vaddr v
-- }}}

-- store Byte signed {{{
mmixVStByte :: MMIX -> VAddr -> Octa -> IO ()
mmixVStByte mmix vaddr v = do
  let high = fldGet (63,7) v
  let highSame = high == 0 || high == 0x1ffffffffffffff
  unless highSame $ issueATrip mmix AEV
  mmixVStByteu mmix vaddr v
-- }}}

-- }}}

-- load (byte/wyde/tetra/octa; signed/unsigned) {{{

-- load Octa {{{
mmixVLdOcta :: MMIX -> VAddr -> IO Octa
mmixVLdOcta mmix vaddr = do
  mbPAddr <- toPAddr mmix vaddr
  v <- case mbPAddr of
    Just paddr -> mmixLdOcta0 mmix paddr
    _ -> return 0
  return v
-- }}}

-- load Tetra signed {{{
mmixVLdTetra :: MMIX -> VAddr -> IO Octa
mmixVLdTetra mmix vaddr = do
  mbPAddr <- toPAddr mmix vaddr
  v <- case mbPAddr of
    Just paddr -> mmixLdTetra0 mmix paddr
    _ -> return 0
  return $ signExtTetraOcta v
-- }}}

-- load Wyde signed {{{
mmixVLdWyde :: MMIX -> VAddr -> IO Octa
mmixVLdWyde mmix vaddr = do
  mbPAddr <- toPAddr mmix vaddr
  v <- case mbPAddr of
    Just paddr -> mmixLdWyde0 mmix paddr
    _ -> return 0
  return $ signExtWydeOcta v
-- }}}

-- load Byte signed {{{
mmixVLdByte :: MMIX -> VAddr -> IO Octa
mmixVLdByte mmix vaddr = do
  mbPAddr <- toPAddr mmix vaddr
  v <- case mbPAddr of
    Just paddr -> mmixLdByte0 mmix paddr
    _ -> return 0
  return $ signExtByteOcta v
-- }}}

-- load Tetra unsigned {{{
mmixVLdTetrau :: MMIX -> VAddr -> IO Octa
mmixVLdTetrau mmix vaddr = do
  mbPAddr <- toPAddr mmix vaddr
  v <- case mbPAddr of
    Just paddr -> mmixLdTetra0 mmix paddr
    _ -> return 0
  return $ cast v
-- }}}

-- load Wyde unsigned {{{
mmixVLdWydeu :: MMIX -> VAddr -> IO Octa
mmixVLdWydeu mmix vaddr = do
  mbPAddr <- toPAddr mmix vaddr
  v <- case mbPAddr of
    Just paddr -> mmixLdWyde0 mmix paddr
    _ -> return 0
  return $ cast v
-- }}}

-- load Byte unsigned {{{
mmixVLdByteu :: MMIX -> VAddr -> IO Octa
mmixVLdByteu mmix vaddr = do
  mbPAddr <- toPAddr mmix vaddr
  v <- case mbPAddr of
    Just paddr -> mmixLdByte0 mmix paddr
    _ -> return 0
  return $ cast v
-- }}}

-- load high Tetra {{{
mmixVLdHighTetra :: MMIX -> VAddr -> IO Octa
mmixVLdHighTetra mmix vaddr = do
  mbPAddr <- toPAddr mmix vaddr
  v <- case mbPAddr of
    Just paddr -> mmixLdTetra0 mmix paddr
    _ -> return 0
  let ht = cvto (v,0)
  return ht
-- }}}

-- }}}

-- load SFP and convert to FP {{{
mmixVLdSF :: MMIX -> VAddr -> IO Octa
mmixVLdSF mmix vaddr = do
  rdm <- toRoundModeRaw <$> mmixGetSR mmix rAIx
  mbPAddr <- toPAddr mmix vaddr
  sfp <- case mbPAddr of
    Just paddr -> mmixLdTetra0 mmix paddr
    _ -> return 0
  let fp = fpPackRaw rdm $ sfpUnpack $ castTtoSF sfp
  return $ castFtoO fp
-- }}}

-- CSwap: if M == rP then M=$X,$X=1 else rP=M,$X=0 {{{
mmixCSwap :: MMIX -> GRIx -> VAddr -> IO ()
mmixCSwap mmix ix vaddr = do
  x <- mmixGetGR mmix ix
  rP <- mmixGetSR mmix rPIx
  v <- mmixVLdOcta mmix vaddr
  if v == rP
  then do
    mmixVStOcta mmix vaddr x
    mmixSetGR mmix ix 1
  else do
    mmixSetSR mmix rPIx v
    mmixSetGR mmix ix 0

-- }}}

-- }}}

-- issue Trip/Trap {{{

-- tripSetABit: set a event bit by trip type. {{{
tripSetABit :: MMIX -> TRIP -> IO ()
tripSetABit mmix trip = do
  rA <- mmixGetSR mmix rAIx
  mmixSetSR mmix rAIx $ rA .|. (tripAEBit trip)
-- }}}

-- tripClearABit: clear a event bit by trip type. {{{
tripClearABit :: MMIX -> TRIP -> IO ()
tripClearABit mmix trip = do
  rA <- mmixGetSR mmix rAIx
  mmixSetSR mmix rAIx $ rA .&. (complement $ tripAEBit trip)
-- }}}

-- issueTrip: (set hidden SR: rTRIP) {{{
issueTrip :: MMIX -> IO ()
issueTrip mmix = mmixSetSR mmix rTRIPIx 1
-- }}}

-- issueTrap: (set hidden SR: rTRAP) {{{
issueTrap :: MMIX -> IO ()
issueTrap mmix = mmixSetSR mmix rTRAPIx 1
-- }}}

-- issueATrip: issue ArithEx Trip {{{
issueATrip :: MMIX -> ArithEx -> IO ()
issueATrip mmix ex = tripSetABit mmix $ cvAEtoT ex
-- }}}

-- issueDTrap: dynamic trap happened (set rQ bit) {{{
issueDTrap :: BitIx -> MMIX -> IO ()
issueDTrap ix mmix = do
  rQ <- mmixGetSR mmix rQIx
  mmixSetSR mmix rQIx $ bitSet1 ix rQ
  -- also keep them in shadow register rQQ
  rQQ <- mmixGetSR mmix rQQIx
  mmixSetSR mmix rQQIx $ bitSet1 ix rQQ
-- }}}

-- issueDTrapP* {{{

-- read permission
issueDTrapPr :: MMIX -> IO ()
issueDTrapPr = issueDTrap rQBr

-- write permission
issueDTrapPw :: MMIX -> IO ()
issueDTrapPw = issueDTrap rQBw

-- execute permission
issueDTrapPx :: MMIX -> IO ()
issueDTrapPx = issueDTrap rQBx

-- refers to a negative virtual address
issueDTrapPn :: MMIX -> IO ()
issueDTrapPn = issueDTrap rQBn

-- privileged instruction
issueDTrapPk :: MMIX -> IO ()
issueDTrapPk = issueDTrap rQBk

-- breaks the rule (illegal insn)
issueDTrapPb :: MMIX -> IO ()
issueDTrapPb = issueDTrap rQBb

-- insn violates security
issueDTrapPs :: MMIX -> IO ()
issueDTrapPs mmix = do
  -- SPECIAL CASE: set S bit of rK to 1
  rK <- mmixGetSR mmix rKIx
  mmixSetSR mmix rKIx $ bitSet1 rKBs rK
  issueDTrap rQBs mmix

-- insn comes from a privileged virtual address
issueDTrapPp :: MMIX -> IO ()
issueDTrapPp = issueDTrap rQBp

-- }}}

-- }}}

-- do Trip/Trap (transfer control) {{{

-- doTrip: really do a trip (set regs, goto entries) {{{
doTrip :: MMIX -> Insn -> VAddr -> TRIP -> IO ()
doTrip mmix insn naddr trip = do
  -- rA, clear the accepted bit.
  tripClearABit mmix trip
  -- rW, return address
  mmixSetSR mmix rWIx naddr
  -- rX, resume info and the insn
  mmixSetSR mmix rXIx $ fldSet1 rXFsign $ cast insn
  -- rY, maybe $Y
  y <- mmixGetGR mmix $ iGetY insn
  mmixSetSR mmix rYIx y
  -- rZ, maybe $Z
  z <- mmixGetGR mmix $ iGetZ insn
  mmixSetSR mmix rZIx z
  -- rB, rB = $255
  r255 <- mmixGetGR mmix 255
  mmixSetSR mmix rBIx r255
  -- r255, r255 = rJ
  rJ <- mmixGetSR mmix rJIx
  mmixSetGR mmix 255 rJ
  -- goto entry
  mmixSetPC mmix $ tripEntry trip
-- }}}

-- doFTrap: really do forced-trap {{{
-- param: mmix, the insn, next pc
doFTrap :: MMIX -> Insn -> VAddr -> IO ()
doFTrap mmix insn naddr = do
  -- rK = 0, inhibiting interrupts
  mmixSetSR mmix rKIx 0
  -- rWW, return address
  mmixSetSR mmix rWWIx naddr
  -- rXX, resume info and the insn
  mmixSetSR mmix rXXIx $ fldSet1 rXFsign $ cast insn
  -- rYY, maybe $Y
  y <- mmixGetGR mmix $ iGetY insn
  mmixSetSR mmix rYYIx y
  -- rZZ, maybe $Z
  z <- mmixGetGR mmix $ iGetZ insn
  mmixSetSR mmix rZZIx z
  -- rBB, rBB = $255
  r255 <- mmixGetGR mmix 255
  mmixSetSR mmix rBBIx r255
  -- r255, r255 = rJ
  rJ <- mmixGetSR mmix rJIx
  mmixSetGR mmix 255 rJ
  -- goto rT
  rT <- mmixGetSR mmix rTIx
  mmixSetPC mmix rT
-- }}}

-- doDTrap: really do dynamic-trap {{{
doDTrap :: MMIX -> Insn -> VAddr -> IO ()
doDTrap mmix insn naddr = do
  -- rK = 0, inhibiting interrupts
  mmixSetSR mmix rKIx 0
  -- rWW, return address
  mmixSetSR mmix rWWIx naddr
  -- rXX, resume info and the insn, dyn-trap also set <program> bits
  pBits <- fldGet rQFprog `fmap` mmixGetSR mmix rQIx
  mmixSetSR mmix rXXIx $ fldSet1 rXXFsign $ fldSet rXXFprog pBits $ cast insn
  -- rYY, maybe $Y
  y <- mmixGetGR mmix $ iGetY insn
  mmixSetSR mmix rYYIx y
  -- rZZ, maybe $Z
  z <- mmixGetGR mmix $ iGetZ insn
  mmixSetSR mmix rZZIx z
  -- rBB, rBB = $255
  r255 <- mmixGetGR mmix 255
  mmixSetSR mmix rBBIx r255
  -- r255, r255 = rJ
  rJ <- mmixGetSR mmix rJIx
  mmixSetGR mmix 255 rJ
  -- goto rTT
  rTT <- mmixGetSR mmix rTTIx
  mmixSetPC mmix rTT
-- }}}

-- }}}

-- access function permission check (rwxnkbsp) {{{

-- basic functions to test rwxnpbsp {{{

-- check read permission of a page
checkPr :: PTE -> Bool
checkPr pte = 1 == bitGet pteBpr pte

-- check write permission of a page
checkPw :: PTE -> Bool
checkPw pte = 1 == bitGet pteBpw pte

-- check execute permission of a page
checkPx :: PTE -> Bool
checkPx pte = 1 == bitGet pteBpx pte

-- check referring negative address
checkPn :: VAddr -> Bool
checkPn vaddr = 1 == bitGet 63 vaddr

-- check security
-- neg-addr or rK(prog) == 0xff => security OK
-- param: addr(i) and rK
-- XXX: trick, just check rK!
checkPs :: Octa -> Bool
checkPs rK = fldGet rKFprog rK == 0xff

-- check instruction come from negative vaddr
-- fail if insn come from negative address and rK(p) = 1
-- param: addr(i) and rK 
-- XXX: trick, just check rK!
checkPp :: Octa -> Bool
checkPp rK = bitGet rKBp rK == 0

-- }}}

-- checkTrapped: check if MMIX has been traped {{{
checkTrapped :: MMIX -> IO Bool
checkTrapped mmix = do
  rQ <- mmixGetSR mmix rQIx
  rK <- mmixGetSR mmix rKIx
  return $ (rQ .&. rK) /= 0
-- }}}

-- checkPCPosition: check and issue (s/p) {{{

-- check insn can run at PC issue Trap
checkPCPosition :: MMIX -> IO ()
checkPCPosition mmix = do
  pc <- mmixGetPC mmix
  if isVirtual pc
  then runDoChecks mmix
  else runDoCheckp mmix

-- check bit s of rK, issue Trap on fail 
runDoChecks :: MMIX -> IO ()
runDoChecks mmix = do
  rK <- mmixGetSR mmix rKIx
  let sOK = checkPs rK
  unless sOK $ issueDTrapPs mmix

-- check bit p and rK, issue Trap on fail 
runDoCheckp :: MMIX -> IO ()
runDoCheckp mmix = do
  rK <- mmixGetSR mmix rKIx
  let pOK = checkPp rK
  unless pOK $ issueDTrapPp mmix
-- }}}

-- check VAddr can execute and issue Trap (x) (neg-addr -> True) {{{
checkVAddrExecute :: MMIX -> VAddr -> IO Bool
checkVAddrExecute mmix vaddr = do
  when (fldGet (1,0) vaddr /= 0) $ traceShowIO ("Check: exec unaligned", vaddr)
  if isVirtual vaddr
  then do
    pte <- vMapGetPTE0 mmix vaddr
    let xOK = 1 == bitGet pteBpx pte
    unless xOK $ issueDTrapPx mmix
    return xOK
  else
    return True
-- }}}

-- check VAddr can be read and issue Trap (r/n) {{{
checkVAddrRead :: MMIX -> VAddr -> IO ()
checkVAddrRead mmix vaddr = do
  when (fldGet (2,0) vaddr /= 0) $ traceShowIO ("Check: read unaligned", vaddr)
  if isVirtual vaddr
  then do
    canRead <- checkPr <$> vMapGetPTE0 mmix vaddr
    unless canRead $ issueDTrapPr mmix
  else
    issueDTrapPn mmix
-- }}}

-- check VAddr can be write and issue Trap (w/n) {{{
checkVAddrWrite :: MMIX -> VAddr -> IO ()
checkVAddrWrite mmix vaddr = do
  when (fldGet (2,0) vaddr /= 0) $ traceShowIO ("Check: write unaligned", vaddr)
  if isVirtual vaddr
  then do
    canWrite <- checkPw <$> vMapGetPTE0 mmix vaddr
    unless canWrite $ issueDTrapPw mmix
  else
    issueDTrapPn mmix
-- }}}

-- check VAddr can be read & write and issue Trap (r/w/n) {{{
checkVAddrReadWrite :: MMIX -> VAddr -> IO ()
checkVAddrReadWrite mmix vaddr = do
  when (fldGet (2,0) vaddr /= 0) $ traceShowIO ("Check: read and write unaligned", vaddr)
  if isVirtual vaddr
  then do
    pte <- vMapGetPTE0 mmix vaddr
    let canWrite = checkPw pte
    unless canWrite $ issueDTrapPw mmix
    let canRead = checkPr pte
    unless canRead $ issueDTrapPr mmix
  else
    issueDTrapPn mmix
-- }}}

-- checkVAddrReadN {{{
checkVAddrReadN :: MMIX -> [VAddr] -> IO ()
checkVAddrReadN mmix vaList = mapM_ (checkVAddrRead mmix) vaList
-- }}}

-- checkVAddrWriteN {{{
checkVAddrWriteN :: MMIX -> [VAddr] -> IO ()
checkVAddrWriteN mmix vaList = mapM_ (checkVAddrWrite mmix) vaList
-- }}}

-- }}}

-- runMMIX and sub-routines {{{

-- run mmix {{{
runMMIX :: MMIX -> IO ()
runMMIX mmix = do
  checkPCPosition mmix -- check s/p
  trapped <- checkTrapped mmix
  unless trapped $ runFetchExec mmix
-- }}}

-- runFetchExec: check x and run {{{

-- fetch and execute insn
runFetchExec :: MMIX -> IO ()
runFetchExec mmix = do
  pc <- mmixGetPC mmix
  checkVAddrExecute mmix pc
  trapped <- checkTrapped mmix
  unless trapped $ runFetchExecAny mmix pc

-- execute on any address, fetch/exec.
runFetchExecAny :: MMIX -> VAddr -> IO ()
runFetchExecAny mmix pc = do
  mbPAddr <- toPAddr mmix pc
  case mbPAddr of
    Just paddr -> runPAddr mmix paddr
    Nothing -> trace "fuck me! shit!" $ return ()

-- }}}

-- exec on PAddr {{{
runPAddr :: MMIX -> PAddr -> IO ()
runPAddr mmix paddr = do
  mbInsn <- mmixLdTetra mmix paddr
  case mbInsn of
    Just insn -> runInsn mmix insn
    Nothing -> trace "fuck me! shit!" $ return ()
-- }}}

-- branch helper (use YZ) {{{
runBranchForward :: MMIX -> Insn -> IO ()
runBranchForward mmix insn = do
  pc <- mmixGetPC mmix
  let npc = pc + (iGetYZu insn `shiftL` 2)
  mmixSetPC mmix npc

runBranchBackward :: MMIX -> Insn -> IO ()
runBranchBackward mmix insn = do
  pc <- mmixGetPC mmix
  let npc = pc + (iGetYZu insn `shiftL` 2) - 0x40000
  mmixSetPC mmix npc
-- }}}

-- jump helper (use XYZ) {{{
runJumpForward :: MMIX -> Insn -> IO ()
runJumpForward mmix insn = do
  pc <- mmixGetPC mmix
  let npc = pc + (iGetXYZu insn `shiftL` 2)
  mmixSetPC mmix npc

runJumpBackward :: MMIX -> Insn -> IO ()
runJumpBackward mmix insn = do
  pc <- mmixGetPC mmix
  let npc = pc + (iGetXYZu insn `shiftL` 2) - 0x4000000
  mmixSetPC mmix npc
-- }}}

-- push-jump helper (return address) {{{
runPJumpForward :: MMIX -> Insn -> IO ()
runPJumpForward mmix insn = do
  pc <- mmixGetPC mmix
  let ret = pc + 4
  mmixSetSR mmix rJIx ret
  runBranchForward mmix insn

runPJumpBackward :: MMIX -> Insn -> IO ()
runPJumpBackward mmix insn = do
  pc <- mmixGetPC mmix
  let ret = pc + 4
  mmixSetSR mmix rJIx ret
  runBranchBackward mmix insn
-- }}}

-- push-go helper (return address) {{{
runPGo :: MMIX -> VAddr -> IO ()
runPGo mmix vaddr = do
  pc <- mmixGetPC mmix
  let ret = pc + 4
  mmixSetSR mmix rJIx ret
  mmixSetPC mmix vaddr
-- }}}

-- putSR: put to sepcial register {{{
putSR :: MMIX -> SRIx -> Octa -> IO ()
putSR mmix ix v
  | ix == rAIx = putSRrA mmix v
  | ix == rLIx = putSRrL mmix v
  | ix == rGIx = putSRrG mmix v
  | ix == rQIx = putSRrQ mmix v
  | ix `elem` rNoChange = putSRNoChange mmix ix v
  | ix `elem` rPrivilege = putSRPrivilege mmix ix v
  | otherwise = mmixSetSR mmix ix v
  -- | ix == rAIx && 
  where
    rNoChange  = [rNIx,rOIx,rSIx]
    rPrivilege = [rCIx,rIIx,rKIx,rQIx,rTIx,rUIx,rVIx,rTTIx]

putSRrA :: MMIX -> Octa -> IO ()
putSRrA mmix v = 
  if fldGet rAFZero v /= 0
  then issueDTrapPb mmix
  else mmixSetSR mmix rAIx v

putSRrL :: MMIX -> Octa -> IO ()
putSRrL mmix v =
  if fldGet rLFZero v /= 0
  then issueDTrapPb mmix
  else do
    rL <- mmixGetSR mmix rLIx
    when (v < rL) $ mmixSetSR mmix rLIx v

putSRrG :: MMIX -> Octa -> IO ()
putSRrG mmix v =
  if fldGet rGFZero v /= 0
  then issueDTrapPb mmix
  else do
    rL <- mmixGetSR mmix rLIx
    if v < rL -- rG < rL is bad
    then issueDTrapPb mmix
    else mmixSetSR mmix rGIx v

putSRNoChange :: MMIX -> SRIx -> Octa -> IO ()
putSRNoChange mmix ix v = do
  -- XXX: or we always issue b ?
  --orig <- mmixGetSR mmix ix
  --when (orig /= v) issueDTrapPb mmix
  issueDTrapPb mmix

putSRPrivilege :: MMIX -> SRIx -> Octa -> IO ()
putSRPrivilege mmix ix v = do
  rK <- mmixGetSR mmix rKIx
  if bitGet rKBk rK == 0
  then mmixSetSR mmix ix v
  else issueDTrapPb mmix

putSRrQ :: MMIX -> Octa -> IO ()
putSRrQ mmix v = do
  rQQ <- mmixGetSR mmix rQQIx
  --mmixSetSR mmix rQQIx 0  -- XXX: need to clear rQQ ?
  putSRPrivilege mmix rQIx $ rQQ .|. v 
-- }}}

-- set/get rOS {{{
runSetrOS :: MMIX -> Octa -> IO ()
runSetrOS mmix v = do
  mmixSetSR mmix rOIx v
  mmixSetSR mmix rSIx v

runGetrOS :: MMIX -> IO Octa
runGetrOS mmix = do
  rO <- mmixGetSR mmix rOIx
  rS <- mmixGetSR mmix rSIx
  when (rO /= rS) $ traceShowIO ("get rO /= rS", rO, rS)
  return rO

-- }}}

-- runStoreMulti: noCheck (from-gr, to-address) {{{
runStoreMultiGR :: MMIX -> [(GRIx, VAddr)] -> IO ()
runStoreMultiGR mmix ((ix,vaddr):rest) = do
  --checkVAddrWrite mmix vaddr
  --trapped <- checkTrapped mmix
  --unless trapped $ do
  r <- mmixGetGR mmix ix
  mmixVStOcta mmix vaddr r
  runStoreMultiGR mmix rest
-- }}}

-- runLoadMulti: noCheck (from-address, to-gr) {{{
runLoadMultiGR :: MMIX -> [(VAddr, GRIx)] -> IO ()
runLoadMultiGR mmix ((vaddr,ix):rest) = do
  --checkVAddrRead mmix vaddr
  --trapped <- checkTrapped mmix
  --unless trapped $ do
  r <- mmixVLdOcta mmix vaddr
  mmixSetGR mmix ix r
  runLoadMultiGR mmix rest
-- }}}

-- runPopGR (for POP) {{{
-- NOTE: Here we design pop as a semi-atomic op.
-- if memory is not ok, POP just do nothing
runPopGR :: MMIX -> GRIx -> IO ()
runPopGR mmix resN = do
  rO <- runGetrOS mmix
  checkVAddrRead mmix (rO - 8)
  trapped <- checkTrapped mmix
  unless trapped $ do
    popN <- cast <$> mmixVLdOcta mmix (rO - 8)
    runPopGRN mmix popN resN

runPopGRN :: MMIX -> GRIx -> GRIx -> IO ()
runPopGRN mmix popN resN0 = do
  rO <- runGetrOS mmix
  let lowAddr = rO - ((cast popN + 1) * 8)
  let rList = take (cast popN) $ [lowAddr, lowAddr + 8 ..]
  checkVAddrReadN mmix rList
  trapped <- checkTrapped mmix
  unless trapped $ do
    l <- cast <$> mmixGetSR mmix rLIx
    g <- cast <$> mmixGetSR mmix rGIx
    let resN = if resN0 > l then l + 1 else resN0
    -- hole: last really reserved reg or 0
    hole <- if 0 < resN && resN <= l
      then mmixGetGR mmix (resN - 1)
      else return 0
    let l' = min (popN + resN) g
    mmixSetSR mmix rLIx $ cast l'
    runSetrOS mmix $ rO - ((cast popN + 1) * 8)
    mmixSetGR mmix popN hole
    runLoadMultiGR mmix $ zip rList [0 ..]

-- }}}

-- runPushGR (for PUSHGO/PUSHJ) {{{
-- NOTE: Here we design push as a semi-atomic op.
-- if memory is not ok, PUSH just put some trash data
-- out of the stack, that is, write something after rO.
runPushGR :: MMIX -> GRIx -> IO ()
runPushGR mmix ix = do
  l <- cast <$> mmixGetSR mmix rLIx
  g <- cast <$> mmixGetSR mmix rGIx
  if ix >= g
  then runPushGR mmix l
  else runPushRaw mmix ix

runPushRaw :: MMIX -> GRIx -> IO ()
runPushRaw mmix n = do
  rO <- runGetrOS mmix
  rL <- mmixGetSR mmix rLIx
  let l = cast rL
  mmixSetGR mmix n $ cast n  -- set $n = n (count)
  let wList = take (cast n + 1) [rO, rO + 8 ..]
  checkVAddrWriteN mmix wList
  runStoreMultiGR mmix $ zip [0, 1 ..] wList
  trapped <- checkTrapped mmix
  unless trapped $ do
    mmixSetSR mmix rLIx $ (rL - (cast n) - 1)
    runSetrOS mmix $ rO + ((cast n + 1) * 8)

-- }}}

-- runPopOne {{{
-- caution: you should do this only when rL == 0
runPopOne :: MMIX -> IO Octa
runPopOne mmix = do
  rO <- runGetrOS mmix
  checkVAddrRead mmix $ rO - 8
  trapped <- checkTrapped mmix
  if trapped then return 0
  else do
    rL <- mmixGetSR mmix rLIx
    when (rL /= 0) $ traceShowIO "fuck, popOne with rL > 0"
    runSetrOS mmix $ rO - 8
    mmixVLdOcta mmix (rO - 8)
-- }}}

-- runPushOne {{{
-- caution: you should do this only when rL == 0
runPushOne :: MMIX -> Octa -> IO ()
runPushOne mmix v = do
  rO <- runGetrOS mmix
  checkVAddrWrite mmix rO
  trapped <- checkTrapped mmix
  unless trapped $ do
    rL <- mmixGetSR mmix rLIx
    when (rL /= 0) $ traceShowIO "fuck, pushOne with rL > 0"
    mmixVStOcta mmix rO v
    runSetrOS mmix $ rO + 8
-- }}}

-- runResume {{{
runResume :: MMIX -> Z -> IO ()
-- resume 0: use rX/rW/rY/rZ, resume from trip
runResume mmix 0 = do
  rX <- mmixGetSR mmix rXIx
  let insert = bitGet rXBsign rX == 0
  let ropcode = fldGet rXFrop rX
  rW <- mmixGetSR mmix rWIx
  when insert $ run0Insert mmix (rW - 4) ropcode
  mmixSetPC mmix rW

runResume mmix 1 = do
  rXX <- mmixGetSR mmix rXXIx
  r255 <- mmixGetGR mmix 255
  mmixSetSR mmix rKIx r255
  rBB <- mmixGetSR mmix rBBIx
  mmixSetGR mmix 255 rBB
  let insert = bitGet rXXBsign rXX == 0
  let ropcode = fldGet rXXFrop rXX
  rWW <- mmixGetSR mmix rWWIx
  when insert $ run1Insert mmix (rWW - 4) ropcode
  mmixSetPC mmix rWW

run0Insert :: MMIX -> VAddr -> Octa -> IO ()
run0Insert mmix vaddr 0 = do undefined

run1Insert :: MMIX -> VAddr -> Octa -> IO ()
run1Insert mmix vaddr 0 = do undefined
  
-- }}}

-- saveSRIxList: saved special registers {{{

-- save sequence:
-- $0 .. $(l-1), count, $g .. $255, this-list, rGA
saveSRIxList :: [SRIx]
saveSRIxList = [rBIx, rDIx, rEIx, rHIx, rJIx, rMIx,
                rRIx, rPIx, rWIx, rXIx, rYIx, rZIx]
-- }}}

-- runSave {{{
-- different with MMIX-DOC:
-- SAVE will do nothing if memory is not all ok.
-- SAVE will not be interrupted if memory is OK.
runSave :: MMIX -> GRIx -> IO ()
runSave mmix ix = do
  rO <- runGetrOS mmix
  rL <- mmixGetSR mmix rLIx
  rG <- mmixGetSR mmix rGIx
  let g = cast rG
  if ix < g -- ix must be global
  then issueDTrapPb mmix
  else do
    --  count = (rL + 1) + (256 - rG) + 12 + 1
    --        = rL - rG + 270
    let count = cast $ 270 + rL - rG 
    checkVAddrWriteN mmix $ take (cast count) [rO, rO + 8 ..]
    trapped <- checkTrapped mmix
    unless trapped $ do
      runPushGR mmix 255
      gList <- mapM (mmixGetGR mmix) [g .. 255]
      mapM (runPushOne mmix) gList
      srList <- mapM (mmixGetSR mmix) saveSRIxList
      mapM (runPushOne mmix) srList
      rA <- mmixGetSR mmix rAIx
      let rGA = mob 0 rA g
      runPushOne mmix rGA
      let topAddr = rO + ((count - 1) * 8)
      mmixSetGR mmix ix topAddr
-- }}}

-- runUnsave {{{
runUnsave :: MMIX -> VAddr -> IO ()
runUnsave mmix topAddr = do
  -- get rGA, L+1, check all address
  checkVAddrRead mmix topAddr
  trapped <- checkTrapped mmix
  unless trapped $ do
    rGA <- mmixVLdOcta mmix topAddr
    let rG = rGA `shiftR` 56
    let rA = rGA .&. 0xffffffff
    let lAddr = topAddr - ((269 - rG) * 8)
    checkVAddrRead mmix lAddr
    trapped <- checkTrapped mmix
    unless trapped $ do
      n <- mmixVLdOcta mmix lAddr
      let lowAddr = lAddr - (n * 8)
      checkVAddrReadN mmix [lowAddr, lowAddr + 8 .. topAddr]
      trapped <- checkTrapped mmix
      unless trapped $ runUnsaveRaw mmix topAddr

-- RAW pop all of them
runUnsaveRaw :: MMIX -> VAddr -> IO ()
runUnsaveRaw mmix topAddr = do
  rGA <- mmixVLdOcta mmix topAddr
  let rG = rGA `shiftR` 56
  mmixSetSR mmix rGIx rG
  let rA = rGA .&. 0xffffffff
  mmixSetSR mmix rAIx rA
  runSetrOS mmix topAddr
  srList <- sequence $ take 12 $ repeat $ runPopOne mmix
  sequence_ $ zipWith (mmixSetSR mmix) (reverse saveSRIxList) srList
  grList <- sequence $ take (256 - cast rG) $ repeat $ runPopOne mmix
  sequence_ $ zipWith (mmixSetGR mmix) [255, 254 ..] grList
  runPopGR mmix 0

-- }}}

-- incPC: pc = pc + 4 {{{
incPC :: MMIX -> IO ()
incPC mmix = do
  pc <- mmixGetPC mmix
  mmixSetPC mmix (pc + 4)
-- }}}

-- runInsn {{{
runInsn :: MMIX -> Insn -> IO ()
runInsn mmix insn = do
  let runOP = insnOpMap ! iGetOPCode insn
  runOP mmix insn
-- }}}

-- }}}

-- all INSN {{{

-- entry point {{{
type RunInsn = MMIX -> Insn -> IO ()

-- runInsnList: runXXX list {{{
runInsnList :: [RunInsn]
runInsnList =
  -- 0*
  [ runTRAP, runFCMP, runFUN, runFEQL
  , runFADD, runFIX, runFSUB, runFIXU
  , runFLOT, runFLOTI, runFLOTU, runFLOTUI
  , runSFLOT, runSFLOTI, runSFLOTU, runSFLOTUI

  -- 1*
  , runFMUL, runFCMPE, runFUNE, runFEQLE
  , runFDIV, runFSQRT, runFREM, runFINT
  , runMUL, runMULI, runMULU, runMULUI
  , runDIV, runDIVI, runDIVU, runDIVUI

  -- 2*
  , runADD, runADDI, runADDU, runADDUI
  , runSUB, runSUBI, runSUBU, runSUBUI
  , runIIADDU, runIIADDUI, runIVADDU, runIVADDUI
  , runVIIIADDU, runVIIIADDUI, runXVIADDU, runXVIADDUI

  -- 3*
  , runCMP, runCMPI, runCMPU, runCMPUI
  , runNEG, runNEGI, runNEGU, runNEGUI
  , runSL, runSLI, runSLU, runSLUI
  , runSR, runSRI, runSRU, runSRUI

  -- 4*
  , runBN, runBNB, runBZ, runBZB
  , runBP, runBPB, runBOD, runBODB
  , runBNN, runBNNB, runBNZ, runBNZB
  , runBNP, runBNPB, runBEV, runBEVB

  -- 5*
  , runPBN, runPBNB, runPBZ, runPBZB
  , runPBP, runPBPB, runPBOD, runPBODB
  , runPBNN, runPBNNB, runPBNZ, runPBNZB
  , runPBNP, runPBNPB, runPBEV, runPBEVB

  -- 6*
  , runCSN, runCSNI, runCSZ, runCSZI
  , runCSP, runCSPI, runCSOD, runCSODI
  , runCSNN, runCSNNI, runCSNZ, runCSNZI
  , runCSNP, runCSNPI, runCSEV, runCSEVI

  -- 7*
  , runZSN, runZSNI, runZSZ, runZSZI
  , runZSP, runZSPI, runZSOD, runZSODI
  , runZSNN, runZSNNI, runZSNZ, runZSNZI
  , runZSNP, runZSNPI, runZSEV, runZSEVI

  -- 8*
  , runLDB, runLDBI, runLDBU, runLDBUI
  , runLDW, runLDWI, runLDWU, runLDWUI
  , runLDT, runLDTI, runLDTU, runLDTUI
  , runLDO, runLDOI, runLDOU, runLDOUI

  -- 9*
  , runLDSF, runLDSFI, runLDHT, runLDHTI
  , runCSWAP, runCSWAPI, runLDUNC, runLDUNCI
  , runLDVTS, runLDVTSI, runPRELD, runPRELDI
  , runPREGO, runPREGOI, runGO, runGOI

  -- A*
  , runSTB, runSTBI, runSTBU, runSTBUI
  , runSTW, runSTWI, runSTWU, runSTWUI
  , runSTT, runSTTI, runSTTU, runSTTUI
  , runSTO, runSTOI, runSTOU, runSTOUI

  -- B*
  , runSTSF, runSTSFI, runSTHT, runSTHTI
  , runSTCO, runSTCOI, runSTUNC, runSTUNCI
  , runSYNCD, runSYNCDI, runPREST, runPRESTI
  , runSYNCID, runSYNCIDI, runPUSHGO, runPUSHGOI

  -- C*
  , runOR, runORI, runORN, runORNI
  , runNOR, runNORI, runXOR, runXORI
  , runAND, runANDI, runANDN, runANDNI
  , runNAND, runNANDI, runNXOR, runNXORI

  -- D*
  , runBDIF, runBDIFI, runWDIF, runWDIFI
  , runTDIF, runTDIFI, runODIF, runODIFI
  , runMUX, runMUXI, runSADD, runSADDI
  , runMOR, runMORI, runMXOR, runMXORI

  -- E*
  , runSETH, runSETMH, runSETML, runSETL
  , runINCH, runINCMH, runINCML, runINCL
  , runORH, runORMH, runORML, runORL
  , runANDNH, runANDNMH, runANDNML, runANDNL

  -- F*
  , runJMP, runJMPB, runPUSHJ, runPUSHJB
  , runGETA, runGETAB, runPUT, runPUTI
  , runPOP, runRESUME, runSAVE, runUNSAVE
  , runSYNC, runSWYM, runGET, runTRIP
  ]
-- }}}

insnOpMap :: Array Byte RunInsn
insnOpMap = array (0,255) $ zip [0..255] runInsnList 

-- }}}

-- 0* trap, floating +-/convert {{{

-- TRAP 'trap' {{{
runTRAP :: RunInsn
runTRAP mmix insn = do
  issueTrap mmix
  incPC mmix
  -- npc <- (+ 4) <$> mmixGetPC mmix
  -- doFTrap mmix insn npc
-- }}}

-- FCMP 'floating compare' {{{
runFCMP :: RunInsn
runFCMP mmix insn = do
  (y,z) <- mmixGetGRFYZ mmix insn
  let re = fromOrdering <$> fpCompare y z
  mapM (issueATrip mmix) $ arithGetEx re
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- FUN 'floating unordered' {{{
runFUN :: RunInsn
runFUN mmix insn = do
  (y,z) <- mmixGetGRFYZ mmix insn
  if fpUnordered y z
  then mmixSetGRX mmix insn 1 -- unordered
  else mmixSetGRX mmix insn 0 -- ordered
  incPC mmix
-- }}}

-- FEQL 'floating equal to' {{{
runFEQL :: RunInsn
runFEQL mmix insn = do
  (y,z) <- mmixGetGRFYZ mmix insn
  mmixSetGRX mmix insn $ fromBool $ fpEqual y z
  incPC mmix
-- }}}
  
-- FADD 'floating add' {{{
runFADD :: RunInsn
runFADD mmix insn = do
  rdm <- toRoundModeRaw <$> mmixGetSR mmix rAIx
  (y,z) <- mmixGetGRFYZ mmix insn
  let re = fpAdd rdm y z
  mapM (issueATrip mmix) $ arithGetEx re
  mmixSetGRFX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- FIX 'convert floating to fixed' {{{
runFIX :: RunInsn
runFIX mmix insn = do
  let y = iGetYu insn
  if y > 4
  then issueDTrapPb mmix
  else do
    rdm0 <- toRoundModeRaw <$> mmixGetSR mmix rAIx
    let rdm = fromMaybe rdm0 $ toRoundModeImm y
    z <- mmixGetGRFZ mmix insn
    let re = fpFix rdm z
    mapM (issueATrip mmix) $ arithGetEx re
    mmixSetGRX mmix insn $ arithGetRx re
    incPC mmix
-- }}}
  
-- FSUB 'floating subtract' {{{
runFSUB :: RunInsn
runFSUB mmix insn = do
  rdm <- toRoundModeRaw <$> mmixGetSR mmix rAIx
  (y,z) <- mmixGetGRFYZ mmix insn
  let re = fpSub rdm y z
  mapM (issueATrip mmix) $ arithGetEx re
  mmixSetGRFX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- FIXU 'convert floating to fixed unsigned' {{{
runFIXU :: RunInsn
runFIXU mmix insn = do
  let y = iGetYu insn
  if y > 4
  then issueDTrapPb mmix
  else do
    rdm0 <- toRoundModeRaw <$> mmixGetSR mmix rAIx
    let rdm = fromMaybe rdm0 $ toRoundModeImm y
    z <- mmixGetGRFZ mmix insn
    let re = fpFixu rdm z
    mapM (issueATrip mmix) $ arithGetEx re
    mmixSetGRX mmix insn $ arithGetRx re
    incPC mmix
-- }}}

-- FLOT 'convert fixed to floating' {{{
runFLOT :: RunInsn
runFLOT mmix insn = do
  let y = iGetYu insn
  if y > 4
  then issueDTrapPb mmix
  else do
    rdm0 <- toRoundModeRaw <$> mmixGetSR mmix rAIx
    let rdm = fromMaybe rdm0 $ toRoundModeImm y
    z <- mmixGetGRZ mmix insn
    let re = fpFloat rdm z
    mapM (issueATrip mmix) $ arithGetEx re
    mmixSetGRFX mmix insn $ arithGetRx re
    incPC mmix
-- }}}

-- FLOTI 'convert fixed to floating immediate' {{{
runFLOTI :: RunInsn
runFLOTI mmix insn = do
  let y = iGetYu insn
  if y > 4
  then issueDTrapPb mmix
  else do
    rdm0 <- toRoundModeRaw <$> mmixGetSR mmix rAIx
    let rdm = fromMaybe rdm0 $ toRoundModeImm y
    let z = iGetZs insn
    let re = fpFloat rdm z
    mapM (issueATrip mmix) $ arithGetEx re
    mmixSetGRFX mmix insn $ arithGetRx re
    incPC mmix
-- }}}

-- FLOTU 'convert fixed to floating unsigned' {{{
runFLOTU :: RunInsn
runFLOTU mmix insn = do
  let y = iGetYu insn
  if y > 4
  then issueDTrapPb mmix
  else do
    rdm0 <- toRoundModeRaw <$> mmixGetSR mmix rAIx
    let rdm = fromMaybe rdm0 $ toRoundModeImm y
    z <- mmixGetGRZ mmix insn
    let re = fpFloatu rdm z
    mapM (issueATrip mmix) $ arithGetEx re
    mmixSetGRFX mmix insn $ arithGetRx re
    incPC mmix
-- }}}

-- FLOTUI 'convert fixed to floating unsigned immediate' {{{
runFLOTUI :: RunInsn
runFLOTUI mmix insn = do
  let y = iGetYu insn
  if y > 4
  then issueDTrapPb mmix
  else do
    rdm0 <- toRoundModeRaw <$> mmixGetSR mmix rAIx
    let rdm = fromMaybe rdm0 $ toRoundModeImm y
    let z = iGetZu insn
    let re = fpFloatu rdm z
    mapM (issueATrip mmix) $ arithGetEx re
    mmixSetGRFX mmix insn $ arithGetRx re
    incPC mmix
-- }}}

-- SFLOT 'convert fixed to short float' {{{
runSFLOT :: RunInsn
runSFLOT mmix insn = do
  let y = iGetYu insn
  if y > 4
  then issueDTrapPb mmix
  else do
    rdm0 <- toRoundModeRaw <$> mmixGetSR mmix rAIx
    let rdm = fromMaybe rdm0 $ toRoundModeImm y
    z <- mmixGetGRZ mmix insn
    let re = fpSFloat rdm z
    mapM (issueATrip mmix) $ arithGetEx re
    mmixSetGRFX mmix insn $ arithGetRx re
    incPC mmix
-- }}}

-- SFLOTI 'convert fixed to short float immediate' {{{
runSFLOTI :: RunInsn
runSFLOTI mmix insn = do
  let y = iGetYu insn
  if y > 4
  then issueDTrapPb mmix
  else do
    rdm0 <- toRoundModeRaw <$> mmixGetSR mmix rAIx
    let rdm = fromMaybe rdm0 $ toRoundModeImm y
    let z = iGetZs insn
    let re = fpSFloat rdm z
    mapM (issueATrip mmix) $ arithGetEx re
    mmixSetGRFX mmix insn $ arithGetRx re
    incPC mmix
-- }}}

-- SFLOTU 'convert fixed to short float unsigned ' {{{
runSFLOTU :: RunInsn
runSFLOTU mmix insn = do
  let y = iGetYu insn
  if y > 4
  then issueDTrapPb mmix
  else do
    rdm0 <- toRoundModeRaw <$> mmixGetSR mmix rAIx
    let rdm = fromMaybe rdm0 $ toRoundModeImm y
    z <- mmixGetGRZ mmix insn
    let re = fpSFloatu rdm z
    mapM (issueATrip mmix) $ arithGetEx re
    mmixSetGRFX mmix insn $ arithGetRx re
    incPC mmix
-- }}}

-- SFLOTUI 'convert fixed to short float unsigned immediate' {{{
runSFLOTUI :: RunInsn
runSFLOTUI mmix insn = do
  let y = iGetYu insn
  if y > 4
  then issueDTrapPb mmix
  else do
    rdm0 <- toRoundModeRaw <$> mmixGetSR mmix rAIx
    let rdm = fromMaybe rdm0 $ toRoundModeImm y
    let z = iGetZu insn
    let re = fpSFloatu rdm z
    mapM (issueATrip mmix) $ arithGetEx re
    mmixSetGRFX mmix insn $ arithGetRx re
    incPC mmix
-- }}}

-- }}}

-- 1* floating, integer mul/div {{{

-- FMUL 'floating multiply' {{{
runFMUL :: RunInsn
runFMUL mmix insn = do
  rdm <- toRoundModeRaw <$> mmixGetSR mmix rAIx
  (y,z) <- mmixGetGRFYZ mmix insn
  let re = fpMult rdm y z
  mapM (issueATrip mmix) $ arithGetEx re
  mmixSetGRFX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- FCMPE 'floating compare (with respect to epsilon)' {{{
runFCMPE :: RunInsn
runFCMPE mmix insn = do
  (y,z) <- mmixGetGRFYZ mmix insn
  e <- mmixGetSRF mmix rEIx
  let re = fromOrdering <$> fpCompareEps e y z
  mapM (issueATrip mmix) $ arithGetEx re
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- FUNE 'floating unordered (with respect to epsilon)' {{{
runFUNE :: RunInsn
runFUNE mmix insn = do
  (y,z) <- mmixGetGRFYZ mmix insn
  e <- mmixGetSRF mmix rEIx
  if fpUnorderedEps e y z
  then mmixSetGRX mmix insn 1 -- unordered
  else mmixSetGRX mmix insn 0 -- ordered
  incPC mmix
-- }}}

-- FEQLE 'floating equivalent (with respect to epsilon)' {{{
runFEQLE :: RunInsn
runFEQLE mmix insn = do
  (y,z) <- mmixGetGRFYZ mmix insn
  e <- mmixGetSRF mmix rEIx
  let re = fromBool <$> fpEqualEps e y z
  mapM (issueATrip mmix) $ arithGetEx re
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- FDIV 'floating divide' {{{
runFDIV :: RunInsn
runFDIV mmix insn = do
  rdm <- toRoundModeRaw <$> mmixGetSR mmix rAIx
  (y,z) <- mmixGetGRFYZ mmix insn
  let re = fpDivide rdm y z
  mapM (issueATrip mmix) $ arithGetEx re
  mmixSetGRFX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- FSQRT 'floating square root' {{{
runFSQRT :: RunInsn
runFSQRT mmix insn = do
  let y = iGetYu insn
  if y > 4
  then issueDTrapPb mmix
  else do
    rdm0 <- toRoundModeRaw <$> mmixGetSR mmix rAIx
    let rdm = fromMaybe rdm0 $ toRoundModeImm y
    z <- mmixGetGRFZ mmix insn
    let re = fpSqrt rdm z
    mapM (issueATrip mmix) $ arithGetEx re
    mmixSetGRFX mmix insn $ arithGetRx re
    incPC mmix
-- }}}

-- FREM 'floating remainder' {{{
runFREM :: RunInsn
runFREM mmix insn = do
  (y,z) <- mmixGetGRFYZ mmix insn
  let re = fpRem y z
  mapM (issueATrip mmix) $ arithGetEx re
  mmixSetGRFX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- FINT 'floating integer' {{{
runFINT :: RunInsn
runFINT mmix insn = do
  let y = iGetYu insn
  if y > 4
  then issueDTrapPb mmix
  else do
    rdm0 <- toRoundModeRaw <$> mmixGetSR mmix rAIx
    let rdm = fromMaybe rdm0 $ toRoundModeImm y
    z <- mmixGetGRFZ mmix insn
    let re = fpInt rdm z
    mapM (issueATrip mmix) $ arithGetEx re
    mmixSetGRFX mmix insn $ arithGetRx re
    incPC mmix
-- }}}

-- MUL 'multiply' {{{
runMUL :: RunInsn
runMUL mmix insn = do
  (y,z) <- mmixGetGRYZ mmix insn
  let re = intMul y z
  mapM (issueATrip mmix) $ arithGetEx re
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- MULI 'multiply immediate' {{{
runMULI :: RunInsn
runMULI mmix insn = do
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  let re = intMul y z
  mapM (issueATrip mmix) $ arithGetEx re
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- MULU 'multiply unsigned' {{{
runMULU :: RunInsn
runMULU mmix insn = do
  (y,z) <- mmixGetGRYZ mmix insn
  let re = intMulu y z
  --mapM (issueATrip mmix) $ arithGetEx re
  let (HD h l) = arithGetRx re
  mmixSetGRX mmix insn l
  mmixSetSR mmix rHIx h
  incPC mmix
-- }}}

-- MULUI 'multiply unsigned immediate' {{{
runMULUI :: RunInsn
runMULUI mmix insn = do
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  let re = intMulu y z
  --mapM (issueATrip mmix) $ arithGetEx re
  let (HD h l) = arithGetRx re
  mmixSetGRX mmix insn l
  mmixSetSR mmix rHIx h
  incPC mmix
-- }}}

-- DIV 'divide' {{{
runDIV :: RunInsn
runDIV mmix insn = do
  (y,z) <- mmixGetGRYZ mmix insn
  let re = intDivide y z
  mapM (issueATrip mmix) $ arithGetEx re
  let (q,r) = arithGetRx re
  mmixSetGRX mmix insn q
  mmixSetSR mmix rRIx r
  incPC mmix
-- }}}

-- DIVI 'divide immediate' {{{
runDIVI :: RunInsn
runDIVI mmix insn = do
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  let re = intDivide y z
  mapM (issueATrip mmix) $ arithGetEx re
  let (q,r) = arithGetRx re
  mmixSetGRX mmix insn q
  mmixSetSR mmix rRIx r
  incPC mmix
-- }}}

-- DIVU 'divide unsigned' {{{
runDIVU :: RunInsn
runDIVU mmix insn = do
  (y,z) <- mmixGetGRYZ mmix insn
  d <- mmixGetSR mmix rDIx
  let re = intDivideu (HD d y) z
  --mapM (issueATrip mmix) $ arithGetEx re
  let (q,r) = arithGetRx re
  mmixSetGRX mmix insn q
  mmixSetSR mmix rRIx r
  incPC mmix
-- }}}

-- DIVUI 'divide unsigned immediate' {{{
runDIVUI :: RunInsn
runDIVUI mmix insn = do
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  d <- mmixGetSR mmix rDIx
  let re = intDivideu (HD d y) z
  --mapM (issueATrip mmix) $ arithGetEx re
  let (q,r) = arithGetRx re
  mmixSetGRX mmix insn q
  mmixSetSR mmix rRIx r
  incPC mmix
-- }}}

-- }}}

-- 2* integer add/sub {{{

-- ADD {{{
runADD :: RunInsn
runADD mmix insn = do
  (y,z) <- mmixGetGRYZ mmix insn
  let re = intAdd y z
  mapM (issueATrip mmix) $ arithGetEx re
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- ADDI {{{
runADDI :: RunInsn
runADDI mmix insn = do
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  let re = intAdd y z
  mapM (issueATrip mmix) $ arithGetEx re
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- ADDU {{{
runADDU :: RunInsn
runADDU mmix insn = do
  (y,z) <- mmixGetGRYZ mmix insn
  let re = intAddu y z
  --mapM (issueATrip mmix) $ arithGetEx re
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- ADDUI {{{
runADDUI :: RunInsn
runADDUI mmix insn = do
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  let re = intAddu y z
  --mapM (issueATrip mmix) $ arithGetEx re
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- SUB {{{
runSUB :: RunInsn
runSUB mmix insn = do
  (y,z) <- mmixGetGRYZ mmix insn
  let re = intSub y z
  mapM (issueATrip mmix) $ arithGetEx re
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- SUBI {{{
runSUBI :: RunInsn
runSUBI mmix insn = do
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  let re = intSub y z
  mapM (issueATrip mmix) $ arithGetEx re
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- SUBU {{{
runSUBU :: RunInsn
runSUBU mmix insn = do
  (y,z) <- mmixGetGRYZ mmix insn
  let re = intSubu y z
  --mapM (issueATrip mmix) $ arithGetEx re
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- SUBUI {{{
runSUBUI :: RunInsn
runSUBUI mmix insn = do
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  let re = intSubu y z
  --mapM (issueATrip mmix) $ arithGetEx re
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- IIADDU {{{
runIIADDU :: RunInsn
runIIADDU mmix insn = do
  (y,z) <- mmixGetGRYZ mmix insn
  let re = int2Addu y z
  --mapM (issueATrip mmix) $ arithGetEx re
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- IIADDUI {{{
runIIADDUI :: RunInsn
runIIADDUI mmix insn = do
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  let re = int2Addu y z
  --mapM (issueATrip mmix) $ arithGetEx re
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- IVADDU {{{
runIVADDU :: RunInsn
runIVADDU mmix insn = do
  (y,z) <- mmixGetGRYZ mmix insn
  let re = int4Addu y z
  --mapM (issueATrip mmix) $ arithGetEx re
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- IVADDUI {{{
runIVADDUI :: RunInsn
runIVADDUI mmix insn = do
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  let re = int4Addu y z
  --mapM (issueATrip mmix) $ arithGetEx re
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- VIIIADDU {{{
runVIIIADDU :: RunInsn
runVIIIADDU mmix insn = do
  (y,z) <- mmixGetGRYZ mmix insn
  let re = int8Addu y z
  --mapM (issueATrip mmix) $ arithGetEx re
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- VIIIADDUI {{{
runVIIIADDUI :: RunInsn
runVIIIADDUI mmix insn = do
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  let re = int8Addu y z
  --mapM (issueATrip mmix) $ arithGetEx re
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- XVIADDU {{{
runXVIADDU :: RunInsn
runXVIADDU mmix insn = do
  (y,z) <- mmixGetGRYZ mmix insn
  let re = int16Addu y z
  --mapM (issueATrip mmix) $ arithGetEx re
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- XVIADDUI {{{
runXVIADDUI :: RunInsn
runXVIADDUI mmix insn = do
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  let re = int16Addu y z
  --mapM (issueATrip mmix) $ arithGetEx re
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- }}}

-- 3* integer compare/neg/shift {{{

-- CMP {{{
runCMP :: RunInsn
runCMP mmix insn = do
  (y,z) <- mmixGetGRYZ mmix insn
  let re = fromOrdering <$> intCompare y z
  --mapM (issueATrip mmix) $ arithGetEx re
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- CMPI {{{
runCMPI :: RunInsn
runCMPI mmix insn = do
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  let re = fromOrdering <$> intCompare y z
  --mapM (issueATrip mmix) $ arithGetEx re
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- CMPU {{{
runCMPU :: RunInsn
runCMPU mmix insn = do
  (y,z) <- mmixGetGRYZ mmix insn
  let re = fromOrdering <$> intCompareu y z
  --mapM (issueATrip mmix) $ arithGetEx re
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- CMPUI {{{
runCMPUI :: RunInsn
runCMPUI mmix insn = do
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  let re = fromOrdering <$> intCompareu y z
  --mapM (issueATrip mmix) $ arithGetEx re
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- NEG {{{
runNEG :: RunInsn
runNEG mmix insn = do
  let y = iGetYu insn
  z <- mmixGetGRZ mmix insn
  let re = intSub y z
  mapM (issueATrip mmix) $ arithGetEx re
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- NEGI {{{
runNEGI :: RunInsn
runNEGI mmix insn = do
  let y = iGetYu insn
  let z = iGetZu insn
  let re = intSub y z
  mapM (issueATrip mmix) $ arithGetEx re
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- NEGU {{{
runNEGU :: RunInsn
runNEGU mmix insn = do
  let y = iGetYu insn
  z <- mmixGetGRZ mmix insn
  let re = intSubu y z
  --mapM (issueATrip mmix) $ arithGetEx re
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- NEGUI {{{
runNEGUI :: RunInsn
runNEGUI mmix insn = do
  let y = iGetYu insn
  let z = iGetZu insn
  let re = intSubu y z
  --mapM (issueATrip mmix) $ arithGetEx re
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- SL {{{
runSL :: RunInsn
runSL mmix insn = do
  (y,z) <- mmixGetGRYZ mmix insn
  let re = bitSL y z
  mapM (issueATrip mmix) $ arithGetEx re
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- SLI {{{
runSLI :: RunInsn
runSLI mmix insn = do
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  let re = bitSL y z
  mapM (issueATrip mmix) $ arithGetEx re
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- SLU {{{
runSLU :: RunInsn
runSLU mmix insn = do
  (y,z) <- mmixGetGRYZ mmix insn
  let re = bitSLu y z
  --mapM (issueATrip mmix) $ arithGetEx re
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- SLUI {{{
runSLUI :: RunInsn
runSLUI mmix insn = do
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  let re = bitSLu y z
  --mapM (issueATrip mmix) $ arithGetEx re
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- SR {{{
runSR :: RunInsn
runSR mmix insn = do
  (y,z) <- mmixGetGRYZ mmix insn
  let re = bitSR y z
  --mapM (issueATrip mmix) $ arithGetEx re
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- SRI {{{
runSRI :: RunInsn
runSRI mmix insn = do
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  let re = bitSR y z
  --mapM (issueATrip mmix) $ arithGetEx re
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- SRU {{{
runSRU :: RunInsn
runSRU mmix insn = do
  (y,z) <- mmixGetGRYZ mmix insn
  let re = bitSRu y z
  --mapM (issueATrip mmix) $ arithGetEx re
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- SRUI {{{
runSRUI :: RunInsn
runSRUI mmix insn = do
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  let re = bitSRu y z
  --mapM (issueATrip mmix) $ arithGetEx re
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- }}}

-- 4* branch {{{

-- BN {{{
runBN :: RunInsn
runBN mmix insn = do
  x <- mmixGetGRX mmix insn
  if testN x
  then runBranchForward mmix insn
  else incPC mmix
-- }}}

-- BNB {{{
runBNB :: RunInsn
runBNB mmix insn = do
  x <- mmixGetGRX mmix insn
  if testN x
  then runBranchBackward mmix insn
  else incPC mmix
-- }}}

-- BZ {{{
runBZ :: RunInsn
runBZ mmix insn = do
  x <- mmixGetGRX mmix insn
  if testZ x
  then runBranchForward mmix insn
  else incPC mmix
-- }}}

-- BZB {{{
runBZB :: RunInsn
runBZB mmix insn = do
  x <- mmixGetGRX mmix insn
  if testZ x
  then runBranchBackward mmix insn
  else incPC mmix
-- }}}

-- BP {{{
runBP :: RunInsn
runBP mmix insn = do
  x <- mmixGetGRX mmix insn
  if testP x
  then runBranchForward mmix insn
  else incPC mmix
-- }}}

-- BPB {{{
runBPB :: RunInsn
runBPB mmix insn = do
  x <- mmixGetGRX mmix insn
  if testP x
  then runBranchBackward mmix insn
  else incPC mmix
-- }}}

-- BOD {{{
runBOD :: RunInsn
runBOD mmix insn = do
  x <- mmixGetGRX mmix insn
  if testODD x
  then runBranchForward mmix insn
  else incPC mmix
-- }}}

-- BODB {{{
runBODB :: RunInsn
runBODB mmix insn = do
  x <- mmixGetGRX mmix insn
  if testODD x
  then runBranchBackward mmix insn
  else incPC mmix
-- }}}

-- BNN {{{
runBNN :: RunInsn
runBNN mmix insn = do
  x <- mmixGetGRX mmix insn
  if testNN x
  then runBranchForward mmix insn
  else incPC mmix
-- }}}

-- BNNB {{{
runBNNB :: RunInsn
runBNNB mmix insn = do
  x <- mmixGetGRX mmix insn
  if testNN x
  then runBranchBackward mmix insn
  else incPC mmix
-- }}}

-- BNZ {{{
runBNZ :: RunInsn
runBNZ mmix insn = do
  x <- mmixGetGRX mmix insn
  if testNZ x
  then runBranchForward mmix insn
  else incPC mmix
-- }}}

-- BNZB {{{
runBNZB :: RunInsn
runBNZB mmix insn = do
  x <- mmixGetGRX mmix insn
  if testNZ x
  then runBranchBackward mmix insn
  else incPC mmix
-- }}}

-- BNP {{{
runBNP :: RunInsn
runBNP mmix insn = do
  x <- mmixGetGRX mmix insn
  if testNP x
  then runBranchForward mmix insn
  else incPC mmix
-- }}}

-- BNPB {{{
runBNPB :: RunInsn
runBNPB mmix insn = do
  x <- mmixGetGRX mmix insn
  if testNP x
  then runBranchBackward mmix insn
  else incPC mmix
-- }}}

-- BEV {{{
runBEV :: RunInsn
runBEV mmix insn = do
  x <- mmixGetGRX mmix insn
  if testEVEN x
  then runBranchForward mmix insn
  else incPC mmix
-- }}}

-- BEVB {{{
runBEVB :: RunInsn
runBEVB mmix insn = do
  x <- mmixGetGRX mmix insn
  if testEVEN x
  then runBranchBackward mmix insn
  else incPC mmix
-- }}}

-- }}}

-- 5* probable branch {{{

-- PBN {{{
runPBN :: RunInsn
runPBN mmix insn = do
  x <- mmixGetGRX mmix insn
  if testN x
  then runBranchForward mmix insn
  else incPC mmix
-- }}}

-- PBNB {{{
runPBNB :: RunInsn
runPBNB mmix insn = do
  x <- mmixGetGRX mmix insn
  if testN x
  then runBranchBackward mmix insn
  else incPC mmix
-- }}}

-- PBZ {{{
runPBZ :: RunInsn
runPBZ mmix insn = do
  x <- mmixGetGRX mmix insn
  if testZ x
  then runBranchForward mmix insn
  else incPC mmix
-- }}}

-- PBZB {{{
runPBZB :: RunInsn
runPBZB mmix insn = do
  x <- mmixGetGRX mmix insn
  if testZ x
  then runBranchBackward mmix insn
  else incPC mmix
-- }}}

-- PBP {{{
runPBP :: RunInsn
runPBP mmix insn = do
  x <- mmixGetGRX mmix insn
  if testP x
  then runBranchForward mmix insn
  else incPC mmix
-- }}}

-- PBPB {{{
runPBPB :: RunInsn
runPBPB mmix insn = do
  x <- mmixGetGRX mmix insn
  if testP x
  then runBranchBackward mmix insn
  else incPC mmix
-- }}}

-- PBOD {{{
runPBOD :: RunInsn
runPBOD mmix insn = do
  x <- mmixGetGRX mmix insn
  if testODD x
  then runBranchForward mmix insn
  else incPC mmix
-- }}}

-- PBODB {{{
runPBODB :: RunInsn
runPBODB mmix insn = do
  x <- mmixGetGRX mmix insn
  if testODD x
  then runBranchBackward mmix insn
  else incPC mmix
-- }}}

-- PBNN {{{
runPBNN :: RunInsn
runPBNN mmix insn = do
  x <- mmixGetGRX mmix insn
  if testNN x
  then runBranchForward mmix insn
  else incPC mmix
-- }}}

-- PBNNB {{{
runPBNNB :: RunInsn
runPBNNB mmix insn = do
  x <- mmixGetGRX mmix insn
  if testNN x
  then runBranchBackward mmix insn
  else incPC mmix
-- }}}

-- PBNZ {{{
runPBNZ :: RunInsn
runPBNZ mmix insn = do
  x <- mmixGetGRX mmix insn
  if testNZ x
  then runBranchForward mmix insn
  else incPC mmix
-- }}}

-- PBNZB {{{
runPBNZB :: RunInsn
runPBNZB mmix insn = do
  x <- mmixGetGRX mmix insn
  if testNZ x
  then runBranchBackward mmix insn
  else incPC mmix
-- }}}

-- PBNP {{{
runPBNP :: RunInsn
runPBNP mmix insn = do
  x <- mmixGetGRX mmix insn
  if testNP x
  then runBranchForward mmix insn
  else incPC mmix
-- }}}

-- PBNPB {{{
runPBNPB :: RunInsn
runPBNPB mmix insn = do
  x <- mmixGetGRX mmix insn
  if testNP x
  then runBranchBackward mmix insn
  else incPC mmix
-- }}}

-- PBEV {{{
runPBEV :: RunInsn
runPBEV mmix insn = do
  x <- mmixGetGRX mmix insn
  if testEVEN x
  then runBranchForward mmix insn
  else incPC mmix
-- }}}

-- PBEVB {{{
runPBEVB :: RunInsn
runPBEVB mmix insn = do
  x <- mmixGetGRX mmix insn
  if testEVEN x
  then runBranchBackward mmix insn
  else incPC mmix
-- }}}

-- }}}

-- 6* conditional set {{{

-- CSN {{{
runCSN :: RunInsn
runCSN mmix insn = do
  (y,z) <- mmixGetGRYZ mmix insn
  when (testN y) $ mmixSetGRX mmix insn z
  incPC mmix
-- }}}

-- CSNI {{{
runCSNI :: RunInsn
runCSNI mmix insn = do
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  when (testN y) $ mmixSetGRX mmix insn z
  incPC mmix
-- }}}

-- CSZ {{{
runCSZ :: RunInsn
runCSZ mmix insn = do
  (y,z) <- mmixGetGRYZ mmix insn
  when (testZ y) $ mmixSetGRX mmix insn z
  incPC mmix
-- }}}

-- CSZI {{{
runCSZI :: RunInsn
runCSZI mmix insn = do
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  when (testZ y) $ mmixSetGRX mmix insn z
  incPC mmix
-- }}}

-- CSP {{{
runCSP :: RunInsn
runCSP mmix insn = do
  (y,z) <- mmixGetGRYZ mmix insn
  when (testP y) $ mmixSetGRX mmix insn z
  incPC mmix
-- }}}

-- CSPI {{{
runCSPI :: RunInsn
runCSPI mmix insn = do
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  when (testP y) $ mmixSetGRX mmix insn z
  incPC mmix
-- }}}

-- CSOD {{{
runCSOD :: RunInsn
runCSOD mmix insn = do
  (y,z) <- mmixGetGRYZ mmix insn
  when (testODD y) $ mmixSetGRX mmix insn z
  incPC mmix
-- }}}

-- CSODI {{{
runCSODI :: RunInsn
runCSODI mmix insn = do
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  when (testODD y) $ mmixSetGRX mmix insn z
  incPC mmix
-- }}}

-- CSNN {{{
runCSNN :: RunInsn
runCSNN mmix insn = do
  (y,z) <- mmixGetGRYZ mmix insn
  when (testNN y) $ mmixSetGRX mmix insn z
  incPC mmix
-- }}}

-- CSNNI {{{
runCSNNI :: RunInsn
runCSNNI mmix insn = do
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  when (testNN y) $ mmixSetGRX mmix insn z
  incPC mmix
-- }}}

-- CSNZ {{{
runCSNZ :: RunInsn
runCSNZ mmix insn = do
  (y,z) <- mmixGetGRYZ mmix insn
  when (testNZ y) $ mmixSetGRX mmix insn z
  incPC mmix
-- }}}

-- CSNZI {{{
runCSNZI :: RunInsn
runCSNZI mmix insn = do
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  when (testNZ y) $ mmixSetGRX mmix insn z
  incPC mmix
-- }}}

-- CSNP {{{
runCSNP :: RunInsn
runCSNP mmix insn = do
  (y,z) <- mmixGetGRYZ mmix insn
  when (testNP y) $ mmixSetGRX mmix insn z
  incPC mmix
-- }}}

-- CSNPI {{{
runCSNPI :: RunInsn
runCSNPI mmix insn = do
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  when (testNP y) $ mmixSetGRX mmix insn z
  incPC mmix
-- }}}

-- CSEV {{{
runCSEV :: RunInsn
runCSEV mmix insn = do
  (y,z) <- mmixGetGRYZ mmix insn
  when (testEVEN y) $ mmixSetGRX mmix insn z
  incPC mmix
-- }}}

-- CSEVI {{{
runCSEVI :: RunInsn
runCSEVI mmix insn = do
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  when (testEVEN y) $ mmixSetGRX mmix insn z
  incPC mmix
-- }}}

-- }}}

-- 7* zero or set {{{

-- ZSN {{{
runZSN :: RunInsn
runZSN mmix insn = do
  (y,z) <- mmixGetGRYZ mmix insn
  mmixSetGRX mmix insn $ if testN y then z else 0
  incPC mmix
-- }}}

-- ZSNI {{{
runZSNI :: RunInsn
runZSNI mmix insn = do
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  mmixSetGRX mmix insn $ if testN y then z else 0
  incPC mmix
-- }}}

-- ZSZ {{{
runZSZ :: RunInsn
runZSZ mmix insn = do
  (y,z) <- mmixGetGRYZ mmix insn
  mmixSetGRX mmix insn $ if testN y then z else 0
  incPC mmix
-- }}}

-- ZSZI {{{
runZSZI :: RunInsn
runZSZI mmix insn = do
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  mmixSetGRX mmix insn $ if testN y then z else 0
  incPC mmix
-- }}}

-- ZSP {{{
runZSP :: RunInsn
runZSP mmix insn = do
  (y,z) <- mmixGetGRYZ mmix insn
  mmixSetGRX mmix insn $ if testP y then z else 0
  incPC mmix
-- }}}

-- ZSPI {{{
runZSPI :: RunInsn
runZSPI mmix insn = do
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  mmixSetGRX mmix insn $ if testP y then z else 0
  incPC mmix
-- }}}

-- ZSOD {{{
runZSOD :: RunInsn
runZSOD mmix insn = do
  (y,z) <- mmixGetGRYZ mmix insn
  mmixSetGRX mmix insn $ if testODD y then z else 0
  incPC mmix
-- }}}

-- ZSODI {{{
runZSODI :: RunInsn
runZSODI mmix insn = do
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  mmixSetGRX mmix insn $ if testODD y then z else 0
  incPC mmix
-- }}}

-- ZSNN {{{
runZSNN :: RunInsn
runZSNN mmix insn = do
  (y,z) <- mmixGetGRYZ mmix insn
  mmixSetGRX mmix insn $ if testNN y then z else 0
  incPC mmix
-- }}}

-- ZSNNI {{{
runZSNNI :: RunInsn
runZSNNI mmix insn = do
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  mmixSetGRX mmix insn $ if testNN y then z else 0
  incPC mmix
-- }}}

-- ZSNZ {{{
runZSNZ :: RunInsn
runZSNZ mmix insn = do
  (y,z) <- mmixGetGRYZ mmix insn
  mmixSetGRX mmix insn $ if testNZ y then z else 0
  incPC mmix
-- }}}

-- ZSNZI {{{
runZSNZI :: RunInsn
runZSNZI mmix insn = do
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  mmixSetGRX mmix insn $ if testNZ y then z else 0
  incPC mmix
-- }}}

-- ZSNP {{{
runZSNP :: RunInsn
runZSNP mmix insn = do
  (y,z) <- mmixGetGRYZ mmix insn
  mmixSetGRX mmix insn $ if testNP y then z else 0
  incPC mmix
-- }}}

-- ZSNPI {{{
runZSNPI :: RunInsn
runZSNPI mmix insn = do
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  mmixSetGRX mmix insn $ if testNP y then z else 0
  incPC mmix
-- }}}

-- ZSEV {{{
runZSEV :: RunInsn
runZSEV mmix insn = do
  (y,z) <- mmixGetGRYZ mmix insn
  mmixSetGRX mmix insn $ if testEVEN y then z else 0
  incPC mmix
-- }}}

-- ZSEVI {{{
runZSEVI :: RunInsn
runZSEVI mmix insn = do
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  mmixSetGRX mmix insn $ if testEVEN y then z else 0
  incPC mmix
-- }}}

-- }}}

-- 8* load {{{

-- LDB {{{
runLDB :: RunInsn
runLDB mmix insn = do
  let x = iGetX insn
  (y,z) <- mmixGetGRYZ mmix insn
  let vaddr = y + z
  checkVAddrRead mmix vaddr
  trapped <- checkTrapped mmix
  unless trapped $ mmixVLdByte mmix vaddr >>= mmixSetGR mmix x
  incPC mmix
-- }}}

-- LDBI {{{
runLDBI :: RunInsn
runLDBI mmix insn = do
  let x = iGetX insn
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  let vaddr = y + z
  checkVAddrRead mmix vaddr
  trapped <- checkTrapped mmix
  unless trapped $ mmixVLdByte mmix vaddr >>= mmixSetGR mmix x
  incPC mmix
-- }}}

-- LDBU {{{
runLDBU :: RunInsn
runLDBU mmix insn = do
  let x = iGetX insn
  (y,z) <- mmixGetGRYZ mmix insn
  let vaddr = y + z
  checkVAddrRead mmix vaddr
  trapped <- checkTrapped mmix
  unless trapped $ mmixVLdByteu mmix vaddr >>= mmixSetGR mmix x
  incPC mmix
-- }}}

-- LDBUI {{{
runLDBUI :: RunInsn
runLDBUI mmix insn = do
  let x = iGetX insn
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  let vaddr = y + z
  checkVAddrRead mmix vaddr
  trapped <- checkTrapped mmix
  unless trapped $ mmixVLdByteu mmix vaddr >>= mmixSetGR mmix x
  incPC mmix
-- }}}

-- LDW {{{
runLDW :: RunInsn
runLDW mmix insn = do
  let x = iGetX insn
  (y,z) <- mmixGetGRYZ mmix insn
  let vaddr = y + z
  checkVAddrRead mmix vaddr
  trapped <- checkTrapped mmix
  unless trapped $ mmixVLdWyde mmix vaddr >>= mmixSetGR mmix x
  incPC mmix
-- }}}

-- LDWI {{{
runLDWI :: RunInsn
runLDWI mmix insn = do
  let x = iGetX insn
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  let vaddr = y + z
  checkVAddrRead mmix vaddr
  trapped <- checkTrapped mmix
  unless trapped $ mmixVLdWyde mmix vaddr >>= mmixSetGR mmix x
  incPC mmix
-- }}}

-- LDWU {{{
runLDWU :: RunInsn
runLDWU mmix insn = do
  let x = iGetX insn
  (y,z) <- mmixGetGRYZ mmix insn
  let vaddr = y + z
  checkVAddrRead mmix vaddr
  trapped <- checkTrapped mmix
  unless trapped $ mmixVLdWydeu mmix vaddr >>= mmixSetGR mmix x
  incPC mmix
-- }}}

-- LDWUI {{{
runLDWUI :: RunInsn
runLDWUI mmix insn = do
  let x = iGetX insn
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  let vaddr = y + z
  checkVAddrRead mmix vaddr
  trapped <- checkTrapped mmix
  unless trapped $ mmixVLdWydeu mmix vaddr >>= mmixSetGR mmix x
  incPC mmix
-- }}}

-- LDT {{{
runLDT :: RunInsn
runLDT mmix insn = do
  let x = iGetX insn
  (y,z) <- mmixGetGRYZ mmix insn
  let vaddr = y + z
  checkVAddrRead mmix vaddr
  trapped <- checkTrapped mmix
  unless trapped $ mmixVLdTetra mmix vaddr >>= mmixSetGR mmix x
  incPC mmix
-- }}}

-- LDTI {{{
runLDTI :: RunInsn
runLDTI mmix insn = do
  let x = iGetX insn
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  let vaddr = y + z
  checkVAddrRead mmix vaddr
  trapped <- checkTrapped mmix
  unless trapped $ mmixVLdTetra mmix vaddr >>= mmixSetGR mmix x
  incPC mmix
-- }}}

-- LDTU {{{
runLDTU :: RunInsn
runLDTU mmix insn = do
  let x = iGetX insn
  (y,z) <- mmixGetGRYZ mmix insn
  let vaddr = y + z
  checkVAddrRead mmix vaddr
  trapped <- checkTrapped mmix
  unless trapped $ mmixVLdTetrau mmix vaddr >>= mmixSetGR mmix x
  incPC mmix
-- }}}

-- LDTUI {{{
runLDTUI :: RunInsn
runLDTUI mmix insn = do
  let x = iGetX insn
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  let vaddr = y + z
  checkVAddrRead mmix vaddr
  trapped <- checkTrapped mmix
  unless trapped $ mmixVLdTetrau mmix vaddr >>= mmixSetGR mmix x
  incPC mmix
-- }}}

-- LDO {{{
runLDO :: RunInsn
runLDO mmix insn = do
  let x = iGetX insn
  (y,z) <- mmixGetGRYZ mmix insn
  let vaddr = y + z
  checkVAddrRead mmix vaddr
  trapped <- checkTrapped mmix
  unless trapped $ mmixVLdOcta mmix vaddr >>= mmixSetGR mmix x
  incPC mmix
-- }}}

-- LDOI {{{
runLDOI :: RunInsn
runLDOI mmix insn = do
  let x = iGetX insn
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  let vaddr = y + z
  checkVAddrRead mmix vaddr
  trapped <- checkTrapped mmix
  unless trapped $ mmixVLdOcta mmix vaddr >>= mmixSetGR mmix x
  incPC mmix
-- }}}

-- LDOU {{{
runLDOU :: RunInsn
runLDOU mmix insn = do
  let x = iGetX insn
  (y,z) <- mmixGetGRYZ mmix insn
  let vaddr = y + z
  checkVAddrRead mmix vaddr
  trapped <- checkTrapped mmix
  unless trapped $ mmixVLdOcta mmix vaddr >>= mmixSetGR mmix x
  incPC mmix
-- }}}

-- LDOUI {{{
runLDOUI :: RunInsn
runLDOUI mmix insn = do
  let x = iGetX insn
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  let vaddr = y + z
  checkVAddrRead mmix vaddr
  trapped <- checkTrapped mmix
  unless trapped $ mmixVLdOcta mmix vaddr >>= mmixSetGR mmix x
  incPC mmix
-- }}}

-- }}}

-- 9* special Ld/St; cache; TC; GO {{{

-- LDSF 'load short float' {{{
runLDSF :: RunInsn
runLDSF mmix insn = do
  let x = iGetX insn
  (y,z) <- mmixGetGRYZ mmix insn
  let vaddr = y + z
  checkVAddrRead mmix vaddr
  trapped <- checkTrapped mmix
  unless trapped $ mmixVLdSF mmix vaddr >>= mmixSetGR mmix x
  incPC mmix
-- }}}

-- LDSFI 'load short float immediate' {{{
runLDSFI :: RunInsn
runLDSFI mmix insn = do
  let x = iGetX insn
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  let vaddr = y + z
  checkVAddrRead mmix vaddr
  trapped <- checkTrapped mmix
  unless trapped $ mmixVLdSF mmix vaddr >>= mmixSetGR mmix x
  incPC mmix
-- }}}

-- LDHT 'load high tetra' {{{
runLDHT :: RunInsn
runLDHT mmix insn = do
  let x = iGetX insn
  (y,z) <- mmixGetGRYZ mmix insn
  let vaddr = y + z
  checkVAddrRead mmix vaddr
  trapped <- checkTrapped mmix
  unless trapped $ mmixVLdHighTetra mmix vaddr >>= mmixSetGR mmix x
  incPC mmix
-- }}}

-- LDHTI 'load high tetra immediate' {{{
runLDHTI :: RunInsn
runLDHTI mmix insn = do
  let x = iGetX insn
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  let vaddr = y + z
  checkVAddrRead mmix vaddr
  trapped <- checkTrapped mmix
  unless trapped $ mmixVLdHighTetra mmix vaddr >>= mmixSetGR mmix x
  incPC mmix
-- }}}

-- CSWAP 'compare and swap octabytes' {{{
runCSWAP :: RunInsn
runCSWAP mmix insn = do
  let x = iGetX insn
  (y,z) <- mmixGetGRYZ mmix insn
  let vaddr = y + z
  checkVAddrReadWrite mmix vaddr
  trapped <- checkTrapped mmix
  unless trapped $ mmixCSwap mmix x vaddr
  incPC mmix
-- }}}

-- CSWAPI 'compare and swap octabytes immediate' {{{
runCSWAPI :: RunInsn
runCSWAPI mmix insn = do
  let x = iGetX insn
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  let vaddr = y + z
  checkVAddrReadWrite mmix vaddr
  trapped <- checkTrapped mmix
  unless trapped $ mmixCSwap mmix x vaddr
  incPC mmix
-- }}}

-- LDUNC 'load octa uncached' {{{
runLDUNC :: RunInsn
runLDUNC mmix insn = do
  let x = iGetX insn
  (y, z) <- mmixGetGRYZ mmix insn
  let vaddr = y + z
  checkVAddrRead mmix vaddr
  trapped <- checkTrapped mmix
  unless trapped $ mmixVLdOcta mmix vaddr >>= mmixSetGR mmix x
  incPC mmix
-- }}}

-- LDUNCI 'load octa uncached immediate' {{{
runLDUNCI :: RunInsn
runLDUNCI mmix insn = do
  let x = iGetX insn
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  let vaddr = y + z
  checkVAddrRead mmix vaddr
  trapped <- checkTrapped mmix
  unless trapped $ mmixVLdOcta mmix vaddr >>= mmixSetGR mmix x
  incPC mmix
-- }}}

-- LDVTS 'load virtual translation status' {{{
runLDVTS :: RunInsn
runLDVTS mmix insn = do
  -- just tell $X not in TC
  -- TODO: implement a TC?
  mmixSetGRX mmix insn 0
  incPC mmix
-- }}}

-- LDVTSI 'load virtual translation status' {{{
runLDVTSI :: RunInsn
runLDVTSI mmix insn = do
  mmixSetGRX mmix insn 0
  incPC mmix
-- }}}

-- PRELD {{{
runPRELD :: RunInsn
runPRELD mmix insn = do
  -- preload M[$Y+$Z] to M[$Y+$Z+X]
  incPC mmix
-- }}}

-- PRELDI {{{
runPRELDI :: RunInsn
runPRELDI mmix insn = do
  -- preload M[$Y+Z] to M[$Y+Z+X]
  incPC mmix
-- }}}

-- PREGO {{{
runPREGO :: RunInsn
runPREGO mmix insn = do
  -- pre-goto M[$Y+$Z] to M[$Y+$Z+X]
  incPC mmix
-- }}}

-- PREGOI {{{
runPREGOI :: RunInsn
runPREGOI mmix insn = do
  -- pre-goto M[$Y+Z] to M[$Y+Z+X]
  incPC mmix
-- }}}

-- GO 'go to location' {{{
runGO :: RunInsn
runGO mmix insn = do
  pc <- mmixGetPC mmix
  mmixSetGRX mmix insn $ pc + 4
  (y,z) <- mmixGetGRYZ mmix insn
  mmixSetPC mmix $ y + z
  
-- }}}

-- GOI 'go to location immediate' {{{
runGOI :: RunInsn
runGOI mmix insn = do
  pc <- mmixGetPC mmix
  mmixSetGRX mmix insn $ pc + 4
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  mmixSetPC mmix $ y + z
-- }}}

-- }}}

-- a* store {{{

-- STB {{{
runSTB :: RunInsn
runSTB mmix insn = do
  (x,y,z) <- mmixGetGRXYZ mmix insn
  let vaddr = y + z
  checkVAddrWrite mmix vaddr
  trapped <- checkTrapped mmix
  unless trapped $ mmixVStByte mmix vaddr x
  incPC mmix
-- }}}

-- STBI {{{
runSTBI :: RunInsn
runSTBI mmix insn = do
  (x,y) <- mmixGetGRXY mmix insn
  let z = iGetZu insn
  let vaddr = y + z
  checkVAddrWrite mmix vaddr
  trapped <- checkTrapped mmix
  unless trapped $ mmixVStByte mmix vaddr x
  incPC mmix
-- }}}

-- STBU {{{
runSTBU :: RunInsn
runSTBU mmix insn = do
  (x,y,z) <- mmixGetGRXYZ mmix insn
  let vaddr = y + z
  checkVAddrWrite mmix vaddr
  trapped <- checkTrapped mmix
  unless trapped $ mmixVStByteu mmix vaddr x
  incPC mmix
-- }}}

-- STBUI {{{
runSTBUI :: RunInsn
runSTBUI mmix insn = do
  (x,y) <- mmixGetGRXY mmix insn
  let z = iGetZu insn
  let vaddr = y + z
  checkVAddrWrite mmix vaddr
  trapped <- checkTrapped mmix
  unless trapped $ mmixVStByteu mmix vaddr x
  incPC mmix
-- }}}

-- STW {{{
runSTW :: RunInsn
runSTW mmix insn = do
  (x,y,z) <- mmixGetGRXYZ mmix insn
  let vaddr = y + z
  checkVAddrWrite mmix vaddr
  trapped <- checkTrapped mmix
  unless trapped $ mmixVStWyde mmix vaddr x
  incPC mmix
-- }}}

-- STWI {{{
runSTWI :: RunInsn
runSTWI mmix insn = do
  (x,y) <- mmixGetGRXY mmix insn
  let z = iGetZu insn
  let vaddr = y + z
  checkVAddrWrite mmix vaddr
  trapped <- checkTrapped mmix
  unless trapped $ mmixVStWyde mmix vaddr x
  incPC mmix
-- }}}

-- STWU {{{
runSTWU :: RunInsn
runSTWU mmix insn = do
  (x,y,z) <- mmixGetGRXYZ mmix insn
  let vaddr = y + z
  checkVAddrWrite mmix vaddr
  trapped <- checkTrapped mmix
  unless trapped $ mmixVStWydeu mmix vaddr x
  incPC mmix
-- }}}

-- STWUI {{{
runSTWUI :: RunInsn
runSTWUI mmix insn = do
  (x,y) <- mmixGetGRXY mmix insn
  let z = iGetZu insn
  let vaddr = y + z
  checkVAddrWrite mmix vaddr
  trapped <- checkTrapped mmix
  unless trapped $ mmixVStWydeu mmix vaddr x
  incPC mmix
-- }}}

-- STT {{{
runSTT :: RunInsn
runSTT mmix insn = do
  (x,y,z) <- mmixGetGRXYZ mmix insn
  let vaddr = y + z
  checkVAddrWrite mmix vaddr
  trapped <- checkTrapped mmix
  unless trapped $ mmixVStTetra mmix vaddr x
  incPC mmix
-- }}}

-- STTI {{{
runSTTI :: RunInsn
runSTTI mmix insn = do
  (x,y) <- mmixGetGRXY mmix insn
  let z = iGetZu insn
  let vaddr = y + z
  checkVAddrWrite mmix vaddr
  trapped <- checkTrapped mmix
  unless trapped $ mmixVStTetra mmix vaddr x
  incPC mmix
-- }}}

-- STTU {{{
runSTTU :: RunInsn
runSTTU mmix insn = do
  (x,y,z) <- mmixGetGRXYZ mmix insn
  let vaddr = y + z
  checkVAddrWrite mmix vaddr
  trapped <- checkTrapped mmix
  unless trapped $ mmixVStTetrau mmix vaddr x
  incPC mmix
-- }}}

-- STTUI {{{
runSTTUI :: RunInsn
runSTTUI mmix insn = do
  (x,y) <- mmixGetGRXY mmix insn
  let z = iGetZu insn
  let vaddr = y + z
  checkVAddrWrite mmix vaddr
  trapped <- checkTrapped mmix
  unless trapped $ mmixVStTetrau mmix vaddr x
  incPC mmix
-- }}}

-- STO {{{
runSTO :: RunInsn
runSTO mmix insn = do
  (x,y,z) <- mmixGetGRXYZ mmix insn
  let vaddr = y + z
  checkVAddrWrite mmix vaddr
  trapped <- checkTrapped mmix
  unless trapped $ mmixVStOcta mmix vaddr x
  incPC mmix
-- }}}

-- STOI {{{
runSTOI :: RunInsn
runSTOI mmix insn = do
  (x,y) <- mmixGetGRXY mmix insn
  let z = iGetZu insn
  let vaddr = y + z
  checkVAddrWrite mmix vaddr
  trapped <- checkTrapped mmix
  unless trapped $ mmixVStOcta mmix vaddr x
  incPC mmix
-- }}}

-- STOU {{{
runSTOU :: RunInsn
runSTOU mmix insn = do
  (x,y,z) <- mmixGetGRXYZ mmix insn
  let vaddr = y + z
  checkVAddrWrite mmix vaddr
  trapped <- checkTrapped mmix
  unless trapped $ mmixVStOcta mmix vaddr x
  incPC mmix
-- }}}

-- STOUI {{{
runSTOUI :: RunInsn
runSTOUI mmix insn = do
  (x,y) <- mmixGetGRXY mmix insn
  let z = iGetZu insn
  let vaddr = y + z
  checkVAddrWrite mmix vaddr
  trapped <- checkTrapped mmix
  unless trapped $ mmixVStOcta mmix vaddr x
  incPC mmix
-- }}}

-- }}}

-- b* special access; cache; sync; pushgo {{{

-- STSF 'store short float' {{{
runSTSF :: RunInsn
runSTSF mmix insn = do
  rdm <- toRoundModeRaw <$> mmixGetSR mmix rAIx
  (x,y,z) <- mmixGetGRXYZ mmix insn
  let re = sfpPack rdm $ fpUnpack $ castOtoF x
  let sfp = cast $ castSFtoT $ arithGetRx re
  let vaddr = y + z
  checkVAddrWrite mmix vaddr
  trapped <- checkTrapped mmix
  unless trapped $ mmixVStTetrau mmix vaddr sfp
  incPC mmix
-- }}}

-- STSFI 'store short float immediate' {{{
runSTSFI :: RunInsn
runSTSFI mmix insn = do
  rdm <- toRoundModeRaw <$> mmixGetSR mmix rAIx
  (x,y) <- mmixGetGRXY mmix insn
  let z = iGetZu insn
  let re = sfpPack rdm $ fpUnpack $ castOtoF x
  let sfp = cast $ castSFtoT $ arithGetRx re
  let vaddr = y + z
  checkVAddrWrite mmix vaddr
  trapped <- checkTrapped mmix
  unless trapped $ mmixVStTetrau mmix vaddr sfp
  incPC mmix
-- }}}

-- STHT 'store high tetra' {{{
runSTHT :: RunInsn
runSTHT mmix insn = do
  (x,y,z) <- mmixGetGRXYZ mmix insn
  let vaddr = y + z
  checkVAddrWrite mmix vaddr
  trapped <- checkTrapped mmix
  unless trapped $ mmixVStTetrau mmix vaddr $ x `shiftR` 32
  incPC mmix
-- }}}

-- STHTI 'store high tetra immediate' {{{
runSTHTI :: RunInsn
runSTHTI mmix insn = do
  (x,y) <- mmixGetGRXY mmix insn
  let z = iGetZu insn
  let vaddr = y + z
  checkVAddrWrite mmix vaddr
  trapped <- checkTrapped mmix
  unless trapped $ mmixVStTetrau mmix vaddr $ x `shiftR` 32
  incPC mmix
-- }}}

-- STCO 'store constant octabyte' {{{
runSTCO :: RunInsn
runSTCO mmix insn = do
  let x = iGetXu insn
  (y,z) <- mmixGetGRYZ mmix insn
  let vaddr = y + z
  checkVAddrWrite mmix vaddr
  trapped <- checkTrapped mmix
  unless trapped $ mmixVStOcta mmix vaddr x
  incPC mmix
-- }}}

-- STCOI 'store constant octabyte immediate' {{{
runSTCOI :: RunInsn
runSTCOI mmix insn = do
  let x = iGetXu insn
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  let vaddr = y + z
  checkVAddrWrite mmix vaddr
  trapped <- checkTrapped mmix
  unless trapped $ mmixVStOcta mmix vaddr x
  incPC mmix
-- }}}

-- STUNC 'store octa uncached' {{{
runSTUNC :: RunInsn
runSTUNC mmix insn = do
  (x,y,z) <- mmixGetGRXYZ mmix insn
  let vaddr = y + z
  checkVAddrWrite mmix vaddr
  trapped <- checkTrapped mmix
  unless trapped $ mmixVStOcta mmix vaddr x
  incPC mmix
-- }}}

-- STUNCI 'store octa uncached immediate' {{{
runSTUNCI :: RunInsn
runSTUNCI mmix insn = do
  (x,y) <- mmixGetGRXY mmix insn
  let z = iGetZu insn
  let vaddr = y + z
  checkVAddrWrite mmix vaddr
  trapped <- checkTrapped mmix
  unless trapped $ mmixVStOcta mmix vaddr x
  incPC mmix
-- }}}

-- SYNCD 'synchronize data' {{{
runSYNCD :: RunInsn
runSYNCD mmix insn = do
  -- Just do nothing, update it in future.
  incPC mmix
-- }}}

-- SYNCDI 'synchronize data immediate' {{{
runSYNCDI :: RunInsn
runSYNCDI mmix insn = do
  -- Just do nothing, update it in future.
  incPC mmix
-- }}}

-- PREST 'prestore data' {{{
runPREST :: RunInsn
runPREST mmix insn = do
  -- Just do nothing, update it in future.
  incPC mmix
-- }}}

-- PRESTI 'prestore data immediate' {{{
runPRESTI :: RunInsn
runPRESTI mmix insn = do
  -- Just do nothing, update it in future.
  incPC mmix
-- }}}

-- SYNCID 'synchronize instructions and data' {{{
runSYNCID :: RunInsn
runSYNCID mmix insn = do
  -- Just do nothing, update it in future.
  incPC mmix
-- }}}

-- SYNCIDI 'synchronize instructions and data immediate' {{{
runSYNCIDI :: RunInsn
runSYNCIDI mmix insn = do
  -- Just do nothing, update it in future.
  incPC mmix
-- }}}

-- PUSHGO 'push registers and go' {{{
runPUSHGO :: RunInsn
runPUSHGO mmix insn = do
  let x = iGetX insn
  (y, z) <- mmixGetGRYZ mmix insn
  runPushGR mmix x
  --trapped <- checkTrapped mmix
  --unless trapped $ 
  runPGo mmix (y + z)
-- }}}

-- PUSHGOI 'push registers and go immediate' {{{
runPUSHGOI :: RunInsn
runPUSHGOI mmix insn = do
  let x = iGetX insn
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  runPushGR mmix x
  --trapped <- checkTrapped mmix
  --unless trapped $
  runPGo mmix (y + z)
-- }}}

-- }}}

-- c* bit logic (and/or/xor/nor) {{{

-- OR {{{
runOR :: RunInsn
runOR mmix insn = do
  (y,z) <- mmixGetGRYZ mmix insn
  mmixSetGRX mmix insn $ y .|. z
  incPC mmix
-- }}}

-- ORI {{{
runORI :: RunInsn
runORI mmix insn = do
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  mmixSetGRX mmix insn $ y .|. z
  incPC mmix
-- }}}

-- ORN {{{
runORN :: RunInsn
runORN mmix insn = do
  (y,z) <- mmixGetGRYZ mmix insn
  mmixSetGRX mmix insn $ y .|. (complement z)
  incPC mmix
-- }}}

-- ORNI {{{
runORNI :: RunInsn
runORNI mmix insn = do
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  mmixSetGRX mmix insn $ y .|. (complement z)
  incPC mmix
-- }}}

-- NOR {{{
runNOR :: RunInsn
runNOR mmix insn = do
  (y,z) <- mmixGetGRYZ mmix insn
  mmixSetGRX mmix insn $ complement $ y .|. z
  incPC mmix
-- }}}

-- NORI {{{
runNORI :: RunInsn
runNORI mmix insn = do
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  mmixSetGRX mmix insn $ complement $ y .|. z
  incPC mmix
-- }}}

-- XOR {{{
runXOR :: RunInsn
runXOR mmix insn = do
  (y,z) <- mmixGetGRYZ mmix insn
  mmixSetGRX mmix insn $ y `xor` z
  incPC mmix
-- }}}

-- XORI {{{
runXORI :: RunInsn
runXORI mmix insn = do
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  mmixSetGRX mmix insn $ y `xor` z
  incPC mmix
-- }}}

-- AND {{{
runAND :: RunInsn
runAND mmix insn = do
  (y,z) <- mmixGetGRYZ mmix insn
  mmixSetGRX mmix insn $ y .&. z
  incPC mmix
-- }}}

-- ANDI {{{
runANDI :: RunInsn
runANDI mmix insn = do
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  mmixSetGRX mmix insn $ y .&. z
  incPC mmix
-- }}}

-- ANDN {{{
runANDN :: RunInsn
runANDN mmix insn = do
  (y,z) <- mmixGetGRYZ mmix insn
  mmixSetGRX mmix insn $ y .&. (complement z)
  incPC mmix
-- }}}

-- ANDNI {{{
runANDNI :: RunInsn
runANDNI mmix insn = do
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  mmixSetGRX mmix insn $ y .&. (complement z)
  incPC mmix
-- }}}

-- NAND {{{
runNAND :: RunInsn
runNAND mmix insn = do
  (y,z) <- mmixGetGRYZ mmix insn
  mmixSetGRX mmix insn $ complement $ y .&. z
  incPC mmix
-- }}}

-- NANDI {{{
runNANDI :: RunInsn
runNANDI mmix insn = do
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  mmixSetGRX mmix insn $ complement $ y .&. z
  incPC mmix
-- }}}

-- NXOR {{{
runNXOR :: RunInsn
runNXOR mmix insn = do
  (y,z) <- mmixGetGRYZ mmix insn
  mmixSetGRX mmix insn $ complement $ y `xor` z
  incPC mmix
-- }}}

-- NXORI {{{
runNXORI :: RunInsn
runNXORI mmix insn = do
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  mmixSetGRX mmix insn $ complement $ y `xor` z
  incPC mmix
-- }}}

-- }}}

-- d* bytes magic {{{

-- BDIF 'byte difference' {{{
runBDIF :: RunInsn
runBDIF mmix insn = do
  (y,z) <- mmixGetGRYZ mmix insn
  mmixSetGRX mmix insn $ diffByte y z
  incPC mmix
-- }}}

-- BDIFI 'byte difference immediate' {{{
runBDIFI :: RunInsn
runBDIFI mmix insn = do
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  mmixSetGRX mmix insn $ diffByte y z
  incPC mmix
-- }}}

-- WDIF 'wyde difference' {{{
runWDIF :: RunInsn
runWDIF mmix insn = do
  (y,z) <- mmixGetGRYZ mmix insn
  mmixSetGRX mmix insn $ diffWyde y z
  incPC mmix
-- }}}

-- WDIFI 'wyde difference immediate' {{{
runWDIFI :: RunInsn
runWDIFI mmix insn = do
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  mmixSetGRX mmix insn $ diffWyde y z
  incPC mmix
-- }}}

-- TDIF 'tetra difference' {{{
runTDIF :: RunInsn
runTDIF mmix insn = do
  (y,z) <- mmixGetGRYZ mmix insn
  mmixSetGRX mmix insn $ diffTetra y z
  incPC mmix
-- }}}

-- TDIFI 'tetra difference immediate' {{{
runTDIFI :: RunInsn
runTDIFI mmix insn = do
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  mmixSetGRX mmix insn $ diffTetra y z
  incPC mmix
-- }}}

-- ODIF 'octa difference' {{{
runODIF :: RunInsn
runODIF mmix insn = do
  (y,z) <- mmixGetGRYZ mmix insn
  mmixSetGRX mmix insn $ diffOcta y z
  incPC mmix
-- }}}

-- ODIFI 'octa difference immediate' {{{
runODIFI :: RunInsn
runODIFI mmix insn = do
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  mmixSetGRX mmix insn $ diffOcta y z
  incPC mmix
-- }}}

-- MUX 'bitwise multiplex' {{{
runMUX :: RunInsn
runMUX mmix insn = do
  (y,z) <- mmixGetGRYZ mmix insn
  m <- mmixGetSR mmix rMIx
  mmixSetGRX mmix insn $ bitMux m y z
  incPC mmix
-- }}}

-- MUXI 'bitwise multiplex immediate' {{{
runMUXI :: RunInsn
runMUXI mmix insn = do
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  m <- mmixGetSR mmix rMIx
  mmixSetGRX mmix insn $ bitMux m y z
  incPC mmix
-- }}}

-- SADD 'sideways add' {{{
runSADD :: RunInsn
runSADD mmix insn = do
  (y,z) <- mmixGetGRYZ mmix insn
  mmixSetGRX mmix insn $ bitSAdd y z
  incPC mmix
-- }}}

-- SADDI 'sideways add immediate' {{{
runSADDI :: RunInsn
runSADDI mmix insn = do
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  mmixSetGRX mmix insn $ bitSAdd y z
  incPC mmix
-- }}}

-- MOR 'multiple or' {{{
runMOR :: RunInsn
runMOR mmix insn = do
  (y,z) <- mmixGetGRYZ mmix insn
  mmixSetGRX mmix insn $ bitMOr y z
  incPC mmix
-- }}}

-- MORI 'multiple or immediate' {{{
runMORI :: RunInsn
runMORI mmix insn = do
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  mmixSetGRX mmix insn $ bitMOr y z
  incPC mmix
-- }}}

-- MXOR 'multiple exclusive-or' {{{
runMXOR :: RunInsn
runMXOR mmix insn = do
  (y,z) <- mmixGetGRYZ mmix insn
  mmixSetGRX mmix insn $ bitMXor y z
  incPC mmix
-- }}}

-- MXORI 'multiple exclusive-or immediate' {{{
runMXORI :: RunInsn
runMXORI mmix insn = do
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  mmixSetGRX mmix insn $ bitMXor y z
  incPC mmix
-- }}}

-- }}}

-- e* wyde-field (set/get/or/andn) {{{

-- SETH {{{
runSETH :: RunInsn
runSETH mmix insn = do
  let yz = iGetYZu insn
  mmixSetGRX mmix insn $ yz `shiftL` 48
  incPC mmix
-- }}}

-- SETMH {{{
runSETMH :: RunInsn
runSETMH mmix insn = do
  let yz = iGetYZu insn
  mmixSetGRX mmix insn $ yz `shiftL` 32
  incPC mmix
-- }}}

-- SETML {{{
runSETML :: RunInsn
runSETML mmix insn = do
  let yz = iGetYZu insn
  mmixSetGRX mmix insn $ yz `shiftL` 16
  incPC mmix
-- }}}

-- SETL {{{
runSETL :: RunInsn
runSETL mmix insn = do
  let yz = iGetYZu insn
  mmixSetGRX mmix insn $ yz
  incPC mmix
-- }}}

-- INCH 'increase by high wyde' {{{
runINCH :: RunInsn
runINCH mmix insn = do
  let yz = iGetYZu insn
  x <- mmixGetGRX mmix insn
  let re = intAddu x $ yz `shiftL` 48
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
  
-- }}}

-- INCMH {{{
runINCMH :: RunInsn
runINCMH mmix insn = do
  let yz = iGetYZu insn
  x <- mmixGetGRX mmix insn
  let re = intAddu x $ yz `shiftL` 32
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- INCML {{{
runINCML :: RunInsn
runINCML mmix insn = do
  let yz = iGetYZu insn
  x <- mmixGetGRX mmix insn
  let re = intAddu x $ yz `shiftL` 16
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- INCL {{{
runINCL :: RunInsn
runINCL mmix insn = do
  let yz = iGetYZu insn
  x <- mmixGetGRX mmix insn
  let re = intAddu x yz
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- ORH {{{
runORH :: RunInsn
runORH mmix insn = do
  let yz = iGetYZu insn
  x <- mmixGetGRX mmix insn
  let x' = x .|. (yz `shiftL` 48)
  mmixSetGRX mmix insn x'
  incPC mmix
-- }}}

-- ORMH {{{
runORMH :: RunInsn
runORMH mmix insn = do
  let yz = iGetYZu insn
  x <- mmixGetGRX mmix insn
  let x' = x .|. (yz `shiftL` 32)
  mmixSetGRX mmix insn x'
  incPC mmix
-- }}}

-- ORML {{{
runORML :: RunInsn
runORML mmix insn = do
  let yz = iGetYZu insn
  x <- mmixGetGRX mmix insn
  let x' = x .|. (yz `shiftL` 16)
  mmixSetGRX mmix insn x'
  incPC mmix
-- }}}

-- ORL {{{
runORL :: RunInsn
runORL mmix insn = do
  let yz = iGetYZu insn
  x <- mmixGetGRX mmix insn
  let x' = x .|. yz
  mmixSetGRX mmix insn x'
  incPC mmix
-- }}}

-- ANDNH {{{
runANDNH :: RunInsn
runANDNH mmix insn = do
  let yz = iGetYZu insn
  x <- mmixGetGRX mmix insn
  let x' = x .&. complement (yz `shiftL` 48)
  mmixSetGRX mmix insn x'
  incPC mmix
-- }}}

-- ANDNMH {{{
runANDNMH :: RunInsn
runANDNMH mmix insn = do
  let yz = iGetYZu insn
  x <- mmixGetGRX mmix insn
  let x' = x .&. complement (yz `shiftL` 32)
  mmixSetGRX mmix insn x'
  incPC mmix
-- }}}

-- ANDNML {{{
runANDNML :: RunInsn
runANDNML mmix insn = do
  let yz = iGetYZu insn
  x <- mmixGetGRX mmix insn
  let x' = x .&. complement (yz `shiftL` 16)
  mmixSetGRX mmix insn x'
  incPC mmix
-- }}}

-- ANDNL {{{
runANDNL :: RunInsn
runANDNL mmix insn = do
  let yz = iGetYZu insn
  x <- mmixGetGRX mmix insn
  let x' = x .&. complement yz
  mmixSetGRX mmix insn x'
  incPC mmix
-- }}}

-- }}}

-- f* jump; call/return; sr; context; trip {{{

-- JMP 'jump forward' {{{
runJMP :: RunInsn
runJMP mmix insn = runJumpForward mmix insn
-- }}}

-- JMPB 'jump backward' {{{
runJMPB :: RunInsn
runJMPB mmix insn = runJumpBackward mmix insn
-- }}}

-- PUSHJ 'push registers and jump forward' {{{
runPUSHJ :: RunInsn
runPUSHJ mmix insn = do
  let x = iGetX insn
  runPushGR mmix x
  --trapped <- checkTrapped mmix
  --unless trapped $
  runPJumpForward mmix insn
-- }}}

-- PUSHJB 'push registers and jump backward' {{{
runPUSHJB :: RunInsn
runPUSHJB mmix insn = do
  let x = iGetX insn
  runPushGR mmix x
  --trapped <- checkTrapped mmix
  --unless trapped $
  runPJumpBackward mmix insn
-- }}}

-- GETA 'get address forward' {{{
runGETA :: RunInsn
runGETA mmix insn = do
  pc <- mmixGetPC mmix
  let addr = pc + (iGetYZu insn `shiftL` 2)
  mmixSetGRX mmix insn addr
  incPC mmix
-- }}}

-- GETAB 'get address backward' {{{
runGETAB :: RunInsn
runGETAB mmix insn = do
  pc <- mmixGetPC mmix
  let addr = pc + (iGetYZu insn `shiftL` 2) - 0x40000
  mmixSetGRX mmix insn addr
  incPC mmix
-- }}}

-- PUT 'put into special register' {{{
runPUT :: RunInsn
runPUT mmix insn = do
  let x = iGetX insn
  let y = iGetY insn
  z <- mmixGetGRZ mmix insn
  if y == 0
  then putSR mmix x z
  else issueDTrapPb mmix
  incPC mmix
-- }}}

-- PUTI 'put into special register immediate' {{{
runPUTI :: RunInsn
runPUTI mmix insn = do
  let x = iGetX insn
  let y = iGetY insn
  let z = iGetZu insn
  if y == 0
  then putSR mmix x z
  else issueDTrapPb mmix
  incPC mmix
-- }}}

-- POP 'pop registers and return from subroutine' {{{
runPOP :: RunInsn
runPOP mmix insn = do
  let x = iGetX insn
  let yz = iGetYZu insn
  runPopGR mmix x
  --trapped <- checkTrapped mmix
  --unless trapped $ do
  rJ <- mmixGetSR mmix rJIx
  mmixSetPC mmix $ rJ + (yz `shiftL` 2)
-- }}}

-- RESUME 'resume after interrupt' {{{
runRESUME :: RunInsn
runRESUME mmix insn = do
  pc <- mmixGetPC mmix
  let xy = iGetXYu insn
  let z = iGetZ insn
  if xy /= 0 || z > 1 || (bitGet 63 pc == 0 && z == 1)
  then issueDTrapPb mmix >> incPC mmix
  else runResume mmix z
-- }}}

-- SAVE 'save process state' {{{
runSAVE :: RunInsn
runSAVE mmix insn = do
  let x = iGetX insn
  let yz = iGetYZu insn
  if yz /= 0
  then issueDTrapPb mmix
  else runSave mmix x
  incPC mmix
-- }}}

-- UNSAVE 'restore process state' {{{
runUNSAVE :: RunInsn
runUNSAVE mmix insn = do
  let xy = iGetXYu insn
  z <- mmixGetGRZ mmix insn
  if xy /= 0
  then issueDTrapPb mmix
  else runUnsave mmix z
  incPC mmix
-- }}}

-- SYNC 'synchronize' {{{
runSYNC :: RunInsn
runSYNC mmix insn = do
  let xyz = iGetXYZu insn
  when (xyz > 7) $ issueDTrapPb mmix
  incPC mmix
-- }}}

-- SWYM 'sympathize with your machinery' {{{
runSWYM :: RunInsn
runSWYM mmix insn = do incPC mmix
-- }}}

-- GET 'get from special register' {{{
runGET :: RunInsn
runGET mmix insn = do
  let z = iGetZ insn
  let y = iGetY insn
  if z > 31 || y /= 0
  then do
    issueDTrapPb mmix
    mmixSetGRX mmix insn 0
  else do
    -- if get rQ then clear rQQ
    when (z == rQIx) $ mmixSetGR mmix rQQIx 0
    sr <- mmixGetSR mmix z
    mmixSetGRX mmix insn sr
  incPC mmix
-- }}}

-- TRIP 'trip' {{{
runTRIP :: RunInsn
runTRIP mmix insn = do
  issueTrip mmix
  incPC mmix
-- }}}

-- }}}

-- }}}

