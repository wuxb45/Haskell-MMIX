-- Copyright 2012 Wu Xingbo <wuxb45@gmail.com>

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
import Control.Monad (when, unless)
import Data.Functor ((<$>))
import Debug.Trace (trace, traceShow)

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

-- issueATrip: (set

-- issueTrip: (set rA's event bit) {{{
issueTrip :: MMIX -> TRIP -> IO ()
issueTrip = tripSetABit
-- }}}

-- issueATrip: issue ArithEx Trip {{{
issueATrip :: MMIX -> ArithEx -> IO ()
issueATrip mmix ex = issueTrip mmix $ cvAEtoT ex
-- }}}


-- issueDTrap*: dynamic trap happened (set rQ bit) {{{

issueDTrap :: BitIx -> MMIX -> IO ()
issueDTrap ix mmix = do
  rQ <- mmixGetSR mmix rQIx
  mmixSetSR mmix rQIx $ bitSet1 ix rQ

issueDTrapPr :: MMIX -> IO ()
issueDTrapPr = issueDTrap rQBr

issueDTrapPw :: MMIX -> IO ()
issueDTrapPw = issueDTrap rQBw

issueDTrapPx :: MMIX -> IO ()
issueDTrapPx = issueDTrap rQBx

issueDTrapPn :: MMIX -> IO ()
issueDTrapPn = issueDTrap rQBn

issueDTrapPk :: MMIX -> IO ()
issueDTrapPk = issueDTrap rQBk

issueDTrapPb :: MMIX -> IO ()
issueDTrapPb = issueDTrap rQBb

issueDTrapPs :: MMIX -> IO ()
issueDTrapPs = issueDTrap rQBs

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

-- check privileged instruction
-- TODO: big impl
checkPk :: Insn -> Bool
checkPk insn = True

-- check instruction rules
-- TODO: big impl
checkPb :: Insn -> Bool
checkPb insn = True

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

-- check vaddr can execute and issue Trap (x) (neg-addr -> True) {{{
checkVAddrExecute :: MMIX -> VAddr -> IO Bool
checkVAddrExecute mmix vaddr = do
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
  if isVirtual vaddr
  then do
    canWrite <- checkPw <$> vMapGetPTE0 mmix vaddr
    unless canWrite $ issueDTrapPw mmix
  else
    issueDTrapPn mmix
-- }}}

-- }}}

-- runMMIX and sub-routines {{{

-- run mmix
runMMIX :: MMIX -> IO ()
runMMIX mmix = do
  checkPCPosition mmix -- check s/p
  trapped <- checkTrapped mmix
  unless trapped $ runFetchExec mmix

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


runInsn :: MMIX -> Insn -> IO ()
runInsn mmix insn = undefined --TODO: call runXXXinsn


-- }}}

-- helpers for runXXInsn {{{

-- normal PC routines {{{

-- pc = pc + 4
incPC :: MMIX -> IO ()
incPC mmix = do
  pc <- mmixGetPC mmix
  mmixSetPC mmix (pc + 4)

-- }}}

-- }}}

-- all INSN {{{

type RunInsn = MMIX -> Insn -> IO ()

-- done
-- 0* trap, floating +-/convert {{{

-- TRAP 'trap' {{{
runTRAP :: RunInsn
runTRAP mmix insn = do
  npc <- (+ 4) <$> mmixGetPC mmix
  doFTrap mmix insn npc
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
  rdm0 <- toRoundModeRaw <$> mmixGetSR mmix rAIx
  let y = iGetYu insn
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
  rdm0 <- toRoundModeRaw <$> mmixGetSR mmix rAIx
  let y = iGetYu insn
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
  rdm0 <- toRoundModeRaw <$> mmixGetSR mmix rAIx
  let y = iGetYu insn
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
  rdm0 <- toRoundModeRaw <$> mmixGetSR mmix rAIx
  let y = iGetYu insn
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
  rdm0 <- toRoundModeRaw <$> mmixGetSR mmix rAIx
  let y = iGetYu insn
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
  rdm0 <- toRoundModeRaw <$> mmixGetSR mmix rAIx
  let y = iGetYu insn
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
  rdm0 <- toRoundModeRaw <$> mmixGetSR mmix rAIx
  let y = iGetYu insn
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
  rdm0 <- toRoundModeRaw <$> mmixGetSR mmix rAIx
  let y = iGetYu insn
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
  rdm0 <- toRoundModeRaw <$> mmixGetSR mmix rAIx
  let y = iGetYu insn
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
  rdm0 <- toRoundModeRaw <$> mmixGetSR mmix rAIx
  let y = iGetYu insn
  let rdm = fromMaybe rdm0 $ toRoundModeImm y
  let z = iGetZu insn
  let re = fpSFloatu rdm z
  mapM (issueATrip mmix) $ arithGetEx re
  mmixSetGRFX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- }}}

-- done
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
  rdm0 <- toRoundModeRaw <$> mmixGetSR mmix rAIx
  let y = iGetYu insn
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
  rdm0 <- toRoundModeRaw <$> mmixGetSR mmix rAIx
  let y = iGetYu insn
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

-- done
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

-- done
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

-- TODO
-- 4* branch {{{

-- BN {{{
runBN :: RunInsn
runBN mmix insn = do undefined
-- }}}

-- BNB {{{
runBNB :: RunInsn
runBNB mmix insn = do undefined
-- }}}

-- BZ {{{
runBZ :: RunInsn
runBZ mmix insn = do undefined
-- }}}

-- BZB {{{
runBZB :: RunInsn
runBZB mmix insn = do undefined
-- }}}

-- BP {{{
runBP :: RunInsn
runBP mmix insn = do undefined
-- }}}

-- BPB {{{
runBPB :: RunInsn
runBPB mmix insn = do undefined
-- }}}

-- BOD {{{
runBOD :: RunInsn
runBOD mmix insn = do undefined
-- }}}

-- BODB {{{
runBODB :: RunInsn
runBODB mmix insn = do undefined
-- }}}

-- BNN {{{
runBNN :: RunInsn
runBNN mmix insn = do undefined
-- }}}

-- BNNB {{{
runBNNB :: RunInsn
runBNNB mmix insn = do undefined
-- }}}

-- BNZ {{{
runBNZ :: RunInsn
runBNZ mmix insn = do undefined
-- }}}

-- BNZB {{{
runBNZB :: RunInsn
runBNZB mmix insn = do undefined
-- }}}

-- BNP {{{
runBNP :: RunInsn
runBNP mmix insn = do undefined
-- }}}

-- BNPB {{{
runBNPB :: RunInsn
runBNPB mmix insn = do undefined
-- }}}

-- BEV {{{
runBEV :: RunInsn
runBEV mmix insn = do undefined
-- }}}

-- BEVB {{{
runBEVB :: RunInsn
runBEVB mmix insn = do undefined
-- }}}

-- }}}

-- TODO
-- 5* probable branch {{{

-- PBN {{{
runPBN :: RunInsn
runPBN mmix insn = do undefined
-- }}}

-- PBNB {{{
runPBNB :: RunInsn
runPBNB mmix insn = do undefined
-- }}}

-- PBZ {{{
runPBZ :: RunInsn
runPBZ mmix insn = do undefined
-- }}}

-- PBZB {{{
runPBZB :: RunInsn
runPBZB mmix insn = do undefined
-- }}}

-- PBP {{{
runPBP :: RunInsn
runPBP mmix insn = do undefined
-- }}}

-- PBPB {{{
runPBPB :: RunInsn
runPBPB mmix insn = do undefined
-- }}}

-- PBOD {{{
runPBOD :: RunInsn
runPBOD mmix insn = do undefined
-- }}}

-- PBODB {{{
runPBODB :: RunInsn
runPBODB mmix insn = do undefined
-- }}}

-- PBNN {{{
runPBNN :: RunInsn
runPBNN mmix insn = do undefined
-- }}}

-- PBNNB {{{
runPBNNB :: RunInsn
runPBNNB mmix insn = do undefined
-- }}}

-- PBNZ {{{
runPBNZ :: RunInsn
runPBNZ mmix insn = do undefined
-- }}}

-- PBNZB {{{
runPBNZB :: RunInsn
runPBNZB mmix insn = do undefined
-- }}}

-- PBNP {{{
runPBNP :: RunInsn
runPBNP mmix insn = do undefined
-- }}}

-- PBNPB {{{
runPBNPB :: RunInsn
runPBNPB mmix insn = do undefined
-- }}}

-- PBEV {{{
runPBEV :: RunInsn
runPBEV mmix insn = do undefined
-- }}}

-- PBEVB {{{
runPBEVB :: RunInsn
runPBEVB mmix insn = do undefined
-- }}}

-- }}}

-- done
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

-- done
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
runLDB mmix insn = do undefined
-- }}}

-- LDBI {{{
runLDBI :: RunInsn
runLDBI mmix insn = do undefined
-- }}}

-- LDBU {{{
runLDBU :: RunInsn
runLDBU mmix insn = do undefined
-- }}}

-- LDBUI {{{
runLDBUI :: RunInsn
runLDBUI mmix insn = do undefined
-- }}}

-- LDW {{{
runLDW :: RunInsn
runLDW mmix insn = do undefined
-- }}}

-- LDWI {{{
runLDWI :: RunInsn
runLDWI mmix insn = do undefined
-- }}}

-- LDWU {{{
runLDWU :: RunInsn
runLDWU mmix insn = do undefined
-- }}}

-- LDWUI {{{
runLDWUI :: RunInsn
runLDWUI mmix insn = do undefined
-- }}}

-- LDT {{{
runLDT :: RunInsn
runLDT mmix insn = do undefined
-- }}}

-- LDTI {{{
runLDTI :: RunInsn
runLDTI mmix insn = do undefined
-- }}}

-- LDTU {{{
runLDTU :: RunInsn
runLDTU mmix insn = do undefined
-- }}}

-- LDTUI {{{
runLDTUI :: RunInsn
runLDTUI mmix insn = do undefined
-- }}}

-- LDO {{{
runLDO :: RunInsn
runLDO mmix insn = do undefined
-- }}}

-- LDOI {{{
runLDOI :: RunInsn
runLDOI mmix insn = do undefined
-- }}}

-- LDOU {{{
runLDOU :: RunInsn
runLDOU mmix insn = do undefined
-- }}}

-- LDOUI {{{
runLDOUI :: RunInsn
runLDOUI mmix insn = do undefined
-- }}}

-- }}}

-- 9* system ?? {{{

-- LDSF {{{
runLDSF :: RunInsn
runLDSF mmix insn = do undefined
-- }}}

-- LDSFI {{{
runLDSFI :: RunInsn
runLDSFI mmix insn = do undefined
-- }}}

-- LDHT {{{
runLDHT :: RunInsn
runLDHT mmix insn = do undefined
-- }}}

-- LDHTI {{{
runLDHTI :: RunInsn
runLDHTI mmix insn = do undefined
-- }}}

-- CSWAP {{{
runCSWAP :: RunInsn
runCSWAP mmix insn = do undefined
-- }}}

-- CSWAPI {{{
runCSWAPI :: RunInsn
runCSWAPI mmix insn = do undefined
-- }}}

-- LDUNC {{{
runLDUNC :: RunInsn
runLDUNC mmix insn = do undefined
-- }}}

-- LDUNCI {{{
runLDUNCI :: RunInsn
runLDUNCI mmix insn = do undefined
-- }}}

-- LDVTS {{{
runLDVTS :: RunInsn
runLDVTS mmix insn = do undefined
-- }}}

-- LDVTSI {{{
runLDVTSI :: RunInsn
runLDVTSI mmix insn = do undefined
-- }}}

-- PRELD {{{
runPRELD :: RunInsn
runPRELD mmix insn = do undefined
-- }}}

-- PRELDI {{{
runPRELDI :: RunInsn
runPRELDI mmix insn = do undefined
-- }}}

-- PREGO {{{
runPREGO :: RunInsn
runPREGO mmix insn = do undefined
-- }}}

-- PREGOI {{{
runPREGOI :: RunInsn
runPREGOI mmix insn = do undefined
-- }}}

-- GO {{{
runGO :: RunInsn
runGO mmix insn = do undefined
-- }}}

-- GOI {{{
runGOI :: RunInsn
runGOI mmix insn = do undefined
-- }}}

-- }}}

-- a* store {{{

-- STB {{{
runSTB :: RunInsn
runSTB mmix insn = do undefined
-- }}}

-- STBI {{{
runSTBI :: RunInsn
runSTBI mmix insn = do undefined
-- }}}

-- STBU {{{
runSTBU :: RunInsn
runSTBU mmix insn = do undefined
-- }}}

-- STBUI {{{
runSTBUI :: RunInsn
runSTBUI mmix insn = do undefined
-- }}}

-- STW {{{
runSTW :: RunInsn
runSTW mmix insn = do undefined
-- }}}

-- STWI {{{
runSTWI :: RunInsn
runSTWI mmix insn = do undefined
-- }}}

-- STWU {{{
runSTWU :: RunInsn
runSTWU mmix insn = do undefined
-- }}}

-- STWUI {{{
runSTWUI :: RunInsn
runSTWUI mmix insn = do undefined
-- }}}

-- STT {{{
runSTT :: RunInsn
runSTT mmix insn = do undefined
-- }}}

-- STTI {{{
runSTTI :: RunInsn
runSTTI mmix insn = do undefined
-- }}}

-- STTU {{{
runSTTU :: RunInsn
runSTTU mmix insn = do undefined
-- }}}

-- STTUI {{{
runSTTUI :: RunInsn
runSTTUI mmix insn = do undefined
-- }}}

-- STO {{{
runSTO :: RunInsn
runSTO mmix insn = do undefined
-- }}}

-- STOI {{{
runSTOI :: RunInsn
runSTOI mmix insn = do undefined
-- }}}

-- STOU {{{
runSTOU :: RunInsn
runSTOU mmix insn = do undefined
-- }}}

-- STOUI {{{
runSTOUI :: RunInsn
runSTOUI mmix insn = do undefined
-- }}}

-- }}}

-- b* system ? {{{

-- STSF {{{
runSTSF :: RunInsn
runSTSF mmix insn = do undefined
-- }}}

-- STSFI {{{
runSTSFI :: RunInsn
runSTSFI mmix insn = do undefined
-- }}}

-- STHT {{{
runSTHT :: RunInsn
runSTHT mmix insn = do undefined
-- }}}

-- STHTI {{{
runSTHTI :: RunInsn
runSTHTI mmix insn = do undefined
-- }}}

-- STCO {{{
runSTCO :: RunInsn
runSTCO mmix insn = do undefined
-- }}}

-- STCOI {{{
runSTCOI :: RunInsn
runSTCOI mmix insn = do undefined
-- }}}

-- STUNC {{{
runSTUNC :: RunInsn
runSTUNC mmix insn = do undefined
-- }}}

-- STUNCI {{{
runSTUNCI :: RunInsn
runSTUNCI mmix insn = do undefined
-- }}}

-- SYNCD {{{
runSYNCD :: RunInsn
runSYNCD mmix insn = do undefined
-- }}}

-- SYNCDI {{{
runSYNCDI :: RunInsn
runSYNCDI mmix insn = do undefined
-- }}}

-- PREST {{{
runPREST :: RunInsn
runPREST mmix insn = do undefined
-- }}}

-- PRESTI {{{
runPRESTI :: RunInsn
runPRESTI mmix insn = do undefined
-- }}}

-- SYNCID {{{
runSYNCID :: RunInsn
runSYNCID mmix insn = do undefined
-- }}}

-- SYNCIDI {{{
runSYNCIDI :: RunInsn
runSYNCIDI mmix insn = do undefined
-- }}}

-- PUSHGO {{{
runPUSHGO :: RunInsn
runPUSHGO mmix insn = do undefined
-- }}}

-- PUSHGOI {{{
runPUSHGOI :: RunInsn
runPUSHGOI mmix insn = do undefined
-- }}}

-- }}}

-- done
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

-- done
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

-- done
-- e* {{{

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

-- f* {{{

-- JMP {{{
runJMP :: RunInsn
runJMP mmix insn = do undefined
-- }}}

-- JMPB {{{
runJMPB :: RunInsn
runJMPB mmix insn = do undefined
-- }}}

-- PUSHJ {{{
runPUSHJ :: RunInsn
runPUSHJ mmix insn = do undefined
-- }}}

-- PUSHJB {{{
runPUSHJB :: RunInsn
runPUSHJB mmix insn = do undefined
-- }}}

-- GETA {{{
runGETA :: RunInsn
runGETA mmix insn = do undefined
-- }}}

-- GETAB {{{
runGETAB :: RunInsn
runGETAB mmix insn = do undefined
-- }}}

-- PUT {{{
runPUT :: RunInsn
runPUT mmix insn = do undefined
-- }}}

-- PUTI {{{
runPUTI :: RunInsn
runPUTI mmix insn = do undefined
-- }}}

-- POP {{{
runPOP :: RunInsn
runPOP mmix insn = do undefined
-- }}}

-- RESUME {{{
runRESUME :: RunInsn
runRESUME mmix insn = do undefined
-- }}}

-- SAVE {{{
runSAVE :: RunInsn
runSAVE mmix insn = do undefined
-- }}}

-- UNSAVE {{{
runUNSAVE :: RunInsn
runUNSAVE mmix insn = do undefined
-- }}}

-- SYNC {{{
runSYNC :: RunInsn
runSYNC mmix insn = do undefined
-- }}}

-- SWYM {{{
runSWYM :: RunInsn
runSWYM mmix insn = do undefined
-- }}}

-- GET {{{
runGET :: RunInsn
runGET mmix insn = do undefined
-- }}}

-- TRIP {{{
runTRIP :: RunInsn
runTRIP mmix insn = do undefined
-- }}}

-- }}}

-- }}}


-- vim: fdm=marker

