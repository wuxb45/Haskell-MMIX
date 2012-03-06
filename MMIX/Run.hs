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

-- get PTE, 0 PTE on fail {{{

vMapGetPTE0 :: MMIX -> VAddr -> IO PTE
vMapGetPTE0 mmix vaddr = do
  mbPTE <- vmapGetPTE mmix vaddr
  case mbPTE of
  Just pte -> return pte
  Nothing -> return 0

-- }}}

-- get PTE from Memory Device {{{
vMapGetPTE :: MMIX -> VAddr -> IO (Maybe PTE)
vMapGetPTE mmix vaddr = do
  rV <- mmixGetSR mmix rVIx -- no fail
  let mbVMapParam = vMapPrepare vaddr rV
  let mbStartPoint = mbVMapParam >>= vMapStartPoint
  case mbStartPoint of
    Just point -> vMapLookup mmix point
    Nothing -> return Nothing
-- }}}

-- get PAddr from PTE {{{
vMapGetPAddr :: Octa -> Octa -> Octa -> PAddr
vMapGetPAddr rV vaddr pte = a .|. offset
  -- make sure 13 <= s <= 48
  where
    s = fldGet rVFs rV
    s' = max 13 $ min 48 s
    a = fldGetRaw (48, s') pte
    offset = fldGetRaw (s' - 1, 0) vaddr
-- }}}

-- vMapChecks (check s field) {{{
vMapChecks :: Octa -> Bool
vMapChecks rV = (s >= 13) && (s <= 48)
  where s = fldGet rVFs rV
-- }}}

-- prepare parameters for recursive find PTE {{{
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
    raddr  = fldSet1 (63,63) $ fldGetRaw rVFr rV
    base   = (b `shiftL` 13) + raddr
    virt   = fldSet0 (63,61) vaddr
    s      = fldGet rVFs rV
    pn     = virt `shiftR` (fI s)
    limit  = (bp `shiftL` 13) + raddr + (if pn == 0 then 1 else 0)
    nval   = fldSet1 (63,63) $ fldGetRaw rVFn rV
-- }}}

-- guard for check n field match ptp {{{
vMapGuardPTP :: Octa -> PTP -> Maybe PTP
vMapGuardPTP nval ptp =
  if (ptp `xor` nval) .&. ptpmask == 0
  then return ptp
  else Nothing
  -- check bit 63 and <n>
  where ptpmask = fldMask (63,63) .|. fldMask rVFn
-- }}}

-- guard for check n field match pte {{{
vMapGuardPTE :: Octa -> PTE -> Maybe PTE
vMapGuardPTE nval pte =
  if (pte `xor` nval) .&. ptemask == 0
  then return pte
  else Nothing
  -- check <n>
  where ptemask = fldMask rVFn
-- }}}

-- guard for base < limit {{{
vMapGuardLimit :: Octa -> Octa -> a -> Maybe a
vMapGuardLimit base limit a =
  if base < limit then return a else Nothing
-- }}}

-- convert page-number to page-index list {{{
vMapGetPIxList :: Octa -> [Octa]
vMapGetPIxList = reverse . splitPn

splitPn 0 = []
splitPn pn = pnIx:(splitPn pnShift)
  where
    pnShift = fldGet (63,10) pn
    pnIx = fldGet (9,0) pn
-- }}}

-- get the start point of lookup: [base, idx-list] {{{
vMapStartPoint :: VMapParam -> Maybe VStartPoint
vMapStartPoint (base, limit, pn, nval) =
  if len < 1 || len > 5
  then Nothing
  else Just (nval, base', pageIxList) >>= vMapGuardLimit base' limit
  where
    pageIxList = vMapGetPIxList pn
    len = fI $ length pageIxList :: Octa
    base' = base + ((len - 1) * 0x2000)
-- }}}

-- load PTX (PTE/PTP) from Memory {{{
vMapLoadPTX :: MMIX -> PAddr -> PAddr -> IO (Maybe Octa)
vMapLoadPTX mmix base index = mmixLdOcta mmix $ base + (index `shiftL` 3)
-- }}}

-- load (valid) PTE (maybe plus some PTP) {{{
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

-- issue Trip/Trap {{{

-- set a event bit by trip type. {{{
tripSetABit :: MMIX -> TRIP -> IO ()
tripSetABit mmix trip = do
  rA <- mmixGetSR mmix rAIx
  mmixSetSR mmix rAIx $ rA .|. (tripAEBit trip)
-- }}}

-- clear a event bit by trip type. {{{
tripClearABit :: MMIX -> TRIP -> IO ()
tripClearABit mmix trip = do
  rA <- mmixGetSR mmix rAIx
  mmixSetSR mmix rAIx $ rA .&. (complement $ tripAEBit trip)
-- }}}

-- issueTrip (set rA's event bit) {{{
issueTrip :: MMIX -> TRIP -> IO ()
issueTrip = tripSetABit
-- }}}

-- dynamic trap happened (set rQ bit) {{{

issueDTrap :: BitIx -> MMIX -> IO ()
issueDTrap ix mmix = do
  rQ <- mmixGetSR mmix rQIx
  mmixSetSR mmix rQIx $ bitSet1 ix rQ

issueDTrapPr :: MMIX -> IO ()
issueDTrapPr = issueDTrap rQFr

issueDTrapPw :: MMIX -> IO ()
issueDTrapPw = issueDTrap rQFw

issueDTrapPx :: MMIX -> IO ()
issueDTrapPx = issueDTrap rQFx

issueDTrapPn :: MMIX -> IO ()
issueDTrapPn = issueDTrap rQFn

issueDTrapPk :: MMIX -> IO ()
issueDTrapPk = issueDTrap rQFk

issueDTrapPb :: MMIX -> IO ()
issueDTrapPb = issueDTrap rQFb

issueDTrapPs :: MMIX -> IO ()
issueDTrapPs = issueDTrap rQFs

issueDTrapPp :: MMIX -> IO ()
issueDTrapPp = issueDTrap rQFp

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
  mmixSetSR mmix rXIx $ fldSet1 rXFsign $ fI insn
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
  mmixSetSR mmix rXXIx $ fldSet1 rXFsign $ fI insn
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
  mmixSetSR mmix rXXIx $ fldSet1 rXXFsign $ fldSet rXXFprog pBits $ fI insn
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

-- access function permission check (rwxnkbsp) {{{

checkPr :: PTE -> Bool
checkPr pte = 1 == bitGet pteBpr pte

checkPw :: PTE -> Bool
checkPw pte = 1 == bitGet pteBpw pte

checkPx :: PTE -> Bool
checkPx pte = 1 == bitGet pteBpx pte

checkPn :: VAddr -> Bool
checkPn vaddr = 1 == bitGet 63 vaddr

-- TODO: big impl
checkPk :: Insn -> Bool
checkPk insn = True

-- TODO: big impl
checkPb :: Insn -> Bool
checkPb insn = True

-- neg-addr or rK(prog) == 0xff => security OK
-- param: addr(i) and rK
checkPs :: VAddr -> Octa -> Bool
checkPs vaddr rK =
  (0 /= bitGet 63 vaddr) || (fldGet rKFprog rK == 0xff)

-- fail if insn come from negative address and rK(p) = 1
-- param: addr(i) and rK 
checkPp :: VAddr -> Octa -> Bool
checkPp vaddr rK =
  (1 /= bitGet 63 vaddr) || (bitGet rKBp rK == 0)

-- }}}

-- runMMIX and sub-routines {{{

-- run mmix
runMMIX :: MMIX -> IO ()
runMMIX mmix = do
  pcOK <- runPositionCheck mmix
  when pcOK $ runFetchExec mmix

-- runPositionCheck {{{
-- check insn can run at PC, True: OK
runPositionCheck :: MMIX -> IO Bool
runPositionCheck mmix = do
  pc <- mmixGetPC mmix
  if isVirtual pc
  then runDoChecks mmix
  else runDoCheckp mmix

-- check bit s of rK, issue Trap on fail 
runDoChecks :: MMIX -> IO Bool
runDoChecks mmix = do
  pc <- mmixGetPC mmix
  rK <- mmixGetSR mmix rKIx
  let sOK = checkPs pc rK
  unless sOK $ issueDTrapPs mmix
  return sOK

-- check bit p and rK, issue Trap on fail 
runDoCheckp :: MMIX -> IO Bool
runDoCheckp mmix = do
  pc <- mmixGetPC mmix
  rK <- mmixGetSR mmix rKIx
  let pOK = checkPp pc rK
  unless pOK $ issueDTrapPp mmix
  return pOK
-- }}}

-- runFetchExec {{{

-- fetch and execute insn
runFetchExec :: MMIX -> IO ()
runFetchExec mmix = do
  pc <- mmixGetPC mmix
  if isVirtual pc
  then do
    xOK <- runDoCheckx mmix pc
    when xOK $ do
      mbPAddr <- toPAddr mmix pc
      case mbPAddr of
        Just paddr -> runPAddr mmix paddr
        Nothing -> return ()
  else do
    mbPAddr' <- toPAddr mmix pc
    case mbPAddr' of
      Just paddr -> runPAddr mmix paddr
      Nothing -> return ()

-- check x bit of PTE or OK if neg-address
runDoCheckx :: MMIX -> VAddr -> IO Bool
runDoCheckx mmix vaddr = do
  pte <- vMapGetPTE0 mmix vaddr
  let xOK = 1 == bitGet pteBpx pte
  unless xOK $ issueDTrapPx mmix
  return xOK

-- }}}

-- exec on PAddr {{{
runPAddr :: MMIX -> PAddr -> IO ()
runPAddr mmix paddr = do
  mbInsn <- mmixLdTetra mmix paddr
  case mbInsn of
    Just insn -> runInsn mmix insn
    Nothing -> return ()
-- }}}

{-
runInsn :: MMIX -> Insn -> IO ()
runInsn mmix insn = --TODO: call runXXXinsn
-}

-- }}}

-- helpers for runXXInsn {{{

-- normal PC routines {{{

-- pc = pc + 4
incPC :: MMIX -> IO ()
incPC mmix = do
  pc <- mmixGetPC mmix
  mmixSetPC mmix (pc + 4)

-- }}}

-- Arithmetic function warppers {{{


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
  mapM (issueTrip mmix) $ arithGetEx re
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
  mapM (issueTrip mmix) $ arithGetEx re
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
  mapM (issueTrip mmix) $ arithGetEx re
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
-- }}}
  
-- FSUB 'floating subtract' {{{
runFSUB :: RunInsn
runFSUB mmix insn = do
  rdm <- toRoundModeRaw <$> mmixGetSR mmix rAIx
  (y,z) <- mmixGetGRFYZ mmix insn
  let re = fpSub rdm y z
  mapM (issueTrip mmix) $ arithGetEx re
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
  mapM (issueTrip mmix) $ arithGetEx re
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- FLOT 'convert fixed to floating' {{{
runFLOT :: RunInsn
runFLOT mmix insn = do
  rdm0 <- toRoundModeRaw <$> mmixGetSR mmix rAIx
  let y = iGetYu insn
  let rdm = fromMaybe rdm0 $ toRoundModeImm y
  z <- mmixGetGRFZ mmix insn
  let re = fpFloat rdm z
  mapM (issueTrip mmix) $ arithGetEx re
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
  mapM (issueTrip mmix) $ arithGetEx re
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
  mapM (issueTrip mmix) $ arithGetEx re
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
  mapM (issueTrip mmix) $ arithGetEx re
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
  mapM (issueTrip mmix) $ arithGetEx re
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
  mapM (issueTrip mmix) $ arithGetEx re
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
  mapM (issueTrip mmix) $ arithGetEx re
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
  mapM (issueTrip mmix) $ arithGetEx re
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
  mapM (issueTrip mmix) $ arithGetEx re
  mmixSetGRFX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- FCMPE 'floating compare (with respect to epsilon)' {{{
runFCMPE :: RunInsn
runFCMPE mmix insn = do
  (y,z) <- mmixGetGRFYZ mmix insn
  e <- mmixGetSRF mmix rEIx
  let re = fromOrdering <$> fpCompareEps e y z
  mapM (issueTrip mmix) $ arithGetEx re
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
  let re = fpEqualEps e y z
  mapM (issueTrip mmix) $ arithGetEx re
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- FDIV 'floating divide' {{{
runFDIV :: RunInsn
runFDIV mmix insn = do
  rdm <- toRoundModeRaw <$> mmixGetSR mmix rAIx
  (y,z) <- mmixGetGRFYZ mmix insn
  let re = fpDivide rdm y z
  mapM (issueTrip mmix) $ arithGetEx re
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
  mapM (issueTrip mmix) $ arithGetEx re
  mmixSetGRFX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- FREM 'floating remainder' {{{
runFREM :: RunInsn
runFREM mmix insn = do
  (y,z) <- mmixGetGRFYZ mmix insn
  let re = fpRem y z
  mapM (issueTrip mmix) $ arithGetEx re
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
  mapM (issueTrip mmix) $ arithGetEx re
  mmixSetGRFX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- MUL 'multiply' {{{
runMUL :: RunInsn
runMUL mmix insn = do
  (y,z) <- mmixGetGRYZ mmix insn
  let re = intMul y z
  mapM (issueTrip mmix) $ arithGetEx re
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- MULI 'multiply immediate' {{{
runMULI :: RunInsn
runMULI mmix insn = do
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  let re = intMul y z
  mapM (issueTrip mmix) $ arithGetEx re
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- MULU 'multiply unsigned' {{{
runMULU :: RunInsn
runMULU mmix insn = do
  (y,z) <- mmixGetGRYZ mmix insn
  let re = intMulu y z
  --mapM (issueTrip mmix) $ arithGetEx re
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
  --mapM (issueTrip mmix) $ arithGetEx re
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
  mapM (issueTrip mmix) $ arithGetEx re
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
  mapM (issueTrip mmix) $ arithGetEx re
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
  --mapM (issueTrip mmix) $ arithGetEx re
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
  --mapM (issueTrip mmix) $ arithGetEx re
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
  mapM (issueTrip mmix) $ arithGetEx re
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- ADDI {{{
runADDI :: RunInsn
runADDI mmix insn = do
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  let re = intAdd y z
  mapM (issueTrip mmix) $ arithGetEx re
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- ADDU {{{
runADDU :: RunInsn
runADDU mmix insn = do
  (y,z) <- mmixGetGRYZ mmix insn
  let re = intAddu y z
  --mapM (issueTrip mmix) $ arithGetEx re
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- ADDUI {{{
runADDUI :: RunInsn
runADDUI mmix insn = do
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  let re = intAddu y z
  --mapM (issueTrip mmix) $ arithGetEx re
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- SUB {{{
runSUB :: RunInsn
runSUB mmix insn = do
  (y,z) <- mmixGetGRYZ mmix insn
  let re = intSub y z
  mapM (issueTrip mmix) $ arithGetEx re
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- SUBI {{{
runSUBI :: RunInsn
runSUBI mmix insn = do
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  let re = intSub y z
  mapM (issueTrip mmix) $ arithGetEx re
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- SUBU {{{
runSUBU :: RunInsn
runSUBU mmix insn = do
  (y,z) <- mmixGetGRYZ mmix insn
  let re = intSubu y z
  --mapM (issueTrip mmix) $ arithGetEx re
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- SUBUI {{{
runSUBUI :: RunInsn
runSUBUI mmix insn = do
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  let re = intSubu y z
  --mapM (issueTrip mmix) $ arithGetEx re
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- IIADDU {{{
runIIADDU :: RunInsn
runIIADDU mmix insn = do
  (y,z) <- mmixGetGRYZ mmix insn
  let re = int2Addu y z
  --mapM (issueTrip mmix) $ arithGetEx re
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- IIADDUI {{{
runIIADDUI :: RunInsn
runIIADDUI mmix insn = do
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  let re = int2Addu y z
  --mapM (issueTrip mmix) $ arithGetEx re
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- IVADDU {{{
runIVADDU :: RunInsn
runIVADDU mmix insn = do
  (y,z) <- mmixGetGRYZ mmix insn
  let re = int4Addu y z
  --mapM (issueTrip mmix) $ arithGetEx re
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- IVADDUI {{{
runIVADDUI :: RunInsn
runIVADDUI mmix insn = do
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  let re = int4Addu y z
  --mapM (issueTrip mmix) $ arithGetEx re
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- VIIIADDU {{{
runVIIIADDU :: RunInsn
runVIIIADDU mmix insn = do
  (y,z) <- mmixGetGRYZ mmix insn
  let re = int8Addu y z
  --mapM (issueTrip mmix) $ arithGetEx re
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- VIIIADDUI {{{
runVIIIADDUI :: RunInsn
runVIIIADDUI mmix insn = do
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  let re = int8Addu y z
  --mapM (issueTrip mmix) $ arithGetEx re
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- XVIADDU {{{
runXVIADDU :: RunInsn
runXVIADDU mmix insn = do
  (y,z) <- mmixGetGRYZ mmix insn
  let re = int16Addu y z
  --mapM (issueTrip mmix) $ arithGetEx re
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- XVIADDUI {{{
runXVIADDUI :: RunInsn
runXVIADDUI mmix insn = do
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  let re = int16Addu y z
  --mapM (issueTrip mmix) $ arithGetEx re
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
  --mapM (issueTrip mmix) $ arithGetEx re
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- CMPI {{{
runCMPI :: RunInsn
runCMPI mmix insn = do
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  let re = fromOrdering <$> intCompare y z
  --mapM (issueTrip mmix) $ arithGetEx re
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- CMPU {{{
runCMPU :: RunInsn
runCMPU mmix insn = do
  (y,z) <- mmixGetGRYZ mmix insn
  let re = fromOrdering <$> intCompareu y z
  --mapM (issueTrip mmix) $ arithGetEx re
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- CMPUI {{{
runCMPUI :: RunInsn
runCMPUI mmix insn = do
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  let re = fromOrdering <$> intCompareu y z
  --mapM (issueTrip mmix) $ arithGetEx re
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- NEG {{{
runNEG :: RunInsn
runNEG mmix insn = do
  let y = iGetYu insn
  z <- mmixGetGRZ mmix insn
  let re = intSub y z
  mapM (issueTrip mmix) $ arithGetEx re
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- NEGI {{{
runNEGI :: RunInsn
runNEGI mmix insn = do
  let y = iGetYu insn
  let z = iGetZu insn
  let re = intSub y z
  mapM (issueTrip mmix) $ arithGetEx re
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- NEGU {{{
runNEGU :: RunInsn
runNEGU mmix insn = do
  let y = iGetYu insn
  z <- mmixGetGRZ mmix insn
  let re = intSubu y z
  --mapM (issueTrip mmix) $ arithGetEx re
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- NEGUI {{{
runNEGUI :: RunInsn
runNEGUI mmix insn = do
  let y = iGetYu insn
  let z = iGetZu insn
  let re = intSubu y z
  --mapM (issueTrip mmix) $ arithGetEx re
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- SL {{{
runSL :: RunInsn
runSL mmix insn = do
  (y,z) <- mmixGetGRYZ mmix insn
  let re = bitSL y z
  mapM (issueTrip mmix) $ arithGetEx re
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- SLI {{{
runSLI :: RunInsn
runSLI mmix insn = do
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  let re = bitSL y z
  mapM (issueTrip mmix) $ arithGetEx re
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- SLU {{{
runSLU :: RunInsn
runSLU mmix insn = do
  (y,z) <- mmixGetGRYZ mmix insn
  let re = bitSLu y z
  --mapM (issueTrip mmix) $ arithGetEx re
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- SLUI {{{
runSLUI :: RunInsn
runSLUI mmix insn = do
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  let re = bitSLu y z
  --mapM (issueTrip mmix) $ arithGetEx re
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- SR {{{
runSR :: RunInsn
runSR mmix insn = do
  (y,z) <- mmixGetGRYZ mmix insn
  let re = bitSR y z
  --mapM (issueTrip mmix) $ arithGetEx re
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- SRI {{{
runSRI :: RunInsn
runSRI mmix insn = do
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  let re = bitSR y z
  --mapM (issueTrip mmix) $ arithGetEx re
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- SRU {{{
runSRU :: RunInsn
runSRU mmix insn = do
  (y,z) <- mmixGetGRYZ mmix insn
  let re = bitSRu y z
  --mapM (issueTrip mmix) $ arithGetEx re
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- SRUI {{{
runSRUI :: RunInsn
runSRUI mmix insn = do
  y <- mmixGetGRY mmix insn
  let z = iGetZu insn
  let re = bitSRu y z
  --mapM (issueTrip mmix) $ arithGetEx re
  mmixSetGRX mmix insn $ arithGetRx re
  incPC mmix
-- }}}

-- }}}

-- TODO
-- 4* branch {{{

-- BN {{{
runBN :: RunInsn
runBN mmix insn = do
-- }}}

-- BNB {{{
runBNB :: RunInsn
runBNB mmix insn = do
-- }}}

-- BZ {{{
runBZ :: RunInsn
runBZ mmix insn = do
-- }}}

-- BZB {{{
runBZB :: RunInsn
runBZB mmix insn = do
-- }}}

-- BP {{{
runBP :: RunInsn
runBP mmix insn = do
-- }}}

-- BPB {{{
runBPB :: RunInsn
runBPB mmix insn = do
-- }}}

-- BOD {{{
runBOD :: RunInsn
runBOD mmix insn = do
-- }}}

-- BODB {{{
runBODB :: RunInsn
runBODB mmix insn = do
-- }}}

-- BNN {{{
runBNN :: RunInsn
runBNN mmix insn = do
-- }}}

-- BNNB {{{
runBNNB :: RunInsn
runBNNB mmix insn = do
-- }}}

-- BNZ {{{
runBNZ :: RunInsn
runBNZ mmix insn = do
-- }}}

-- BNZB {{{
runBNZB :: RunInsn
runBNZB mmix insn = do
-- }}}

-- BNP {{{
runBNP :: RunInsn
runBNP mmix insn = do
-- }}}

-- BNPB {{{
runBNPB :: RunInsn
runBNPB mmix insn = do
-- }}}

-- BEV {{{
runBEV :: RunInsn
runBEV mmix insn = do
-- }}}

-- BEVB {{{
runBEVB :: RunInsn
runBEVB mmix insn = do
-- }}}

-- }}}

-- TODO
-- 5* probable branch {{{

-- PBN {{{
runPBN :: RunInsn
runPBN mmix insn = do
-- }}}

-- PBNB {{{
runPBNB :: RunInsn
runPBNB mmix insn = do
-- }}}

-- PBZ {{{
runPBZ :: RunInsn
runPBZ mmix insn = do
-- }}}

-- PBZB {{{
runPBZB :: RunInsn
runPBZB mmix insn = do
-- }}}

-- PBP {{{
runPBP :: RunInsn
runPBP mmix insn = do
-- }}}

-- PBPB {{{
runPBPB :: RunInsn
runPBPB mmix insn = do
-- }}}

-- PBOD {{{
runPBOD :: RunInsn
runPBOD mmix insn = do
-- }}}

-- PBODB {{{
runPBODB :: RunInsn
runPBODB mmix insn = do
-- }}}

-- PBNN {{{
runPBNN :: RunInsn
runPBNN mmix insn = do
-- }}}

-- PBNNB {{{
runPBNNB :: RunInsn
runPBNNB mmix insn = do
-- }}}

-- PBNZ {{{
runPBNZ :: RunInsn
runPBNZ mmix insn = do
-- }}}

-- PBNZB {{{
runPBNZB :: RunInsn
runPBNZB mmix insn = do
-- }}}

-- PBNP {{{
runPBNP :: RunInsn
runPBNP mmix insn = do
-- }}}

-- PBNPB {{{
runPBNPB :: RunInsn
runPBNPB mmix insn = do
-- }}}

-- PBEV {{{
runPBEV :: RunInsn
runPBEV mmix insn = do
-- }}}

-- PBEVB {{{
runPBEVB :: RunInsn
runPBEVB mmix insn = do
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
runLDB mmix insn = do
-- }}}

-- LDBI {{{
runLDBI :: RunInsn
runLDBI mmix insn = do
-- }}}

-- LDBU {{{
runLDBU :: RunInsn
runLDBU mmix insn = do
-- }}}

-- LDBUI {{{
runLDBUI :: RunInsn
runLDBUI mmix insn = do
-- }}}

-- LDW {{{
runLDW :: RunInsn
runLDW mmix insn = do
-- }}}

-- LDWI {{{
runLDWI :: RunInsn
runLDWI mmix insn = do
-- }}}

-- LDWU {{{
runLDWU :: RunInsn
runLDWU mmix insn = do
-- }}}

-- LDWUI {{{
runLDWUI :: RunInsn
runLDWUI mmix insn = do
-- }}}

-- LDT {{{
runLDT :: RunInsn
runLDT mmix insn = do
-- }}}

-- LDTI {{{
runLDTI :: RunInsn
runLDTI mmix insn = do
-- }}}

-- LDTU {{{
runLDTU :: RunInsn
runLDTU mmix insn = do
-- }}}

-- LDTUI {{{
runLDTUI :: RunInsn
runLDTUI mmix insn = do
-- }}}

-- LDO {{{
runLDO :: RunInsn
runLDO mmix insn = do
-- }}}

-- LDOI {{{
runLDOI :: RunInsn
runLDOI mmix insn = do
-- }}}

-- LDOU {{{
runLDOU :: RunInsn
runLDOU mmix insn = do
-- }}}

-- LDOUI {{{
runLDOUI :: RunInsn
runLDOUI mmix insn = do
-- }}}

-- }}}

-- 9* system ?? {{{

-- LDSF {{{
runLDSF :: RunInsn
runLDSF mmix insn = do
-- }}}

-- LDSFI {{{
runLDSFI :: RunInsn
runLDSFI mmix insn = do
-- }}}

-- LDHT {{{
runLDHT :: RunInsn
runLDHT mmix insn = do
-- }}}

-- LDHTI {{{
runLDHTI :: RunInsn
runLDHTI mmix insn = do
-- }}}

-- CSWAP {{{
runCSWAP :: RunInsn
runCSWAP mmix insn = do
-- }}}

-- CSWAPI {{{
runCSWAPI :: RunInsn
runCSWAPI mmix insn = do
-- }}}

-- LDUNC {{{
runLDUNC :: RunInsn
runLDUNC mmix insn = do
-- }}}

-- LDUNCI {{{
runLDUNCI :: RunInsn
runLDUNCI mmix insn = do
-- }}}

-- LDVTS {{{
runLDVTS :: RunInsn
runLDVTS mmix insn = do
-- }}}

-- LDVTSI {{{
runLDVTSI :: RunInsn
runLDVTSI mmix insn = do
-- }}}

-- PRELD {{{
runPRELD :: RunInsn
runPRELD mmix insn = do
-- }}}

-- PRELDI {{{
runPRELDI :: RunInsn
runPRELDI mmix insn = do
-- }}}

-- PREGO {{{
runPREGO :: RunInsn
runPREGO mmix insn = do
-- }}}

-- PREGOI {{{
runPREGOI :: RunInsn
runPREGOI mmix insn = do
-- }}}

-- GO {{{
runGO :: RunInsn
runGO mmix insn = do
-- }}}

-- GOI {{{
runGOI :: RunInsn
runGOI mmix insn = do
-- }}}

-- }}}

-- a* store {{{

-- STB {{{
runSTB :: RunInsn
runSTB mmix insn = do
-- }}}

-- STBI {{{
runSTBI :: RunInsn
runSTBI mmix insn = do
-- }}}

-- STBU {{{
runSTBU :: RunInsn
runSTBU mmix insn = do
-- }}}

-- STBUI {{{
runSTBUI :: RunInsn
runSTBUI mmix insn = do
-- }}}

-- STW {{{
runSTW :: RunInsn
runSTW mmix insn = do
-- }}}

-- STWI {{{
runSTWI :: RunInsn
runSTWI mmix insn = do
-- }}}

-- STWU {{{
runSTWU :: RunInsn
runSTWU mmix insn = do
-- }}}

-- STWUI {{{
runSTWUI :: RunInsn
runSTWUI mmix insn = do
-- }}}

-- STT {{{
runSTT :: RunInsn
runSTT mmix insn = do
-- }}}

-- STTI {{{
runSTTI :: RunInsn
runSTTI mmix insn = do
-- }}}

-- STTU {{{
runSTTU :: RunInsn
runSTTU mmix insn = do
-- }}}

-- STTUI {{{
runSTTUI :: RunInsn
runSTTUI mmix insn = do
-- }}}

-- STO {{{
runSTO :: RunInsn
runSTO mmix insn = do
-- }}}

-- STOI {{{
runSTOI :: RunInsn
runSTOI mmix insn = do
-- }}}

-- STOU {{{
runSTOU :: RunInsn
runSTOU mmix insn = do
-- }}}

-- STOUI {{{
runSTOUI :: RunInsn
runSTOUI mmix insn = do
-- }}}

-- }}}

-- b* system ? {{{

-- STSF {{{
runSTSF :: RunInsn
runSTSF mmix insn = do
-- }}}

-- STSFI {{{
runSTSFI :: RunInsn
runSTSFI mmix insn = do
-- }}}

-- STHT {{{
runSTHT :: RunInsn
runSTHT mmix insn = do
-- }}}

-- STHTI {{{
runSTHTI :: RunInsn
runSTHTI mmix insn = do
-- }}}

-- STCO {{{
runSTCO :: RunInsn
runSTCO mmix insn = do
-- }}}

-- STCOI {{{
runSTCOI :: RunInsn
runSTCOI mmix insn = do
-- }}}

-- STUNC {{{
runSTUNC :: RunInsn
runSTUNC mmix insn = do
-- }}}

-- STUNCI {{{
runSTUNCI :: RunInsn
runSTUNCI mmix insn = do
-- }}}

-- SYNCD {{{
runSYNCD :: RunInsn
runSYNCD mmix insn = do
-- }}}

-- SYNCDI {{{
runSYNCDI :: RunInsn
runSYNCDI mmix insn = do
-- }}}

-- PREST {{{
runPREST :: RunInsn
runPREST mmix insn = do
-- }}}

-- PRESTI {{{
runPRESTI :: RunInsn
runPRESTI mmix insn = do
-- }}}

-- SYNCID {{{
runSYNCID :: RunInsn
runSYNCID mmix insn = do
-- }}}

-- SYNCIDI {{{
runSYNCIDI :: RunInsn
runSYNCIDI mmix insn = do
-- }}}

-- PUSHGO {{{
runPUSHGO :: RunInsn
runPUSHGO mmix insn = do
-- }}}

-- PUSHGOI {{{
runPUSHGOI :: RunInsn
runPUSHGOI mmix insn = do
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

-- TODO
-- e* {{{

-- SETH {{{
runSETH :: RunInsn
runSETH mmix insn = do
-- }}}

-- SETMH {{{
runSETMH :: RunInsn
runSETMH mmix insn = do
-- }}}

-- SETML {{{
runSETML :: RunInsn
runSETML mmix insn = do
-- }}}

-- SETL {{{
runSETL :: RunInsn
runSETL mmix insn = do
-- }}}

-- INCH {{{
runINCH :: RunInsn
runINCH mmix insn = do
-- }}}

-- INCMH {{{
runINCMH :: RunInsn
runINCMH mmix insn = do
-- }}}

-- INCML {{{
runINCML :: RunInsn
runINCML mmix insn = do
-- }}}

-- INCL {{{
runINCL :: RunInsn
runINCL mmix insn = do
-- }}}

-- ORH {{{
runORH :: RunInsn
runORH mmix insn = do
-- }}}

-- ORMH {{{
runORMH :: RunInsn
runORMH mmix insn = do
-- }}}

-- ORML {{{
runORML :: RunInsn
runORML mmix insn = do
-- }}}

-- ORL {{{
runORL :: RunInsn
runORL mmix insn = do
-- }}}

-- ANDNH {{{
runANDNH :: RunInsn
runANDNH mmix insn = do
-- }}}

-- ANDNMH {{{
runANDNMH :: RunInsn
runANDNMH mmix insn = do
-- }}}

-- ANDNML {{{
runANDNML :: RunInsn
runANDNML mmix insn = do
-- }}}

-- ANDNL {{{
runANDNL :: RunInsn
runANDNL mmix insn = do
-- }}}

-- }}}

-- f* {{{

-- JMP {{{
runJMP :: RunInsn
runJMP mmix insn = do
-- }}}

-- JMPB {{{
runJMPB :: RunInsn
runJMPB mmix insn = do
-- }}}

-- PUSHJ {{{
runPUSHJ :: RunInsn
runPUSHJ mmix insn = do
-- }}}

-- PUSHJB {{{
runPUSHJB :: RunInsn
runPUSHJB mmix insn = do
-- }}}

-- GETA {{{
runGETA :: RunInsn
runGETA mmix insn = do
-- }}}

-- GETAB {{{
runGETAB :: RunInsn
runGETAB mmix insn = do
-- }}}

-- PUT {{{
runPUT :: RunInsn
runPUT mmix insn = do
-- }}}

-- PUTI {{{
runPUTI :: RunInsn
runPUTI mmix insn = do
-- }}}

-- POP {{{
runPOP :: RunInsn
runPOP mmix insn = do
-- }}}

-- RESUME {{{
runRESUME :: RunInsn
runRESUME mmix insn = do
-- }}}

-- SAVE {{{
runSAVE :: RunInsn
runSAVE mmix insn = do
-- }}}

-- UNSAVE {{{
runUNSAVE :: RunInsn
runUNSAVE mmix insn = do
-- }}}

-- SYNC {{{
runSYNC :: RunInsn
runSYNC mmix insn = do
-- }}}

-- SWYM {{{
runSWYM :: RunInsn
runSWYM mmix insn = do
-- }}}

-- GET {{{
runGET :: RunInsn
runGET mmix insn = do
-- }}}

-- TRIP {{{
runTRIP :: RunInsn
runTRIP mmix insn = do
-- }}}

-- }}}

-- }}}

-- vim: fdm=marker

