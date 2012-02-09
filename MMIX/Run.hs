-- Copyright 2012 Wu Xingbo <wuxb45@gmail.com>

module MMIX.Run where

import Data.Maybe (fromMaybe)
import Data.Bits (xor, shiftL, shiftR, (.|.), (.&.))

import MMIX.Basics
import MMIX.OMem


-- type VMapParam: (base, limit, pn, nval) {{{
type VMapParam = (Octa, Octa, Octa, Octa)
-- }}}

-- type VStartPoint: (nval, len, indexList) {{{
type VStartPoint = (Octa, Octa, [Octa])
-- }}}

-- vMapChecks {{{

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
-- check bit 63 and <n>
vMapGuardPTP :: Octa -> PTP -> Maybe PTP
vMapGuardPTP nval ptp =
  if (ptp `xor` nval) .&. ptpmask == 0
  then return ptp
  else Nothing
  where ptpmask = fldMask (63,63) .|. fldMask rVFn
-- }}}

-- guard for check n field match pte {{{
-- check <n>
vMapGuardPTE :: Octa -> PTE -> Maybe PTE
vMapGuardPTE nval pte =
  if (pte `xor` nval) .&. ptemask == 0
  then return pte
  else Nothing
  where ptemask = fldMask rVFn
-- }}}

-- guard for base < limit {{{
vMapGuardLimit :: Octa -> Octa -> a -> Maybe a
vMapGuardLimit base limit a =
  if base < limit
  then return a
  else Nothing
-- }}}

-- convert page-number to page-index list {{{
vMapGetPIxList :: Octa -> [Octa]
vMapGetPIxList 0 = []
vMapGetPIxList pn = reverse $ pnIx:(vMapGetPIxList pnShift)
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

-- top function: get PTE from Memory Device {{{
vMapGetPTE :: MMIX -> VAddr -> IO PTE
vMapGetPTE mmix vaddr = do
  rV <- mmixGetSR mmix rVIx
  let mbVMapParam = vMapPrepare vaddr rV
  let mbStartPoint = mbVMapParam >>= vMapStartPoint
  case mbStartPoint of
    Just point -> fromMaybe 0 `fmap` vMapLookup mmix point
    Nothing -> return 0
-- }}}

-- load PTX (PTE/PTP) from Memory {{{
vMapLoadPTX :: MMIX -> PAddr -> PAddr -> IO (Maybe Octa)
vMapLoadPTX mmix base index = mmixLdOcta mmix $ base + (index `shiftL` 3)
-- }}}

-- load the PTE (maybe plus some PTP) {{{
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

-- vim: fdm=marker
