{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

module Misc where

import Debug.Trace
import Data.Maybe
import Control.Arrow
import Control.Lens
import Data.List
import qualified Data.List.NonEmpty as NE
import qualified Data.IntMap.Strict as M
import qualified Data.Sequence as S
import qualified Data.Foldable as F (toList)
import qualified Data.Vector as V

debug :: c -> String -> c
debug = flip trace

tracef :: Show r => (String, a -> r) -> a -> a
tracef (msg, f) v = trace (msg++" "++ show (f v)) v

debugf :: Show r => a -> (String, a -> r) -> a
debugf v (msg, f) = trace (msg++" "++ show (f v)) v

traceThis :: (Show a) => a -> a
traceThis x = trace (show x) x

traceThisM :: (Show a) => String -> a -> a
traceThisM msg x = trace (msg++" "++show x) x

traceShow :: (Show a) => a -> b -> b
traceShow = trace . show

(\>) = (&)
infixl 1 \>

toggleMaybe :: a1 -> Maybe a -> Maybe a1
toggleMaybe v m =
  if isJust m then Nothing
  else Just v

toggleList :: Foldable t => [t1] -> t a -> [t1]
toggleList v lst =
  if null lst then v
  else []


toggleMap :: M.IntMap a1 -> M.IntMap a -> M.IntMap a1
toggleMap v m =
  if M.null m then v
  else M.empty

toggleMapEntry :: Int -> a -> M.IntMap a -> M.IntMap a
toggleMapEntry ind v m =
  if isJust $ m&M.lookup ind then
    m&M.delete ind
  else
    m&M.insert ind v

toggleMapIndex :: M.Key -> a -> M.IntMap a -> M.IntMap a
toggleMapIndex i v m =
  m&M.lookup i & maybe
    (m&M.insert i v)
    (const $ m&M.delete i)

toggleSeq :: (Eq a, Monoid a, Monoid a1) => a1 -> a -> a1
toggleSeq v s =
  if s == mempty then v
  else mempty

mapInd :: (Enum b, Num b) => (a -> b -> c) -> [a] -> [c]
mapInd f l = zipWith f l [0..]


replaceAtIndex :: Int -> (a -> a) -> [a] -> [a]
replaceAtIndex n f ls = a ++ (f e : b)
  where (a, e:b) = splitAt n ls


replaceMaybeAtIndex :: Int -> (b -> b) -> [Maybe b] -> [Maybe b]
replaceMaybeAtIndex mutateIndex mutator items =
  if mutateIndex < length (catMaybes items) then
    replaceAtIndex replaceInd (fmap mutator) items
  else items
  where
    replaceInd =
      (
      mapInd (\n ind -> fmap (const ind) n)
      >>> catMaybes
      >>> drop mutateIndex
      >>> take 1
      >>> last) items

safeCycle :: [t] -> [t]
safeCycle l =
  case l of
    [] -> []
    xs -> cycle xs


boundedAdd :: (Num a, Ord a) => a -> a -> a -> a -> a
boundedAdd minVal maxVal step v =
  min (max minVal (v+step)) maxVal


cycleAdd :: Integral a => a -> a -> a -> a -> a
cycleAdd minVal maxVal step v =
  if added < minVal then
    maxVal - (minVal-added-1)
  else if added > maxVal then
    minVal + (added-maxVal-1)
  else
    added
  where
    delta = maxVal-minVal
    added = v+(step `mod` (delta+1))



takeEveryN :: Int -> [a] -> [a]
takeEveryN n = map head . takeWhile (not . null) . iterate (drop n)


third :: (t, t1, t2) -> t2
third (_,_,x) = x

lmoveToHead :: Int -> [a] -> [a]
lmoveToHead n xs = x : prev ++ post
  where (prev, x:post) = splitAt n xs

smoveToHead :: Int -> S.Seq a -> S.Seq a
smoveToHead n xs =
  S.fromList $ lmoveToHead n (F.toList xs)

lmoveItemToHead :: Eq a => a -> [a] -> [a]
lmoveItemToHead item list =
  list&elemIndex item
  &maybe list
  (\ind -> list&lmoveToHead ind)

smoveItemToHead :: Eq a => a -> S.Seq a -> S.Seq a
smoveItemToHead item sq =
  sq&S.elemIndexL item
  &maybe sq
  (\ind -> sq&smoveToHead ind)

lmoveItemToBack:: Eq a => a -> [a] -> [a]
lmoveItemToBack item list =
  list&elemIndex item
  &maybe list
  (\ind -> list&lmoveToBack ind)


smoveItemToBack:: Eq a => a -> S.Seq a -> S.Seq a
smoveItemToBack item sq =
  sq&S.elemIndexL item
  &maybe sq
  (\ind -> sq&smoveToBack ind)


lmoveToBack:: Int -> [a] -> [a]
lmoveToBack n xs = prev ++ post ++ [x]
  where (prev, x:post) = splitAt n xs


smoveToBack:: Int -> S.Seq a -> S.Seq a
smoveToBack n xs =
  S.fromList $ lmoveToBack n (F.toList xs)


adjustListLen :: a -> Int -> [a] -> [a]
adjustListLen defVal newLen l =
  take newLen (l++(repeat defVal))


neHead :: Functor f => (a -> f a) -> NE.NonEmpty a -> f (NE.NonEmpty a)
neHead f (frst NE.:| rest) =  fmap (\newFirst -> newFirst NE.:| rest) (f frst)


trimList :: (a -> Bool) -> [a] -> [a]
trimList emptyPred list =
  let
    emptyLen = list & (reverse>>>takeWhile emptyPred>>>length)
    len = list&length
  in
    take (len - emptyLen) list


ffoldl :: Foldable t => b -> (b -> a -> b)  -> t a -> b
ffoldl = flip foldl'


highestMapIndex :: M.IntMap b -> M.Key
highestMapIndex mp =
  mp\>M.foldlWithKey' (\largest k _ ->
    if k > largest then k
    else largest
  ) 0


sparseListFromMap :: M.IntMap b -> [Maybe b]
sparseListFromMap mp =
  sparseListFrom' (highestMapIndex mp) mp


sparseListFrom' :: (Index s ~ Int, Ixed s) => Int -> s -> [Maybe (IxValue s)]
sparseListFrom' maxLen xs =
  [0 .. maxLen]
  \> map (\i -> xs^?ix i)


mapFromSparseList :: Foldable t => t (Maybe a) -> M.IntMap a
mapFromSparseList lst =
  (lst&foldl' (\(m, ind) mbX ->
    mbX&maybe (m, ind+1) (\x -> (m&M.insert ind x, ind+1))
  ) (mempty, 0))
  & fst


adjustMapWithDefault :: a -> M.Key -> (a -> a) -> M.IntMap a -> M.IntMap a
adjustMapWithDefault def key f m =
  let x = M.findWithDefault def key m
  in M.adjust f key (M.insert key x m)


revMapLookup :: (a -> Bool) -> M.IntMap a -> Maybe M.Key
revMapLookup f =
  M.assocs
  >>> find (\(_,v)->f v)
  >>> fmap fst



editVectorSlice :: Int -> Int -> (V.Vector a -> V.Vector a) -> V.Vector a -> V.Vector a
editVectorSlice start end f vec =
  V.concat [
    V.take start vec
    , f $ V.slice start subLen vec
    , V.drop (start+subLen) vec
  ]
  where
    subLen = end-start



sequenceMaps :: Int -> [M.IntMap a] -> M.IntMap a
sequenceMaps offset = \case
  [x] -> x
  frstMap:restMaps ->
    restMaps
    & ffoldl (frstMap,1) (\(accum, mpInd) mp ->
        (mp&M.foldlWithKey' (innerFold mpInd) accum, mpInd+1)
      )
    & fst
  [] -> M.empty
  where
    innerFold mpInd accum keyInd x =
      accum&M.insert (mpInd*offset + keyInd) x


modMapRange :: M.Key -> M.Key -> (Maybe a -> Maybe a) -> M.IntMap a -> M.IntMap a
modMapRange start end f m =
  [start..end]
  & foldl' (\accum ind ->
    let
      mbNewVal = f $ M.lookup ind accum
    in
      mbNewVal&maybe (accum&M.delete ind) (\newVal ->
        accum&M.insert ind newVal
      )
    ) m



moveMapIndices :: M.Key -> M.Key -> M.IntMap a -> M.IntMap a
moveMapIndices maxVal step =
  M.foldlWithKey' moveInd mempty
  where
    moveInd accum k v =
      accum&M.insert (cycleAdd 0 maxVal step k) v















