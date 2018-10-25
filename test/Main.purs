module Test.Main where

import Data.Array as Array
import Data.Foldable as F
import Data.Map as Map
import Data.Maybe (Maybe, fromJust, maybe)
import Data.Newtype as Newtype
import Data.NonEmpty (fromNonEmpty)
import Data.NonEmpty.Indexed as Indexed
import Data.Set as Set
import Data.Traversable as T
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Console as Console
import Math as Math
import Partial.Unsafe (unsafePartial, unsafePartialBecause)
import Prelude
import Test.QuickCheck (Result, arbitrary, quickCheckGen, (<?>))
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Gen as Gen

import Math.Probability (joinDists, marginalize)
import Math.Probability.Dist (Dist)
import Math.Probability.Dist as Dist
import Math.Probability.Information (divergence, entropy, entropyNum, mutualInformation, nonCond)
import Math.Probability.Prob.Number (Prob(..))

type Dist' = Dist Prob

-- TODO: Finish out properties. But this is all plagued with floating point issues.
main :: Effect Unit
main = do
  Console.log "H(X) >= 0"
  quickCheckGen $ entPos <$> genStringDist
  Console.log "H(X|Y) <= H(X)"
  quickCheckGen $ condRed <$> genStringDistPair
  Console.log "H(X,Y) <= H(X) + H(Y)"
  quickCheckGen $ indepLimit <$> genStringDistPair
  Console.log "H(X,Y) = H(X|Y) + H(Y)"
  quickCheckGen $ entChain <$> genStringDistPair
  Console.log "I(X;Y) >= 0"
  quickCheckGen $ infoPos <$> genStringDistPair
  -- Console.log "I(X;Y) = H(X) - H(X|Y)"
  -- quickCheckGen $ infoEnt <$> genStringDistPair
  -- Console.log "I(X;Y|Z) = H(X|Z) - H(X|Y,Z)"
  -- quickCheck condInfoEnt
  -- Console.log "I(X1,X2;Y) = I(X2;Y|X1) + Y(X1|Y)"
  -- quickCheck infoChain
  Console.log "D(p(x)||q(x)) >= 0"
  quickCheckGen $ divPos <$> genStringDivPair
  -- Console.log "D(p(x,y)||p(x)p(y)) = I(X;Y)"
  -- quickCheckGen $ divInfo <$> genStringDistPair
  -- Console.log "D(p(x,y)||q(x,y)) = D(p(x)||q(x)) + D(p(y|x)||q(y|x))"
  -- quickCheck divChain

epsilon = 0.05
approxEq a b = Math.abs (a - b) < epsilon
infix 4 approxEq as ~~
approxLt a b = a - b < epsilon
infix 4 approxLt as <~

divInfo :: DistPair String String -> Result
divInfo i@(DistPair ys xs'y) =
  i_yxs ~~ d_yxs_ysxs <?> Array.intercalate "\n" [show i, show i_yxs, show d_yxs_ysxs] where
    yxs = joinDists Tuple ys xs'y
    xs = marginalize snd yxs
    ysxs = joinDists Tuple ys (const xs)
    i_yxs = entropyNum.to $ nonCond mutualInformation yxs fst snd
    d_yxs_ysxs = entropyNum.to $ divergence (pure unit) (const yxs) (const ysxs)

divPos :: DivPair String -> Result
divPos i@(DivPair p q) =
  0.0 <~ div <?> Array.intercalate "\n" [show p, show q, show div]
  where
    div = entropyNum.to (divergence (pure unit) (const p) (const q))

infoEnt :: DistPair String String -> Result
infoEnt i@(DistPair ys xs'y) =
  i_yxs ~~ e_xs - e_xs'y <?> Array.intercalate "\n" [show i, show yxs, show xs, show i_yxs, show e_xs, show e_xs'y] where
    yxs = joinDists Tuple ys xs'y
    xs = marginalize snd yxs
    i_yxs = entropyNum.to $ nonCond mutualInformation yxs fst snd
    e_xs = wrapEnt xs
    e_xs'y = entropyNum.to $ entropy ys xs'y

entChain :: DistPair String String -> Result
entChain i@(DistPair ys xs'y) =
  e_xs'y + e_ys ~~ e_xys <?> Array.intercalate "\n" [show i, show e_xs'y, show e_ys,  show e_xys] where
    e_xs'y = entropyNum.to $ entropy ys xs'y
    e_ys = wrapEnt ys
    e_xys = wrapEnt $ joinDists Tuple ys xs'y

condRed :: forall a b. Ord a => Show a => Ord b => Show b => DistPair a b -> Result
condRed i@(DistPair ys xs'y) =
  e_xs'y <~ e_xs <?> Array.intercalate "\n" [show i, show e_xs'y, show e_xs]
  where
    e_xs'y = entropyNum.to $ entropy ys xs'y
    xs = marginalize snd $ joinDists Tuple ys xs'y
    e_xs = wrapEnt xs

indepLimit :: DistPair String String -> Result
indepLimit i@(DistPair ys xs'y) =
  e_yxs <~ e_xs + e_ys <?> Array.intercalate "\n" [show i, show e_yxs, show e_xs, show e_ys] where
    yxs = joinDists Tuple ys xs'y
    xs = marginalize snd yxs
    e_yxs = wrapEnt yxs
    e_xs = wrapEnt xs
    e_ys = wrapEnt ys

entPos :: Dist' String -> Result
entPos d = 0.0 <~ wrapEnt d <?> show d

infoPos :: DistPair String String -> Result
infoPos i@(DistPair ys xs'y) =
  0.0 <~ i_yxs <?> Array.intercalate "\n" [show i, show i_yxs] where
    yxs = joinDists Tuple ys xs'y
    i_yxs = entropyNum.to $ nonCond mutualInformation yxs fst snd

wrapEnt :: forall a. Ord a => Dist' a -> Number
wrapEnt = entropyNum.to <<< nonCond entropy

genDist :: forall a. Ord a => Gen a -> Gen (Dist' a)
genDist gen = go
  where
    go = do
      p <- fromFreqs <<< coarsenPairs <<< normalizePairs <$> Gen.arrayOf (Tuple <$> gen <*> (MkProb <$> Gen.choose 0.10 1.0))
      maybe go pure $ p

genStringDist :: Gen (Dist' String)
genStringDist = genDist arbitrary

data DistPair a b = DistPair (Dist' a) (a -> Dist' b)
instance showDistPair :: (Ord a, Show a, Show b) => Show (DistPair a b) where
  show (DistPair a b)= "(DistPair " <> show a <> " " <> show mp <> ")"
    where
      mp = Map.fromFoldable <<< map (\a -> Tuple a (b a)) <<< asArray <<< Set.toUnfoldable <<< fromNonEmpty Set.insert <<< Dist.values $ a
      asArray :: forall x. Array x -> Array x
      asArray = identity

genDistPair :: forall a b. Ord a => Gen a -> Gen (Dist' b) -> Gen (DistPair a b)
genDistPair genA genB = do
  d <- genDist genA
  m <- Map.fromFoldable <$> T.traverse (\s -> Tuple s <$> genB) (asArray <<< Set.toUnfoldable <<< fromNonEmpty Set.insert <<< Dist.values $ d)
  pure <<< DistPair d <<< unsafePartial $ (\a -> fromJust $ a `Map.lookup` m)
  where
    asArray :: forall x. Array x -> Array x
    asArray = identity

genStringDistPair :: Gen (DistPair String String)
genStringDistPair = genDistPair arbitrary (genDist arbitrary)

data DivPair a = DivPair (Dist' a) (Dist' a)

genDivPair :: forall a. Ord a => Gen a -> Gen (DivPair a)
genDivPair gen = do
  d' <- genDist gen
  let states = Set.toUnfoldable <<< fromNonEmpty Set.insert <<< Dist.values $ d'
  d <- zipDist states <$> (probListArb $ F.length states)
  pure $ DivPair d' d

coarsenPairs :: forall a. Array (Tuple a Prob) -> Array (Tuple a Prob)
coarsenPairs = (\l -> mapHead (second $ (_ + (top - sum l))) l) <<< map (second $ Newtype.over MkProb $ \n -> Math.round (n * 100.0) / 100.0)
  where
    mapHead f = maybe mempty (\{head, tail} -> head Array.: tail) <<< map (\{head, tail} -> { head: f head, tail }) <<< Array.uncons
    sum = F.sum <<< map snd

coarsen :: Array Number -> Array Number
coarsen = (\l -> mapHead (_ + (1.0 - F.sum l)) l) <<< map (\n -> Math.round (n * 100.0) / 100.0)
  where
    mapHead f = maybe mempty (\{head, tail} -> head Array.: tail) <<< map (\{head, tail} -> { head: f head, tail }) <<< Array.uncons

second f (Tuple a b) = Tuple a (f b)

fromFreqs :: forall a. Ord a => Array (Tuple a Prob) -> Maybe (Dist' a)
fromFreqs = map Dist.make <<< nonEmptyMap <<< Map.fromFoldable

zipDist :: forall a. Ord a => Array a -> Array Prob -> Dist' a
zipDist keys ps = unsafePartialBecause "Test code" $ Dist.make <<< fromJust <<< nonEmptyMap <<< Map.fromFoldable $ Array.zip keys ps

genStringDivPair :: Gen (DivPair String)
genStringDivPair = genDivPair arbitrary

normalize :: Array Number -> Array Number
normalize ns = let s = F.sum ns in flip (/) s <$> ns

normalizePairs :: forall a. Array (Tuple a Prob) -> Array (Tuple a Prob)
normalizePairs xs = second (_ / s) <$> xs
  where
    s = F.sum <<< map snd $ xs

probListArb :: Int -> Gen (Array Prob)
probListArb n =
  map MkProb <<< coarsen <<< normalize <$> Gen.vectorOf n (Gen.choose 0.10 1.0)

nonEmptyMap m =
  (\l -> (Tuple l.key l.value) Indexed.:| (l.key `Map.delete` m)) <$> Map.findMin m
