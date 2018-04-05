module Test.Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Console as Console
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Data.Array as A
import Data.Foldable as F
import Data.Map as M
import Data.Maybe (fromJust, maybe)
import Data.Traversable as T
import Data.Tuple (Tuple(..), fst, snd)
import Partial.Unsafe (unsafePartial, unsafePartialBecause)
import Prelude
import Test.QuickCheck (Result, arbitrary, quickCheckGen, (<?>))
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Gen as Gen

import Math.Probability (extract, fromFreqs, joinDists, marginalize, prob, probList, zipDist)
import Math.Probability.Internal (Dist, ProbList, (<~), (~~))
import Math.Probability.Information (divergence, entropy, entropyNum, mutualInformation, nonCond)

-- TODO: Finish out properties
main :: forall eff. Eff (console :: CONSOLE, exception :: EXCEPTION, random :: RANDOM | eff) Unit
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
  Console.log "I(X;Y) = H(X) - H(X|Y)"
  quickCheckGen $ infoEnt <$> genStringDistPair
  -- Console.log "I(X;Y|Z) = H(X|Z) - H(X|Y,Z)"
  -- quickCheck condInfoEnt
  -- Console.log "I(X1,X2;Y) = I(X2;Y|X1) + Y(X1|Y)"
  -- quickCheck infoChain
  Console.log "D(p(x)||q(x)) >= 0"
  quickCheckGen $ divPos <$> genStringDivPair
  Console.log "D(p(x,y)||p(x)p(y)) = I(X;Y)"
  quickCheckGen $ divInfo <$> genStringDistPair
  -- Console.log "D(p(x,y)||q(x,y)) = D(p(x)||q(x)) + D(p(y|x)||q(y|x))"
  -- quickCheck divChain

divInfo :: DistPair String String -> Result
divInfo (DistPair ys xs'y) =
  i_yxs ~~ d_yxs_ysxs <?> show ys where
    yxs = joinDists Tuple ys xs'y
    xs = marginalize snd yxs
    ysxs = joinDists Tuple ys (const xs)
    i_yxs = entropyNum.to $ nonCond mutualInformation yxs fst snd
    d_yxs_ysxs = entropyNum.to $ divergence (pure unit) (const yxs) (const ysxs)

divPos :: DivPair String -> Result
divPos (DivPair p q) =
  0.0 <~ entropyNum.to (divergence (pure unit) (const p) (const q)) <?>
  show p <> " " <> show q

infoEnt :: DistPair String String -> Result
infoEnt (DistPair ys xs'y) =
  i_yxs ~~ e_xs - e_xs'y <?> show ys where
    yxs = joinDists Tuple ys xs'y
    xs = marginalize snd yxs
    i_yxs = entropyNum.to $ nonCond mutualInformation yxs fst snd
    e_xs = wrapEnt xs
    e_xs'y = entropyNum.to $ entropy ys xs'y

entChain :: DistPair String String -> Result
entChain (DistPair ys xs'y) =
  e_xs'y + e_ys ~~ e_xys <?> show ys where
    e_xs'y = entropyNum.to $ entropy ys xs'y
    e_ys = wrapEnt ys
    e_xys = wrapEnt $ joinDists Tuple ys xs'y

condRed :: forall a b. Show a => Eq b => DistPair a b -> Result
condRed (DistPair ys xs'y) =
  e_xs'y <~ e_xs <?> show ys
  where
    e_xs'y = entropyNum.to $ entropy ys xs'y
    xs = marginalize snd $ joinDists Tuple ys xs'y
    e_xs = wrapEnt xs

indepLimit :: DistPair String String -> Result
indepLimit (DistPair ys xs'y) =
  e_yxs <~ e_xs + e_ys <?> show ys where
    yxs = joinDists Tuple ys xs'y
    xs = marginalize snd yxs
    e_yxs = wrapEnt yxs
    e_xs = wrapEnt xs
    e_ys = wrapEnt ys

entPos :: Dist String -> Result
entPos d = 0.0 <~ wrapEnt d <?> show d

infoPos :: DistPair String String -> Result
infoPos (DistPair ys xs'y) =
  0.0 <~ i_yxs <?> show ys where
    yxs = joinDists Tuple ys xs'y
    i_yxs = entropyNum.to $ nonCond mutualInformation yxs fst snd

wrapEnt :: forall a. (Eq a) => Dist a -> Number
wrapEnt = entropyNum.to <<< nonCond entropy

genDist :: forall a. Gen a -> Gen (Dist a)
genDist gen = go
  where
    go = do
      p <- fromFreqs <$> Gen.arrayOf (Tuple <$> gen <*> arbitrary)
      maybe go pure $ p

genStringDist :: Gen (Dist String)
genStringDist = genDist arbitrary

data DistPair a b = DistPair (Dist a) (a -> Dist b)

genDistPair :: forall a b. Ord a => Gen a -> Gen (Dist b) -> Gen (DistPair a b)
genDistPair genA genB = do
  d <- genDist genA
  m <- M.fromFoldable <$> T.traverse (\s -> Tuple s <$> genB) (extract d)
  pure <<< DistPair d <<< unsafePartial $ (\a -> fromJust $ a `M.lookup` m)

genStringDistPair :: Gen (DistPair String String)
genStringDistPair = genDistPair arbitrary (genDist arbitrary)

data DivPair a = DivPair (Dist a) (Dist a)

genDivPair :: forall a. Gen a -> Gen (DivPair a)
genDivPair gen = do
  d' <- genDist gen
  let states = extract d'
  d <- zipDist states <$> (probListArb $ A.length states)
  pure $ DivPair d' d

genStringDivPair :: Gen (DivPair String)
genStringDivPair = genDivPair arbitrary

normalize :: Array Number -> Array Number
normalize ns = let s = F.sum ns in flip (/) s <$> ns

probListArb :: Int -> Gen ProbList
probListArb n =
  unsafePartialBecause "Constructed statically to be normalized" $
  fromJust <<< (probList <=< T.traverse prob) <<< normalize <$> Gen.vectorOf n (Gen.choose 0.0 1.0)
