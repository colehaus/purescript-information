module Tests where

import Control.Bind
import qualified Data.Array as A
import qualified Data.Foldable as F
import qualified Data.Map as M
import Data.Maybe
import Data.Maybe.Unsafe
import qualified Data.Traversable as T
import Data.Tuple
import Debug.Trace
import Test.QuickCheck
import Test.QuickCheck.Gen hiding (uniform)

import Math.Probability hiding (choose, oneOf, ProbList(), Prob(), Dist())
import Math.Probability.Internal ((<~), (~~), ProbList(), Prob(), Dist())
import Math.Probability.Information

-- TODO: Finish out properties
main = do
  trace "H(X) >= 0"
  quickCheck entPos
  trace "H(X|Y) <= H(X)"
  quickCheck condRed
  trace "H(X,Y) <= H(X) + H(Y)"
  quickCheck indepLimit
  trace "H(X,Y) = H(X|Y) + H(Y)"
  quickCheck entChain
  trace "I(X;Y) >= 0"
  quickCheck infoPos
  trace "I(X;Y) = H(X) - H(X|Y)"
  quickCheck infoEnt
  -- trace "I(X;Y|Z) = H(X|Z) - H(X|Y,Z)"
  -- quickCheck condInfoEnt
  -- trace "I(X1,X2;Y) = I(X2;Y|X1) + Y(X1|Y)"
  -- quickCheck infoChain
  trace "D(p(x)||q(x)) >= 0"
  quickCheck divPos
  trace "D(p(x,y)||p(x)p(y)) = I(X;Y)"
  quickCheck divInfo
  -- trace "D(p(x,y)||q(x,y)) = D(p(x)||q(x)) + D(p(y|x)||q(y|x))"
  -- quickCheck divChain

divInfo :: DistPair String String -> Result
divInfo (DistPair ys xs'y) =
  i_yxs ~~ d_yxs_ysxs <?> show ys where
    yxs = joinDists Tuple ys xs'y
    xs = marginalize snd yxs
    ysxs = joinDists Tuple ys (const xs)
    i_yxs = to entropyNum $ nonCond mutualInformation yxs fst snd
    d_yxs_ysxs = to entropyNum $ divergence (pure unit) (const yxs) (const ysxs)

divPos :: DivPair String -> Result
divPos (DivPair p q) =
  0 <~ to entropyNum (divergence (pure unit) (const p) (const q)) <?>
  show p <> " " <> show q

infoEnt :: DistPair String String -> Result
infoEnt (DistPair ys xs'y) =
  i_yxs ~~ e_xs - e_xs'y <?> show ys where
    yxs = joinDists Tuple ys xs'y
    xs = marginalize snd yxs
    i_yxs = to entropyNum $ nonCond mutualInformation yxs fst snd
    e_xs = wrapEnt xs
    e_xs'y = to entropyNum $ entropy ys xs'y

entChain :: DistPair String String -> Result
entChain (DistPair ys xs'y) =
  e_xs'y + e_ys ~~ e_xys <?> show ys where
    e_xs'y = to entropyNum $ entropy ys xs'y
    e_ys = wrapEnt ys
    e_xys = wrapEnt $ joinDists Tuple ys xs'y

condRed :: DistPair String String -> Result
condRed (DistPair ys xs'y) =
  e_xs'y <~ e_xs <?> show ys where
    e_xs'y = to entropyNum $ entropy ys xs'y
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
entPos d = 0 <~ wrapEnt d <?> show d

infoPos :: DistPair String String -> Result
infoPos (DistPair ys xs'y) =
  0 <~ i_yxs <?> show ys where
    yxs = joinDists Tuple ys xs'y
    i_yxs = to entropyNum $ nonCond mutualInformation yxs fst snd

wrapEnt :: forall a. (Eq a) => Dist a -> Number
wrapEnt = to entropyNum <<< nonCond entropy

instance arbMap :: (Arbitrary k, Arbitrary v, Ord k) => Arbitrary (M.Map k v) where
  arbitrary = M.fromList <$> arbitrary

instance arbProb :: Arbitrary Prob where
  arbitrary = do
    p <- prob <$> choose 0 1
    maybe arbitrary pure $ p

instance arbDist :: (Arbitrary a) => Arbitrary (Dist a) where
  arbitrary = do
    p <- fromFreqs <$> arbitrary
    maybe arbitrary pure $ p

data DistPair a b = DistPair (Dist a) (a -> Dist b)
instance arbDistPair :: (Arbitrary a, Ord a, Arbitrary b) =>
         Arbitrary (DistPair a b) where
  arbitrary = do
    d <- arbitrary
    let ss = extract d
    m <- M.fromList <$> T.traverse (\s -> Tuple s <$> arbitrary) ss
    pure <<< DistPair d $ (\a -> fromJust $ a `M.lookup` m)

data DivPair a = DivPair (Dist a) (Dist a)
instance arbDivPair :: (Arbitrary a) => Arbitrary (DivPair a) where
  arbitrary = do
    d' <- arbitrary
    let states = extract d'
    d <- zipDist states <$> (probListArb $ A.length states)
    pure $ DivPair d' d

normalize :: [Number] -> [Number]
normalize ns = let s = F.sum ns in flip (/) s <$> ns
probListArb :: Number -> Gen ProbList
probListArb n =
  fromJust <<< (probList <=< T.traverse prob) <<< normalize <$>
  vectorOf n (choose 0 1)
