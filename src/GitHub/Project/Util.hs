module GitHub.Project.Util
  ( foldableToAlternative
  , listToSeq
  , vectorToSeq
  ) where

import Control.Applicative (Alternative(..), Applicative(..))
import Data.Foldable (Foldable(..))
import Data.Function ((.))
import Data.Sequence (Seq)

import qualified GitHub.Internal.Prelude as GIP

foldableToAlternative :: (Foldable f, Foldable g, Alternative g) => f a -> g a
foldableToAlternative = h . g
  where
    f :: (Alternative g) => g a -> a -> g a
    f acc value = pure value <|> acc
    h :: (Foldable g, Alternative g) => g a -> g a
    h = foldl' f empty
    g :: (Foldable f, Foldable g, Alternative g) => f a -> g a
    g = foldl' f empty

listToSeq :: GIP.Vector a -> [a]
listToSeq = foldableToAlternative

vectorToSeq :: GIP.Vector a -> Seq a
vectorToSeq = foldableToAlternative
