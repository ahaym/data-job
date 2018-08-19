module Data.Job where

import Data.Foldable
import qualified Data.IntMap as M

newtype Job t r a = Job { unJob :: t r -> r -> a }

instance Functor t => Functor (Job t r) where
    fmap f (Job j) = Job $ \rows row -> f $ j rows row

instance Functor t => Applicative (Job t r) where
    pure = Job . const . const
    (Job fj) <*> (Job j) = Job $ \rows row -> fj rows row (j rows row)

instance Functor t => Monad (Job t r) where
    return = pure
    (Job j) >>= f = Job $ \rows row -> unJob (f (j rows row)) rows row

col :: (r -> a) -> Job t r a
col f = Job $ \_ row -> f row

row :: Job t r r
row = Job $ \_ row -> row

everything :: Job t r (t r)
everything = Job const

runJob :: Functor t => Job t r a -> t r -> t a
runJob (Job j) rs = let f = j rs in f `seq` f <$> rs

withGroups :: (Foldable t, Enum b) => (r -> b) -> Job [] r a -> Job t r a
withGroups grouper_ (Job f) = Job $ \rows -> let groups = M.map f (mkGroups rows) in \row -> (groups M.! grouper row) row
    where
        grouper = fromEnum . grouper_
        insGroup row = M.insertWith (++) (grouper row) [row]
        mkGroups = foldr' insGroup M.empty

groupSummarize :: (Foldable t, Enum b) => (r -> b) -> (r -> c) -> ([c] -> a) -> Job t r a
groupSummarize grouper mapper summarizer = withGroups grouper $ summarizer. fmap mapper <$> everything

-- Horribly slow. Don't use this.
groupBy :: Enum b => (r -> b) -> Job [] r [r]
groupBy grouper = withGroups grouper everything
