module Data.Job where

import Control.Applicative
import Data.Foldable
import qualified Data.IntMap as M
import Data.Maybe

newtype Job r a = Job { unJob :: [r] -> r -> [a] }

instance Functor (Job r) where
    fmap f (Job j) = Job $ \rows row -> f <$> j rows row

instance Applicative (Job r) where
    pure x = Job $ \_ _ -> [x]
    (Job fj) <*> (Job j) = Job $ \rows row -> fj rows row <*> j rows row

instance Monad (Job r) where
    return = pure
    (Job j) >>= f = Job $ \rows row -> do
        ff <- unJob . f <$> j rows row
        ff rows row

instance Alternative (Job r) where
    empty = Job $ \_ _ -> []
    (Job j0) <|> (Job j1) = Job $ \rows row ->
        case j0 rows row of
            [] -> j1 rows row
            stuff -> stuff

col :: (r -> a) -> Job r a
col f = Job $ \_ row -> [f row]

row :: Job r r
row = Job $ \_ row -> [row]

everything :: Job r [r]
everything = Job $ \rows _ -> [rows]

runJob :: Job r a -> [r] -> [a]
runJob (Job j) rs = let f = j rs in concatMap f rs

summarize :: (r -> a) -> ([a] -> b) -> Job r b
summarize mapper summarizer = Job $ \rows _ -> return . summarizer . fmap mapper $ rows

by :: ([a] -> b) -> (r -> a) -> Job r b
by = flip summarize

-- FIXME: For joins, compute key map on the smaller collection

enumJoinInner :: (Enum k, Foldable t) => (r -> k) -> (s -> k) -> t s -> Job r s
enumJoinInner primary external target = Job $ \_ row -> 
    M.findWithDefault [] (fromEnum $ primary row) groups
    where
        groups = mkGroups (fromEnum . external) target

enumJoinLeft :: (Enum k, Foldable t) => (r -> k) -> (s -> k) -> t s -> Job r (Maybe s)
enumJoinLeft primary external target = Job $ \_ row ->
    case M.lookup (fromEnum $ primary row) groups of
        Just matches -> Just <$> matches
        Nothing -> pure Nothing
    where
        groups = mkGroups (fromEnum . external) target

-- | An inner join with unmatched items in a seperate list.
-- A proper right join seems very tricky to me without some 
-- sort of natural transformations magic.
-- Probably very slow, don't use
enumJoinRightFake :: (Enum k, Foldable t) => (r -> k) -> (s -> k) -> t s -> Job r (s, [s])
enumJoinRightFake primary external target = Job $ \rows -> 
    let pg = mkGroups p rows in \row ->
    (flip (,) . concat . M.elems $ M.difference groups pg) <$>
        M.findWithDefault [] (fromEnum $ primary row) groups
    where
        p = fromEnum . primary
        f = fromEnum . external
        groups = mkGroups f target

withGroups :: Enum b => (r -> b) -> Job r a -> Job r a
withGroups grouper_ (Job f) = Job $ \rows -> let groups = M.map f (mkGroups grouper rows) in \row -> (groups M.! grouper row) row
    where
        grouper = fromEnum . grouper_

groupSummarize :: Enum b => (r -> b) -> (r -> c) -> ([c] -> a) -> Job r a
groupSummarize grouper mapper summarizer = withGroups grouper $ summarize mapper summarizer

-- Horribly slow. Don't use this.
groupBy :: Enum b => (r -> b) -> Job r [r]
groupBy grouper = withGroups grouper everything

-- Auxillaries
insGroup :: (r -> M.Key) -> r -> M.IntMap [r] -> M.IntMap [r]
insGroup f row = M.insertWith (++) (f row) [row]

mkGroups :: Foldable t => (r -> M.Key) -> t r -> M.IntMap [r]
mkGroups f = foldr' (insGroup f) M.empty
