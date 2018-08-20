import Control.Applicative
import Data.Foldable
import Control.Monad

import Data.Job

data Person = Person
    { height :: Double
    , weight :: Double
    , gender :: Gender
    }

data Gender = M | F | Unknown deriving Enum

-- Contrived Example
heightVsWOGM :: Job Person (Double, Bool, Double, String) 
heightVsWOGM = do
    -- Operate on individual rows with "row"
    person <- row

    -- Filtering with Alternative instance
    guard $ case gender person of
        Unknown -> False
        _ -> True
    
    -- By-group summaries 
    genderAvgWeights <- withGroups gender $ do
        person <- row
        guard $ weight person > 0
        mean `by` weight

    -- Enum left and inner joins on any Foldable target
    let genderNames = Just (M, "Male")
        genderNames2 = [(M, error "Won't be used"), (F, "Female")]

    -- Sane Alternative instance for failed joins
    (_, gn) <- enumJoinInner gender fst genderNames
        <|> enumJoinInner gender fst genderNames2

    let tallFuncs = [(M,  (>=190))]
   
    -- Left joins are represented with a Maybe value.
    tallFunc'm <- enumJoinLeft gender fst tallFuncs

    -- No right joins yet, sorry.
    -- Might be possible with some natural transformations magic.
    -- For now, you have to run another job over the target.
    let isTall = maybe (>=175) snd tallFunc'm $ height person
    
    -- Return data in the format you want it output.
    return (height person, isTall, genderAvgWeights, gn)

-- I wrote it specialized to Lists, but it can probably 
-- be rewritten towork with any Traversable + Alternative.
-- Vectors are super slow due to repeated concats.
main = mapM_ print res
    where res = runJob heightVsWOGM people

-- Small example.
people :: [Person]
people = [Person 180 72 M, Person 160 50 F, Person 175 60 F, Person 200 100 M, Person 175 68 M, Person 9001 9001 Unknown]

-- Bigger example.
-- DO NOT RUN WITHOUT AT LEAST -O.
-- Not fast, but surprisingly not slow!
-- Processes the below in 1.722s with -O2.
main' = mapM_ print $ take 32 res
    where res = runJob heightVsWOGM people2

people2 :: [Person]
people2 = do
    h <- [1..1000]
    w <- [1..1000]
    g <- [M, F, F, M]
    return $ Person h w g

mean :: Fractional a => [a] -> a
mean xs = s' / l'
    where
        (s', l') = foldl go (0, 0) xs
        go (s, l) x = s `seq` l `seq` (s + x, l + 1)
