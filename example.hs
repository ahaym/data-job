import Data.Foldable
import qualified Data.Vector as V
import Data.Job

data Gender = M | F deriving (Enum, Ord, Eq, Show)

data Person = Person
    { height :: Double
    , weight :: Double
    , gender :: Gender
    } deriving Show

people :: [Person]
people = [Person 180 72 M, Person 160 50 F, Person 175 60 F, Person 200 100 M, Person 175 68 M]

people2 :: [Person]
people2 = do
    h <- [1..100]
    w <- [1..100]
    g <- [M, F, F, M]
    return $ Person h w g

-- Person's height vs. their difference from their gender's mean weight
heightVsWOGM :: Traversable t => Job t Person (Double, Double) 
heightVsWOGM = do
    h <- col height
    w <- col weight

    genderAvgWeights <- withGroups gender $ do
        ev <- everything
        return . mean . fmap weight $ ev

    return (h, w / genderAvgWeights)

mean xs = sum xs / fromIntegral (length xs)

vp = V.fromList people2

pipeline = [people2, people, people2]

main = mapM_ print $ runJob heightVsWOGM people2
