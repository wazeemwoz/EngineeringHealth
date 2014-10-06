import System.Random
import Data.List
population = 100
healthCareEngineeringProgressRate = 1
basicTechnology = 10
beginYear = 1979
main = do 
    randomLifeStyle  <- newStdGen 
    randomQualityOfService  <- newStdGen
    let popHealth = people $ generate population randomLifeStyle
    let healthcareSpecialists = people $ generate population randomQualityOfService
    saveLives popHealth healthcareSpecialists basicTechnology healthCareEngineeringProgressRate 0 beginYear
saveLives _ _ _ _ 100 _ = do print "Technology is consistent, and as it advances we can achieve a world without medical error."
saveLives popHealth healthcareSpecialists engineeringSkill rateOfImprovement _ year = do
    let engProgress = engineeringSkill + rateOfImprovement
    let healthcareGiven = map (\(a,b) -> a - b) $ zip popHealth healthcareSpecialists
    let livesSaved = sum $ map (\a -> if a > 0 then 0 else 1) healthcareGiven
    let livesSavedWithTechnology = sum $ map (\a -> if a > 0 then 0 else 1) (healthcareEngineering engProgress popHealth healthcareSpecialists)
    print $ "In Year: " ++ (show year) ++ " Healthcare specialists save " ++ (show livesSaved) ++ " out of "  ++ (show population)
    print $ "In Year: " ++ (show year) ++ " With technology they save " ++ (show livesSavedWithTechnology) ++ " out of " ++ (show population)
    saveLives popHealth healthcareSpecialists engProgress rateOfImprovement livesSavedWithTechnology (year+1)
people [] = [] 
people (x:xs) = (mod x 100) : people xs
generate :: Int -> StdGen -> [Int]
generate n = take n . unfoldr (Just . random)
healthcareEngineering _ [] _ = []
healthcareEngineering _ _ [] = []
healthcareEngineering r xs ys = map ((\r n -> n-r) r) $ connectTheDots xs xs ys
connectTheDots [] ys _ = ys
connectTheDots _ ys [] = ys
connectTheDots (x:xs) ys (d:ds) = connectTheDots xs (connectedCare x ys d) ds
connectedCare h xs d = map ((\z a b -> if (a==b) then b-z else b) d h) xs