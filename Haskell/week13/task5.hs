import Data.List

main :: IO()
main = do
    print $ hardestSubject [("John", "Maths", 5), ("Kennedy", "English", 2), ("Joe", "Programming", 4), ("Claudia", "Programming", 6), ("Sam", "Maths", 4), ("Jenn", "English", 2)] == "English"
    print $ hardestSubject [("John", "Maths", 5), ("Kennedy", "English", 5), ("Joe", "Programming", 4), ("Claudia", "Programming", 6), ("Sam", "Maths", 4), ("Jenn", "English", 5)] == "Maths"

type Student = String
type Subject = String
type Note = Double
type Record = (Student, Subject, Note)

hardestSubject :: [Record] -> Subject
hardestSubject [] = ""
hardestSubject records = snd $ head $ sort $ map (\ recs -> ((sum $ map snd recs) / (fromIntegral $ length recs), fst $ head recs)) grouped
 where grouped = groupBy (\ (x,_) (y,_) -> x==y) $ sort $ map (\ (_,s,g) -> (s,g)) records