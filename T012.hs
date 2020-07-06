import DataFile
import Data.List

-- Start of code

-- helper functions

breakToAList someString = groupBy (\word gotWord -> gotWord /= ' ' && gotWord /= '!' && gotWord /= '#' && gotWord /= '$'&& gotWord /= '%'
                                    && gotWord /= '&' && gotWord /= ',' && gotWord /= '.' && gotWord /= ':' && gotWord /= ';' && gotWord /= '?'
                                    && gotWord /= '@' && gotWord /= '`' && gotWord /= '|' && gotWord /= '~') someString

removeSpace someString = if (head someString == ' ') then
                             tail someString
                         else someString

removeSpaces [] = []
removeSpaces someList = (removeSpace (head someList)):(removeSpaces (tail someList))

repeatedTwo word1 word2 list = if length list >= 2 then
                                   if word1 == (head list) && word2 == (head (tail list))
                                       then True
                                    else repeatedTwo word1 word2 (tail list)
                                else False

repeatedThree word1 word2 word3 list = if length list >= 3 then
                                           if  word1 == (head list) && word2 == (head (tail list)) && word3 == (head (tail (tail list)))
                                               then True
                                           else 
                                                repeatedThree word1 word2 word3 (tail (tail list))
                                        else False

countTwo word1 word2 list = if length list > 1 then 
                                if word1 == (head list) && word2 == (head (tail list))
                                    then 1 + countTwo word1 word2 (tail list)
                                else countTwo word1 word2 (tail list)
                            else 0

countBigrams [] _ = []
countBigrams bigrams list = (countTwo (fst (head bigrams)) (snd (head bigrams)) list):(countBigrams (tail bigrams) list)

countThree word1 word2 word3 list = if length list > 2 then
                                        if word1 == (head list) && word2 == (head (tail list)) && word3 == (head (tail (tail list)))
                                            then 1 + countThree word1 word2 word3 (tail list)
                                        else 
                                            countThree word1 word2 word3 (tail list)
                                    else 0

first (x, _, _) = x
second (_, x, _) = x
third (_, _, x) = x

countTrigrams trigrams list = if length trigrams >= 1 then
    (countThree (first (head trigrams)) (second (head trigrams)) (third (head trigrams)) list):(countTrigrams (tail trigrams) list)
                              else []

twoWordsFreq _ [] = 0
twoWordsFreq list list2 = if (head list == (first (fst (head list2))) ) && (head list == (first (fst (head list2))) )
                               then snd (head list2)
                           else twoWordsFreq list (tail list2)

checkWords _ [] = []
checkWords list list2 = if (head list == (first (fst (head list2))) ) && (head list == (first (fst (head list2))) )
                                 then if (twoWordsFreq list list2) > 0.03 then
                                          (third (fst (head list2))):(checkWords list (tail list2))
                                  else
                                           checkWords list (tail list2)
                        else
                            checkWords list (tail list2)

myGenerator list = genProbPairs (trigramsFreq modifiedList) (bigramsFreq modifiedList)
                   where modifiedList = wordTokenList list

getLastTwo list = if (length list /= 2) then
                      getLastTwo (tail list)
                  else list

-- main functions

wordToken:: String -> [String]
wordToken someString = removeSpaces (breakToAList someString)

wordTokenList:: [String] -> [String]
wordTokenList [] = []
wordTokenList (first:rest) = (wordToken first) ++ wordTokenList(rest)

uniqueBigrams :: [String] -> [(String,String)]
uniqueBigrams list = if length list > 1 then
                         if repeatedTwo (head list) (head (tail list)) (tail (tail list))
                             then uniqueBigrams (tail list)
                         else (head list, head (tail list)):uniqueBigrams(tail list)
                     else []

uniqueTrigrams :: [String] -> [(String,String,String)]
uniqueTrigrams list = if length list > 2 then
                          let first = head list
                              second = head (tail list)
                              third = head (tail (tail list))
                          in if repeatedThree first second third (tail (tail (tail list)))
                              then uniqueTrigrams(tail list)
                             else (first, second, third):uniqueTrigrams(tail list)
                     else []

bigramsFreq :: Num a => [String] -> [((String,String),a)]
bigramsFreq list = zip (uniqueBigrams list) (countBigrams (uniqueBigrams list) list)

trigramsFreq:: Num a => [String] -> [((String,String,String),a)]
trigramsFreq list = zip (uniqueTrigrams list) (countTrigrams (uniqueTrigrams list) list)

getFreq :: (Eq a, Num b) => a -> [(a,b)] -> b
getFreq _ [] = 0
getFreq letter list = if letter == fst (head list)
                          then snd (head list)
                      else
                          getFreq letter (tail list)

generateOneProb :: Fractional a =>((String,String,String),a)-> [((String,String),a)] -> a
generateOneProb ((word1,word2,word3),s) list = s/(getFreq (word1,word2) list)

genProbPairs :: Fractional a => [((String,String,String),a)] ->[((String,String),a)] -> [((String,String,String),a)]
genProbPairs [] _ = []
genProbPairs ((head,prob):tail) list  = (head,(generateOneProb (head,prob) list)):genProbPairs tail list

generateNextWord :: (Ord a, Fractional a) => [String] -> [((String,String,String),a)] -> String
generateNextWord list list2 = if (length (checkWords (getLastTwo list) list2) >= 1) then
                                  (checkWords (getLastTwo list) list2) !! (randomZeroToX (length (checkWords (getLastTwo list) list2)  - 1))
                              else
                                  error "Sorry, it is not possible to infer from current database"

generateText :: String -> Int -> String
generateText someString 0 = someString
generateText someString number = generateText
                                  ( let something = generateNextWord (wordToken someString) (myGenerator docs)
                                   in if (head something) `elem` punct
                                     then someString ++ something
                                   else
                                          someString ++ " " ++ something
                                  ) (number - 1)
--------------------------------------------3akkkkkkkkkkkkkkkkk
removeSpacesString :: String -> String
removeSpacesString  list = if (head list == ' ') then tail list
		     else list
--reomveSpacesList :: [String] -> [String]
removeSpacesList :: [String] -> [String]
removeSpacesList [] =[]
removeSpacesList list = ((removeSpacesString (head list)):(removeSpacesList (tail list)))
breakToAlist2 :: String -> [String]
breakToAlist2 string = groupBy(\word x -> x/=' ') string
wordToken2 :: String -> [String]
wordToken2 string =  (breakToAlist2 string)
wordTokenList2 :: [String] -> [String]
wordTokenList2 [] = [] 
wordTokenList2 list =  (wordToken2 (head list))++(wordTokenList2 (tail list))
repeatedTwo2 :: String -> String -> Bool
repeatedTwo2 word1 word2 = if(word1 == word2)then False
			   else True 
--if length list >= 3 then
repeatedThree3 word1 word2 word3 list = if length list >=3 then
					if (word1 == head list) && word2 == head (tail list) && word3 == head (tail (tail list)) then True
					else repeatedThree3 word1 word2 word3 (tail (tail (tail list)))
					else False 
uniqueTigrams2 :: [String] -> [(String,String,String)]
uniqueTigrams2 list =  if length list >= 3 then
			if repeatedThree3 (head list) (head (tail list)) (head (tail (tail list))) (tail (tail (tail list))) then uniqueTigrams2 (tail list)
		      else (head list,head (tail list),head (tail (tail list))):uniqueTigrams2 (tail list)
			else error "Sorry"

countBigrams2 :: Num a => (String,String) -> [String] -> a
countBigrams2 (x,y) [] = 0
countBigrams2 (x,y) list = if length list >= 2 then
			   if (x,y) == ((head list),(head (tail list))) then (1 + (countBigrams2 (x,y) (tail (tail list))))
			   else countBigrams2 (x,y) (tail (tail list))
			   else 0


counting word1 word2 list = if length list >= 2 then
			    if word1 == (head list) && word2==(head (tail list)) then 1 + counting word1 word2 (tail list)
			    else counting word1 word2 (tail list)
			    else 0 
--countingList list = (counting (head list) (head (tail list) list) ++ counting((head list) (head (tail list)) (tail list))
--bigramsFreq2 :: Num a => [String] -> [((String,String),a)]
--bigramsFreq2 list = zip (uniqueBigrams list) (countingList list)
count3 word1 word2 word3 list = if length list >= 3 then
				if word1 == (head list) && word2 == (head (tail list)) && word3 == (head (tail (tail list))) then 
					1 + count3 word1 word2 word3 (tail list)
				else count3 word1 word2 word3 (tail list)
				else 0
---first(x,_,_)=x
sec(_,y,_)=y
thrd(_,_,z)=z
(countThree (first (head trigrams)) (second (head trigrams)) (third (head trigrams)) list):(countTrigrams (tail trigrams) list)
                              else []
--countTrig :: Num a => [(String,String,String)] -> [String] -> a
countTrig trig [] = []
countTrig trig list = if list >=1 then
		      (count3 (first (head trig)) (sec (head trig)) (thrd (head trig)) list):
		      else []
trigFreq list = zip (uniqueTrigrams list) (countTrig (uniqueTrigrams list) list)
