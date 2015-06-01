{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
-- |									    | --
-- |		CPSC449 Programming Paradigms Assignment 2b                 | --
-- |				Chris Wozniak                               | --
-- |				  10109820                                  | --
-- |                                                                        | --
-- |                      Covers Questions 4, and 5                         | --
-- | 	    These functions show the value of a list comprehension          | --
-- |        by defining two functions which count the number of             | --
-- |        even numbers when provided with a list of integers.             | --
-- |                                                                        | --
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}

{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
-- |			List Comprehension Example (Q4)			    | --
-- |                                                                        | --
-- |        These two functions each return the integer count of            | --
-- |        even numbers within a list of integers. One function            | --
-- |        solves the problem through recursion, and the other             | --
-- |        is solved through a list comprehension.                         | --
-- |                                                                        | --
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
-- Recursive Solution
recRun :: [Integer] -> Integer
recRun a
	| a == [] 		    = 0
	| (((head a) `mod` 2) == 0) = 1 + recRun (tail a)
	| otherwise		    = recRun (tail a)

-- List Comprehension Solution.
lComp :: [Int] -> Int
lComp ans = (length [ x |  x <- ans, ((x `mod` 2)== 0)])

{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
-- |			    Database example (Q5)			    | --
-- |                                                                        | --
-- |        The database question had an interesting challenge where        | --
-- |        if you provided a string that begun at one part, and            | --
-- |        moved through the string you could find "thng" from             | --
-- |        "thing" Therefore a possible match function was                 | --
-- |        created to handle such an event. The function                   | --
-- |        'contains' recursively parses the string otherwise.             | --
-- |                                                                        | --
-- |        The findString function was handled through a                   | --
-- |        list comprehension, seemed like the simplest choice.            | --
-- |                                                                        | --
-- |        Note: dataBase, and the relation saved are provided from        | --
-- |              the lecture slides. They were imported as per             | --
-- }              the requirements in the question.                         | -- 
-- |                                                                        | --
-- |                                                                        | --
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
type Client = Int
type Video = [Char]
type Relation = [(Client, Video)]
dataBase :: Relation
dataBase = [
	    (763547, "The Thing"), 
	    (929845, "The Thing"), 
	    (181014, "Big Trouble in Little China"),
	    (929845, "Escape from New York")
	   ]

-- Database "find" function
findString :: Relation -> [Char] -> [[Char]]
findString out fstr = [ s | (_,s) <- out, (contains s fstr)]

-- String Search Function
contains :: [Char] -> [Char] -> Bool
contains s fstr
	| fstr == [] 		    = True
	| s == [] 		    = False
	| ((head s) == (head fstr)) = possibleMatch (tail s) (tail fstr) fstr
	| otherwise 		    = contains (tail s) fstr

-- If a match is not 100%, return to the original function
possibleMatch :: [Char] -> [Char] -> [Char]-> Bool
possibleMatch s fstr preservedString
	| fstr == [] 		    = True
	| s == []		    = False
	| ((head s) == (head fstr)) = possibleMatch (tail s) (tail fstr) preservedString
	| otherwise		    = contains (tail s) preservedString

