{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
-- |									    | --
-- |		CPSC449 Programming Paradigms Assignment 2a                 | --
-- |				Chris Wozniak                               | --
-- |				  10109820                                  | --
-- |                                                                        | --
-- |                         Covers Questions 1-3                           | --
-- | 	    This Module defines ternary logic over the values               | --
-- | 	    True, False, and Unknown.                                       | --
-- |                                                                        | --
-- |        NOTE: Question 3 used the default "Nothing" type                | --
-- |              provided with the Maybe type declaration                  | --
-- |              allowing the same result with the generic                 | --
-- |              bool data type.                                           | --
-- |                                                                        | --
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}

{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
-- |			        Ternary Logic (TBool)			    | --
-- |									    | --
-- |        In order for the logic to work given the state of 3             | --
-- |        values we create our own type definitions.                      | --
-- |		                                                            | --
-- |        -We add Eq, and Show as these functions must be equal           | --
-- |         or not. Order and Enumeration do not matter.                   | --
-- |                                                                        | --
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
data TBool = T | F | U
	     deriving (Eq, Show)

{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
-- |			        Ternary NOT				    | --
-- |									    | --
-- |        Ternary Not is similar to the not function in regular           | --
-- |        boolean functions, with the addition of 'unknown'		    | --
-- |									    | --
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
tNOT :: TBool -> TBool
tNOT n
	| (n == T)  = F
	| (n == F)  = T
	| otherwise = U
	
{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
-- |			        Ternary AND				    | --
-- |                                                                        | --
-- |	    In Ternary AND we find that we only have one case which         | --
-- |        returns true. As above, the function is self explanitory	    | --
-- |									    | --
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
tAND :: TBool -> TBool -> TBool
tAND a b
	| ((a == T) && (b == T)) = T
	| ((a == F) || (b == F)) = F
	| otherwise 		 = U

{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
-- |			         Ternary OR				    | --
-- |                                                                        | --
-- |              Ternary OR is the reverse of Ternary AND                  | --
-- |									    | --
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
tOR :: TBool -> TBool -> TBool
tOR a b
	| ((a == F) && (b == F)) = F
	| ((a == T) || (b == T)) = T
	| otherwise 		 = U

{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
-- |		         Maybe Type Ternary Logic (Q3)		            | --
-- |                                                                        | --
-- |        Here I opted to not count on a personal type, since the         | --
-- |        Maybe type already consists of an 'unknown' type called         | --
-- |        'Nothing' Therefore, Using the system default is                | --
-- |        a much simpler way to produce the same result.                  | --
-- |									    | --
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
-- NOT --
maybe_tNOT :: (Maybe Bool) -> (Maybe Bool)
maybe_tNOT (Just a) = (Just (not a))
maybe_tNOT Nothing  = Nothing

-- AND --
maybe_tAND :: (Maybe Bool) -> (Maybe Bool) -> (Maybe Bool)
maybe_tAND a b
	| (a == (Just True)) && (b == (Just True)) 	= Just True
	| ((a == (Just False)) || (b == (Just False)))  = Just False
	| otherwise 				  	= Nothing
-- OR --
maybe_tOR :: (Maybe Bool) -> (Maybe Bool) -> (Maybe Bool)
maybe_tOR a b
	| ((a == (Just False)) && (b == (Just False))) 	= Just False
	| ((a == (Just True)) || (b == (Just True))) 	= Just True
	| otherwise 		 			= Nothing


