

--Variables
type State = int
type TapeA = Char
type Alpha = Char
data InTransition  = (State, Alpha)
data OutTransition = (State, TapeA, Char)
data Transition    = (InTransition, OutTransition)

--read
  --the following variables will be instantiated
--states :: [State]
--  states   = ints between {} and before 1st ;
--alphabet :: [Alpha]
--  alphabet = char between {} and before 2nd ;
--tape :: [TapeA]
--  tape     = char between {} and before 3rd ;
--transitions :: [Transition]
--  transitions = transitions before 4th ;
--startingState :: State
--  startingState = state before 5th ;
--empty :: TapeA
--  empty = char before 6th ;
--accState :: [State]
--  accState = states between {} before 7th ;



first :: a -> b
first (x:y:z:s) = x

second :: a -> b
second (x:y:z:s) = y

third :: a -> b
third (x:y:z:s) = z

isValid :: String -> Bool
isValid x = x `elem` alphabet

--[Transition] == [(InTransition, OutTransition)] == [( (State,Alpha) , (State,TapeA,Char) )]
canTransition :: [Transition] -> InTransition -> Transition
canTransition [] _ = null
canTransition (t:ts) (s,a) | first (first t) == s && second (first t) == a = second t
                           | otherwise = findTransition ts (s,a)
--take in currentState, head of the inputString, currentTapePointer, Tape
--returns new State, new TapePosition, newTape
update :: state -> String -> Integer -> String -> (State, Integer, String)
update s i tp t = let trans  = canTransition (s,i)
                      tpoint a | a == 'L' || a == 'l' = -1
                               | a == 'R' || a == 'r' = 1
                               | otherwise = 0
                      nTape a b (x:xs) | a == 0 = (b:xs)
                                       | a > 0  = x:(ntape (a-1) b xs)
                  in ((first trans), (tpoint (third trans)), (nTape tp (second trans) t))

currentPointer = 1;
tape = [empty:empty:empty]
turing :: State -> Integer -> [TapeA] -> String -> State
turing -1 _ _ _ = -1
turing currentState _ _ [] = currentState
turing currentState currentPointer tape (x:xs)  | isValid x && currentState >= 0 =  let u = update currentState currentPointer tape
                                                                                        a = first u
                                                                                        b = second u
                                                                                        c = currentPointer + (third u)
                                                                                    in  turing (a b c xs)
                                                | otherwise = -1

isFinal :: State -> Bool
isFinal s = s `elem` states





