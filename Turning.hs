

--Variables
type State = int
type TapeA = Char
type Alpha = Char
data InTransition  = (State, Alpha)
data OutTransition = (State, TapeA, Char)
data Transition    = (InTransition, OutTransition)

-- File Template
--({q1, q2, q3, ...}; {0,1,a,b,_, ...}; {a,b,...}; D; q0; _; {q2, q3, ...})
  -- d(q1, a) = (q3, 0, R)
  -- d(q3, 0) = (q1, a, L)
  -- ...
-- where states DO start with q then # and the tape/alphabet cannot have characters that start with q or d/D
-- where the tape alphabet includes the input alphabet and the blank symbol

--data Token = StateSet | TapeSet | AlphaSet | StartState | Blank | FinalSet
--  | KeywordState State | KeywordTape TapeA | KeywordAlpha Alpha
--  | Comma | Semi | LBracket | RBracket | LPar | RPar | Eql
--  | Err deriving Show

--or (thinking ^ is wrong)

data Token = Machine | Delta
  | KeywordState State | KeywordAlphabet Char | Keyword String
  | Comma | LBracket | RBracket | LPar | RPar | Eql
  | Err deriving Show

classify :: String -> Token
classify "=" = Eql
classify "(" = LPar
classify ")" = RPap
classify "{" = LBracket
classify "}" = RBracket
classify "," = Comma
classify ";"
classify x | isState x = KeywordState (toState x)
classify x | isAlphabet x = KeywordAlpha x --one of the last parts that needs to be defined

sr :: [Token] -> [Token] -> [State] -> [Alpha] -> [TapeA] -> [Transition] -> State -> TapeA -> [State] -> ([State], [Alpha], [TapeA], [Transition], State, TapeA, [State])
sr [] (x:xs) _ _ _ _ _ _ _ = sr [x] xs _ _ _ _ _ _ _
sr (LPar : ts) l _ tape _ _ _ _ _   | tape == [] = sr ts l _ _ _ _ _ _ _
                                    | --otherwise is a transition
sr (RPar : ts) l _ tape _ _ _ _ _   | -- will be different because it is at end of file
sr () _ _ _ _ _ _ _ _ = 
sr _ _ _ _ _ _ _ _ _ = 
sr _ _ _ _ _ _ _ _ _ = 

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

isAlpha :: Char -> Bool
isAlpha x = ('A' <= x && x <= 'Z') || ('a' <= x && x <= 'z')

isDigit :: Char -> Bool
isDigit x = '0' <= x && x <= '9'

isState :: String -> Bool
isState "" = False
isState ('q':x:xs) = 
  let q1 "" = True
      q1 (x:xs) | isDigit x = q1 xs
      q1 _ = False
  in  isDigit x && q1 xs
isState _ = False

toState :: String -> State
toState ('q':xs)  = toState xs
toState (x:xs)    = (read x) : toState xs
toState []        = []

-- Anything that can be in the INPUT or TAPE alphabet
isAlphabet :: String -> Bool
isAlphabet "" = False
isAlphabet (x:xs) = (x /= 'q') && (x /= 'd')

isValid :: String -> Bool
isValid x = x `elem` alphabet

--[Transition] == [(InTransition, OutTransition)] == [( (State,Alpha) , (State,TapeA,Char) )]
canTransition :: [Transition] -> InTransition -> Transition
canTransition [] _ = null
canTransition (t:ts) (s,a) | first (first t) == s && second (first t) == a = second t
                           | otherwise = canTransition ts (s,a)
--take in currentState, head of the inputString, currentTapePointer, Tape
--returns new State, new TapePosition, newTape
update :: state -> String -> Integer -> String -> (State, Integer, String)
update s i tp t = let trans  = canTransition transitions (s,i)
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





