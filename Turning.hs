

--Variables
type State = int
type TapeA = Char
type Direction = L | R
data InTransition  = (State, TapeA)
data OutTransition = (State, TapeA, Direction)
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
  | KeywordState State | KeywordTape TapeA | Keyword String
  | Comma | Semi | LBracket | RBracket | LPar | RPar | Eql
  | Err deriving Show

classify :: String -> Token
classify "=" = Eql
classify "(" = LPar
classify ")" = RPap
classify "{" = LBracket
classify "}" = RBracket
classify "," = Comma
classify ";" = Semi
classify x | isState x = KeywordState (toState x)
classify x | isTape x = KeywordTape x --one of the last parts that needs to be defined

-- input 0 for current. It stands for the current part of the tuple being processed
sr :: [Token] -> [State] -> [TapeA] -> [TapeA] -> [Transition] -> State -> TapeA -> [State] -> Integer -> ([State], [TapeA], [TapeA], [Transition], State, TapeA, [State])
-- I don't think we'll need the transition stack
-- sr [] (x:xs) states tape alpha trans start empty final current = sr [x] xs states tape alpha trans start empty final current
sr (LPar : xs) l s t a tr st e f c  | c == 0 = sr xs l s t a tr st e f (c+1)
                                    | --otherwise is a transition
sr (RPar : xs) l s t a tr st e f c  | c == 7 = sr xs l s t a  tr st e f 4
                                    | -- otherwise is a transition
sr (LBracket : xs) l s t a tr st e f c = sr xs l s t a tr st e f c
sr (RBracket : xs) l s t a tr st e f c = sr xs l s t a tr st e f c
sr (KeywordState x : xs)  l s t a tr st e f c | c == 1 = sr xs l (x:s) t a tr st e f c
                                              | c == 5 = sr xs l s t a tr x e f c
                                              | otherwise = sr xs l s t a tr st e (x:f) c
sr (KeywordTape x : xs)  l s t a tr st e f c | c == 2 = sr xs l s (x:t) a tr st e f c
                                                 | c == 
sr (Semi : xs) l s t a tr st e f c = sr xs l s t a  tr st e f (c+1)
sr (Comma : xs) l s t a tr st e f c = sr xs l s t a tr st e f c
sr _ _ _ _ _ _ _ _ _ = 
sr _ _ _ _ _ _ _ _ _ = 
sr _ _ _ _ _ _ _ _ _ = 
sr _ _ _ _ _ _ _ _ _ = 
sr _ _ _ _ _ _ _ _ _ = 
sr _ _ _ _ _ _ _ _ _ = 
sr _ _ _ _ _ _ _ _ _ = 
sr _ _ _ _ _ _ _ _ _ = 

--read
  --the following variables will be instantiated
--states :: [State]
--  states   = ints between {} and before 1st ;
--alphabet :: [TapeA]
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
isTape :: String -> Bool
isTape "" = False
isTape (x:xs) = (x /= 'q') && (x /= 'd')

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





