

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

data Token = Delta | KwS State | KwT TapeA | Dir Direction
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
classify ";" = Semi
classify x | x == "d" || x == "D" = Delta
classify x | isDirection x = Dir (toDirection x)
classify x | isState x = KwS (toState x)
classify x | isTapeA x = KwT x --one of the last parts that needs to be defined

-- input 0 for current. It stands for the current part of the tuple being processed
sr :: [Token] -> [State] -> [TapeA] -> [TapeA] -> [Transition] -> State -> TapeA -> [State] -> Integer -> ([State], [TapeA], [TapeA], [Transition], State, TapeA, [State])
-- I don't think we'll need the transition stack
-- sr [] (x:xs) states tape alpha trans start empty final current = sr [x] xs states tape alpha trans start empty final current
sr (LPar : xs) s t a tr st e f c  | c == 0 = sr xs s t a tr st e f (c+1)
sr (RPar : xs) s t a tr st e f c  | c == 7 = sr xs s t a  tr st e f 4
sr (LBracket : xs) s t a tr st e f c = sr xs s t a tr st e f c
sr (RBracket : xs) s t a tr st e f c = sr xs s t a tr st e f c
sr (Semi : xs) s t a tr st e f c = sr xs s t a  tr st e f (c+1)
sr (Comma : xs) s t a tr st e f c = sr xs s t a tr st e f c
sr (KeywordState x : xs) s t a tr st e f c | c == 1 = sr xs (x:s) t a tr st e f c
                                           | c == 5 = sr xs s t a tr x e f c
                                           | c == 7 = sr xs s t a tr st e (x:f) c
sr (KeywordTape x : xs) s t a tr st e f c | c == 2 = sr xs s (x:t) a tr st e f c
                                          | c == 3 = sr xs s t (x:a) tr st e f c
                                          | c == 6 = sr xs s t a tr st a f c
sr (Delta : LPar : KwS x1 : Comma : KwT x2 : RPar : Eql : KwS x3 : Comma : KwT x4 : Comma : Dir x5 : RPar : xs) s t a tr st e f c = sr xs s t a (((x1, x2), (x3, x4, x5)):tr) st a f c
sr [] s t a tr st e f _ = (s, t, a, tr, st, e, f)

first :: a -> b
first (x:xs) = x

second :: a -> b
second (x1:x2:xs) = x2

third :: a -> b
third (x1:x2:x3:xs) = x3

fourth :: a -> b
fourth (x1:x2:x3:x4:xs) = x4

fifth :: a -> b
fifth (x1:x2:x3:x4:x5:xs) = x5

sixth :: a -> b
sixth (x1:x2:x3:x4:x5:x6:xs) = x6

seventh :: a -> b
seventh (x1:x2:x3:x4:x5:x6:x7:xs) = x7


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
isTapeA :: String -> Bool
isTapeA "" = False
isTapeA (x:xs) = (x /= 'q') && (x /= 'd')

isValid :: String -> Bool
isValid x = x `elem` alphabet

isDirection :: String -> Bool
isDirection x = x == "R" || x == "L"

toDirection :: String -> Direction
toDirection "R" = R
toDirection "L" = L
toDirection _ = error "invalid direction in reading"

--[Transition] == [(InTransition, OutTransition)] == [( (State,Alpha) , (State,TapeA,Char) )]
canTransition :: [Transition] -> InTransition -> Transition
canTransition [] _ = null
canTransition (t:ts) (s,a) | first (first t) == s && second (first t) == a = second t
                           | otherwise = canTransition ts (s,a)
--take in currentState, head of the inputString, currentTapePointer, Tape
--returns new State, new TapePosition, newTape
update :: state -> String -> Integer -> String -> (State, Integer, String)
-- ***********Will have to look for null cases here... not implemented*****************
update s i tp t = let trans  = canTransition transitions (s,i)
                      tpoint a | a == L = -1
                               | a == R = 1
                      nTape a b (x:xs) | a == 0 = (b:xs)
                                       | a > 0  = x:(ntape (a-1) b xs)
                  in ((first trans), (tpoint (third trans)), (nTape tp (second trans) t))

-- pointer points to element in the tape. If the pointer is ever > or < the tape, add a blank element in front
-- take in current state, position of the pointer, tape, and input string
--turing :: State -> Integer -> [TapeA] -> String -> State
--turing -1 _ _ _ = -1
--turing currentState _ _ [] = currentState
--turing s p t (x:xs) | isValid x && s >= 0 && p > 0 && p < (length t) =  let u = update s p t
--                                                                            a = first u
--                                                                            b = second u
--                                                                            c = currentPointer + (third u)
--                                                                        in  turing (a b c xs)
--                    | isValid x && s >= 0 && p == 0 = let u = update s 1 (blank:t)
--                                                          a = first u
--                                                          b = second u
--                                                          c = currentPointer + (third u)
--                                                      in  turing (a b c xs)
--                    | isValid x && s >= 0 && p > (length t) = let u = update s p (t++[blank])
--                                                                  a = first u
--                                                                  b = second u
--                                                                  c = currentPointer + (third u)
--                                                              in  turing (a b c xs)
--                    | otherwise = -1

-- take in current state, position of the pointer, tape
turing :: State -> Integer -> [TapeA] -> State
turing -1 _ _ = -1
--turing currentState _ _ [] = currentState
turing s p (t:ts) | isValid t && s >= 0 && p > 0 && p < (length (t:ts)) = let u = update s p (t:ts)
                                                                              a = first u
                                                                              b = currentPointer + (second u)
                                                                              c = third u
                                                                          in  turing (a b c)
                    | isValid x && s >= 0 && p == 0 = let u = update s 1 (blank:t)
                                                          a = first u
                                                          b = currentPointer + (second u)
                                                          c = third u
                                                      in  turing (a b c)
                    | isValid x && s >= 0 && p > (length (t:ts)) =  let u = update s p (t++[blank])
                                                                        a = first u
                                                                        b = currentPointer + (second u)
                                                                        c = third u
                                                                    in  turing (a b c)
                    | otherwise = -1

-- extracting from read/sr
lexerParser :: String -> ([State], [TapeA], [TapeA], [Transition], State, TapeA, [State])
lexerParser l = sr (lexer l)

lexer :: String -> [Token]
lexer s = map classify (words (addSpaces s))

machine :: ([State], [TapeA], [TapeA], [Transition], State, TapeA, [State])

xInAlpha :: String -> Bool
xInAlpha [] = True
xInAlpha (x:xs) | x `elem` alphabet = xInAlpha xs
                | otherwise = False

states :: [State]
states = first machine

tape :: [TapeA]
tape = second machine

alphabet :: [TapeA]
alphabet = third machine

transitions :: [Transition]
transitions = fourth machine

start :: State
start = fifth machine

blank :: TapeA
blank = sixth machine

final :: [State]
final = seventh machine

isFinal :: State -> Bool
isFinal s = s `elem` states



main :: IO()
main = do
    putStrLn "Enter file name: "
    x <- getLine
    contents <- readFile x
    machine = lexerParser contents
    let loop = do
        putStrLn "Enter a string or quit command"
        x <- getLine
        case x of
            "quit" -> do
                putStrLn "Bye!"
                return()
            _ -> do
                if xInAlpha x then (let run = turing start 1 x
                                    in  if isFinal run then (putStrLn x 
                                                            putStrLn " does accept")
                                        else (putStrLn x
                                              putStrLn " does NOT accept"))
                              else (putStrLn x
                                    putStrLn " is not a valid input")
                loop
    loop




