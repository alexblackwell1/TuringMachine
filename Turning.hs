import Data.Maybe

--Variables
type State = Int
type TapeA = Char
data Direction = L | R deriving (Eq, Show)
type InTransition = (State, TapeA)
type OutTransition = (State, TapeA, Direction)
type Transition = (InTransition, OutTransition)
--		states	alpha	tape-alpha	transitions	start	blank	final
type Machine = ([State], [TapeA], [TapeA], [Transition], State, TapeA, [State])

-- File Template
-- ({q1, q2, q3, ...}; {0,1,a,b,_, ...}; {a,b,...}; D; q0; _; {q2, q3, ...})
  -- d(q1, a) = (q3, 0, R)
  -- d(q3, 0) = (q1, a, L)
  -- ...
-- where states DO start with q then # and the tape/alphabet cannot have characters that start with q or d/D
-- where the tape alphabet includes the input alphabet and the blank symbol

-- ( { q0 , q1 } ; { 0 , 1 } ; { 0 , 1 , _ } ; D ; q0 ; _ ; { q1 } ) d ( q0 , 0 ) = ( q1 , 0 , R ) d ( q1 , 1 ) = ( q1 , 1 , R )

data Token = Delta | KwS State | KwT TapeA | Dir Direction
  | Comma | Semi | LBracket | RBracket | LPar | RPar | Eql
  | Err deriving Show

classify :: String -> Token
classify "=" = Eql
classify "(" = LPar
classify ")" = RPar
classify "{" = LBracket
classify "}" = RBracket
classify "," = Comma
classify ";" = Semi
classify x | x == "d" || x == "D" = Delta
classify x | isDirection x = Dir (toDirection x)
classify x | isState x = KwS (toState x)
classify x | isTapeA x = KwT (extractKwT x) --one of the last parts that needs to be defined
classify _ = Err

-- input 0 for current. It stands for the current part of the tuple being processed
sr :: [Token] -> [State] -> [TapeA] -> [TapeA] -> [Transition] -> State -> TapeA -> [State] -> Int -> Machine
-- sr [] (x:xs) states tape alpha trans start empty final current = sr [x] xs states tape alpha trans start empty final current
sr (LPar : xs) s t a tr st e f c  | c == 0 = sr xs s t a tr st e f (c+1)
sr (RPar : xs) s t a tr st e f c  | c == 7 = sr xs s t a  tr st e f 4
sr (LBracket : xs) s t a tr st e f c = sr xs s t a tr st e f c
sr (RBracket : xs) s t a tr st e f c = sr xs s t a tr st e f c
sr (Semi : xs) s t a tr st e f c = sr xs s t a  tr st e f (c+1)
sr (Comma : xs) s t a tr st e f c = sr xs s t a tr st e f c
sr (KwS x : xs) s t a tr st e f c | c == 1 = sr xs (x:s) t a tr st e f c
                                           | c == 5 = sr xs s t a tr x e f c
                                           | c == 7 = sr xs s t a tr st e (x:f) c
sr (KwT x : xs) s t a tr st e f c	| c == 2 = sr xs s (x:t) a tr st e f c
									| c == 3 = sr xs s t (x:a) tr st e f c
									| c == 6 = sr xs s t a tr st x f c
sr (Delta : LPar : KwS x1 : Comma : KwT x2 : RPar : Eql : LPar : KwS x3 : Comma : KwT x4 : Comma : Dir x5 : RPar : xs) s t a tr st e f c = sr xs s t a (((x1, x2), (x3, x4, x5)):tr) st e f c
sr (Delta : xs) s t a tr st e f c | c == 4 = sr xs s t a tr st e f c
sr [] s t a tr st e f _ = (s, t, a, tr, st, e, f)
sr (Err : xs) _ _ _ _ _ _ _ _ = error "Could not convert file"
sr (x : xs) _ _ _ _ _ _ _ _ = error (show (x : xs))

extractKwT :: String -> TapeA --Already checked if the string is TapeA
extractKwT (x:xs) = x :: TapeA

first2 (x,y) = x

first3 (x,y,z) = x

first7 (x1,x2,x3,x4,x5,x6,x7) = x1

second2 (x,y) = y

second3 (x,y,z) = y

second7 (x1,x2,x3,x4,x5,x6,x7) = x2

third3 (x,y,z) = z

third7 (x1,x2,x3,x4,x5,x6,x7) = x3

fourth7 (x1,x2,x3,x4,x5,x6,x7) = x4

fifth7 (x1,x2,x3,x4,x5,x6,x7) = x5

sixth7 (x1,x2,x3,x4,x5,x6,x7) = x6

seventh7 (x1,x2,x3,x4,x5,x6,x7) = x7

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
toState ('q':xs)  = read xs

-- Anything that can be in the INPUT or TAPE alphabet
isTapeA :: String -> Bool
isTapeA "" = False
isTapeA (x:xs) = (x /= 'q') && (x /= 'd') && (xs == "")

isDirection :: String -> Bool
isDirection x = x == "R" || x == "L"

toDirection :: String -> Direction
toDirection "R" = R
toDirection "L" = L
toDirection _ = error "invalid direction in reading"

dir2Int :: Direction -> Int
dir2Int a 	| a == L = -1
			| otherwise = 1

--[Transition] == [(InTransition, OutTransition)] == Maybe [( (State,Alpha) , (State,TapeA,Direction) )]
canTransition :: [Transition] -> InTransition -> Maybe OutTransition
canTransition [] _ = Nothing
canTransition (t:ts) (s,a) | first2 (first2 t) == s && second2 (first2 t) == a = Just (second2 t)
                           | otherwise = canTransition ts (s,a)
                           
--take in currentState, currentElement, currentTapePointer, Tape
--returns new State, new TapePosition, newTape
-- update :: Machine -> State -> TapeA -> Int -> [TapeA] -> (State, Int, [TapeA])
-- update m s i tp t | isNothing (canTransition (fourth7 m) (s,i)) = (-1, tp, t)
-- update m s i tp t = update2 m s i tp t (fromJust (canTransition (fourth7 m) (s,i)))

-- JUST updates the tape charater at the nth position (whatever the pointer is pointing to)
update :: Machine -> State -> TapeA -> Int -> [TapeA] -> OutTransition -> (State, Int, [TapeA])
update m s i tp t trans = 	let nTape a b (x:xs)	| a == 1 = (b:xs)
													| a > 1  = x:(nTape (a-1) b xs)
							in (first3 trans, dir2Int (third3 trans), nTape tp (second3 trans) t)

newElement :: Machine -> Int -> [TapeA] -> TapeA
newElement m a []	| a == 1 = sixth7 m
					| otherwise = error "Length of tape is < pointer"
newElement m a (t:ts) 	| a > 1 = newElement m (a-1) ts
						| otherwise = t

-- take in current state, position of the pointer, tape
turing :: Machine -> State -> Int -> TapeA -> [TapeA] -> State
turing _ s _ _ _ | s < 0 = -1
turing m s p ce t 	| (p < 1 || p > length t || ce == (sixth7 m)) && isNothing (canTransition (fourth7 m) (s,ce)) = s
					| isValid m ce && p >= 1 && p <= (length t) && not (isNothing (canTransition (fourth7 m) (s,ce))) = let u = update m s ce p t (fromJust (canTransition (fourth7 m) (s,ce)))
																														in turing m (first3 u) (p + (second3 u)) (newElement m (p + (second3 u)) (third3 u)) (third3 u)
                    | isValid m ce && p == 0 && not (isNothing (canTransition (fourth7 m) (s,ce))) = 	let u = update m s ce 1 ((sixth7 m):t) (fromJust (canTransition (fourth7 m) (s,ce)))
																										in  turing m (first3 u) (p + (second3 u)) (newElement m (p + (second3 u)) (third3 u)) (third3 u)
                    | isValid m ce && p > (length t) && not (isNothing (canTransition (fourth7 m) (s,ce))) =	let u = update m s ce p (t++[(sixth7 m)]) (fromJust (canTransition (fourth7 m) (s,ce)))
																												in  turing m (first3 u) (p + (second3 u)) (newElement m (p + (second3 u)) (third3 u)) (third3 u)
					| otherwise = -1
-- take out otherwise line and change the 1st case condition to only isNothing

-- turing m s p (t:ts) | t == (sixth7 m) && isNothing (canTransition (fourth7 m) (s,t)) = s
					-- | isValid m t && p >= 1 && p <= (length (t:ts)) && not (isNothing (canTransition (fourth7 m) (s,t))) = 	let u = update m s t p (t:ts)
																															-- in  turing m (first3 u) (p + (second3 u)) (third3 u)
                    -- | isValid m t && p == 0 && not (isNothing (canTransition (fourth7 m) (s,t))) = 	let u = update m s t 1 ((sixth7 m):t:ts)
																									-- in  turing m (first3 u) (p + (second3 u)) (third3 u)
                    -- | isValid m t && p > (length (t:ts)) && not (isNothing (canTransition (fourth7 m) (s,t))) =	let u = update m s t p ((t:ts)++[(sixth7 m)])
																												-- in  turing m (first3 u) (p + (second3 u)) (third3 u)
					-- | otherwise = -1
-- [01_]
-- turing m 1 2 [0]

-- extracting from read/sr
lexerParser :: String -> Machine
lexerParser l = sr (lexer l) [] [] [] [] 0 ' ' [] 0

lexer :: String -> [Token]
lexer s = map classify (words s)
--lexer s = map classify (words (addSpaces s))

xInAlpha :: Machine -> String -> Bool
xInAlpha m [] = True
xInAlpha m (x:xs) | x `elem` (second7 m) = xInAlpha m xs
                | otherwise = False


isFinal :: Machine -> State -> Bool
isFinal m s = s `elem` (seventh7 m)

isValid :: Machine -> TapeA -> Bool
isValid m x = x `elem` (third7 m)



main :: IO()
main = do
    putStrLn "Enter file name: "
    x <- getLine
    contents <- readFile x
    let machine = lexerParser contents
	in 	let loop = do
			putStrLn "Enter a string or quit command"
			y <- getLine
			case y of
				"" -> do
					putStrLn "Bye!"
					return()
				"quit" -> do
					putStrLn "Bye!"
					return()
				_ -> do
					if xInAlpha machine y then 	let run = turing machine (fifth7 machine) 1 (head y) y
												in  if (isFinal machine run) then putStrLn (y ++ " does accept")
													else putStrLn (y ++ " does NOT accept")
					else putStrLn (y ++ " is not a valid input")
					loop
		in loop

