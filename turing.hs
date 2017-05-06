{-# LANGUAGE MultiWayIf #-}
-- Описание ленты
data Line a = Line [a] a [a]

smallLeft, smallRight :: Line a -> Line a

smallLeft (Line (a:as) x bs) = Line as a (x:bs)
smallRight (Line as x (b:bs)) = Line (x:as) b bs

left l n = 
	if  | n == 0 -> l
		| otherwise -> left (smallLeft l) (n - 1)
right l n = 
	if  | n == 0 -> l
		| otherwise -> right (smallRight l) (n - 1)

exact :: Line a -> a
exact (Line _ x _) = x
--Описание правил
type Regulations = [Rule]
data Rule = Rule {
	state,
	readSymbol,
	nextState,
	writeSymbol,
	jump :: Int
}

--Описание шайтан машины
step :: Line a -> Regulations -> - Line a
step l rules = 

x = Line [-1,-2..] 0 [1,2..]

main :: IO()
main = print (exact (right x 200))