data Tree = Leaf Int | Branch Tree Tree

height :: Tree -> Int
height (Leaf x) = 
height (Branch left right)
	| (height light) > (height right) = (height left)+1 
	| otherwise = (height right) + 1

data Tree = Leaf Int | Node Tree Int Tree
collectnode :: Int -> Tree -> [a]
collectnode n (Leaf x)
	| n < x = n:
	| otherwise = [] 
collectnode n (Node lt x rt)
	| collectnode n lt ++ [x] ++ collectnode n rt
	| otherwise = collectnode n rt ++ collectnode n lt
