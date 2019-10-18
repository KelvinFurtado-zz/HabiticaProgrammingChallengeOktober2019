-- charlist
-- Ex: input : "string"
-- output: ["s","t","r","i","n","g"]
charlist :: String -> [[Char]]
charlist "" = []
charlist (x:xs) = [x] : charlist xs

--sumstring
-- input: "string haskell"
-- output words st: ["string" , "haskell"]
-- output map (charlist) (words st): [["s","t","r","i","n","g"] , ["h","a","s","k","e","l","l"]]
-- output map (letterTonum)(map (charlist)(words st)) : [[1,1,1,1,1,2] , [4,1,1,5,1,1,1]]
--output foldl1 (++)(map (letterTonum)(map (charlist)(words st))) : [1,1,1,1,1,2,4,1,1,5,1,1,1]
--output foldl1 (+) (foldl1 (++)(map (letterTonum)(map (charlist)(words st)))): 1+1+1+1+1+2+4+1+1+5+1+1+1  = 21

sumstring :: String -> Int
sumstring "" = 0
sumstring st = foldl1 (+) (foldl1 (++)(map (letterTonum)(map (charlist)(words st))))


--letterTonum
--input: ["a","b","c"]
--output [1,3,3]

letterTonum :: [[Char]] -> [Int]
letterTonum [] = []
letterTonum (x:xs)
	| elem x ["a","e","i","o","u","l","n","r","s","t"] == True = 1: letterTonum xs
	| elem x ["d","g"] == True = 2 : letterTonum xs
	| elem x ["b","c","m","p"] == True =  3: letterTonum xs
	| elem x ["f","h","v","w","y"] == True = 4: letterTonum xs
	| elem x ["k"] == True = 5: letterTonum xs
	| elem x ["j","x"] == True = 8: letterTonum xs
	| elem x ["q","z"] == True = 10: letterTonum xs
	| otherwise = 0: letterTonum xs 

