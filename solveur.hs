{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Fuse foldr/map" #-}
{-# HLINT ignore "Redundant where" #-}
{-# HLINT ignore "Redundant pair" #-}
{-# HLINT ignore "Use uncurry" #-}
{-# HLINT ignore "Use init" #-}
import System.IO

data Tile = W | EU | EV | T | M  deriving (Eq,Show)
type Game = ([[Tile]], (Int,Int))

showGame :: Game -> String
showGame (xs,_)= montre xs

montre :: [[Tile]] -> String
montre []=""
montre (x:xs)= foldr (\string1 string2-> string1 ++ " " ++ string2) "" (map tiletostring x) ++ "\n" ++ montre xs

tiletostring :: Tile -> String
tiletostring W = "*"
tiletostring EU = "o"
tiletostring EV = "x"
tiletostring T = "T"
tiletostring M = "M"

readGame :: Handle -> IO Game
readGame handle = do
   contents <- hGetContents handle
   let xs = lines contents
       ys = concatMap words $ drop 2 xs
       n = read (head $ tail xs) :: Int
       bs = (reverse.tail.reverse) $ slice  (complete ys) n
       e=  trouveT bs
   return (bs,e)

complete :: [String] -> [Tile]
complete []=[]
complete (x:xs)
    | x== "*" =W:zs
    | x== "o" =EU:zs
    | x== "M" =M:zs
    | x== "T" =T:zs
    | x== "x"=EV:zs
    where zs = complete xs

slice :: [a] -> Int -> [[a]]
slice [] _ =[[]]
slice xs n = take n xs :slice ( drop n xs ) n

trouveT :: [[Tile]] -> (Int,Int)
trouveT xss = (ligne T xss,colone T xss)
  where zs = [map estM xs |  xs <- xss]
        estM M = True
        estM _ = False
        ligne e xss =  head ([i| i<- [0..], e `elem` (xss !! i) ])
        colone e xss = head ([i| i<- [0..], (xss !! ligne e xss ) !! i  == e ])

main :: IO ()
main = do
   putStrLn "Quel est le nom du fichier: "
   fichier <- getLine
   handle <- openFile fichier ReadMode
   game <- readGame handle
   let xs = showGame game
   putStrLn "Voici la position actuelle du combat"
   putStrLn xs
   let a = solve game
   afficheSol a
   hClose handle

afficheSol :: Maybe Game -> IO ()
afficheSol Nothing = do 
   putStrLn "Il n'y a pas de solution"
afficheSol (Just x )= do
   putStrLn "Voici la solution optimale"
   (putStrLn.showGame) x


step :: Game -> [Game]
step (xs,(a,b))=  map (actualise (xs,(a,b))) allplace
   where allplace = checkPlace xs (a,b)
         actualise :: Game -> (Int,Int)-> Game
         actualise (xs,(a,b)) (x,y)=(ls,(x,y))
            where --zs = take a xs ++ [take b (xs !! a) ++ [EV] ++drop (b+1) (xs !! a)] ++ drop (a+1) xs
                  ls = take x xs ++ [take y (xs !! x) ++ [EV] ++drop (y+1) (xs !! x)] ++ drop (x+1) xs

checkPlace :: [[Tile]] -> (Int,Int) -> [(Int,Int)]
checkPlace [] _ =[]
checkPlace xss (a,b)=filter (/=(-1,-1)) $ zipWith (\condition x -> if condition then x else (-1,-1) ) [droite,gauche,haut,bas] [(a,b+1),(a,b-1),(a-1,b),(a+1,b)]
   where droite= b<(length (head xss)-1) && ((xss !! a) !! (b+1) == EU || (xss !! a) !! (b+1) == M)
         gauche= b>0  && ((xss !! a) !! (b-1) == EU ||(xss !! a) !! (b-1) == M )
         haut=a>0 && ((xss !! (a-1)) !! b == EU || (xss !! (a-1)) !! b == M)
         bas=a<(length xss -1) && ((xss !! (a+1)) !! b == EU || (xss !! (a+1)) !! b == M)


ink :: Int -> Game -> [Game]
ink 0 x = [x]
ink k x = zs ++ (hs >>= step)
   where zs = filter estSol ls
         hs = filter (not.estSol) ls
         ls = ink (k-1) x

solveink' :: Int -> Game -> [Game]
solveink' k x = filter estSol (ink k x)

solveink :: Int -> Game -> [Game]
solveink k = solveink' (k-1)

estSol :: Game -> Bool
estSol game = posMinautore game `elem` checkPlace (fst game ) (snd game)

posMinautore :: ([[Tile]], b) -> (Int, Int)
posMinautore game = if (not.null) ([(i,j) | i<-[0..length (fst game) -1], j<- [0.. length (head (fst game))-1], (fst game!!i ) !! j == M ]) then head ([(i,j) | i<-[0..length (fst game) -1], j<- [0.. length (head (fst game))-1], (fst game!!i ) !! j == M ]) else (0,0)

solve :: Game -> Maybe Game
solve w= if (not.null) zs then Just (head zs) else Nothing
      where zs = concat ([solveink k w | k <- [1..n]])
            n = length (fst w)*length (fst w)