import System.IO
import System.Environment

freePlaces :: String -> Int -> [Int]
freePlaces "" _ = []
freePlaces (x:xs) a = if x == '_' then a : freePlaces xs (a + 1) else freePlaces xs (a + 1)


readFileToList :: Handle -> IO [[Int]]
readFileToList fileHandle = do
   eof <- hIsEOF fileHandle
   if eof then return []
          else do line <- hGetLine fileHandle
                  rest <- readFileToList fileHandle
                  return (freePlaces line 0 : rest)


-- [(współczynnik, potęga)]
--Wielomian i dodawanie zrobiłem podobnie jak na wiki haskella https://wiki.haskell.org/Add_polynomials
type Poly = [(Int,Int)]

addPoly :: Poly -> Poly -> Poly
addPoly [] ys = ys
addPoly xs [] = xs
addPoly (x:xs) (y:ys)
    | snd x == snd y  = (fst x + fst y, snd x):addPoly xs ys
    | snd x < snd y = x:addPoly xs (y:ys)
    | otherwise  = y:addPoly (x:xs) ys

multiplyPolyByX :: Poly -> Poly
multiplyPolyByX = map (\x-> (fst x, snd x + 1))

printPoly :: Poly -> String
printPoly (x:xs) = foldl (\acc y -> acc ++ " + " ++ show (fst y) ++ "*x^" ++ show (snd y)) (show (fst x)) xs

deleteCol :: Int -> [[Int]] -> [[Int]]
deleteCol = map.filter.(/=)

-- skorzystamy ze wzoru rB(x) = rB1(x) + x * rB2(x) poznanego na kursie Matematyki Dyskretnej (twierdzenie 2.13 z wykładu 2 slajd 20)
-- niech B będzie naszą tablicą z polami dopuszczalnymi jak i zabronionymi dla której chcemy policzyć wielomian
-- wybieramy jeden kwadrat dopuszczalny s
-- rB1(x) to wielomian szachowy tablicy B1 która jest taka sama jak tablica B z tą róznicą że kwadrat B jest zabroniony
-- rB2(x) to wielomian szachowy tablicy B2, która jest tablicą B z wykreślonym wierszem i kolumną w której znajdował się
-- kwadrat s


rookPolynomials :: [[Int]] -> Poly
rookPolynomials [] = [(1,0)]
rookPolynomials ([]:xs) = rookPolynomials xs
rookPolynomials (x:xs) = addPoly (rookPolynomials (tail x : xs)) (multiplyPolyByX (rookPolynomials (deleteCol (head x) xs))) 


main :: IO ()
main = do
          (inFileName:rest) <-getArgs
          inFileHandle <-openFile inFileName ReadMode
          list<-readFileToList inFileHandle
          print (printPoly (rookPolynomials list))
          hClose inFileHandle
      