
-- =============== DO NOT MODIFY ===================

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}

-- ==================================================

module Tasks where

import Dataset
import Data.List
import Text.Printf
import Data.Array
import Data.Char
import Data.Maybe

import Common
import qualified Refs as R
import qualified Dataset as D

type CSV = String
type Value = String
type Row = [Value]
type Table = [Row]
type ColumnName = String

-- Prerequisities
split_by :: Char -> String -> [String]
split_by x = foldr op [""]
  where op char acc
            | char == x = "":acc
            | otherwise = (char:head(acc)):tail(acc)

read_csv :: CSV -> Table
read_csv = (map (split_by ',')) . (split_by '\n')

write_csv :: Table -> CSV
write_csv = (foldr (++) []).
            (intersperse "\n").
            (map (foldr (++) [])).
            (map (intersperse ","))


{-
    TASK SET 1
-}


-- Task 1

--creaza capul de tabel al tabelei formatate rezultat : Name, Average Number of Steps
createHead :: [[Char]] -> [[Char]]
createHead (x:xs) = [x, foldr (\x acc -> acc) "Average Number of Steps" xs]

--realizeaza suma elementelor listei (Row)
sumInteger :: [Int] -> Int
sumInteger = foldr (+) 0

--calculeaza media numarului de pasi si intoarce un sir rezultat din conversia Float -> String
calculateAverage :: (PrintfType t, Integral a) => a -> t
calculateAverage m = printf "%.2f" ((fromIntegral m :: Float) / 8)

--transforma tipul Intreg in String a elementelor listei 
convertStringtoInt :: [String] -> [Int]
convertStringtoInt = map (read :: String -> Int)

--creaza liste pentru fiecare persoana in formatul : Nume, Average Number of Steps
convertRow :: [String] -> [String]
convertRow (x:xs) = [x, calculateAverage (sumInteger (convertStringtoInt xs))]

--creaza setul de date cerut
createDataSet :: [[String]] -> [[String]]
createDataSet = map convertRow

--functia care realizeaza task-ul 1
compute_average_steps :: Table -> Table
compute_average_steps m = createHead (head m) : createDataSet (tail m)


-- Task 2

-- Number of people who have achieved their goal:

sumInteger2 :: (Foldable t, Num b) => t b -> b
sumInteger2 = foldr (+) 0

convertStringtoInt2 :: String -> Int
convertStringtoInt2 = (read :: String -> Int)

convertRow2 m = sumInteger2 (map (read :: String -> Int) m )
convertRow2 :: [String] -> Int

--calculeaza numarul de pasi realizati in 8 ore de catre o persoana
createListStep :: Row -> Int
createListStep [] = 0
createListStep (x : xs) = convertRow2 xs

--functia intoarce numarul de persoane care au facut cel putin 1000 de pasi / zi
get_passed_people_num :: Table -> Int
get_passed_people_num m = length $ filter (>1000) (map createListStep (tail m ))


-- Percentage of people who have achieved their:

get_passed_people_percentage :: Table -> Float
get_passed_people_percentage m = a / b
    where a = fromIntegral (get_passed_people_num m) :: Float
          b = fromIntegral (length m - 1) :: Float

-- Average number of daily steps
get_steps_avg :: Table -> Float
get_steps_avg m = (fromIntegral (sum $ (map createListStep(tail m))) :: Float ) / fromIntegral (length m - 1) :: Float


-- Task 3

--creaza capul de tabel 
createHead3 = ["H10" , "H11" , "H12" , "H13" , "H14" , "H15", "H16" , "H17"]
createHead3 :: [[Char]]

convertStringtoInt3 :: [String] -> [Int]
convertStringtoInt3 m = map (read :: String -> Int) m

sumInteger3 :: (Foldable t, Num b) => t b -> b
sumInteger3 m = foldr (+) 0 m

--pentru fiecare persoana se scrie un string reprezentand valoarea float a mediei aritmetice a pasilor pe ora 
convertRow3 ::  [String] -> [String]
convertRow3 m = [printf "%.2f" ((fromIntegral (sumInteger3 (convertStringtoInt3 m)) :: Float) / fromIntegral (length m) :: Float)]

--creaza setul de date cerut
createDataSet3 :: [[String]] -> [String]
createDataSet3 m = foldr (++) [] (map convertRow3 m)

--creaza elementele listei rezultat, prin apelul recursiv al functiei createListHx  
createListHx :: Table -> Table
createListHx ([]: _ ) = []
createListHx m = [map head m] ++ (createListHx (map tail m))

get_avg_steps_per_h :: Table -> Table
get_avg_steps_per_h m = createHead3 : [createDataSet3 (createListHx (map tail (tail m)))]


-- Task 4

createHead4 :: [[Char]]
createHead4 = ["column", "range1", "range2", "range3"]

--creaza lista Active Minute utilizand functia de la task-ul 3, createListHx
--tabela utilizata din Dataset este physical_activity 
createListAM :: Table -> Table
createListAM m = createListHx (map (drop 3) m)

--se obtin elementele tabelei rezultat prin numararea elementelor din tabela lista AM
--folosim filtrarea corespunzator intervalelor date pentru numarul de minute
--se iau in considerare elementele de pozitiile 4, 5, 6 (dupa coloana)  
createRow :: [String] -> [String]
createRow m = [head m] ++ [printf "%d" (length $ filter(\x -> (x < 50) && (x >= 0)) (convertStringtoInt3 (tail m)))] ++ [printf "%d" (length $ filter(\x -> (x >= 50) && (x < 100)) (convertStringtoInt3 (tail m)))] ++ [printf "%d" (length $ filter(\x -> (x >= 100) && (x < 500)) (convertStringtoInt3 (tail m)))]

createDataSet4 :: [[String]] -> [[String]]
createDataSet4 = map createRow

get_activ_summary :: Table -> Table
get_activ_summary m = createHead4 : createDataSet4 (createListAM m)


-- Task 5

createHead5 :: [[Char]]
createHead5 = ["Name", "Total Steps"]

--se extrage din tabela physical_activity primele 2 coloane (take 2) si se sorteaza 
--utilizand functia sortBy si functia de comparare compareLists
--se utilizeaza o structura if then else pentru ordoanarea dupa coloana 2 si apoi coloana 1
--conform cerintelor
compareLists :: Ord a => [a] -> [a] -> Ordering
compareLists list1 list2 = if head (tail list1) == head (tail list2) then compare (head list1) (head list2) else compare (head (tail list1)) (head (tail list2))

get_ranking :: Table -> Table
get_ranking m = createHead5 : sortBy compareLists (map (take 2) (tail m))


-- Task 6


createHead6 :: [[Char]]
createHead6 = ["Name", "Average first 4h", "Average last 4h", "Difference"]

--se calculeaza media pasilor pentru primele 4 ore in format Float -> String cu ajutorul
--take 4 si a ultimelor 4 utilizand drop 4
calculateAverage6 :: (PrintfType t, Integral a) => a -> t
calculateAverage6 m = printf "%.2f" ((fromIntegral m :: Float) / 4)

--se calculeaza media pasilor pentru primele 4 ore in format Int cu ajutorul
--take 4 si a ultimelor 4 utilizand drop 4
calculateAverage61 :: Integral a => a -> Float
calculateAverage61 m = (fromIntegral m :: Float) / 4

convertRow6 :: [String] -> [String]
convertRow6 (x:xs) = [x, calculateAverage6 (sumInteger (convertStringtoInt (take 4 xs))), calculateAverage6 (sumInteger (convertStringtoInt (drop 4 xs))), printf "%.2f"(abs(calculateAverage61 (sumInteger (convertStringtoInt (take 4 xs)))-calculateAverage61 (sumInteger (convertStringtoInt (drop 4 xs)))))]


createDataSet6 :: [[String]] -> [[String]]
createDataSet6 = map convertRow6

compareLists6 :: [String] -> [String] -> Ordering
compareLists6 list1 list2 = if last list1 == last list2 then compare (head list1) (head list2) else compare  (read (last list1) :: Float) (read (last list2) :: Float)


get_steps_diff_table :: Table -> Table
get_steps_diff_table m = createHead6 : sortBy compareLists6 (createDataSet6 (tail m))


-- Task 7

-- Applies the given function to all the values

--se utilizeaza conceptul Higher Order Functions prin care o functie are ca parametri o alta functie
vmap :: (Value -> Value) -> Table -> Table
vmap f  = map (map f)


-- Task 8

-- Applies the given function to all the entries

rmap :: (Row -> Row) -> [String] -> Table -> Table
rmap f s m = s : map f m

--parametrul primit de functie nu mai este de tipul Table ci este de tipul Row
get_sleep_total :: Row -> Row
get_sleep_total r = head r : [printf "%.2f" (sum (map (read::String -> Float) (tail r)))]

{-
    TASK SET 2
-}

-- Task 1

--functie utilizata la sort si compara in functie de tipul elementului String sau Numeric
compareLists21 :: Int ->[String] -> [String] -> Ordering
compareLists21 nrCol list1 list2 = if list1!!nrCol == list2!!nrCol
     then compare (head list1) (head list2) else if isDigit (head (list1!!nrCol)) then  compare  (read (list1!!nrCol) :: Float)  (read (list2!!nrCol) :: Float) else compare  (list1!!nrCol)  (list2!!nrCol)

--intoarce numarul unei coloane 
--0 daca nu exista coloana
colNr ::  Value -> Row ->Int 
colNr col []  = 0
colNr col (x:xs)  = if x == col then 0 else 1 + colNr col xs 

--sortarea
tsort :: ColumnName -> Table -> Table
tsort column (h : t) = h : sortBy (compareLists21 (colNr column h )) t

-- Task 2

--concateneaza cele 2 tabele daca au acelasi cap de tabel
vunion :: Table -> Table -> Table
vunion (h1 : t1) (h2 : t2)
 =if ( h1 /= h2) then h1 : t1 else (h1 : t1)++t2


-- Task 3

--se face concatenarea randurilor utilizand functia zipWith
--daca lungimea tabelei 1 este mai mica decat cea a tabelei 2, atunci se introduc siruri vide in locul randurilor tabelei 1
hunion :: Table -> Table -> Table
hunion t1 t2 = zipWith (++) (t1 ++ take (length t2 - length t1) (repeat (take (length (head t1)) (repeat "")))) (t2++ take (length t1 - length t2) (repeat (take (length (head t2)) (repeat ""))))

-- Task 4

--se obtine linia din tabela 2 ce face match cu o linie din tabela 1 (au aceeasi valoare pe coloana cheie)
get_matching_line _ [] _ = []
get_matching_line new_field table key_index = filter (\x -> (x !! key_index) == new_field) table

--se modifica fiecare element de pe o linie din tabela 1 corespunzator, daca e cazul si se adauga elementele ramase de pe linia match
modify_element table1 table2 key_index2 index1 value field
    | colNr ((head table1) !! index1) (head table2) /= length (head table2) = 
        if (head (get_matching_line field table2 key_index2) !! (colNr ((head table1) !! index1) (head table2))) == ""
            then value else (head (get_matching_line field table2 key_index2) !! (colNr ((head table1) !! index1) (head table2)))
    | otherwise = value

--se efectueaza join-ul intre continutul celor doua tabele
join_contents [] _ _ _ _ _ = []
join_contents _ [] _ _ _ _ = []
join_contents fst_tb snd_tb key_index1 key_index2 t1 t2
    | (get_matching_line ((head fst_tb) !! key_index1) snd_tb key_index2) == [] = 
        join_contents (tail fst_tb) (snd_tb) key_index1 key_index2 t1 t2
    | otherwise = 
        ((map (\x -> (modify_element t1 t2 key_index2 (colNr x (head fst_tb)) x ((head fst_tb) !! key_index1))) (head fst_tb)) 
        ++ (filter (\x -> colNr ((head t2) !! (colNr x (head (get_matching_line ((head fst_tb) !! key_index1) snd_tb key_index2)))) (head t1) == length (head t1)) (head(get_matching_line ((head fst_tb) !! key_index1) snd_tb key_index2)))) 
        : (join_contents (tail fst_tb) (snd_tb) key_index1 key_index2 t1 t2)

--la final, la header-ul nou de tabela se adauga join-ul continuturilor pentru a obtine tabela rezultat
tjoin :: ColumnName -> Table -> Table -> Table
tjoin key_column t1 t2 = [((head t1) ++ (filter (\x -> colNr x (head t1) == length (head t1)) (head t2)))] ++ (join_contents (tail t1) (tail t2) (colNr key_column (head t1)) (colNr key_column (head t2)) t1 t2)

-- Task 5

--apeleaza f pe una din liniile t1 cu toate liniile din t2 si apoi aplica map 
func f l t = map (\x -> f l x) t

--produs cartezian
cartesian :: (Row -> Row -> Row) -> [ColumnName] -> Table -> Table -> Table
cartesian new_row_function new_column_names t1 t2 = (new_column_names) : (foldr (++) [] (map (\x -> func new_row_function x (tail t2)) (tail t1)))

-- Task 6

--extrage elementele de coloana determinata de keys
func1 l columns = filter(\x-> elem (head x)  columns) l

projection :: [String] -> Table -> Table
projection keys t = transpose ((\x -> func1 x keys) (transpose t))

-- Task 7

--filtreaza randurile prin aplicarea conditiei condition
filterTable :: (Value -> Bool) -> ColumnName -> Table -> Table
filterTable condition key_column t = (head t) : (filter (\x -> condition (x!! (colNr key_column (head t)))) (tail t)) 

{-
    TASK SET 3
-}


-- 3.1

data Query =
    FromTable Table
    | AsList String Query
    | Sort String Query
    | ValueMap (Value -> Value) Query
    | RowMap (Row -> Row) [String] Query
    | VUnion Query Query
    | HUnion Query Query
    | TableJoin String Query Query
    | Cartesian (Row -> Row -> Row) [String] Query Query
    | Projection [String] Query
    | forall a. FEval a => Filter (FilterCondition a) Query -- 3.4
    | Graph EdgeOp Query -- 3.5

instance Show QResult where
    show (List l) = show l
    show (Table t) = show t

class Eval a where
    eval :: a -> QResult

-- extrage indicele numeri al coloanei 
-- de ex colnames = ["Name", "TotalSteps"] se transforma in [0,1]
extNrCol [] t = []
extNrCol (x:xs) t =(colNr x (head t)):(extNrCol xs t)

-- extrage coloana al carei indice este obtinut din extNrCol
extCols [] t = []
extCols (x:xs) t =  t!!x : (extCols xs t) 

--extrage lista cu coloanele indicate in lista colnames
--utilizate eval RowMap 
subList colnames t = map (extCols (extNrCol colnames t)) (tail t)

--utilizat la eval Graph
genGr x y cond acc
    |(head x) == "" || (head y) == "" = acc
    |otherwise = ((min (head x) (head y)):(max (head y) (head x)):(fromJust (cond x y)):[]):acc

instance Eval Query where
    eval (FromTable t) = (Table t)
    eval (AsList colname (FromTable t)) = List (map ( !!fromJust(elemIndex colname (head t))) (tail t))
    eval (Sort colname query) = (Table (op colname (eval query)))
        where
         op colname (Table t) = (head t):sortBy (compareLists21 (colNr colname (head t) )) (tail t)
    -- s-a ales solutia cu where pentru utilizarea cu tipul QResult 

    eval (ValueMap op (FromTable t)) = Table ((head t): (map (map op) (tail t)))
    eval (RowMap op colnames (FromTable t)) = Table (colnames: map op (tail t))
    eval (VUnion (FromTable t1) (FromTable t2)) = (Table(vunion t1 t2))
    eval (HUnion (FromTable t1) (FromTable t2)) = (Table(hunion t1 t2))
    eval (TableJoin colname (FromTable t1) (FromTable t2)) = (Table(tjoin colname t1 t2))
    eval (Cartesian op colnames (FromTable t1) (FromTable t2)) = (Table(cartesian op colnames t1 t2))
    eval (Projection colnames (FromTable t) ) = (Table(projection colnames t))
    eval (Filter (Eq colname list) (FromTable t)) = (Table((head t):(filter (\x -> (feval (head t) (Eq colname list) x)) (tail t))))
    eval (Filter (Lt colname list) (FromTable t)) = (Table((head t):(filter (\x -> (feval (head t) (Lt colname list) x)) (tail t))))
    eval (Filter (Gt colname list) (FromTable t)) = (Table((head t):(filter (\x -> (feval (head t) (Gt colname list) x)) (tail t))))
    eval (Filter (FNot cond) (FromTable t)) = (Table((head t):(filter (\x -> (feval (head t) (FNot cond) x)) (tail t))))
    eval (Filter (In colname list) (FromTable t)) = (Table((head t):(filter (\x -> (feval (head t) (In colname list) x)) (tail t))))
    eval (Filter cond (FromTable t)) = (Table((head t):(filter (\x -> (feval (head t) cond x)) (tail t))))
    eval (Graph cond (FromTable t)) = (Table (["From", "To", "Value"]:(funct  t)))
          where
              funct  t = concat $ faux  (tail t)
                 where
                       faux [] = []
                       faux t = if (compR (head t) (tail t)) /= [] 
                                 then (compR  (head t) (tail t)):(faux  (tail t)) 
                                 else (faux  (tail t))
                            where
                                 compR  x xs = foldr (\y acc -> if (cond x y) /= Nothing
                                 then (genGr x y cond acc) else acc) [] xs


-- 3.2 & 3.3

type FilterOp = Row -> Bool

data FilterCondition a =
    Eq String a |
    Lt String a |
    Gt String a |
    In String [a] | 
    FNot (FilterCondition a) |
    FieldEq String String

class FEval a where
    feval :: [String] -> (FilterCondition a) -> FilterOp

instance FEval Float where 
    feval row (Eq colname ref) = (\x -> if ((read(x !! (colNr colname row)) :: Float) == ref) then True else False)
    feval row (Lt colname ref) = (\x -> if ((read(x !! (colNr colname row)) :: Float) < ref) then True else False)
    feval row (Gt colname ref) = (\x -> if ((read(x !! (colNr colname row)) :: Float) > ref) then True else False)
    feval row (In colname list) = (\x -> if (elem (isfloat x) list) then True else False)
         where 
            isfloat x = if (x !! (colNr colname row) == "") then 0 else (read(x !! (colNr colname row)) :: Float)
    feval row (FNot cond) = not.feval row cond
    feval row (FieldEq colname1 colname2) = (\x -> if ((read(x !! colNr colname1 row) :: Float) == 
        (read(x !! colNr colname2 row) :: Float)) then True else False)

instance FEval String where 
    feval row (Eq colname ref) = (\x -> if x !! (colNr colname row) == ref then True else False)
    feval row (Lt colname ref) = (\x -> if x !! (colNr colname row) < ref then True else False)
    feval row (Gt colname ref) = (\x -> if x !! (colNr colname row) > ref then True else False)
    feval row (In colname list) = (\x -> if (elem(x !! (colNr colname row)) list) then True else False)

    feval row (FNot cond) = not.feval row cond
    feval row (FieldEq colname1 colname2) = (\x -> if ((x !! colNr colname1 row) == 
        (x !! colNr colname2 row)) then True else False)


-- 3.4

-- where EdgeOp is defined:
type EdgeOp = Row -> Row -> Maybe Value

-- cerintele 3.5 si 3.6 nu au fost implementate

-- 3.5
similarities_query :: Query
similarities_query = (FromTable Dataset.eight_hours)

-- 3.6 (Typos)
correct_table :: String -> Table -> Table -> Table
correct_table col csv1 csv2 = []
