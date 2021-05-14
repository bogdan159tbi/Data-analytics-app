{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-
	PP Project 2021

	This is where you will write the implementation for the given tasks.
	You can add other modules aswell.
-}

module Tasks where

import Dataset
import Data.List
import Text.Printf
import Text.Read
type CSV = String
type Value = String
type Row = [Value]
type Table = [Row]

{-
	TASK SET 1
-}

-- Task 1
--takes 1 row at a time and get questions sum 
getQSum :: Row -> Float
getQSum row = foldr op 0 $take 6  $drop 1 row where
                      op "" acc = acc -- pentru cazul in care nu are nota pt intrebare
                      op elem acc = (read elem::Float) / 4 + acc
--finds student's final grade
getFinalGrade :: Row -> Float
getFinalGrade row = (getQSum row) + (read (head(drop 7 row))::Float)
--finds student's name from a row
getName :: Row -> Value
getName row =  head $take 1  row
-- [[String]]. ex = [["Nume1","nota"],["nume2","nota"]]
charSep :: Char -> Value -> Row
charSep sep = foldr op [] where
                op c []
                    | (c == sep) = []
                    | otherwise = [[c]]
                op c (x:xs)
                    | (c == sep) = "":x:xs
                    | otherwise  = (c:x):xs
--create table row after concatenating name&final_grade ,then splitting
getRow :: Row -> Row
getRow row = (charSep ',')((getName row)++","++(printf "%.2f"  (getFinalGrade row)))
--takes 1 parameter and creates table 
compute_exam_grades :: Table -> Table
compute_exam_grades = \table ->(["Nume","Punctaj Exam"]):( map getRow  (tail table))

-- Task 2
-- avem deja functie care verifica final exam grade pt fiecare student
-- functie care numara cati studenti au final_exam > 2.5
isPassed :: Row -> Int
isPassed row
              |(getFinalGrade row >= 2.5) = 1
              | otherwise  = 0
-- Number of students who have passed the exam:
get_passed_students_num :: Table -> Int
get_passed_students_num = \table -> foldr op 0 (tail table) where
                                   op row acc = acc + (isPassed row)

-- Percentage of students who have passed the exam:
get_passed_students_percentage :: Table -> Float
get_passed_students_percentage = \table -> (read (show(get_passed_students_num table))::Float) / (getStudentsNo (tail table))

-- Average exam grade

--get students no
getStudentsNo :: Table -> Float
getStudentsNo = \table -> foldr op 0 table where
                op student acc = 1 + acc
--get student's grade o folosesc la randurile 397 -407
getGrade :: Row -> Float
getGrade row = (read (head (drop 7 row)))::Float
--get grades sum
getGradesSum :: Table -> Float
getGradesSum = \table -> foldr op 0 table where
                op row acc = (getFinalGrade row) + acc

get_exam_avg :: Table -> Float
get_exam_avg = \table -> (getGradesSum (tail table)) / (getStudentsNo (tail table)) -- tail pt a nu lua header ul din table

-- Number of students who gained at least 1.5p from homework:
-- pentru taskul 2.4 am incercat sa folosest un rand din hw_grades
-- sub forma de Student
type Lab = String
type Hw1 = String
type Hw2 = String
type Hw3 = String
type Ex1 = String
type Ex2 = String
type Ex3 = String
type Ex4 = String


data Student = Stud String Lab Hw1 Hw2 Hw3 Ex1 Ex2 Ex3 Ex4
               deriving Show
-- compute final exam grade

toStudent :: Row -> Student
toStudent (name:lab:h1:h2:h3:e1:e2:e3:e4:[]) = Stud name lab h1 h2 h3 e1 e2 e3 e4

--sum for each stud
--tratez cazul cand una dintre h = ""

mread :: String -> Float
mread h
        |(h /= "") = read h::Float
        |otherwise  = 0
getHw :: Student -> Float
getHw (Stud name _ h1 h2 h3 _ _ _ _ ) = mread h1 + mread h2 + mread h3


getStudents :: Table -> Float
getStudents table = foldr op 0 $tail table where
                    op [] acc = acc
                    op row acc = 1 + acc
get_passed_hw_num :: Table -> Int
get_passed_hw_num = \table -> foldr op 0  (tail table) where
                              op row acc
                                        |getHw(toStudent row) >= 1.5 = 1 + acc
                                        |otherwise = acc

-- Task 3
-- we already have a function that gets students number
-- we need a function that gets the sum for each question

--verificam inainte daca pt q1 este ""
getQ1Grade :: Row -> String
getQ1Grade row = (head $tail (take 2 row))
getStudentQ1 :: Row -> Float
getStudentQ1 row
                | (getQ1Grade row == "") = 0
                | otherwise  = read (getQ1Grade row)::Float

getStudentsQ1Sum :: Table -> Float
getStudentsQ1Sum = \table -> foldr op 0 (tail table) where
                    op row acc = acc + (getStudentQ1 row)
getQ1Avg :: Table -> Value
getQ1Avg table =  printf "%.2f"((getStudentsQ1Sum table) / (getStudentsNo  (tail table)))
-- #####################
getQ2Grade :: Row -> String
getQ2Grade row = (head $drop 2 $take 3 row)
getStudentQ2 :: Row -> Float
getStudentQ2 row
                | (getQ2Grade row == "") = 0
                | otherwise  = read (getQ2Grade row)::Float

getStudentsQ2Sum :: Table -> Float
getStudentsQ2Sum = \table -> foldr op 0 (tail table) where
                    op row acc = acc + (getStudentQ2 row)
getQ2Avg :: Table -> Value
getQ2Avg table =  printf "%.2f"((getStudentsQ2Sum table) / (getStudentsNo (tail table)))
-- #####################
getQ3Grade :: Row -> String
getQ3Grade row = (head $drop 3 $take 4 row)

getStudentQ3 :: Row -> Float
getStudentQ3 row
                | (getQ3Grade row == "") = 0
                | otherwise  = read (getQ3Grade row)::Float

getStudentsQ3Sum :: Table -> Float
getStudentsQ3Sum = \table -> foldr op 0 (tail table) where
                    op row acc = acc + (getStudentQ3 row)
getQ3Avg :: Table -> Value
getQ3Avg table =  printf "%.2f"((getStudentsQ3Sum table) / (getStudentsNo (tail table)))
-- #####################
getQ4Grade :: Row -> String
getQ4Grade row = (head $drop 4 $take 5 row)

getStudentQ4 :: Row -> Float
getStudentQ4 row
                | (getQ4Grade row == "") = 0
                | otherwise  = read (getQ4Grade row)::Float

getStudentsQ4Sum :: Table -> Float
getStudentsQ4Sum = \table -> foldr op 0 (tail table) where
                    op row acc = acc + (getStudentQ4 row)
getQ4Avg :: Table -> Value
getQ4Avg table =  printf "%.2f"((getStudentsQ4Sum table) / (getStudentsNo (tail table)))
-- #####################
getQ5Grade :: Row -> String
getQ5Grade row = (head $drop 5 $take 6 row)

getStudentQ5 :: Row -> Float
getStudentQ5 row
                | (getQ5Grade row == "") = 0
                | otherwise  = read (getQ5Grade row)::Float

getStudentsQ5Sum :: Table -> Float
getStudentsQ5Sum = \table -> foldr op 0 (tail table) where
                    op row acc = acc + (getStudentQ5 row)
getQ5Avg :: Table -> Value
getQ5Avg table =  printf "%.2f" ((getStudentsQ5Sum table) / (getStudentsNo (tail table)))
-- #####################
getQ6Grade :: Row -> String
getQ6Grade row = (head $drop 6 $take 7 row)

getStudentQ6 :: Row -> Float
getStudentQ6 row
                | (getQ6Grade row == "") = 0
                | otherwise  = read (getQ6Grade row)::Float

getStudentsQ6Sum :: Table -> Float
getStudentsQ6Sum = \table -> foldr op 0 (tail table) where
                    op row acc = acc + (getStudentQ6 row)
getQ6Avg :: Table -> Value
getQ6Avg table =  printf "%.2f" ((getStudentsQ6Sum table) / (getStudentsNo (tail table)))
-- #####################
-- [["q1_avg","q2_avg","q3_Avg"],
getAvgRow :: Table -> Row
getAvgRow table = (charSep ',')
                  ((getQ1Avg table)++","++
                  (getQ2Avg table)++","++
                  (getQ3Avg table)++","++
                  (getQ4Avg table)++","++
                  (getQ5Avg table)++","++
                  (getQ6Avg table))
average_exam_header = ["Q1","Q2","Q3","Q4","Q5","Q6"]
get_avg_responses_per_qs :: Table -> Table
get_avg_responses_per_qs = \table -> average_exam_header:((getAvgRow table):[])

-- Task 4
--count every grade for each question
getQ1_0 :: Table ->  Integer
getQ1_0 table = foldr op 0 (tail table) where
                 op row acc
                        | ((getStudentQ1 row)== 0.0 || (show(getStudentQ1 row))== "") = 1 + acc
                        | otherwise = acc
getQ1_1 :: Table ->  Integer
getQ1_1 table = foldr op 0 (tail table) where
                 op row acc
                        | ((getStudentQ1 row)== 1.0) = 1 + acc
                        | otherwise = acc
getQ1_2 :: Table ->  Integer
getQ1_2 table = foldr op 0 (tail table) where
                 op row acc
                        | ((getStudentQ1 row)== 2.0) = 1 + acc
                        | otherwise = acc
getQ1Row :: Table -> Row
getQ1Row table = "Q1":(charSep ',')(
                 (show (getQ1_0 table))++","++
                 (show(getQ1_1 table))++","++
                 (show(getQ1_2 table)))
-- ##################
getQ2_0 :: Table ->  Integer
getQ2_0 table = foldr op 0 (tail table) where
                 op row acc
                        | ((getStudentQ2 row)== 0.0 || (show(getStudentQ2 row))== "") = 1 + acc
                        | otherwise = acc
getQ2_1 :: Table ->  Integer
getQ2_1 table = foldr op 0 (tail table) where
                 op row acc
                        | ((getStudentQ2 row)== 1.0) = 1 + acc
                        | otherwise = acc
getQ2_2 :: Table ->  Integer
getQ2_2 table = foldr op 0 (tail table) where
                 op row acc
                        | ((getStudentQ2 row)== 2.0) = 1 + acc
                        | otherwise = acc
getQ2Row :: Table -> Row
getQ2Row table = "Q2":(charSep ',')(
                 (show (getQ2_0 table))++","++
                 (show(getQ2_1 table))++","++
                 (show(getQ2_2 table)))
-- #####################
getQ3_0 :: Table ->  Integer
getQ3_0 table = foldr op 0 (tail table) where
                 op row acc
                        | ((getStudentQ3 row)== 0.0 || (show(getStudentQ3 row))== "") = 1 + acc
                        | otherwise = acc
getQ3_1 :: Table ->  Integer
getQ3_1 table = foldr op 0 (tail table) where
                 op row acc
                        | ((getStudentQ3 row)== 1.0) = 1 + acc
                        | otherwise = acc
getQ3_2 :: Table ->  Integer
getQ3_2 table = foldr op 0 (tail table) where
                 op row acc
                        | ((getStudentQ3 row)== 2.0) = 1 + acc
                        | otherwise = acc
getQ3Row :: Table -> Row
getQ3Row table = "Q3":(charSep ',')(
                 (show (getQ3_0 table))++","++
                 (show(getQ3_1 table))++","++
                 (show(getQ3_2 table)))
-- ######################
getQ4_0 :: Table ->  Integer
getQ4_0 table = foldr op 0 (tail table) where
                 op row acc
                        | ((getStudentQ4 row)== 0.0 || (show(getStudentQ4 row))== "" ) = 1 + acc
                        | otherwise = acc
getQ4_1 :: Table ->  Integer
getQ4_1 table = foldr op 0 (tail table) where
                 op row acc
                        | ((getStudentQ4 row)== 1.0) = 1 + acc
                        | otherwise = acc
getQ4_2 :: Table ->  Integer
getQ4_2 table = foldr op 0 (tail table) where
                 op row acc
                        | ((getStudentQ4 row)== 2.0) = 1 + acc
                        | otherwise = acc
getQ4Row :: Table -> Row
getQ4Row table = "Q4":(charSep ',')(
                 (show (getQ4_0 table))++","++
                 (show(getQ4_1 table))++","++
                 (show(getQ4_2 table)))
-- ######################
getQ5_0 :: Table ->  Integer
getQ5_0 table = foldr op 0 (tail table) where
                 op row acc
                        | ((getStudentQ5 row)== 0.0 || (show(getStudentQ5 row))== "") = 1 + acc
                        | otherwise = acc
getQ5_1 :: Table ->  Integer
getQ5_1 table = foldr op 0 (tail table) where
                 op row acc
                        | ((getStudentQ5 row)== 1.0) = 1 + acc
                        | otherwise = acc
getQ5_2 :: Table ->  Integer
getQ5_2 table = foldr op 0 (tail table) where
                 op row acc
                        | ((getStudentQ5 row)== 2.0) = 1 + acc
                        | otherwise = acc
getQ5Row :: Table -> Row
getQ5Row table = "Q5":(charSep ',')(
                 (show (getQ5_0 table))++","++
                 (show(getQ5_1 table))++","++
                 (show(getQ5_2 table)))
-- ######################
getQ6_0 :: Table ->  Integer
getQ6_0 table = foldr op 0 (tail table) where
                 op row acc
                        | ((getStudentQ6 row)== 0.0 || (show(getStudentQ6 row))== "") = 1 + acc
                        | otherwise = acc
getQ6_1 :: Table ->  Integer
getQ6_1 table = foldr op 0 (tail table) where
                 op row acc
                        | ((getStudentQ6 row)== 1.0) = 1 + acc
                        | otherwise = acc
getQ6_2 :: Table ->  Integer
getQ6_2 table = foldr op 0 (tail table) where
                 op row acc
                        | ((getStudentQ6 row)== 2.0) = 1 + acc
                        | otherwise = acc
getQ6Row :: Table -> Row
getQ6Row table = "Q6":(charSep ',')(
                 (show (getQ6_0 table))++","++
                 (show(getQ6_1 table))++","++
                 (show(getQ6_2 table)))

get_exam_summary :: Table -> Table
get_exam_summary = \table -> ["Q","0","1","2"]:(getQ1Row table):
                             (getQ2Row table):
                             (getQ3Row table):
                             (getQ4Row table):
                             (getQ5Row table):
                             (getQ6Row table):[]
-- Task 5
--getFinalGrade :: Row -> Float  pentru nota finala
-- tabel rezultat = nume,punctaj exam
task5_header :: Row
task5_header = ["Nume","Punctaj Exam"]

compareFinalGrade :: Float -> Float -> Ordering
compareFinalGrade g1 g2
                     | (g1 < g2) = LT
                     | otherwise  = GT
compareNames2 :: String -> String -> Ordering  -- e invers fata de task6
compareNames2  n1 n2
                   | (n1 < n2) = LT
                   | otherwise  = GT
--  sort names alphabetical order if final score is the same

--  get names from exam tables 
-- = functia getRankingName :: Row -> String 
getRankingName :: Row -> String
getRankingName row = undefined
--  get final grades from exam_tables
-- = getFinalGrade :: Row -> Float  de la task 1

--  create row for ranking_table 
getLeaderBoardRow :: Row -> Row
getLeaderBoardRow row = (head row): -- head e pt nume
                        (printf "%.2f" (getFinalGrade row)):[]


--  get final grade from unsorted ranking table
getRankGrade :: Row -> Float
getRankGrade row = read (head (tail row))::Float
--  compara fiecare rand
compareLeaderRow :: Row -> Row -> Ordering
compareLeaderRow r1 r2
                     | (getRankGrade r1 /= getRankGrade r2) = compareFinalGrade (getRankGrade r1 ) (getRankGrade r2)
                     | otherwise  = compareNames2 (head r1) (head r2) -- head e pt nume
--  obtine table nesortat
get_ranking_unsorted :: Table -> Table
get_ranking_unsorted table = map getLeaderBoardRow (tail table)
get_ranking :: Table -> Table
get_ranking = \table -> task5_header:(sortBy compareLeaderRow $ get_ranking_unsorted table)

-- Task 6
-- 
-- incerc doar pentru suma si fac dif = oral (/4 la suma) - scris
--suma int oral = getQSum row
-- func pentru a lua nota scris = getGrade row (cred)
-- func pentru diferenta dintre ele
-- dif e sumQ - grade daca sum e mai mare
-- altfel e grade - sum
getDif :: Row -> Float
getDif row
         | ((getQSum row) > (getGrade row)) =(getQSum row) - (getGrade row)
         | otherwise = (getGrade row) - (getQSum row)
-- sa fac mai intai tabelul si dupa sa l sortez descresc dupa diferenta?

compareDif :: Float -> Float -> Ordering
compareDif dif1 dif2
                     |(dif1 <= dif2) = LT
                     | otherwise  = GT
-- func pt nume = getName row
get_exam_row :: Row -> Row
get_exam_row row = (getName row):
                   (printf "%.2f" (getQSum row)):
                   (printf "%.2f"  (getGrade row)):
                   (printf "%.2f" (getDif row)):[]

exam_table_header :: Row
exam_table_header = ["Nume","Punctaj interviu","Punctaj scris","Diferenta"]
-- obtine tabelul nesortat
get_exam_table :: Table -> Table
get_exam_table table = map get_exam_row (tail table)
-- functie care ia dif din exam_table
getDifRow :: Row -> Float
getDifRow row = read (head(drop 3 row))::Float

getNameRow :: Row -> String
getNameRow row = head(take 1 row)

compareName :: String -> String -> Ordering
compareName  n1 n2
                   | (n1 <= n2) = LT
                   | otherwise  = GT
compareRow :: Row -> Row -> Ordering
compareRow r1 r2
               | ((getDifRow r1) /= (getDifRow r2)) = (compareDif (getDifRow r1) (getDifRow r2))
               | otherwise  = compareName (getNameRow r1) (getNameRow r2)
get_exam_diff_table :: Table -> Table
--get_exam_diff_table = \table -> exam_table_header:(sortBy compareRow (get_exam_table table))
get_exam_diff_table = undefined


-- ############ MILESTONE 2 #####################
splitByR :: (Foldable t, Eq a) => a -> t a -> [[a]]
splitByR sep string = foldr (\char acc@(currentStr:ls) ->
                      if char == sep then [] : acc else (char : currentStr) : ls) [[]] string

-- init elimina ultimul elem dintr o lista
-- init(splitByR ',' (head $tail(splitByR '\n' hw_grades_csv)))
-- ca sa iau cate un rand din hw_grades_csv sub forma de lista cu fiecare elem
data HwGrades = HwGrades {
                    nameHw :: [Char],
                    lab :: Float ,
                    hw1 :: Float,
                    hw2 :: Float,
                    hw3 :: Float,
                    ex1 :: Float,
                    ex2 :: Float,
                    ex3 :: Float,
                    ex4 :: Float
                }

data ExamGrades = ExamGrades {
                    nameEx :: String,
                    q1 :: Float,
                    q2 :: Float,
                    q3 :: Float,
                    q4 :: Float,
                    q5 :: Float,
                    q6 :: Float,
                    ex_scris :: Float
                  }

--reading floats that can be assigned ""
fread :: String -> Float
fread str
        |(str == "") = 0
        |otherwise  = (read str::Float)
--afisare Hw + date
instance Show HwGrades where
  show hw = (nameHw hw) ++" "++(show $lab hw)
            ++ " " ++ (show $hw1 hw) ++
            " "++(show $hw2 hw)++" "++(show $hw3 hw)
            ++" "++(show $ex1 hw)++" "++(show $ex2 hw)
            ++" "++(show $ex3 hw)++" "++(show $ex4 hw)
-- ca sa pot sorta HwGrades trebuie sa fie Ord
compareNames :: HwGrades -> HwGrades -> Bool
compareNames h1 h2 =  (nameHw h1) < (nameHw h2)

equalHwGrades :: HwGrades -> HwGrades -> Bool
equalHwGrades h1 h2 = (nameHw h1) == (nameHw h2)

instance Eq HwGrades where
  (==) = equalHwGrades
instance Ord HwGrades where
  (<=) = compareNames
-- conversie row din hw_grades la HwGrades + data
tble :: [HwGrades]
tble = [HwGrades "Olivia Noah" 0.42 0.49 1.0 1.05 0.0 0.0 0.0 0.0]

toHw :: Row -> HwGrades
toHw (nume:lab:h1:h2:h3:e1:e2:e3:e4:[]) = HwGrades nume
                                              (fread lab)
                                              (fread h1)
                                              (fread h2)
                                              (fread h3)
                                              (fread e1)
                                              (fread e2)
                                              (fread e3)
                                              (fread e4)
-- transform tabelul FARA header
-- intr o lista cu elem de tip hwgrades 
tableHw :: Table -> [HwGrades]
tableHw table = foldr ((:).toHw) [] $tail table

--implementare read_csv
-- lista de elem de tip string unde fiecare elem e un rand din tabel
csvToList :: CSV -> [String]
csvToList csv = (splitByR '\n' csv)

read_csv :: CSV -> Table
read_csv csv = map (splitByR ',' ) (csvToList csv)

--implementare write_csv
--fiecare row sa fie de tipul csv
rowToCsv :: Row -> CSV
rowToCsv row = tail $ foldr concat "\n" row  where -- tail pt ca primul carac din rand o sa fie ,
               concat column acc
                                | (acc == "\n" && column == "") = "," ++ acc
                                | otherwise = "," ++ column ++ acc
write_csv :: Table -> CSV
write_csv table = foldr concatRows "" table where
                  concatRows row csv
                                    |(csv == "") = (head $splitByR '\n' $rowToCsv row)++ csv -- la ultimul rand sa nu mai fie '\n'
                                    | otherwise =  (rowToCsv row)  ++ csv

-- task 1
-- Write a function which takes a column name and a Table 
-- returns the values from that column as a list.
-- numerotarea coloanelor incepe de la 0 !
columnNr :: Row -> String -> Int -> Int
columnNr [] colName acc = acc
columnNr (x:xs) colName acc
       |(x == colName) = acc
       | otherwise = columnNr xs colName (1 + acc)

getColNr :: Table -> String -> Int
getColNr table colName = columnNr (head table) colName 0


-- ia al x -lea elem din fiecare rand din tabel
-- N ar trebui drop colNr -1 ??
getRowElem :: Row -> Int -> String
getRowElem row colNr = head $ (drop colNr) row

-- pun tail table in foldr ca sa nu iau header
-- pe urma iau nr coloanei respective si iau fiecare elem din rand la indexul gasit
-- si l adaug la lista
as_list :: String -> Table -> [String]
as_list colName table =  foldr op [] (tail table) where
                         op row acc = (getRowElem row (getColNr table colName)) : acc

-- task 2
-- column name si table
-- sorteaza table in functie de coloana cu nume = column name
-- ori iau o functie de sortare pentru fiecare coloana
-- criterii de sortare = 1.ascending for numeric values / lexicograph for strings
--                       2.sorted by first column 

-- functie sortare in functie de nr coloana
-- poate sa fie generala a -> a -> ordering deoarece se alege in functie de nr
-- o functie compareAtX ca sa compare in functie de valorile gasite la coloana cu nr X
-- pot fi fie float fie string

-- getFirst = ia prima valoare dintr un hw grades row
-- dar ce fac ca nu stiu din care tabel iau '
-- deci tre sa am tsort pt fiecare tabel => o "interfata" pentru sortarea tabelelor( nu stiu daca tre neaparat)

getAtX :: Int -> Row -> String
getAtX columnNr row = head (drop columnNr row )

cmpXCol :: Int -> Row -> Row -> Ordering
cmpXCol nr r1 r2
               | (getAtX nr r1 == "") && (getAtX nr r2 /= "") = LT
               | (getAtX nr r1 /= "")  && (getAtX nr r2 == "") = GT
               | (getAtX nr r1 == "") && (getAtX nr r2 == "") = compare (getAtX 0 r1)  (getAtX 0 r2)
               |  (readMaybe (getAtX nr r1):: Maybe Double) /= Nothing  && (readMaybe (getAtX nr r1):: Maybe Double) < (readMaybe (getAtX nr r2):: Maybe Double) = LT --(getAtX nr r1) < (getAtX nr r2) = LT 
               | getAtX nr r1 == getAtX nr r2 = compare (getAtX 0 r1)  (getAtX 0 r2)
               | otherwise  = GT

-- pentru prima coloana se ia 0 
tsort :: String -> Table -> Table
tsort colName table = [(head table)] ++ (sortBy (cmpXCol (getColNr table colName)) (tail table))
-- am comentat si in Main.hs
-- task 3 = map
-- value = string

vmap :: (Value -> Value) -> Table -> Table
vmap func table = map (map func) table
-- ex of using vmap
correct_exam_table :: Table
correct_exam_table = vmap (\x -> if x == "" then "0" else x) exam_grades

-- task 4 =  row = [String]
--pentru functia asta iau hw_grades table
-- si fac suma coloanelor 2-8
--functia ia fiecare rand in parte 
--Explicatie: ma folosesc de constructorul meu de tip pentru a lua fiecare elem
--mai usor din fiecare rand si sa le adun pe toate odata
sumCol :: HwGrades -> Float
sumCol hw = (hw1 hw)+
            (hw2 hw) + (hw3 hw)+
            (ex1 hw) + (ex2 hw)+
            (ex3 hw) + (ex4 hw)

-- head row pentru a lua numele din fiecare rand
-- [] pt a-l face row
-- show sum pentru a transforma float ul = suma intr un string
get_hw_grade_total :: Row -> Row
get_hw_grade_total row =  [head row] ++ [printf "%.2f" $ sumCol (toHw row)]

--table_sum = ca sa vad suma pt fiecare nume 
--doar sa testez eu
table_sum :: Table
table_sum = map get_hw_grade_total (tail hw_grades)

rmap :: (Row -> Row) -> [String] -> Table -> Table
rmap func names table = [names] ++ (map func (tail table))

-- Task 5
--adauga coloanele care coincid din t2 la t1 la sfarsit

isEqual :: Row -> Row -> Bool
isEqual [] [] = True
isEqual h1 h2 = foldr (&&) True (zipWith (==) h1 h2)

vunion :: Table -> Table -> Table
vunion t1 t2
            | (isEqual (head t1) (head t2)) = t1 ++ (tail t2)
            | otherwise = t1
--TASK 6
-- ia nr de linii dintr un tabel
getLines :: Table -> Int
getLines table = foldr count 0 table where
                 count row acc = 1 + acc

--verifica dimensiunea liniilor din fiecare tabel
cmpLines :: Table -> Table -> Bool
cmpLines t1 t2 = (getLines t1) < (getLines t2)

--creeaza o linie doar cu spatii libere
getEmptyLine :: Row -> Int -> Row
getEmptyLine emptyRow nrColumns
                       |(nrColumns  > 0) = getEmptyLine ([""] ++ emptyRow) (nrColumns -1)
                       | otherwise = emptyRow
-- in cazul in care un tabel are mai putine linii
-- adauga diffLines linii goale
completeEmptyLines :: Int -> Table -> Table
completeEmptyLines diffLines table
                                   |(diffLines > 0) = completeEmptyLines (diffLines -1) $ table ++ [(getEmptyLine [] (length (head table)))]
                                   |otherwise = table
hunion :: Table -> Table -> Table
hunion t1 t2
            | (cmpLines t1 t2 == True) = zipWith (++) t2 (completeEmptyLines (getLines t2 - getLines t1) t1)
            | otherwise = zipWith (++)  t1 (completeEmptyLines (getLines t1 - getLines t2) t2)
-- TASk 6 done

--TASK 7
--se da numele unei coloane
--adauga coloanele inexistente in t1 din t2
--daca t1 si t2 au acc coloane, diferite de coloana data ca parametru,
--suprascrie cu valorile din coloanele lui t2 daca val != ""
--daca nu exista niciun rand din t2 care sa aiba acc valoare
--la coloana data ca param ,se completeaza cu "" la randul din t1 
--pentru coloanele diferite din t2 fata de t1

memberOf :: (Eq a) =>  a -> [a] -> Bool
memberOf name [] = False
memberOf name (x:xs)
              | (name == x ) = True
              | otherwise  = memberOf name xs
nrColumnDiff :: Row -> Row -> Integer
nrColumnDiff h1 h2 = foldr countDiff 0 h1 where
                     countDiff n1 acc
                                   | (n1 `memberOf` h2) = acc
                                   | otherwise = 1 + acc
{-
h1 = header 1
h2 = header 2
r1 r2 sunt randurile din tabel 1 tabel 2
pentru coloanele cu valorile din header egale
-}

addDiffHeader :: Row -> Row -> Row
addDiffHeader h1 h2 = foldl addDiff h1 h2 where -- daca am pus foldl in loc de foldr tre sa inversez h1 cu h2? (da bine cred asa)
                      addDiff acc column
                                       |(column `memberOf` acc) = acc
                                       | otherwise = acc ++ [column]
--se suprascrie coloana dupa care facem tjoin
--dar daca facem tjoin doar pt randurile care au val din coloana key egale
--oricum nu modifica
--nu stiu daca se cere tjoin si pt randurile care au val din coloana data ca param diferite
modifyRow :: String -> Row -> Row -> Value -> Row
modifyRow _ [] r _ = r
modifyRow columnName (h:header) (v:row) val
                   | (columnName == h && val /= "") = (val:row)
                   | otherwise  = [v] ++ modifyRow columnName header row val

overwriteRows :: Row -> Row -> Row -> Row ->Row
overwriteRows h1 [] r1 _= r1
overwriteRows h1 _ r2 [] = r2
overwriteRows h1 (name2:h2) r1 (v2:r2) = overwriteRows h1 h2 (modifyRow name2 h1 r1 v2) r2

addDiffColumns :: Row -> Row -> Row -> Row -> Row
addDiffColumns [] h2 r1 r2 = r1 ++ r2
addDiffColumns h1 [] r1 _ = r1
addDiffColumns _ _ r1 [] = r1
addDiffColumns (n1:h1) (n2:h2) r1 (v2:r2)
               |(n1 /= n2) = addDiffColumns h1 h2 (r1++[v2]) r2 -- nu stiu daca adaug bine la sfarsitul randului coloana noua
               |otherwise = addDiffColumns h1 h2 r1 r2

joinRows :: Row -> Row -> Row -> Row -> Row
joinRows h1 h2 r1 r2 = addDiffColumns h1 h2 (overwriteRows h1 h2 r1 r2) r2

columnExists :: Row -> String -> Bool
columnExists h1 colName = foldr check False h1 where
                          check name acc =  ((name == colName) || acc)
--verifica valoarea coloanei key dintr un rand
getValueKey :: String -> Row -> Row -> Value
getValueKey key [] _ = []
getValueKey key (h:header) (r:row)
              |(key == h) = r
              | otherwise  = getValueKey key header row

sameValue :: Row -> Row -> Row -> Row -> String -> Bool
sameValue h1 h2 r1 r2 key = (getValueKey key h1 r1) == (getValueKey key h2 r2)
--nu am inteles exact cum se completeaza coloanele care coincid 
--si ce fac cu coloanele care nu coincid

--adauga rand in cazul in care valorile pt key sunt diferite
addDiffRow :: Row -> Row -> Row -> Row -> Row
addDiffRow = undefined

getEntry :: String ->String ->Table -> Row
getEntry key value t = foldr checkRow [] t where
                       checkRow row acc
                                      |(getValueKey key (head t) row == value && acc == []) = acc++row
                                      | otherwise  = acc

tjoin :: String -> Table -> Table -> Table
tjoin key t1 t2 = foldr op [] (tail t1) where
                  op r1 acc = acc ++ [(joinRows (head t1) (head t2) r1 (getEntry key (getValueKey key (head t1) r1) (tail t2)))]
--TASK 8
--iau fiecare rand din t1
--si aplic functia data pe toate randurile din t2 pentru randul r din t1
cartesian :: (Row -> Row -> Row) -> [String] -> Table -> Table -> Table
cartesian generateEntry colNames t1 t2 = [colNames] ++ foldr prdct [] (tail t1) where
                                                        prdct row acc = (map (generateEntry row) (tail t2)) ++ acc
--TASK 9
--return given columns from table
--folosim functia columnExists de la task 7
--unde verific coloana daca exista in lista de nume

-- functie care construieste recursiv fiecare rand
-- take 1 row  => ia coloana doar daca se afla in lista ceruta
-- altfel o ignora
selectColumns :: Row -> Row -> [String] -> Row
selectColumns row header columnList
              | (header == [] || row == [] ) = []
              | (columnExists columnList (head header)) = (take 1 row) ++ (selectColumns (tail row) (tail header) columnList)
              | otherwise  = selectColumns (tail row) (tail header) columnList
-- ia fiecare rand si l adauga dupa ce l modifica
projection :: [String] -> Table -> Table
projection names t1 = foldr addToTable [] t1 where
                      addToTable row acc = (selectColumns row (head t1) names) : acc


--MILESTONE 3

data Query =
    FromCSV CSV |
    ToCSV Query |
    AsList String Query |
    Sort String Query |
    ValueMap (Value -> Value) Query |
    RowMap (Row -> Row) [String] Query |
    VUnion Query Query |
    HUnion Query Query |
    TableJoin String Query Query |
    Cartesian (Row -> Row -> Row) [String] Query Query |
    Projection [String] Query |
    forall a. FEval a => Filter (FilterCondition a) Query |
    Graph EdgeOp Query

-- where EdgeOp is defined:
type EdgeOp = Row -> Row -> Maybe Value

data QResult = CSV CSV |
               Table Table |
               List [String]

instance Show QResult where
       show (CSV csvContent)= show csvContent
       show (Table t) = write_csv t
       show (List stringList) = show stringList

class Eval a where
    eval :: a -> QResult

evalQResToString :: QResult -> Table
evalQResToString (Table table) = table
evalQResToString _ = undefined --nu stiu daca tre neaparat

graphHeader = ["From", "To", "Value"]

fromJust :: Maybe [Char] -> [Char]
fromJust (Just t) = t

getEdge :: Row -> Row -> Maybe [Char] -> Row
getEdge (f1:row1) (f2:row2) val
                              | (f1 < f2) = f1:f2:(fromJust val):[]
                              | otherwise  = f2:f1:(fromJust val):[]


checkEq  (from2:to2:edge) (from1:to1:val)
       |(from1 == from2 && to1 == to2) || (from1 == to2 && from2 == to1) = True
       | otherwise = False

edgeExist :: Table -> Row -> Bool
edgeExist graph (from2:to2:edge)
                            | (graph == []) = True
                            | memberOf True $ map (checkEq (from2:to2:edge)) graph = True
                            | otherwise = False
-- edgeExist acc (getUnorientedEdge row1 row2 edgeop)
getUnorientedEdge :: Table -> Row -> Row
getUnorientedEdge graph row
              | (edgeExist graph row) = row
              | otherwise  = []


findStartingTable :: Row -> Table -> Table
findStartingTable row [] = []
findStartingTable row (r:table)
                     | (row == r) = table
                     | otherwise  = findStartingTable row table

getNodes edgeop row table = foldr op [] table where
                            op rowTable acc
                                   | (row /= rowTable && edgeop row rowTable /= Nothing)
                                      = (getEdge row rowTable (edgeop row rowTable)) : acc
                                   | otherwise  = acc
getGraph edgeop table = graphHeader :
                        (foldr op  [] (tail table)) where
                         op row acc = (getNodes edgeop row (findStartingTable row (tail table)))++acc


test1_graph = eval $ Graph edge_op3 (FromCSV hw_grades_csv)
edge_op3 (n1:l1:_) (n2:l2:_)
            | l1 == l2 = Just l1
            | otherwise = Nothing
instance Eval Query where
       eval (FromCSV str) = Table (read_csv str) -- CSV tip de date = String
       eval (ToCSV query) = CSV (write_csv (evalQResToString (eval query))) -- cum obtinem din query un table
       eval (AsList colname query) = List (as_list colname (evalQResToString (eval query)))
       eval (Sort colname query) = Table (tsort colname (evalQResToString (eval query)))
       eval (ValueMap op query) = Table (vmap op (evalQResToString (eval query)) )
       eval (RowMap op colnames query) = Table (rmap op colnames (evalQResToString (eval query)) )
       eval (VUnion query1 query2) = Table (vunion (evalQResToString (eval query1)) (evalQResToString (eval query2)))
       eval (HUnion query1 query2) = Table (hunion (evalQResToString (eval query1)) (evalQResToString (eval query2)))
       eval (TableJoin colname query1 query2) = undefined -- nu am facut rez la task 7 de la etapa trecuta
       eval (Cartesian op colnames query1 query2) = Table (cartesian op colnames (evalQResToString (eval query1)) (evalQResToString (eval query2)))
       eval (Projection colnames query) = Table (projection colnames (evalQResToString (eval query)) )
       eval (Filter cond query) = Table ([(head (evalQResToString (eval query)))] ++
                                        (filter (feval (head (evalQResToString (eval query))) cond) (tail (evalQResToString (eval query)))))
       eval (Graph edgeop query) = Table (getGraph edgeop (evalQResToString (eval query)) )

data FilterCondition a =
    Eq String a |
    Lt String a |
    Gt String a |
    In String [a] |
    FNot (FilterCondition a) |
    FieldEq String String

type FilterOp = Row -> Bool

class FEval a where
    feval :: [String] -> (FilterCondition a) -> FilterOp

--function that checks if value from colname is equal to value 
getElem :: [String] -> String  -> Row -> String
getElem header colname row = (getRowElem row (columnNr header colname 0))

getFloatElem :: [String] -> String  -> Row -> Float
getFloatElem header colname row
              | getRowElem row (columnNr header colname 0) == "" = 0
              | otherwise = read (getRowElem row (columnNr header colname 0)) :: Float

instance FEval Float where
       feval colnames (Eq colname ref) = (\row -> ((getFloatElem colnames colname ) row)  == ref)
       feval colnames (Lt colname ref) = (\row -> ((getFloatElem colnames colname ) row)  < ref)
       feval colnames (Gt colname ref) = (\row -> ((getFloatElem colnames colname ) row)  > ref)
       feval colnames (In colname list) = \row -> (getFloatElem colnames colname row) `memberOf` list
       feval colnames (FNot cond) = \row -> not ( (feval colnames cond) row)
       feval colnames (FieldEq colname1 colname2) = (\row ->((getFloatElem colnames colname1 ) row) == ((getFloatElem colnames colname2 ) row))

instance FEval String where
       feval colnames (Eq colname ref) = (\row -> ((getElem colnames colname ) row)  == ref)
       feval colnames (Lt colname ref) = (\row -> ((getElem colnames colname ) row)  < ref)
       feval colnames (Gt colname ref) = (\row -> ((getElem colnames colname ) row)  > ref)
       feval colnames (In colname list) = (\row -> memberOf ((getElem colnames colname ) row) list)
       feval colnames (FNot cond) = \row -> not ( (feval colnames cond) row)
       feval colnames (FieldEq colname1 colname2) = (\row ->((getElem colnames colname1 ) row) == ((getElem colnames colname2 ) row))

--tabelul pt similarities este lecture_grades
--header tabel = ["email" + lista de note]


distanceGrades :: Row -> Row -> [Bool]
distanceGrades row1 row2 = zipWith (\x y -> x==y) (tail row1) (tail row2)
calculateEqGrades :: [Bool] -> Integer
calculateEqGrades boolList = foldr op 0 boolList where
                             op boolValue acc
                                   | (boolValue == True) = 1 + acc
                                   | otherwise = acc
isSimilar :: Row -> Row -> Maybe String
isSimilar (email:row1) (email2:row2)
              | (email == "" || email2 == "") = Nothing
              | calculateEqGrades (distanceGrades (email:row1) (email2:row2)) >= 5 = Just (show (calculateEqGrades (distanceGrades (email:row1) (email2:row2))))
              | otherwise = Nothing

compareSimilarities :: Row -> Row -> Ordering
compareSimilarities (from:to:value:[]) (from2:to2:value2:[])
                     | ((read value :: Integer)  < (read value2 :: Integer) ) = LT
                     | ( (read value :: Integer) == (read value2 :: Integer) && from < from2) = LT
                     | otherwise = GT

tsort2 :: String -> Table -> Table
tsort2 colname table = [head table] ++ sortBy compareSimilarities (tail table)

-- Graph operatie query
-- operatie :: row -> row -> maybeValue
-- dupa sort value_column query

similarities_query1 :: Query
similarities_query1 =  Graph isSimilar (FromCSV lecture_grades_csv)
similarities_query2 :: Query
similarities_query2 = Sort "Value" similarities_query1

--imi da ca in ref sortand dupa from si value
--dar valorile in functie de to sunt in ordine aleatoare si in ref si in output
--pentru ca nu se precizeaza criteriul asta
similarities_query :: Query
similarities_query = similarities_query2

test24 = show $ eval $ Filter (In "Q6" [(read "1" :: Float), (read "1" :: Float)]) (FromCSV exam_grades_csv)

plm =  foldr op [] (tail exam_grades) where    
       op row acc = (getFloatElem (head exam_grades) "Q6" row `memberOf` [1,1]) : acc