{-
	PP Project 2021

	This is where you will write the implementation for the given tasks.
	You can add other modules aswell.
-}

module Tasks where

import Dataset
import Data.List

type CSV = String
type Value = String
type Row = [Value]
type Table = [Row]

{-
	TASK SET 1
-}

-- Task 1
{-
nume tabel exam grades = exam_grades
sau sub format csv = exam_grades_csv
-}
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
getRow row = (charSep ',')((getName row)++","++(show (getFinalGrade row)))
--takes 1 parameter and creates table 
compute_exam_grades :: Table -> Table 
compute_exam_grades = \table ->(["Nume","Punctaj Exam"]):( map getRow  (tail table))

-- Task 2
-- daca pentru a trece cursul se refera la nota din examen sa fie > 2.5
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
-- am pus read show ca sa fac din string de int direct float pt ca func mea returna un int
get_passed_students_percentage :: Table -> Float
get_passed_students_percentage = \table -> (read (show(get_passed_students_num table))::Float) / (getStudentsNo (tail table))

-- Average exam grade

-- atentie ca ia si prima linie care nu reprezinta un student ci header
-- si unde am folosit getstudentsno am pus tail de table
-- pot modifica in functie (tail table) 
--get students no
getStudentsNo :: Table -> Float 
getStudentsNo = \table -> foldr op 0 table where
                op student acc = 1 + acc
--get student's grade
getGrade :: Row -> Float 
getGrade row = (read (head (drop 7 row)))::Float
--get grades sum
getGradesSum :: Table -> Float 
getGradesSum = \table -> foldr op 0 table where
                op row acc = (getGrade row) + acc

get_exam_avg :: Table -> Float
get_exam_avg = \table -> (getGradesSum (tail table)) / (getStudentsNo (tail table))

-- Number of students who gained at least 1.5p from homework:
-- student's sum of hw
getStudentHw:: Row -> Float 
getStudentHw row = foldr op 0 (drop 1 $take 4 row) where
                   op "" acc = acc --pentru cazul cand o celula e doar ""
                   op cell acc = acc + (read cell::Float)
hwSum :: Row  -> Bool 
hwSum row = (getStudentHw row) >= 1.5
-- find students who have hwSum > 1.5 => use filter 

get_passed_hw_num :: Table -> Int
get_passed_hw_num = \table -> foldr op 0 (tail table) where
                              op studRow acc
                                            | (hwSum studRow) = 1 + acc
                                            | otherwise = acc
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
getStudentsQ1Sum = \table -> foldr op 0 (tail table) where --tail de table pt ca prima linie repr nume notaq1 nota q2..
                    op row acc = acc + (getStudentQ1 row)
getQ1Avg :: Table -> Value
getQ1Avg table = show((getStudentsQ1Sum table) / (getStudentsNo  (tail table))) -- tail de table la getStudentNo ca sa nu numere linia pentru header gen
-- #####################
getQ2Grade :: Row -> String 
getQ2Grade row = (head $drop 2 $take 3 row)
getStudentQ2 :: Row -> Float 
getStudentQ2 row 
                | (getQ2Grade row == "") = 0
                | otherwise  = read (getQ2Grade row)::Float

getStudentsQ2Sum :: Table -> Float 
getStudentsQ2Sum = \table -> foldr op 0 (tail table) where --tail de table pt ca prima linie repr nume notaq1 nota q2..
                    op row acc = acc + (getStudentQ2 row)
getQ2Avg :: Table -> Value
getQ2Avg table = show((getStudentsQ2Sum table) / (getStudentsNo (tail table)))
-- #####################
getQ3Grade :: Row -> String 
getQ3Grade row = (head $drop 3 $take 4 row)

getStudentQ3 :: Row -> Float 
getStudentQ3 row 
                | (getQ3Grade row == "") = 0
                | otherwise  = read (getQ3Grade row)::Float

getStudentsQ3Sum :: Table -> Float 
getStudentsQ3Sum = \table -> foldr op 0 (tail table) where --tail de table pt ca prima linie repr nume notaq1 nota q2..
                    op row acc = acc + (getStudentQ3 row)
getQ3Avg :: Table -> Value
getQ3Avg table = show((getStudentsQ3Sum table) / (getStudentsNo (tail table)))
-- #####################
getQ4Grade :: Row -> String 
getQ4Grade row = (head $drop 4 $take 5 row)

getStudentQ4 :: Row -> Float 
getStudentQ4 row 
                | (getQ4Grade row == "") = 0
                | otherwise  = read (getQ4Grade row)::Float

getStudentsQ4Sum :: Table -> Float 
getStudentsQ4Sum = \table -> foldr op 0 (tail table) where --tail de table pt ca prima linie repr nume notaq1 nota q2..
                    op row acc = acc + (getStudentQ4 row)
getQ4Avg :: Table -> Value
getQ4Avg table = show((getStudentsQ4Sum table) / (getStudentsNo (tail table)))
-- #####################
getQ5Grade :: Row -> String 
getQ5Grade row = (head $drop 5 $take 6 row)

getStudentQ5 :: Row -> Float 
getStudentQ5 row 
                | (getQ5Grade row == "") = 0
                | otherwise  = read (getQ5Grade row)::Float

getStudentsQ5Sum :: Table -> Float 
getStudentsQ5Sum = \table -> foldr op 0 (tail table) where --tail de table pt ca prima linie repr nume notaq1 nota q2..
                    op row acc = acc + (getStudentQ5 row)
getQ5Avg :: Table -> Value
getQ5Avg table = show ((getStudentsQ5Sum table) / (getStudentsNo (tail table)))
-- #####################
getQ6Grade :: Row -> String 
getQ6Grade row = (head $drop 6 $take 7 row)

getStudentQ6 :: Row -> Float 
getStudentQ6 row 
                | (getQ6Grade row == "") = 0
                | otherwise  = read (getQ6Grade row)::Float

getStudentsQ6Sum :: Table -> Float 
getStudentsQ6Sum = \table -> foldr op 0 (tail table) where --tail de table pt ca prima linie repr nume notaq1 nota q2..
                    op row acc = acc + (getStudentQ6 row)
getQ6Avg :: Table -> Value
getQ6Avg table = show ((getStudentsQ6Sum table) / (getStudentsNo (tail table)))
-- #####################
-- [["q1_avg","q2_avg","q3_Avg"],
-- !! nu mi dau bine toate mediile
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
getQ1_0 :: Table ->  Integer  
getQ1_0 table = foldr op 0 (tail table) where 
                 op row acc 
                        | ((getStudentQ1 row)== 0 || (show(getStudentQ1 row))== "") = 1 + acc
                        | otherwise = acc
getQ1_1 :: Table ->  Integer  
getQ1_1 table = foldr op 0 (tail table) where 
                 op row acc 
                        | ((getStudentQ1 row)== 1) = 1 + acc
                        | otherwise = acc
getQ1_2 :: Table ->  Integer  
getQ1_2 table = foldr op 0 (tail table) where 
                 op row acc 
                        | ((getStudentQ1 row)== 2) = 1 + acc
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
                        | ((getStudentQ2 row)== 0 || (show(getStudentQ2 row))== "") = 1 + acc
                        | otherwise = acc
getQ2_1 :: Table ->  Integer  
getQ2_1 table = foldr op 0 (tail table) where 
                 op row acc 
                        | ((getStudentQ2 row)== 1) = 1 + acc
                        | otherwise = acc
getQ2_2 :: Table ->  Integer  
getQ2_2 table = foldr op 0 (tail table) where 
                 op row acc 
                        | ((getStudentQ2 row)== 2) = 1 + acc
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
                        | ((getStudentQ3 row)== 0 || (show(getStudentQ3 row))== "") = 1 + acc
                        | otherwise = acc
getQ3_1 :: Table ->  Integer  
getQ3_1 table = foldr op 0 (tail table) where 
                 op row acc 
                        | ((getStudentQ3 row)== 1) = 1 + acc
                        | otherwise = acc
getQ3_2 :: Table ->  Integer  
getQ3_2 table = foldr op 0 (tail table) where 
                 op row acc 
                        | ((getStudentQ3 row)== 2) = 1 + acc
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
                        | ((getStudentQ4 row)== 0 || (show(getStudentQ4 row))== "" ) = 1 + acc
                        | otherwise = acc
getQ4_1 :: Table ->  Integer  
getQ4_1 table = foldr op 0 (tail table) where 
                 op row acc 
                        | ((getStudentQ4 row)== 1) = 1 + acc
                        | otherwise = acc
getQ4_2 :: Table ->  Integer  
getQ4_2 table = foldr op 0 (tail table) where 
                 op row acc 
                        | ((getStudentQ4 row)== 2) = 1 + acc
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
                        | ((getStudentQ5 row)== 0 || (show(getStudentQ5 row))== "") = 1 + acc
                        | otherwise = acc
getQ5_1 :: Table ->  Integer  
getQ5_1 table = foldr op 0 (tail table) where 
                 op row acc 
                        | ((getStudentQ5 row)== 1) = 1 + acc
                        | otherwise = acc
getQ5_2 :: Table ->  Integer  
getQ5_2 table = foldr op 0 (tail table) where 
                 op row acc 
                        | ((getStudentQ5 row)== 2) = 1 + acc
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
                        | ((getStudentQ6 row)== 0 || (show(getStudentQ6 row))== "") = 1 + acc
                        | otherwise = acc
getQ6_1 :: Table ->  Integer  
getQ6_1 table = foldr op 0 (tail table) where 
                 op row acc 
                        | ((getStudentQ6 row)== 1) = 1 + acc
                        | otherwise = acc
getQ6_2 :: Table ->  Integer  
getQ6_2 table = foldr op 0 (tail table) where 
                 op row acc 
                        | ((getStudentQ6 row)== 2) = 1 + acc
                        | otherwise = acc
getQ6Row :: Table -> Row
getQ6Row table = "Q6":(charSep ',')(
                 (show (getQ6_0 table))++","++
                 (show(getQ6_1 table))++","++
                 (show(getQ6_2 table)))
{-
pentru q1,2,3 nu mi da la 0 bine
pentru q4,5,6 nu da la punctajul de 2dbine 
-}
get_exam_summary :: Table -> Table
get_exam_summary = \table -> (getQ1Row table):
                             (getQ2Row table):
                             (getQ3Row table):
                             (getQ4Row table):
                             (getQ5Row table):
                             (getQ6Row table):[]

-- Task 5
get_ranking :: Table -> Table
get_ranking = undefined

-- Task 6
-- pentru nota din interviul oral nu stiu daca trebuie suma lor sau si / 4
-- incerc doar pentru suma si fac dif = oral (/4 la suma) - scris
--suma int oral = getQSum row
-- func pentru a lua nota scris = getGrade row (cred)
--func pentru diferenta dintre ele
-- dif e sumQ - grade daca sum e mai mare
-- altfel e grade - sum
getDif :: Row -> Float 
getDif row 
         | ((getQSum row) > (getGrade row)) =(getQSum row) - (getGrade row)
         | otherwise = (getGrade row) - (getQSum row)
-- sa fac mai intai tabelul si dupa sa l sortez descresc dupa diferenta?
compareDif :: Float -> Float -> Ordering 
compareDif dif1 dif2
                     |(dif1 <= dif2) = LT -- am pus <= in loc de < pt ca ia si alfabetic sa dea bine la ultimele doua
                     | otherwise  = GT
-- func pt nume = getName row
get_exam_row :: Row -> Row
get_exam_row row = (getName row):
                   (show(getQSum row)):
                   (show(getGrade row)):
                   (show(getDif row)):[]

exam_table_header :: Row    
exam_table_header = ["Nume","Punctaj interviu","Punctaj scris","Diferenta"]
-- obtine tabelul nesortat
get_exam_table :: Table -> Table
get_exam_table table = map get_exam_row (tail table)
-- sa am o functie de sortat row
-- functie care ia dif din exam_table
getDifRow :: Row -> Float 
getDifRow row = read (head(drop 3 row))::Float  

getNameRow :: Row -> String 
getNameRow row = head(take 1 row)

compareNames :: String -> String -> Ordering 
compareNames  n1 n2
                   | (n1 <= n2) = LT
                   | otherwise  = GT
compareRow :: Row -> Row -> Ordering 
compareRow r1 r2 
               | ((getDifRow r1) /= (getDifRow r2)) = (compareDif (getDifRow r1) (getDifRow r2))
               | otherwise  = compareNames (getNameRow r1) (getNameRow r2)
-- daca diferenta e egala trebuie verifica pentru ca in ref e alfabetic dupa nume
get_exam_diff_table :: Table -> Table
get_exam_diff_table = \table -> exam_table_header:(sortBy compareRow (get_exam_table table))
-- mai am de facut pentru task 6 in cazul in care dif sunt = sa iau in ordine alfabetica
