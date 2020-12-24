module Part3 where

import Data.List (nub, sort)
------------------------------------------------------------
-- PROBLEM #18
--
-- Проверить, является ли число N простым (1 <= N <= 10^9)
prob18 :: Integer -> Bool
prob18 n = getDivs n == [n]

getDivs :: Integer -> [Integer]
getDivs  = getCurrentDivs 2
  where
    getCurrentDivs :: Integer -> Integer -> [Integer]
    getCurrentDivs _ 1 = []
    getCurrentDivs divisor n 
      |divisor * divisor > n = [n]
      |mod n divisor == 0 = divisor : getCurrentDivs divisor (div n divisor)
      |otherwise = getCurrentDivs (succ divisor) n

------------------------------------------------------------
-- PROBLEM #19
--
-- Вернуть список всех простых делителей и их степеней в
-- разложении числа N (1 <= N <= 10^9). Простые делители
-- должны быть расположены по возрастанию
prob19 :: Integer -> [(Integer, Int)]
prob19 = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #20
--
-- Проверить, является ли число N совершенным (1<=N<=10^10)
-- Совершенное число равно сумме своих делителей (меньших
-- самого числа)
prob20 :: Integer -> Bool
prob20 n = n == sum (divisors n)


-- Делители без самого числа
divisors :: Integral a => a -> [a]
divisors n = (l++) $ nub
  $ concat [[x, n `div` x] | x <- [2..floor . sqrt $ fromIntegral n],
  n `mod` x == 0]
    where
      l = if n == 1 then [] else [1]

------------------------------------------------------------
-- PROBLEM #21
--
-- Вернуть список всех делителей числа N (1<=N<=10^10) в
-- порядке возрастания
prob21 :: Integer -> [Integer]
prob21 n = (sort . divisors) n ++ [n]

------------------------------------------------------------
-- PROBLEM #22
--
-- Подсчитать произведение количеств букв i в словах из
-- заданной строки (списка символов)
prob22 :: String -> Integer
prob22 str = product $ (map count) (words str)
    where
      count :: String -> Integer
      count wght = toInteger $ length (filter(=='i') wght)

------------------------------------------------------------
-- PROBLEM #23
--
-- На вход подаётся строка вида "N-M: W", где N и M - целые
-- числа, а W - строка. Вернуть символы с N-го по M-й из W,
-- если и N и M не больше длины строки. Гарантируется, что
-- M > 0 и N > 0. Если M > N, то вернуть символы из W в
-- обратном порядке. Нумерация символов с единицы.
prob23 :: String -> Maybe String
prob23 = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #24
--
-- Проверить, что число N - треугольное, т.е. его можно
-- представить как сумму чисел от 1 до какого-то K
-- (1 <= N <= 10^10)
prob24 :: Integer -> Bool
prob24 num = checkNum(sqrt (1 + 8 * fromInteger num)) == 0
  where
    checkNum x = x - fromIntegral (floor x)

------------------------------------------------------------
-- PROBLEM #25
--
-- Проверить, что запись числа является палиндромом (т.е.
-- читается одинаково слева направо и справа налево)
prob25 :: Integer -> Bool
prob25 = isPalindrome . show
  where
    isPalindrome xs = xs == reverse xs

------------------------------------------------------------
-- PROBLEM #26
--
-- Проверить, что два переданных числа - дружественные, т.е.
-- сумма делителей одного (без учёта самого числа) равна
-- другому, и наоборот
prob26 :: Integer -> Integer -> Bool
prob26 m n = sum (divisors m) == n && sum (divisors n) == m 

------------------------------------------------------------
-- PROBLEM #27
--
-- Найти в списке два числа, сумма которых равна заданному.
-- Длина списка не превосходит 500
prob27 :: Int -> [Int] -> Maybe (Int, Int)
prob27 = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #28
--
-- Найти в списке четыре числа, сумма которых равна
-- заданному.
-- Длина списка не превосходит 500
prob28 :: Int -> [Int] -> Maybe (Int, Int, Int, Int)
prob28 num eq = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #29
--
-- Найти наибольшее число-палиндром, которое является
-- произведением двух K-значных (1 <= K <= 3)
prob29 :: Int -> Int
prob29 k = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #30
--
-- Найти наименьшее треугольное число, у которого не меньше
-- заданного количества делителей
prob30 :: Int -> Integer
prob30 n = head (filter (\x -> length (divisors x) >= n) triNums)

-- Генератор треугольных чисел
triNums :: [Integer]
triNums = map (\n -> n * (n + 1) `div` 2) [0..]

------------------------------------------------------------
-- PROBLEM #31
--
-- Найти сумму всех пар различных дружественных чисел,
-- меньших заданного N (1 <= N <= 10000)
prob31 :: Int -> Int
prob31 = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #32
--
-- В функцию передаётся список достоинств монет и сумма.
-- Вернуть список всех способов набрать эту сумму монетами
-- указанного достоинства
-- Сумма не превосходит 100
prob32 :: [Int] -> Int -> [[Int]]

prob32 coins 0 = [[]]
prob32 coins sum
  | sum < 0 || null coins = []
  | c == sum = [[c]]
  | otherwise = prob32 cs sum ++ map (\x -> x ++ [c]) (prob32 coins (sum - c))
  where (c:cs) = coins
