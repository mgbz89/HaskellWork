module HW4 where

import Data.Char
import Prelude hiding (Either, Left, Right)

data Op = Val Int | Plus | Minus | Mul | IntDiv  deriving (Show , Eq)
type  PExp   = [Op]

data  RPNError    = DivByZero | InvalidInput  deriving (Show , Eq)
data Either a b = Left a | Right b deriving (Show , Eq)
type RPNResult   = Either  RPNError  Int

rpnParse :: String -> PExp
rpnParse "" = []
rpnParse (x:xs)
  | x == '+' = Plus:rpnParse xs -- add plus op
  | x == '-' = Minus:rpnParse xs -- add minus op
  | x == '*' = Mul:rpnParse xs -- add mul op
  | x == '/' = IntDiv:rpnParse xs -- add div op
  | x == ' ' = rpnParse xs -- ignore spaces
  | isDigit x == True = -- if there is a digit char
    let length = digitsInARow 0 xs -- find how many digits in a row
    in (Val (read (x:(take length xs))::Int)):rpnParse (drop length xs) -- add int for all char digits in a row
  | otherwise = error "Input not valid characters" -- invalid characters

digitsInARow :: Int -> String -> Int
digitsInARow num [] = num -- Helper func to find char ints in a row
digitsInARow num (x:xs)
  | isDigit x = digitsInARow (num + 1) xs
  | otherwise = num


eval :: PExp -> Int
eval [] = error "Empty expression"
eval xs = let answer = foldl operation [] xs -- operation helper function, acc []
          in if(length answer == 1)
                then head answer
                else error "Invalid expression"  -- more than one int left in acc, no ops, invalid
  where operation xs (Val y)      = y:xs -- add Val y to acc
        operation [] _            = error "Invalid expression" -- no int in acc, more ops, invalid
        operation [_] _           = error "Invalid expression" -- 1 int in acc, more ops, invalid
        operation (x:y:ys) Plus   = (y + x):ys -- Perform op on first two in acc
        operation (x:y:ys) Minus  = (y - x):ys
        operation (x:y:ys) Mul    = (y * x):ys
        operation (x:y:ys) IntDiv = (quot y x):ys

evalSafe :: PExp -> RPNResult
evalSafe [] = Left InvalidInput -- Empty exp, invalid
evalSafe xs = evalSafe' [] xs -- use helper with acc []

evalSafe' :: [Int] -> PExp -> RPNResult
evalSafe' [x] []          = Right x -- One int in acc, empty ops, return int
evalSafe' xs []           = Left InvalidInput -- More than one int in acc, empty ops, invalid
evalSafe' xs ((Val y):ys) = evalSafe' (y:xs) ys -- op is Val, add int to acc
evalSafe' [] _            = Left InvalidInput -- no int in acc, more ops, invalid
evalSafe' [_] _           = Left InvalidInput -- 1 int in acc, more ops, invalid
evalSafe'  (x:y:ys) (z:zs) -- 2 ints in acc, 1 op -- do the following operations
  | z == Plus   = evalSafe' ((y + x):ys) zs
  | z == Minus  = evalSafe' ((y - x):ys) zs
  | z == Mul    = evalSafe' ((y * x):ys) zs
  | z == IntDiv = if(x == 0)
                    then Left DivByZero -- op is IntDiv, int is 0, invalid
                    else evalSafe' ((quot y x):ys) zs

rpnTrans :: PExp -> Either String String
rpnTrans xs = print xs []
              where print [] (x:[])               = Right x -- only one char
                    print [] (x:y:[])             = Left "Invalid Input" -- two chars, invalid
                    print ((Val x):(Val y):[]) xs = Left "Invalid Input" -- 2 vals left, no ops, invalid
                    print ((Val x):xs) (ys)       = print xs ((show x):ys) -- val followed by val or op, add int to string
                    print (Plus:xs) (x:y:ys)      = print xs (("(" ++ y ++ " + " ++ x ++ ")"):ys) -- split previous to chars with op
                    print (Minus:xs) (x:y:ys)     = print xs (("(" ++ y ++ " - " ++ x ++ ")"):ys) -- ""
                    print (Mul:xs) (x:y:ys)       = print xs (("(" ++ y ++ " * " ++ x ++ ")"):ys) -- ""
                    print (IntDiv:xs) ("0":y:ys)  = Left "Divide by Zero"
                    print (IntDiv:xs) (x:y:ys)    = print xs (("(" ++ y ++ " / " ++ x ++ ")"):ys) -- ""
                    print _ _                     = Left "Invalid Input" -- Any invalid chars
