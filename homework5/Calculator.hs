{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Calc where

import Parser
import ExprT
import StackVM
import Data.Maybe

import qualified Data.Map as M

-- Exercise 1

eval :: ExprT -> Integer
eval (ExprT.Lit i) = i
eval (ExprT.Add x y) = eval x + eval y
eval (ExprT.Mul x y) = eval x * eval y


-- Exercise 2

evalStr :: String -> Maybe Integer
evalStr =  mInt . parseExp ExprT.Lit ExprT.Add ExprT.Mul
    where
        mInt :: Maybe ExprT -> Maybe Integer
        mInt Nothing = Nothing
        mInt (Just x) =  Just $ eval x


-- Exercise 3

class Expr a where
    add :: a -> a -> a
    mul :: a -> a -> a
    lit :: Integer -> a

reify :: ExprT -> ExprT
reify = id


-- Exercise 4

instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr ExprT where
    add = ExprT.Add
    mul = ExprT.Mul
    lit = ExprT.Lit

instance Expr Bool where
    add = (||)
    mul = (&&)
    lit x
        | x > 0 = True
        | otherwise = False

newtype MinMax  = MinMax Integer deriving (Eq, Show)
newtype Mod7    = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
    add (MinMax x) (MinMax y) = MinMax (max x y)
    mul (MinMax x) (MinMax y) = MinMax (min x y)
    lit = MinMax

instance Expr Mod7 where
    add (Mod7 x) (Mod7 y) = Mod7 ((x + y) `mod` 7)
    mul (Mod7 x) (Mod7 y) = Mod7 ((x * y) `mod` 7)
    lit x = Mod7 (x `mod` 7)


testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3*-4) + 5"

testInteger  = testExp :: Maybe Integer
testBool     = testExp :: Maybe Bool
testMM       = testExp :: Maybe MinMax
testSat      = testExp :: Maybe Mod7


-- Exercise 5

instance Expr Program where
    add x y = x ++ y ++ [StackVM.Add]
    mul x y = x ++ y ++ [StackVM.Mul]
    lit x = [StackVM.PushI x]

compile :: String -> Maybe Program
compile = parseExp lit add mul


-- Exercise 6

class HasVars a where
    var :: String -> a

data VarExprT = VarExprT String Integer
    deriving (Show, Eq)

instance HasVars (M.Map String Integer -> Maybe Integer) where
    var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
    lit x = (\_ -> Just x)
    add x y = (\a -> if isNothing (x a) || isNothing (y a) then Nothing else Just (fromJust (x a) + fromJust (y a)))
    mul x y = (\a -> if isNothing (x a) || isNothing (y a) then Nothing else Just (fromJust (x a) * fromJust (y a)))

withVars :: [(String, Integer)] -> (M.Map String Integer -> Maybe Integer) -> Maybe Integer
withVars vs exp = exp $ M.fromList vs

