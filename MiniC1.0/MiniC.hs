{-|
Module      : MiniC
Description : An implementation of the evaluation of untyped Mini C programs.
Copyright   : (c) Fernando Abigail Galicia-Mendoza, 2019
License     : GPL-3
Maintainer  : fernandogamen@ciencias.unam.mx
Stability   : experimental-education
Portability : POSIX

This script contains the functions to calculate the evaluation of typed Mini C
Programs.
-}
module MiniC where

  import Data.List
  import Data.Char

  -- | Type that represents the set of possible variable names.
  type Name = String

  -- | A 'MiniC' is a implementation of the abstract syntax of typed Mini C programs.
  data MiniC = V Name -- ^ Constructor for the typed variables.
              | N Int -- ^ Constructor for the typed numbers.
              | B Bool -- ^ Constructor for the typed booleans.
              | Suc MiniC -- ^ Constructor for the typed successor operator.
              | Pred MiniC -- ^ Constructor for the typed predecessor operator.
              | Plus MiniC MiniC -- ^ Constructor for the typed plus operator.
              | Prod MiniC MiniC -- ^ Constructor for the typed product operator.
              | Neg MiniC -- ^ Constructor for the typed negation operator.
              | Conj MiniC MiniC -- ^ Constructor for the typed conjunction operator.
              | Disy MiniC MiniC -- ^ Constructor for the typed disjunction operator.
              | Gt MiniC MiniC -- ^ Constructor for the typed greater than operator.
              | Lt MiniC MiniC -- ^ Constructor for the typed lower than operator.
              | Equi MiniC MiniC -- ^ Constructor for the typed equality operator.
              | Ift MiniC MiniC MiniC -- ^ Constructor for the typed if-else conditional.
              | L Name MiniC -- ^ Constructor for the typed abstraction.
              | App MiniC MiniC -- ^ Constructor for the typed application.
              -- Imperative operators for memory
              | Li Label -- ^ Constructor for the memory labels
              | Assign MiniC MiniC -- ^ Constructor for the assignments
              | Ref MiniC -- ^ Constructor for the references
              | DeRef MiniC -- ^ Constructor for the dereferences
              | Void -- ^ Constructor for the empty expression
              | Seq MiniC MiniC -- ^ Constructor for the sequence of programs
              | While MiniC MiniC -- ^ Constructor for the while cycle

  type Label = Int

  -- | A 'Memory' is the implementation of a abstract memory. Consists in a pair (l,v) that represents
  -- the expression (l |-> v).
  type Memory = [(Int,MiniC)]

  -- | A 'Exec' is a representation of a state of the execution of the program.
  type Exec = (Memory,MiniC)

  instance Show MiniC where
    show l = case l of
      V x -> x
      N n -> "num["++show n++"]"
      B b -> "bool["++show b++"]"
      Suc e-> "suc("++show e++")"
      Pred e-> "pred("++show e++")"
      Plus e1 e2-> "("++show e1++"+"++show e2++")"
      Prod e1 e2-> "("++show e1++"*"++show e2++")"
      Neg e-> "not("++show e++")"
      Conj e1 e2-> "("++show e1++"&&"++show e2++")"
      Disy e1 e2-> "("++show e1++"||"++show e2++")"
      Gt e1 e2-> "("++show e1++">"++show e2++")"
      Lt e1 e2-> "("++show e1++"<"++show e2++")"
      Equi e1 e2-> "("++show e1++"=="++show e2++")"
      Ift e1 e2 e3-> "if "++show e1++" then "++show e2++" else "++show e3
      L x t -> "(lam ("++x++")"++" => "++show t++")"
      App t s -> "("++show t++" <+> "++show s++")"
      --
      Seq e1 e2 -> "("++show e1++";"++show e2++")"
      Li x -> "(l_"++show x++")"
      Assign e1 e2 -> "("++show e1++" := "++show e2++")"
      Ref e -> "(ref ("++show e++"))"
      DeRef e -> "(!("++show e++"))"
      Void -> "()"
      While e1 e2 -> "(while ("++show e1++","++show e2++"))"

  -- | A 'Subst' represents a substitution.
  type Subst = (Name,MiniC)

  -- | The 'fv' function takes a typed Mini Haskell program and returns their free variables.
  fv :: MiniC -> [Name]
  fv _ = error "error"

  {-|
  The 'newId' function creates a new variable with the following conditions:
  1. If at the end of the variable is not a number then the function 
  add the number 0 at the end of the variable.
  2. If at the end of the variable is a number then the function
  replace the original number with its sucessor.
  -} 
  newId :: Name -> Name
  newId x = let (p,s) = splitName x ("","") in
              if s == "" then
                x++"0"
              else
                p++(show (fromInteger $ (read s::Integer)+1))

  {-|
  The 'splitName' function tries to split strings of the form "vn" returning
  the pair (v,n) where "v" could be any string but "n" is a string with only numbers.
  If the string doesn't end with a number then "n" will be equal to the empty string.
  -}
  splitName :: Name -> (Name,Name) -> (Name,Name)
  splitName [] s = s
  splitName n@(c:cs) (p,s) = case isDigit c of
    True -> (p,n++s)
    False -> splitName cs (p++[c],s)

  -- | The 'alpha' function generates the alpha-equivalence of a typed Mini Haskell program.
  alpha :: MiniC -> MiniC
  alpha _ = error "error"

  -- | The 'substitution' function applies the substitution given as 
  -- a parameter to a typed Mini Haskell program.
  substitution :: MiniC -> Subst -> MiniC
  substitution _ _ = error "error"

  -- | The 'eval' function is an implementation of the evaluation for typed Mini Haskell
  -- programs.
  eval1 :: Exec -> Exec
  eval1 (m,e) = error ("Execution Error: Locked state."++show (m,e))

  -- | The 'newL' function returns a new location memory.
  newL :: Memory -> Int
  newL _ = error "error"

  -- | The 'getVal' function returns the value stored in a memory and location given
  -- as parameters.
  getVal :: Label -> Memory -> MiniC
  getVal _ _ = error "error"

  -- | The 'changeMem' function returns the updated memory.
  changeMem :: Memory -> Label -> MiniC -> Memory
  changeMem _ _ _ = error "error"

  -- | The 'isValue' is the predicate that determines if a typed Mini Haskell
  -- program is a value.
  isVal :: MiniC -> Bool
  isVal (N _) = True
  isVal (B _) = True
  isVal (L _ _) = True
  isVal (Li _) = True
  isVal Void = True
  isVal _ = False

  {-| 
  The 'evals' function is the implementation of the relexive-transitive closure
  of the evaluation relation.
  -}
  evals' :: Exec -> Exec
  evals' _ = error "error" 

  -- | The 'exec' function evaluates a program in a empty memory.
  exec :: MiniC -> Exec
  exec e = error "error"

  printExec :: Exec -> String
  printExec (m,e) = "Memory:\n"++printMem m++"\n\nFinal Execution:\n"++show e

  printMem :: Memory -> String
  printMem [] = ""
  printMem [(l,v)] = show l++" : "++show v
  printMem ((l,v):ms) = show l++" : "++show v++"\n"++printMem ms

  examplee1 = Assign (Ref (N 0)) (Suc (N 1))
  examplee2 = Assign (V "x") (Plus (DeRef (V "1")) (N 3))
  example = App (L "x" (Seq examplee1 examplee2)) (Ref (Pred (N 1)))

  -- l1 = Assign (V "x") (N 1)
  -- l2 = Assign (V "y") (N 0)
  -- l3 = Seq l1 (Seq l2 whilel)
  -- whilel = While (Lt (DeRef (V "x")) (N 2)) l4
  -- l4 = Seq l5 l6
  -- l5 = Assign (V "x") (Plus (DeRef (V "x")) (DeRef (V "x")))
  -- l6 = Assign (V "y") (Plus (DeRef (V "y")) (N 1))

  -- examplel1 = Assign (V "ret") (N 0)
  
  -- examplef = Assign (V "ret") (Plus (V "n") (V "m"))
  -- exampledf =  Assign (V "func1") (L "n" (L "m" examplef))

  -- examplef1 = App (App (DeRef (V "func1")) (V "r")) (N 3)
  -- exampledf1 =  Assign (V "doble") (L "r" (Assign (V "ret") examplef1))

  -- exampleR1 = App (DeRef (V "doble")) (N 4)

  -- examplel4 = Seq examplel1 (Seq exampledf (Seq exampledf1 exampleR1))