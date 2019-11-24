{-|
Module      : Main
Description : The main file for our Mini C evaluator.
Copyright   : (c) Fernando Abigail Galicia-Mendoza, 2019
License     : GPL-3
Maintainer  : fernandogamen@ciencias.unam.mx
Stability   : experimental-education
Portability : POSIX
-}

import System.Environment
import ParserMinC
import MiniC

-- | The 'transform' function takes a program parsed (called, statement)
-- and returns the abstract syntax of a MiniHaskell program. 
-- [NOTE]: The function deletes the sugar syntax.
transform :: Stmt -> MiniC
transform (Var x) = V x
transform (Num n) = N (fromIntegral n)
transform (Boolean b) = B b
transform (SucS e) = Suc (transform e)
transform (PredS e) = Pred (transform e)
transform (NegS e) = Neg (transform e)
transform (PlusS e1 e2) = Plus (transform e1) (transform e2)
transform (ProdS e1 e2) = Prod (transform e1) (transform e2)
transform (ConjS e1 e2) = Conj (transform e1) (transform e2)
transform (DisyS e1 e2) = Disy (transform e1) (transform e2)
transform (GtS e1 e2) = Gt (transform e1) (transform e2)
transform (LtS e1 e2) = Lt (transform e1) (transform e2)
transform (EquiS e1 e2) = Equi (transform e1) (transform e2)
transform (IftS e1 e2 e3) = Ift (transform e1) (transform e2) (transform e3)
transform (AppS e1 e2) = App (transform e1) (transform e2)
transform (FunS xs e) = unfoldLams xs (transform e)
transform (LetES x e1 e2) = App (L x (transform e2)) (transform e1)
transform (FunFS f xs e) = transform (FunS xs e)
transform (SeqS es) = unfoldseqs es
transform (AssignS e1 e2) = Assign (transform e1) (transform e2)
transform (RefS e) = Ref (transform e)
transform (DeRefS e) = DeRef (transform e)
transform (LiS x) = Li x
transform (WhileS e1 e2) = While (transform e1) (transform e2)
transform VoidS = Void

unfoldseqs :: [Stmt] -> MiniC
unfoldseqs [e] = transform e
unfoldseqs (e:es) = Seq (transform e) (unfoldseqs es)

-- | The 'unfoldLams' takes a list of variables (arguments of a anonymous function)
-- and a Mini Haskell program and returns the composition of the lambdas.
-- In other words, given a list [x1,x2,...,xn] and a program e, the function
-- returns: LU x1 (L x2 (...(L xn e)...))
unfoldLams :: [Name] -> MiniC -> MiniC
unfoldLams [x] e = L x e
unfoldLams (x:xs) e = L x (unfoldLams xs e)

-- | The main function of the evaluator.
main = do
  args <- getArgs
  case args of
    [file] -> do
      x <- parseFile file
      let e = transform x
      putStrLn $ "Your program is:\n\n"++show x++"\nProgram without sugar syntax:\n"
      putStrLn $ show e
      putStrLn "Execution:\n"
      let (m,e') = exec e 
      putStrLn $ printExec $ (m,e')
      putStrLn ""
    _ -> putStrLn "Error: Only put the name of the file."