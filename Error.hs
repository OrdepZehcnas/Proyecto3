module Error where

import Data.List (union,(\\))
import Data.Char (isDigit)

data Frame = PlusL () MiniHs
           | PlusR MiniHs ()
           | SucF ()
           | PredF ()             
           | ProdL () MiniHs
           | ProdR MiniHs ()
           | NegF ()
           | ConjL () MiniHs
           | ConjR MiniHs ()
           | DisyL () MiniHs
           | DisyR MiniHs ()
           | GtL () MiniHs
           | GtR MiniHs ()
           | LtL () MiniHs
           | LtR MiniHs ()
           | EquiL () MiniHs
           | EquiR MiniHs ()
           | IftF () MiniHs MiniHs
           | AppL () MiniHs
           | AppR MiniHs ()
           | RaiseF ()
           | HandleF () Name MiniHs
           deriving(Show)

type Stack = [Frame]

type Name = String

data MiniHs = V Name -- ^ Constructor for the untyped variables.
           | N Int -- ^ Constructor for untyped numbers.
           | B Bool -- ^ Constructor for untyped booleans.
           | Suc MiniHs -- ^ Constructor for untyped successor operator.
           | Pred MiniHs -- ^ Construtor for untyped predecessor operator.
           | Plus MiniHs MiniHs -- ^ Constructor for untyped plus operator.
           | Prod MiniHs MiniHs -- ^ Constructor for untyped product operator.
           | Neg MiniHs -- ^ Constructor for untyped negation operator.
           | Conj MiniHs MiniHs -- ^ Constructor for untyped conjunction operator.
           | Disy MiniHs MiniHs -- ^ Constructor for untyped disjunction operator.
           | Gt MiniHs MiniHs -- ^ Constructor for untyped greater than operator.
           | Lt MiniHs MiniHs -- ^ Constructor for untyped lower than operator.
           | Equi MiniHs MiniHs -- ^ Constructor for untyped equality operator.
           | Ift MiniHs MiniHs MiniHs -- ^ Constructor for untyped if-else operator.
           | L Name MiniHs -- ^ Constructor for untyped abstraction.
           | Fix Name MiniHs -- ^ Constructor for untyped fix operator.
           | App MiniHs MiniHs -- ^ Constructor for untyped application operator.
           | Raise MiniHs
           | Handle MiniHs Name MiniHs
            deriving(Show)

data KState = Eval Stack MiniHs | Return Stack MiniHs | Err Stack MiniHs deriving(Show)

type Subst = (Name,MiniHs)

fv :: MiniHs -> [Name]
fv _ = error "error"

newId :: Name -> Name
newId x = let (p,s) = splitName x ("","") in
  if s == "" then
    x++"0"
  else
    p++(show (fromInteger $ (read s::Integer)+1))

splitName :: Name -> (Name,Name) -> (Name,Name)
splitName [] s = s
splitName n@(c:cs) (p,s) = case isDigit c of
  True -> (p,n++s)
  False -> splitName cs (p++[c],s)

alpha :: MiniHs -> MiniHs
alpha _ = error "error"


substitution :: MiniHs -> Subst -> MiniHs
substitution _ _ = error "error"

isVal :: MiniHs -> Bool
isVal (N _) = True
isVal (B _) = True
isVal (L _ _) = True
isVal _ = False

eval :: KState -> KState
eval _ = error "K: Undefined."

evals :: KState -> KState
evals _ = error "error"

minus x y = Plus x (Prod y (N (-1)))

restaPos = L "x" (L "y" (Ift (Lt (V "x") (V "y")) (Raise (N 0)) (minus (V "x") (V "y"))))

exampleE = Handle (minus (N 1) (App (App restaPos (N 2)) (Suc (N 2)))) "x" (Prod (N 2) (V "x"))
