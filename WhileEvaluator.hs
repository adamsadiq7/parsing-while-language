
import Prelude hiding (Num)
import qualified Prelude (Num)

type Num = Integer
type Var = String

type Z = Integer
type T = Bool

type State = Var -> Z

data Aexp = N Num | V Var | Mult Aexp Aexp | Add Aexp Aexp |
 Sub Aexp Aexp  deriving (Show, Eq, Read)

data Bexp = TRUE | FALSE | Neg Bexp | And Bexp Bexp |
 Eq Aexp Aexp | Le Aexp Aexp deriving (Show, Eq, Read)

data Stm = Ass Var Aexp | Skip | Comp Stm Stm | If Bexp Stm Stm |
 While Bexp Stm deriving (Show, Eq, Read)

p :: Stm
p = (Comp
 (Ass "y" (N 1))
 (While
 (Neg (Eq (V "x") (N 1)))
 (Comp
 (Ass "y" (Mult (V "y") (V "x")))
 (Ass "x" (Sub (V "x") (N 1))))))


n_val :: Num -> Z
n_val = id

s :: State
s "x" = 7
s "y" = 2
s "z" = 3
s _ = 0

s' :: State
s' "x" = 7
s' "y" = 6
s' "z" = 5
s' _ = 0

a_val :: Aexp -> State -> Z
a_val (N n) s = n_val n
a_val (V v) s = s v
a_val (Mult a b) s = (a_val a s) * (a_val b s)
a_val (Add a b) s = (a_val a s) + (a_val b s)
a_val (Sub a b) s = (a_val a s) - (a_val b s)


b_val :: Bexp -> State -> T
b_val (TRUE) s = True
b_val (FALSE) s = False
b_val (Neg b) s = not (b_val b s)
b_val (And a b) s = (b_val a s) && (b_val b s)
b_val (Eq a b) s = (a_val a s) == (a_val b s)
b_val (Le a b) s = (a_val a s) <= (a_val b s)

update :: State -> Z -> Var -> State
update s' n v' v
  | v' == v = n
  | otherwise = s' v

cond :: (a -> T, a -> a, a -> a) -> (a -> a)
cond (b, s1, s2) s
    | b s == True   = s1 s
    | otherwise     = s2 s

fix :: ((State -> State) -> (State -> State)) -> (State -> State)
fix ff = ff (fix ff)


fix' :: ((a -> a) -> (a -> a)) -> (a -> a)
fix' ff = ff (fix' ff)

s_ds :: Stm -> State -> State
s_ds (Skip) s = s
s_ds (Ass variable aexp') s = update s (a_val aexp' s) variable
s_ds (Comp s1 s2) s = ((s_ds s2)) (s_ds s1 s)
s_ds (If b s1 s2) s = cond(b_val b, s_ds s1 , s_ds s2) s
s_ds (While b s1) s = fix f s where
  f :: (State -> State) -> (State -> State)
  f g = cond(b_val b, g . s_ds s1, id)



--s_ds (Ass "x" (Add (N 100) (N 1))) s "x" in compiler

--s_ds (If TRUE (Ass "x" (N 100)) Skip) s "x"
