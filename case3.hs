-- Case Study 3 -- Tautology Checker

data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Or Prop Prop
          | Imply Prop Prop
          | Equiv Prop Prop
          deriving Show

type Subst = [(Char,Bool)]

----- Example propositions for testing -----

p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))

p2 :: Prop
p2 = Equiv (And (Var 'A') (Var 'B')) (And (Var 'B') (Var 'A'))

p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

----------

vars :: Prop -> [Char]
vars (Const b)   = []
vars (Var x)     = [x]
vars (Not p)     = vars p
vars (And p q)   = vars p ++ vars q
vars (Or p q)    = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars (Equiv p q) = vars p ++ vars q

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)

-- n >= 0
bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) bbs ++ map (True:) bbs
          where bbs = bools (n-1)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
           where vs = rmdups (vars p)

find :: Eq k => k -> [(k,v)] -> v
find k t = head [v | (k',v) <- t, k == k']

eval :: Subst -> Prop -> Bool
eval s (Const b)   = b
eval s (Var x)     = find x s
eval s (Not p)     = not (eval s p)
eval s (And p q)   = eval s p && eval s q
eval s (Or p q)    = eval s p || eval s q
eval s (Imply p q) = eval s p <= eval s q
eval s (Equiv p q) = eval s p == eval s q

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]
