module Eval1
  ( eval
  , State
  )
where

import           AST
import qualified Data.Map.Strict               as M
import           Data.Strict.Tuple
import Data.Maybe (fromJust)

-- Estados
type State = M.Map Variable Int

-- Estado vacío
-- Completar la definición
initState :: State
initState = M.empty

-- Busca el valor de una variable en un estado
-- Completar la definición
lookfor :: Variable -> State -> Int
lookfor var map = fromJust(M.lookup var map)

-- Cambia el valor de una variable en un estado
-- Completar la definición
update :: Variable -> Int -> State -> State
update var x map = M.insert var x map 

-- Evalúa un programa en el estado vacío
eval :: Comm -> State
eval p = stepCommStar p initState

-- Evalúa múltiples pasos de un comando en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> State
stepCommStar Skip s = s
stepCommStar c    s = Data.Strict.Tuple.uncurry stepCommStar $ stepComm c s

-- Evalúa un paso de un comando en un estado dado
-- Completar la definición
stepComm :: Comm -> State -> Pair Comm State
stepComm Skip s = (Skip :!: s)
stepComm (Let v e) s =  let (n :!: s') = evalExp e s
                        in (Skip :!: update v n s')
stepComm (Seq Skip c2) s = (c2 :!: s)
stepComm (Seq c1 c2) s = let (c1' :!: s') = stepComm c1 s
                          in ((Seq c1' c2) :!: s')
stepComm (IfThenElse b c1 c2) s = let (b' :!: s') = evalExp b s
                                  in if b'
                                      then (c1 :!: s')
                                      else (c2 :!: s')  
stepComm (RepeatUntil c b) s = (Seq c (IfThenElse b Skip (RepeatUntil c b)) :!: s)

-- Evalúa una expresión
-- Completar la definición
evalExp :: Exp a -> State -> Pair a State
evalExp (Const n) s = (n :!: s)
evalExp (Var x) s = (lookfor x s :!: s)
evalExp (VarInc x) s = let val = lookfor x s
                           s' = update x (val + 1) s
                       in (val+1 :!: s')
evalExp (UMinus e) s = let (n :!: s') = evalExp e s
                       in (-n :!: s')
evalExp (Plus e1 e2) s = let (n1 :!: s') = evalExp e1 s
                             (n2 :!: s'') = evalExp e2 s'
                             in (n1 + n2 :!: s'')
evalExp (Minus e1 e2) s = let (n1 :!: s') = evalExp e1 s
                              (n2 :!: s'') = evalExp e2 s'
                             in (n1 - n2 :!: s'')
evalExp (Times e1 e2) s = let (n1 :!: s') = evalExp e1 s
                              (n2 :!: s'') = evalExp e2 s'
                             in (n1 * n2 :!: s'')
evalExp (Div e1 e2) s = let (n1 :!: s') = evalExp e1 s
                            (n2 :!: s'') = evalExp e2 s'
                             in (n1 `div` n2 :!: s'')
evalExp BTrue s = (True :!: s)
evalExp BFalse s = (False :!: s)
evalExp (Lt e1 e2) s = let (n1 :!: s') = evalExp e1 s
                           (n2 :!: s'') = evalExp e2 s'
                         in ((n1 < n2) :!: s'')
evalExp (Gt e1 e2) s = let (n1 :!: s') = evalExp e1 s
                           (n2 :!: s'') = evalExp e2 s'
                        in ((n1 > n2) :!: s'')                         
evalExp (And p1 p2) s = let (b1 :!: s') = evalExp p1 s
                            (b2 :!: s'') = evalExp p2 s'
                        in ((b1 && b2) :!: s'')   
evalExp (Or p1 p2) s = let (b1 :!: s') = evalExp p1 s
                           (b2 :!: s'') = evalExp p2 s'
                        in ((b1 || b2) :!: s'')                
evalExp (Eq e1 e2) s = let (n1 :!: s') = evalExp e1 s
                           (n2 :!: s'') = evalExp e2 s'
                         in ((n1 == n2) :!: s'')
evalExp (NEq e1 e2) s = let (n1 :!: s') = evalExp e1 s
                            (n2 :!: s'') = evalExp e2 s'
                         in ((n1 /= n2) :!: s'')
evalExp (Not p) s = let (b :!: s') = evalExp p s
                    in ((not b) :!: s')

                        
