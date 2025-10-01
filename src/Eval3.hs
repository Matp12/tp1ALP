module Eval3
  ( eval
  , State
  )
where

import AST
import qualified Data.Map.Strict as M
import Data.Strict.Tuple

-- Estados 
type State = (M.Map Variable Int, String)

-- Estado vacío
-- Completar la definición
initState :: State
initState = (M.empty, "")

-- Busca el valor de una variable en un estado
-- Completar la definición
lookfor :: Variable -> State -> Either Error Int
lookfor var (m, _) = case M.lookup var m of
                    Nothing -> Left UndefVar
                    Just v  -> Right v
 

-- Cambia el valor de una variable en un estado
-- Completar la definición
update :: Variable -> Int -> State -> State
update var x (map,s) = (M.insert var x map, s)   

-- Agrega una traza dada al estado
-- Completar la definición
addTrace :: String -> State -> State
addTrace traza (m, s) = (m, s ++ traza) 

-- Evalúa un programa en el estado vacío
eval :: Comm -> Either Error State
eval p = stepCommStar p initState

-- Evalúa múltiples pasos de un comnado en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> Either Error State
stepCommStar Skip s = return s
stepCommStar c    s = do
  (c' :!: s') <- stepComm c s
  stepCommStar c' s'

-- Evalúa un paso de un comando en un estado dado
-- Completar la definición
stepComm :: Comm -> State -> Either Error (Pair Comm State)
stepComm Skip s = Right (Skip :!: s)
stepComm (Let v e) s = do
                        (n :!: s') <- evalExp e s
                        let s'' = update v n s'
                            s''' = addTrace ("Let " ++ v ++ " " ++ show n ++ " ") s'' 
                        return (Skip :!: s''')
stepComm (Seq Skip c2) s = Right (c2 :!: s)
stepComm (Seq c1 c2) s = do
                          (c1' :!: s') <- stepComm c1 s
                          return ((Seq c1' c2) :!: s')
stepComm (IfThenElse b c1 c2) s = do
                                    (b' :!: s') <- evalExp b s
                                    if b'
                                      then return (c1 :!: s')
                                      else return (c2 :!: s')  
stepComm (RepeatUntil c b) s = Right (Seq c (IfThenElse b Skip (RepeatUntil c b)) :!: s)


-- Evalúa una expresión
-- Completar la definición
evalExp :: Exp a -> State -> Either Error (Pair a State)
evalExp (Const n) s = Right (n :!: s)
evalExp (Var x) s = do 
                      n <- lookfor x s
                      return (n :!: s)
evalExp (VarInc x) s = do 
                        val <- lookfor x s 
                        let s' = update x (val + 1) s    
                        return (val+1 :!: s')
evalExp (UMinus e) s = do 
                        (n :!: s') <- evalExp e s
                        return (-n :!: s')
evalExp (Plus e1 e2) s = do
                          (n1 :!: s') <- evalExp e1 s
                          (n2 :!: s'') <- evalExp e2 s'
                          return (n1 + n2 :!: s'')
evalExp (Minus e1 e2) s = do
                          (n1 :!: s') <- evalExp e1 s
                          (n2 :!: s'') <- evalExp e2 s'
                          return (n1 - n2 :!: s'')
evalExp (Times e1 e2) s = do
                          (n1 :!: s') <- evalExp e1 s
                          (n2 :!: s'') <- evalExp e2 s'
                          return (n1 * n2 :!: s'')
evalExp (Div e1 e2) s = do
                          (n1 :!: s') <- evalExp e1 s
                          (n2 :!: s'') <- evalExp e2 s'
                          if n2 == 0 
                            then Left DivByZero
                            else Right (n1 `div` n2 :!: s'')
evalExp BTrue s = Right(True :!: s)
evalExp BFalse s = Right(False :!: s)
evalExp (Lt e1 e2) s = do
                          (n1 :!: s') <- evalExp e1 s
                          (n2 :!: s'') <- evalExp e2 s'
                          return ((n1 < n2) :!: s'')
evalExp (Gt e1 e2) s = do
                          (n1 :!: s') <- evalExp e1 s
                          (n2 :!: s'') <- evalExp e2 s'
                          return ((n1 > n2) :!: s'')                   
evalExp (And p1 p2) s = do
                          (n1 :!: s') <- evalExp p1 s
                          (n2 :!: s'') <- evalExp p2 s'
                          return ((n1 && n2) :!: s'')
evalExp (Or p1 p2) s = do
                          (n1 :!: s') <- evalExp p1 s
                          (n2 :!: s'') <- evalExp p2 s'
                          return ((n1 || n2) :!: s'')              
evalExp (Eq e1 e2) s = do
                          (n1 :!: s') <- evalExp e1 s
                          (n2 :!: s'') <- evalExp e2 s'
                          return ((n1 == n2) :!: s'')
evalExp (NEq e1 e2) s = do
                          (n1 :!: s') <- evalExp e1 s
                          (n2 :!: s'') <- evalExp e2 s'
                          return ((n1 /= n2) :!: s'')
evalExp (Not p) s = do
                      (b :!: s') <- evalExp p s
                      return ((not b) :!: s')
                        
