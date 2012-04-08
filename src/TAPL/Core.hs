module TAPL.Core where

-- TODO
type Info = Int
dummyInfo = 0 :: Info
-- End TODO

-- Type for representing terms
data Term = TmTrue Info
          | TmFalse Info
          | TmIf Info Term Term Term
          | TmZero Info
          | TmSucc Info Term
          | TmPred Info Term
          | TmIsZero Info Term
            
isNumericalVal :: Term -> Bool
isNumericalVal (TmZero _)    = True
isNumericalVal (TmSucc _ t1) = isNumericalVal t1
isNumericalVal _             = False 

isVal :: Term -> Bool
isVal (TmTrue _)  = True
isVal (TmFalse _) = True
isVal t
    | isNumericalVal t = True
    | otherwise        = False
                         
-- Evaluation
eval1 :: Term -> Term
eval1 (TmIf _ (TmTrue _) t2 t3)  = t2
eval1 (TmIf _ (TmFalse _) t2 t3) = t3
eval1 (TmIf fi t1 t2 t3)         = let t1' = eval1 t1
                                   in TmIf fi t1' t2 t3
eval1 (TmSucc fi t1)             = let t1' = eval1 t1 
                                   in TmSucc fi t1'
eval1 (TmPred _ (TmZero _))      = TmZero dummyInfo
eval1 (TmPred _ (TmSucc _ nv1))
    | isNumericalVal nv1 = nv1
    | otherwise          = error "No rule applies" -- TODO Exceptions
eval1 (TmPred fi t1)     =  let t1' = eval1 t1
                            in TmPred fi t1'
eval1 (TmIsZero _ (TmZero _)) = TmTrue dummyInfo
eval1 (TmIsZero _ (TmSucc _ nv1))
       | isNumericalVal nv1 = TmFalse dummyInfo
       | otherwise = error "No rule applies" -- TODO: Exceptions
eval1 (TmIsZero fi t1) = let t1' = eval1 t1
                         in TmIsZero fi t1'
eval1 _ = error "No rule applies"

-- The evaluation function
eval :: Term -> Bool
eval t = let t' = eval1 t
         in eval t'