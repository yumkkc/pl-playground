module Chap1 where

import Control.Monad.Except
import Control.Monad.Identity
import Data.Either

type ListError = ExceptT String Identity

nthElement :: [a] -> Int -> Maybe a
nthElement [] _     = Nothing -- we will change this to error
nthElement (x:_)  0 = Just x
nthElement (_:xs) n = nthElement xs $ n - 1

nthElementError :: Show a => [a] -> Int -> ListError String
nthElementError [] n = throwError $ "List is short by " ++ show (n + 1) ++ " elements"
nthElementError (x:_) 0 = return $ show x
nthElementError (_:xs) n = nthElementError xs $ n - 1

findNthElement :: Show a => [a] -> Int -> String
findNthElement lst n = fromRight "Something went wrong" (runIdentity $ runExceptT $ catchError (nthElementError lst n) return)

-- runIdentity $ runExceptT $ nthElementError [1 2] 0
