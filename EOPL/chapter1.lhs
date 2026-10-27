> import Test.Hspec
> import Desc
> import Control.Monad.Except
  
  
Grammers which will be useful ::=

s-list ::= ({s-exp}*)
s-exp  ::= Symbol | s-list

lst := () | (a . lst)

For slist, we create a new datatype for this.


Exercise 1.15 [?] (duple n x) returns a list containing n copies of x.
- (duple 2 3)
- (3 3)
(duple 4 '(ha ha))
((ha ha) (ha ha) (ha ha) (ha ha))
- (duple 0 '(blah))
()

> duple :: Int -> a -> [a]
> duple 0 x = []
> duple n x = x : duple (n - 1) x


Exercise 1.16 [⋆] (invert lst),where lst is a list of 2-lists (lists of length two), returns a list with each 2-list reversed.

(invert '((a 1) (a 2) (1 b) (2 b)))
((1 a) (2 a) (b 1) (b 2))

In haskell terms its, lets use list of tuples.

> invert :: [(a, b)] -> [(b, a)]
> invert []         = []
> invert ((x,y) : xs) = (y,x) : invert xs


> invertTests = describe "invert" $ do
>   it "returns empty list for empty input" $ do
>       invert ([] ::[(Int,Int)]) `shouldBe` []
>   it "reverses a single input" $ do
>       invert [(1,2)] `shouldBe` [(2,1)]


Exercise 1.17 [⋆] (down lst) wraps parentheses around each top-level element of lst.
=> (down '(1 2 3))
((1) (2) (3))
=> (down '((a) (fine) (idea)))
(((a)) ((fine)) ((idea)))
=> (down '(a (more (complicated)) object))
((a) ((more (complicated))) (object))

This can be done by looking at the grammer of the list

list := () | (a . list)
Lets, write a few test cases in haskell just using the lists.
 
> downTests = describe "down tests" $ do
>   it "keep mpty list just as it is" $ do
>       down ([] :: [Int]) `shouldBe` []
>   it "puts list around top level" $ do
>       down [1,2,3] `shouldBe` [[1], [2],[3]]

> down :: [a] -> [[a]]
> down []     = []
> down (x:xs) = [x] : down xs


Exercise 1.18 [*] (swapper s1 s2 slist) returns a list the same as slist, but 
with all occurrences of s1 replaced by s2 and all occurrences of s2 replaced by s1.
(swapper 'a 'd '(a b c d))
(d b c a) 
(swapper 'a 'd '(a d () c d))
(d a () c a) 
(swapper 'x 'y '((x) y (z (x))))
((y) x (z (y)))

Some equivalent in datatype we made above for tests

> swapperTests = describe "swapper tests " $ do
>   it "swaps a and b in list " $ do
>       swapper "a" "b" (parseExp "(a b c d)") `shouldBe` (parseExp "(b a c d)")
>   it "swaps x and y even in nested structure " $ do
>       swapper "x" "y" (parseExp "((x) y (z (x)))") `shouldBe` (parseExp "((y) x (z (y)))")

> swapper :: Symbol -> Symbol -> SList -> SList
> swapper _ _ (SList [])        = SList []
> swapper a b (SList (x:xs))    = SList (x' : xs')
>                                   where (SList xs')  = swapper a b (SList xs)
>                                         x'           = sexpSwap a b x

> sexpSwap :: Symbol -> Symbol -> SExp -> SExp 
> sexpSwap a b (Symbol x)   | a == x     = Symbol b
>                           | b == x     = Symbol a
>                           | otherwise  = Symbol x
> sexpSwap a b (SExpList xs) = SExpList $ swapper a b xs


Exercise 1.19 [**] (list-set lst n x) returns a list like lst, except that the n-th
element, using zero-based indexing, is x.
(list-set '(a b c d) 2 '(1 2))
(a b (1 2) d) 
(list-ref (list-set '(a b c d) 3 '(1 5 10)) 3)
(1 5 10)

We cannot store different types in list here in haskell, so

> listSet :: [a] -> Integer -> a -> WithError [a]
> listSet [] _ _      = throwError "large index than number of elements"
> listSet (x:xs) 0 x' = return $ x':xs
> listSet (x:xs) n x' = do
>    xs' <- listSet xs (n-1) x'
>    return $ x' : xs'

Exercise 1.20 [*]

> countOccurrences :: Symbol -> SList -> Integer
> countOccurrences sym (SList [])     = 0
> countOccurrences sym (SList (x:xs)) = countOccInSym sym x + countOccurrences sym (SList xs)


> countOccInSym :: Symbol -> SExp -> Integer
> countOccInSym sym (Symbol s) | sym == s  = 1
>                              | otherwise = 0
> countOccInSym sym (SExpList xs) = countOccurrences sym xs

> countTests = describe "countOccurences test" $ do
>   it "test 1" $ do
>       countOccurrences "a" (parseExp "(a b c)") `shouldBe` 1
>   it "test 2" $ do
>       countOccurrences "a" (parseExp "(a b (a b c) c (a))") `shouldBe` 3

Exercise 1.21 (product sos1 sos2)

> product' :: [Symbol] -> [Symbol] -> [(Symbol, Symbol)]
> product' [] _  = []
> product' _ []  = []
> product' xs ys = concatMap (\x -> map (x,) ys) xs


> mergePred :: (Integer -> Integer -> Bool) -> [Integer] -> [Integer] -> [Integer]
> mergePred _ [] b              = b
> mergePred _ a []              = a
> mergePred pred a@(x:xs) b@(y:ys) | pred x y    = x : mergePred pred xs b
>                                  | otherwise   = y : mergePred pred a ys

Exercise 1.28
merge :: ListOf(Int) -> ListOf(Int) -> ListOf (Int)
usage? :: merge two sorted list of integers

> merge :: [Integer] -> [Integer] -> [Integer]
> merge = mergePred (<)


Exercise 1.29
sort :: ListOf (Intger) -> ListOf (Integer)
usage? :: sort list of integers

> sort :: [Integer] -> [Integer]
> sort []  = []
> sort [x] = merge [x] []
> sort (x:xs) = merge [x] $ sort xs


Exercise 1.30 (sort/predicate pred loi) returns a list of elements sorted
by the predicate.

> sortPredicate :: (Integer -> Integer -> Bool) -> [Integer] -> [Integer]
> sortPredicate _ [] = []
> sortPredicate pred [x] = mergePred pred [x] []
> sortPredicate pred (x:xs) = mergePred pred [x] $ sortPredicate pred xs

This is just a generalized form of sort so sort can be written with this

sort = sortPredicate (<)

> main :: IO ()
> main = hspec $ do countTests