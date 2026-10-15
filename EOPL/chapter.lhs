> import Test.Hspec

In the Exercise written, list grammer is just 

lst := () | (a . lst)

so it does not contain nested substructure unlike

s-list := ({s-exp}*)
s-exp  := s-exp | s-list

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

(invert ’((a 1) (a 2) (1 b) (2 b)))
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
=> (down ’(1 2 3))
((1) (2) (3))
=> (down ’((a) (fine) (idea)))
(((a)) ((fine)) ((idea)))
=> (down ’(a (more (complicated)) object))
((a) ((more (complicated))) (object))

This can be done by looking at the grammer of the list

list := () | (cons sexp list)

> main :: IO ()
> main = hspec $ do invertTests
