module Hw where
import Prelude(Show, undefined)

-- Data Declarations - "deriving Show" will allow
-- data values to be printed by interpreter

data Bool = True | False     deriving Show

data Nat =  Zero | Succ Nat    deriving Show



not :: Bool -> Bool
not False = True
not True  = False


and :: Bool -> Bool -> Bool
and True True = True
and _ _       = False


or :: Bool -> Bool -> Bool
or True _  = True
or _ True  = True
or _ _     = False


-- define the following function
xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _ = False

-- you can also define an "if" construct
cond  True  x  y = x
cond  False x  y = y

-- basic arithmetic

-- constants are like 0-ary functions (no arguments)
-- and must be in lower case

-- the first 5 numbers
zero = Zero
one = (Succ zero)
two = Succ one
three = Succ two
four = Succ three
five = Succ four


-- the 6 and 9
six :: Nat
six = (Succ five)

nine :: Nat
nine = (Succ (Succ (Succ six)))

add ::  Nat -> Nat -> Nat
add x Zero = x
add x (Succ y) = Succ (add x y)

sub :: Nat -> Nat -> Nat
sub x Zero = x
sub Zero x = x
sub (Succ x) (Succ y) = sub x y

mult ::  Nat -> Nat -> Nat
mult x Zero     = Zero
mult x (Succ y) = add x (mult x y)

exp ::  Nat -> Nat -> Nat
exp x Zero      = (Succ Zero)
exp x (Succ y)  = mult x (exp x y)

-- when are 2 Nats equal?
eq :: Nat -> Nat -> Bool
eq Zero Zero         = True
eq (Succ x) (Succ y) = eq x y
eq _  _              = False

-- when are 2 Nats not equal?
ne :: Nat -> Nat -> Bool
ne x y = not (eq x y)

-- the less then function, when is the first argument less then the 2nd
lt :: Nat -> Nat -> Bool
lt Zero (Succ x)     = True
lt (Succ x) (Succ y) = lt x y
lt _ _               = False

le x y = or (lt x y) (eq x y)

gt x y = lt y x

ge x y = le y x

-- define the following function

isEven :: Nat -> Bool
isEven Zero = True
isEven (Succ (Zero)) = False
isEven (Succ (Succ x)) = isEven x


--return the largest of 2 Nats
max :: Nat -> Nat -> Nat
max x y = cond (lt x y) y x



-- write a data type for the days of the week
data Weekday = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving Show

-- what is your favorite Day?
favoriteDay :: Weekday
favoriteDay = Friday

-- write a function that returns true if it is a weekend
isWeekend :: Weekday -> Bool
isWeekend Saturday = True
isWeekend Sunday = True
isWeekend  _  = False


-- write a data type for the Months of the year
data Month = January | February | March | April | May | June | July | August | September | October | November | December deriving Show

-- what month has your birthday?
partyMonth :: Month
partyMonth = July

-- write a function that tells when a month is in the 2018 Fall Semester (see https://www.bu.edu/reg/calendars/) return True if it is
needToDoWork :: Month -> Bool
needToDoWork September = True
needToDoWork October = True
needToDoWork November = True
needToDoWork December = True
needToDoWork _ = False

-- write a data type that contains a month, a Nat (representing which week), and a weekday
data Date = Date Month Nat Weekday deriving Show

-- when is this homework due?
homworkIsDueOn :: Date
homworkIsDueOn  = Date September three Tuesday

-- write a program that returns true on CS 320 lecture dates
lectureDay :: Date -> Bool
lectureDay (Date September _ Tuesday) = True
lectureDay (Date September _ Thursday) = True
lectureDay (Date October _ Tuesday) = True
lectureDay (Date October _ Thursday) = True
lectureDay (Date November _ Tuesday) = True
lectureDay (Date November _ Thursday) = True
lectureDay (Date December _ Tuesday) = True
lectureDay (Date December _ Thursday) = True
lectureDay (Date _ _ _) = False

-- write a data type for a 2D point where x and y are Nats
data Point  = MakeP Nat Nat deriving Show

-- take in 2 Nats to make your point
makePoint :: Nat -> Nat -> Point
makePoint x y = (MakeP x y)


-- the Manhattan distance is the distance in the x direction plus the distance in the y direction
-- for instance the manhatan distance of points (2,5) and (3,1) is 5
manhattanDistance :: Point -> Point -> Nat
manhattanDistance (MakeP x y) (MakeP a b) = (add (sub x a) (sub y b))


-- assume there is a sad math calss where students can only answer with a Bool OR with a Nat, write a data type for that answer
data SadAnswer = MakeBool Bool | MakeNat Nat deriving Show

-- make a Nat answer
answerNat :: Nat -> SadAnswer
answerNat x = MakeNat x

-- make a Bool answer
answerBool :: Bool -> SadAnswer
answerBool x = MakeBool x

-- what is 100 - 99?
ans1 :: SadAnswer
ans1 = MakeNat one

-- is 100 - 99 an odd number?
ansTrue :: SadAnswer
ansTrue = MakeBool True


-- we saw in lab how to write lists for specific data
data ListNat = NilNat | ConsNat Nat ListNat deriving Show

-- write a function that adds everything in the list
sum :: ListNat -> Nat
sum (ConsNat x y) = (add x (sum y))
sum NilNat = Zero

-- write a function that tells when 2 nat lists are equal
eq_list NilNat NilNat = True
eq_list NilNat _ = False
eq_list _ NilNat = False
eq_list (ConsNat x y) (ConsNat a b) = (cond (eq x a) (eq_list y b) False)

-- write a function that tests when a Nat is in a list
member :: Nat -> ListNat -> Bool
member x (ConsNat y next) = (cond (eq x y) (True) (member x next))
member _ NilNat = False

-- we also saw how to write general lists
data List a = Nil | Cons a (List a)    deriving Show


-- write a list of all the Bools
listOfBool :: (List Bool)
listOfBool = (Cons True (Cons False Nil))

-- write a list of the first three Nats
listOfNat :: (List Nat)
listOfNat = (Cons zero (Cons one (Cons two Nil)))

-- write a list of all the week days
listOfWork :: (List Weekday)
listOfWork = (Cons Monday (Cons Tuesday (Cons Wednesday (Cons Thursday (Cons Friday (Cons Saturday (Cons Sunday Nil)))))))

-- functions on lists
length :: (List a) -> Nat
length Nil = Zero
length (Cons a next) = (Succ (length next))
--
-- in lab we saw that we can return Maybe data when we want to return something but might not
data Maybe x = Nothing | Just x deriving Show

-- functions that returns something if it is the last element of the list and Nothing if the list is empty

last :: (List a) -> (Maybe a)
last Nil = Nothing
last (Cons a Nil) = Just a
last (Cons _ _) = Nothing

append :: (List a) -> (List a) -> (List a)
append Nil Nil = Nil
append Nil x = x
append x Nil = x
append (Cons a Nil) b = (Cons a b)
append (Cons a b) c = (Cons a (append b c))

addToEnd :: (List a) -> a -> (List a)
addToEnd Nil x = Cons x Nil
addToEnd (Cons a Nil) b = (Cons a (Cons b Nil))
addToEnd (Cons a next) b = (Cons a (addToEnd next b))

--reverse a List
rev :: (List a) -> (List a)
rev (Cons a next)= (addToEnd (rev next) a)
rev Nil = Nil

-- like lists you can constuct a binary tree of Nats

data TreeNat = NullNat | NodeNat (TreeNat) Nat (TreeNat)     deriving Show

memberTreeNat :: Nat -> TreeNat -> Bool
memberTreeNat _ NullNat = False
memberTreeNat x (NodeNat leftTree y rightTree) = (cond (eq x y) (True) (or (memberTreeNat x leftTree) (memberTreeNat x rightTree)))

eqTreeNat :: TreeNat -> TreeNat -> Bool
eqTreeNat NullNat _ = False
eqTreeNat _ NullNat = False
eqTreeNat (NodeNat leftX x rightX) (NodeNat leftY y rightY) = (cond (eq x y) (True) (or (eqTreeNat leftX leftY) (eqTreeNat rightX rightY)))


-- or a general binary tree

data Tree a = Null | Node (Tree a) a (Tree a)     deriving Show

size :: (Tree a) -> Nat
size Null = Zero
size (Node (Null) _ (Null)) = (Succ Zero)
size (Node (right) _ (left)) = (Succ (add (size right) (size left)))

-- suppose the height is number of nodes in longest path from root to leaf

height :: (Tree a) -> Nat
height Null = Zero
height (Node (left) _ (Null)) = (Succ (height left))
height (Node (Null) _ (right)) = (Succ (height right))
height (Node (left) _ (right)) = (Succ (max (height left) (height right)))

-- do an inorder traversal
inorder :: (Tree a) -> (List a)
inorder  Null = Nil
inorder (Node (Null) x (Null)) = (Cons x Nil)
inorder (Node (left) x (right)) = (append (inorder left) (Cons x (inorder right)))

-- do an preorder traversal
preorder :: (Tree a) -> (List a)
preorder Null = Nil
preorder (Node (Null) x (Null)) = (Cons x Nil)
preorder (Node (left) x (right)) = (append (Cons x (preorder left)) (preorder right))
