{--
moudle Shapes
(
 Day(..),
 Cellphone
 )where
--}
--Type with nullary
data Day = 
		Monday | 
		ThuesDay | 
		Wednesday |
		Thursday | 
		Friday | 
		Saturday | 
		Sunday
		deriving(Show, Ord, Eq, Read, Bounded, Enum)

--record grammer
data Cellphone = Cellphone{ 
    manufacturer :: String,
    operator :: String,
    number :: Int,
    system :: String,
    size :: Double
}deriving(Show)

cp = Cellphone{
	manufacturer = "NOKIA", 
	operator = "China-Mobile",
	number = 13531172135,
	system = "Windows Phone 8 Cyan",
	size = 4.3
}
--custom list
infixr 5 :- 
data Lst a = Nil | a :- (Lst a) deriving(Eq, Ord)

instance (Show a) => Show (Lst a) where
		show Nil = "\b]"
		show (x :- xs) = show x ++ "," ++ show xs

