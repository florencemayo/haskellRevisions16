import Test.QuickCheck

--variable
euroRate :: Double
euroRate = 10

--function  convert euro to sek
sek::Double->Double
sek m = m*euroRate

--function to convert sek to euro
euro::Double->Double
euro m = m/euroRate

-- function to do the test
prop_EuroSek::Double->Bool
prop_EuroSek a = sek (euro a) ~== a

-- function to cover the differences
(~==)::Double->Double->Bool
a ~== b = abs(a-b) < 10e-15

--property to test again
prop_SekEuro::Double->Bool
prop_SekEuro a = euro (sek a) ~== a

--defining a variable
price :: Double
price = 10

--function to get the absolute value
absolute::Integer->Integer
absolute x | x >= 0     = x
           | otherwise  = -x

--recursive function to get a power
power::Double->Double->Double
power x 0       = 1
power x n | n>0 = x * power x (n-1)

--recursive function to get number of intersection
intersection::Integer->Integer
intersection 0       = 0
intersection n | n>0 = (n-1) + intersection (n-1)


--Examples tuples
exampleTriple:: (Double,Bool)
exampleTriple = (3.15,True)

--example tuple function
exampleFunction:: (Bool,Int,String)->Bool
exampleFunction (b,i,s)= not b && length s < i

--a summarize function
summarize :: [String]-> String
summarize []  = "Nothing"
summarize [x] = "Only "++ x
summarize _   = "Several things."


--a double function
doubles::[Integer]->[Integer]
doubles []   = []
doubles (x:xs) = (2*x):doubles xs


--pythagoras formula
pythag :: Int -> [(Int, Int, Int)]
pythag n = [(x,y,z) | x<-[1..n], y<- [x..n], z<-[y..n], x*x+y*y==z*z]