import Test.QuickCheck

--data type suit
data Suit = Spades | Hearts | Diamonds | Clubs deriving (Show,Eq)

-- data type color
data Colour = Black | Red deriving Show

--data type rank
data Rank = Numeric Integer | Jack | Queen | King | Ace deriving (Show,Eq)

-- function to beat a rank
rankBeats :: Rank -> Rank -> Bool
rankBeats _ Ace                   = False
rankBeats Ace _                   = True
rankBeats _ King                  = False
rankBeats King _                  = True
rankBeats _ Jack                  = False
rankBeats Jack _                  = True
rankBeats _ Queen                 = False
rankBeats Queen _                 = True
rankBeats (Numeric a) (Numeric b) = a>b

-- property to check the ranks
--ain't working for now
prop_rankBeats a b =  a/=b ==> rankBeats a b || rankBeats b a


--Modelling a card
data Card = Card Rank Suit
   deriving Show

--a function to give a rank from a card
fRank :: Card -> Rank
fRank (Card r s) = r

--a fucnction to give a suit from a card
fSuit :: Card -> Suit
fSuit (Card r s) = s

-- a function card beats card
cardBeats :: Card -> Card -> Bool
cardBeats c1 c2
    |  fSuit c1 == fSuit c2  = rankBeats (fRank c1) (fRank c2)
    |  otherwise             = False


-- Modelling a hand of cards
data Hand = Empty | Add Card Hand
  deriving Show

-- a hand beats a card
handBeats :: Hand -> Card -> Bool
handBeats Empty card     = False
handBeats (Add c h) card = cardBeats c card || handBeats h card

{--- choose a card to play
chooseCard :: Card -> Hand -> Hand
chooseCard beat (Add c Empty) = c
chooseCard beat (Add c rest)
    | -}


