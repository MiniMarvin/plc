data Command = Forward Int | Backward Int | TurnLeft | TurnRight 
    deriving (Eq, Show)

data Direction = ToNorth | ToSouth | ToWest | ToEast deriving (Show, Eq)


destinationStatus :: (Int, Int, Direction) -> [Command] -> (Int, Int, Direction)
destinationStatus pos [] = pos
destinationStatus (x, y, d) ((Forward v):cs)
    | d == ToEast = destinationStatus (x + v, y, d) cs
    | d == ToWest = destinationStatus (x - v, y, d) cs
    | d == ToNorth = destinationStatus (x, y + v, d) cs
    | d == ToSouth = destinationStatus (x, y - v, d) cs
destinationStatus (x, y, d) ((Backward v):cs)
    | d == ToEast = destinationStatus (x - v, y, d) cs
    | d == ToWest = destinationStatus (x + v, y, d) cs
    | d == ToNorth = destinationStatus (x, y - v, d) cs
    | d == ToSouth = destinationStatus (x, y + v, d) cs
destinationStatus (x, y, d) (c:cs)
    | c == TurnLeft && d == ToEast = destinationStatus (x, y, ToNorth) cs
    | c == TurnLeft && d == ToNorth = destinationStatus (x, y, ToWest) cs
    | c == TurnLeft && d == ToWest = destinationStatus (x, y, ToSouth) cs
    | c == TurnLeft && d == ToSouth = destinationStatus (x, y, ToEast) cs
    | c == TurnRight && d == ToSouth = destinationStatus (x, y, ToWest) cs
    | c == TurnRight && d == ToWest = destinationStatus (x, y, ToNorth) cs
    | c == TurnRight && d == ToNorth = destinationStatus (x, y, ToEast) cs
    | c == TurnRight && d == ToEast = destinationStatus (x, y, ToSouth) cs
    | otherwise = destinationStatus (x, y, d) cs

justPos :: (Int, Int, Direction) -> (Int, Int)
justPos (x, y, d) = (x, y)

justFace :: (Int, Int, Direction) -> Direction
justFace (x, y, d) = d

-- original destination: toEast
destination :: (Int,Int) -> [Command] -> (Int,Int)
destination pos [] = pos
destination (x, y) commands = justPos (destinationStatus (x, y, ToNorth) commands)

faces ::  Direction -> [Command] -> Direction
faces d [] = d
faces d commands = justFace (destinationStatus (0, 0, d) commands)

