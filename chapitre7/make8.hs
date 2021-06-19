import Data.Char
type Bit = Int

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)
