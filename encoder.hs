import System.Random
import Control.Monad
import Data.Bits

randbits :: IO [Bool]
randbits = do
    length <- randomRIO (200, 1000)
    replicateM length randomIO


divide_message :: Int -> [Bool] -> [[Bool]]
divide_message _ [] = []
divide_message blockSize bits = take blockSize bits : divide_message blockSize (drop blockSize bits)

paritybits :: [[Bool]] -> [[Bool]]
paritybits = map (\block -> block ++ [oddParity block])
  where
    oddParity block = odd . popCount $ foldr (\b acc -> if b then acc `xor` (1 :: Int) else acc) (0 :: Int) block

main :: IO() = do
  print("hi")