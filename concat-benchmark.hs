#!/usr/bin/env stack
-- stack --resolver lts-16.31 script

import Control.DeepSeq
import Criterion.Main

-- Concat looks linear
-- benchmarking concat/256
-- time                 2.474 μs   (2.466 μs .. 2.484 μs)
--                      1.000 R²   (1.000 R² .. 1.000 R²)
-- mean                 2.485 μs   (2.472 μs .. 2.498 μs)
-- std dev              45.33 ns   (37.19 ns .. 55.72 ns)
-- variance introduced by outliers: 19% (moderately inflated)
--
-- benchmarking concat/1024
-- time                 10.45 μs   (10.35 μs .. 10.57 μs)
--                      0.999 R²   (0.999 R² .. 1.000 R²)
-- mean                 10.41 μs   (10.33 μs .. 10.48 μs)
-- std dev              243.1 ns   (204.8 ns .. 296.0 ns)
-- variance introduced by outliers: 25% (moderately inflated)
--
-- benchmarking concat/4096
-- time                 41.92 μs   (41.67 μs .. 42.23 μs)
--                      1.000 R²   (0.999 R² .. 1.000 R²)
-- mean                 42.02 μs   (41.76 μs .. 42.33 μs)
-- std dev              991.6 ns   (790.8 ns .. 1.260 μs)
-- variance introduced by outliers: 22% (moderately inflated)

benchConcatRoot :: Int -> Benchmark
benchConcatRoot i = bench (show i) $ xs `seq` nf concat xs
  where
    root = floor (sqrt (fromIntegral i)) :: Int
    individualList = replicate root (0.0 :: Double)
    totalList = replicate root individualList
    xs = force totalList

-- benchmarking concat/16
-- time                 3.772 μs   (3.761 μs .. 3.786 μs)
--                      1.000 R²   (1.000 R² .. 1.000 R²)
-- mean                 3.809 μs   (3.789 μs .. 3.836 μs)
-- std dev              83.20 ns   (65.32 ns .. 117.2 ns)
-- variance introduced by outliers: 24% (moderately inflated)
--
-- benchmarking concat/32
-- time                 8.904 μs   (8.557 μs .. 9.340 μs)
--                      0.987 R²   (0.982 R² .. 0.992 R²)
-- mean                 8.717 μs   (8.505 μs .. 9.043 μs)
-- std dev              882.2 ns   (711.8 ns .. 1.230 μs)
-- variance introduced by outliers: 87% (severely inflated)
--
-- benchmarking concat/64
-- time                 16.73 μs   (16.57 μs .. 17.01 μs)
--                      0.999 R²   (0.997 R² .. 1.000 R²)
-- mean                 16.69 μs   (16.56 μs .. 16.83 μs)
-- std dev              453.9 ns   (325.9 ns .. 670.8 ns)
-- variance introduced by outliers: 29% (moderately inflated)

benchConcatManyLists :: Int -> Benchmark
benchConcatManyLists i = bench (show i) $ xs `seq` nf concat xs
  where
    individualList = replicate 25 (0.0 :: Double)
    totalList = replicate i individualList
    xs = force totalList

-- benchmarking benchConcatLongLists/16
-- time                 3.896 μs   (3.876 μs .. 3.917 μs)
--                      1.000 R²   (0.999 R² .. 1.000 R²)
-- mean                 3.893 μs   (3.877 μs .. 3.922 μs)
-- std dev              69.18 ns   (48.06 ns .. 115.7 ns)
-- variance introduced by outliers: 17% (moderately inflated)
--
-- benchmarking benchConcatLongLists/32
-- time                 8.111 μs   (8.068 μs .. 8.150 μs)
--                      1.000 R²   (0.999 R² .. 1.000 R²)
-- mean                 8.106 μs   (8.068 μs .. 8.155 μs)
-- std dev              140.6 ns   (112.6 ns .. 189.7 ns)
-- variance introduced by outliers: 16% (moderately inflated)
--
-- benchmarking benchConcatLongLists/64
-- time                 16.61 μs   (16.38 μs .. 16.92 μs)
--                      0.999 R²   (0.997 R² .. 1.000 R²)
-- mean                 16.50 μs   (16.39 μs .. 16.68 μs)
-- std dev              461.9 ns   (304.2 ns .. 805.3 ns)
-- variance introduced by outliers: 31% (moderately inflated)
benchConcatLongLists :: Int -> Benchmark
benchConcatLongLists i = bench (show i) $ xs `seq` nf concat xs
  where
    individualList = replicate i (0.0 :: Double)
    totalList = replicate 25 individualList
    xs = force totalList

main :: IO ()
main =
  defaultMain
    [ bgroup
        "benchConcatRoot"
        [benchConcatRoot (4 ^ i) | i <- [4, 5, 6]],
      bgroup
        "benchConcatManyLists"
        [benchConcatManyLists (2 ^ i) | i <- [4, 5, 6]],
      bgroup
        "benchConcatLongLists"
        [benchConcatLongLists (2 ^ i) | i <- [4, 5, 6]]
    ]
