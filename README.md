# xeno

A test XML parser in Haskell.

* It currently doesn't do anything.
* It walks across a string looking for tags in a dumb way, without
  even processing attributes.
* Just to see what the baseline is.

Speed benchmarks.

* hexml is a parser written in C, so that is the baseline.

Memory benchmarks for Xeno:

    Case        Bytes  GCs  Check
    4kb parse   1,160    0  OK
    42kb parse  1,472    0  OK
    52kb parse  1,160    0  OK

Speed benchmarks:

    benchmarking 4KB/hexml
    time                 6.149 μs   (6.125 μs .. 6.183 μs)
                         1.000 R²   (1.000 R² .. 1.000 R²)
    mean                 6.185 μs   (6.159 μs .. 6.215 μs)
    std dev              93.00 ns   (82.22 ns .. 105.0 ns)
    variance introduced by outliers: 13% (moderately inflated)

    benchmarking 4KB/xeno
    time                 2.691 μs   (2.665 μs .. 2.712 μs)
                         0.999 R²   (0.999 R² .. 0.999 R²)
    mean                 2.700 μs   (2.661 μs .. 2.744 μs)
    std dev              139.7 ns   (106.6 ns .. 219.0 ns)
    variance introduced by outliers: 66% (severely inflated)

    benchmarking 31KB/hexml
    time                 9.209 μs   (9.166 μs .. 9.266 μs)
                         1.000 R²   (0.999 R² .. 1.000 R²)
    mean                 9.239 μs   (9.195 μs .. 9.313 μs)
    std dev              164.0 ns   (104.5 ns .. 246.8 ns)
    variance introduced by outliers: 16% (moderately inflated)

    benchmarking 31KB/xeno
    time                 2.012 μs   (1.996 μs .. 2.030 μs)
                         0.999 R²   (0.999 R² .. 0.999 R²)
    mean                 2.025 μs   (1.989 μs .. 2.047 μs)
    std dev              90.80 ns   (72.68 ns .. 113.9 ns)
    variance introduced by outliers: 59% (severely inflated)

    benchmarking 211KB/hexml
    time                 255.2 μs   (254.9 μs .. 255.4 μs)
                         1.000 R²   (1.000 R² .. 1.000 R²)
    mean                 255.2 μs   (255.0 μs .. 255.5 μs)
    std dev              764.4 ns   (578.6 ns .. 950.8 ns)

    benchmarking 211KB/xeno
    time                 140.9 μs   (139.6 μs .. 142.1 μs)
                         0.999 R²   (0.999 R² .. 0.999 R²)
    mean                 141.0 μs   (139.5 μs .. 142.4 μs)
    std dev              5.196 μs   (4.415 μs .. 6.999 μs)
    variance introduced by outliers: 36% (moderately inflated)
