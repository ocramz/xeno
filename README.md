# xeno

A test XML parser in Haskell.

* It currently doesn't do anything.
* It walks across a string looking for tags in a dumb way, without
  even processing attributes.
* Just to see what the baseline is.

Speed benchmarks.

* hexml is a parser written in C, so that is the baseline.
* xml is written in Haskell, which is much slower: 1.917ms is
  1917μs. So the `xml` package is about 319x slower than `hexml`.
* Apparently a baseline jumping of `<` to `>` and so on in Haskell
  is not bad.

Memory benchmarks for Xeno:

    Case         Bytes  GCs  Check
    4kb parse    1,168    0  OK
    42kb parse   1,560    0  OK
    52kb parse   1,168    0  OK
    182kb parse  1,168    0  OK

Speed benchmarks:

    benchmarking 4KB/hexml
    time                 6.142 μs   (6.115 μs .. 6.181 μs)
                         1.000 R²   (0.999 R² .. 1.000 R²)
    mean                 6.193 μs   (6.161 μs .. 6.228 μs)
    std dev              118.2 ns   (100.7 ns .. 155.7 ns)
    variance introduced by outliers: 19% (moderately inflated)

    benchmarking 4KB/xeno
    time                 3.138 μs   (3.103 μs .. 3.171 μs)
                         0.999 R²   (0.998 R² .. 0.999 R²)
    mean                 3.149 μs   (3.107 μs .. 3.187 μs)
    std dev              139.4 ns   (112.0 ns .. 183.1 ns)
    variance introduced by outliers: 58% (severely inflated)

    benchmarking 4KB/xml
    time                 1.881 ms   (1.872 ms .. 1.888 ms)
                         1.000 R²   (1.000 R² .. 1.000 R²)
    mean                 1.891 ms   (1.885 ms .. 1.904 ms)
    std dev              31.57 μs   (13.80 μs .. 55.76 μs)

    benchmarking 211KB/hexml
    time                 255.7 μs   (255.4 μs .. 256.1 μs)
                         1.000 R²   (1.000 R² .. 1.000 R²)
    mean                 255.6 μs   (255.4 μs .. 255.8 μs)
    std dev              732.9 ns   (597.5 ns .. 888.6 ns)

    benchmarking 211KB/xeno
    time                 167.0 μs   (164.0 μs .. 170.5 μs)
                         0.998 R²   (0.997 R² .. 0.999 R²)
    mean                 165.6 μs   (163.7 μs .. 167.7 μs)
    std dev              6.754 μs   (5.906 μs .. 8.087 μs)
    variance introduced by outliers: 40% (moderately inflated)

    benchmarking 211KB/xml
    time                 1.868 ms   (1.858 ms .. 1.879 ms)
                         1.000 R²   (0.999 R² .. 1.000 R²)
    mean                 1.878 ms   (1.870 ms .. 1.891 ms)
    std dev              35.51 μs   (26.31 μs .. 47.69 μs)

    benchmarking 31KB/hexml
    time                 10.12 μs   (9.760 μs .. 10.43 μs)
                         0.994 R²   (0.992 R² .. 0.997 R²)
    mean                 9.638 μs   (9.481 μs .. 9.834 μs)
    std dev              610.8 ns   (453.1 ns .. 775.4 ns)
    variance introduced by outliers: 71% (severely inflated)

    benchmarking 31KB/xeno
    time                 2.241 μs   (2.212 μs .. 2.273 μs)
                         0.999 R²   (0.998 R² .. 0.999 R²)
    mean                 2.234 μs   (2.209 μs .. 2.263 μs)
    std dev              93.85 ns   (73.12 ns .. 147.7 ns)
    variance introduced by outliers: 56% (severely inflated)

    benchmarking 31KB/xml
    time                 1.354 ms   (1.347 ms .. 1.360 ms)
                         1.000 R²   (1.000 R² .. 1.000 R²)
    mean                 1.352 ms   (1.347 ms .. 1.357 ms)
    std dev              18.65 μs   (15.08 μs .. 24.90 μs)
