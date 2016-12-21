# xeno

A test XML parser in Haskell.

* It currently doesn't do anything.
* It walks across a string looking for tags in a dumb way, without
  even processing attributes.
* Just to see what the baseline is.

Here are some benchmarks.

* hexml is a parser written in C, so that is the baseline.
* xml is written in Haskell, which is much slower: 1.917ms is
  1917μs. So the `xml` package is about 319x slower than `hexml`.
* Apparently a baseline jumping of `<` to `>` and so on in Haskell
  is not bad.

Benchmarks:

    time                 6.181 μs   (6.173 μs .. 6.190 μs)
                         1.000 R²   (1.000 R² .. 1.000 R²)
    mean                 6.218 μs   (6.201 μs .. 6.249 μs)
    std dev              77.70 ns   (59.95 ns .. 99.90 ns)

    benchmarking 4KB/xeno
    time                 2.630 μs   (2.601 μs .. 2.660 μs)
                         0.999 R²   (0.998 R² .. 0.999 R²)
    mean                 2.611 μs   (2.582 μs .. 2.636 μs)
    std dev              99.31 ns   (75.18 ns .. 127.3 ns)
    variance introduced by outliers: 51% (severely inflated)

    benchmarking 4KB/xml
    time                 1.915 ms   (1.909 ms .. 1.923 ms)
                         0.999 R²   (0.999 R² .. 1.000 R²)
    mean                 1.936 ms   (1.925 ms .. 1.954 ms)
    std dev              44.38 μs   (30.23 μs .. 59.90 μs)
    variance introduced by outliers: 11% (moderately inflated)

    benchmarking 42KB/hexml
    time                 37.55 μs   (37.33 μs .. 37.73 μs)
                         1.000 R²   (1.000 R² .. 1.000 R²)
    mean                 37.41 μs   (37.31 μs .. 37.53 μs)
    std dev              403.4 ns   (289.3 ns .. 586.2 ns)

    benchmarking 42KB/xeno
    time                 7.814 μs   (7.745 μs .. 7.913 μs)
                         0.999 R²   (0.998 R² .. 0.999 R²)
    mean                 7.864 μs   (7.786 μs .. 8.012 μs)
    std dev              328.1 ns   (262.2 ns .. 472.4 ns)
    variance introduced by outliers: 52% (severely inflated)

    benchmarking 42KB/xml
    time                 17.40 ms   (17.12 ms .. 17.70 ms)
                         0.999 R²   (0.997 R² .. 0.999 R²)
    mean                 17.38 ms   (17.25 ms .. 17.50 ms)
    std dev              306.5 μs   (255.7 μs .. 373.8 μs)

    benchmarking 52KB/hexml
    time                 60.62 μs   (60.23 μs .. 60.98 μs)
                         1.000 R²   (1.000 R² .. 1.000 R²)
    mean                 61.06 μs   (60.71 μs .. 61.41 μs)
    std dev              1.035 μs   (954.3 ns .. 1.221 μs)
    variance introduced by outliers: 12% (moderately inflated)

    benchmarking 52KB/xeno
    time                 12.08 μs   (11.92 μs .. 12.25 μs)
                         0.999 R²   (0.998 R² .. 0.999 R²)
    mean                 12.05 μs   (11.92 μs .. 12.20 μs)
    std dev              525.9 ns   (453.7 ns .. 641.7 ns)
    variance introduced by outliers: 53% (severely inflated)

    benchmarking 52KB/xml
    time                 21.97 ms   (21.75 ms .. 22.13 ms)
                         1.000 R²   (0.999 R² .. 1.000 R²)
    mean                 21.97 ms   (21.89 ms .. 22.12 ms)
    std dev              229.1 μs   (151.5 μs .. 351.6 μs)

    benchmarking 182KB/hexml
    time                 33.81 μs   (32.74 μs .. 34.81 μs)
                         0.994 R²   (0.990 R² .. 0.997 R²)
    mean                 32.99 μs   (32.57 μs .. 33.95 μs)
    std dev              2.067 μs   (1.465 μs .. 3.147 μs)
    variance introduced by outliers: 67% (severely inflated)

    benchmarking 182KB/xeno
    time                 111.2 μs   (110.0 μs .. 112.5 μs)
                         0.999 R²   (0.998 R² .. 0.999 R²)
    mean                 112.2 μs   (110.9 μs .. 113.8 μs)
    std dev              4.580 μs   (3.779 μs .. 5.696 μs)
    variance introduced by outliers: 41% (moderately inflated)

    benchmarking 182KB/xml
    time                 780.9 μs   (771.3 μs .. 792.9 μs)
                         0.999 R²   (0.999 R² .. 1.000 R²)
    mean                 773.4 μs   (771.1 μs .. 776.7 μs)
    std dev              10.21 μs   (7.215 μs .. 15.36 μs)
