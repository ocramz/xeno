# Introduction

There are two issues with old `xeno` benchmarks:

* They run on threaded Haskell RTS (compiled with `-threaded`). Threading lead to very big variance in benchmarks
  (sometimes R² is less then 0.8). Therefore threaded benchmarking is not accurate.
* They run on relatively small files. Some speed improvements become apparent only on big files (more then 100 Mb) and
  benchmarking with small files does not detect an effect of all improvements.

So here are several type of benchmarks.

# Summary

SAX (`xeno-sax` benchmarks):

| size  |xeno threaded | improved xeno threaded | xeno non-threaded | improved xeno non-threaded | imp. xeno w/o copy |
| ----- | ------------ | ---------------------- | ----------------- | -------------------------- | ------------------ |
| 4KB   | 24.44 μs     | 16.39 μs               | 5.638 μs          | 2.866 μs                   | 2.923 μs           |
| 31KB  | 13.01 μs     | 121.1 μs               | 2.477 μs          | 1.947 μs                   | 1.289 μs           |
| 211KB | 965.9 μs     | 760.0 μs               | 274.0 μs          | 139.3 μs                   | 140.5 μs           |
| 46MB  | 16.27 ms     | 14.72 ms               | 14.48 ms          | 14.04 ms                   | 9.061 ms           |
| 624MB | 5.089 μs     | 3.315 μs               | 4.763 μs          | 3.376 μs                   | 2.912 μs           |
| 921MB | 2.304 s      | 994.8 ms               | 2.189 s           | 1.023 s                    | 1.032 s            |
| 1.6GB | 321.5 ms     | 293.5 ms               | 288.0 ms          | 299.3 ms                   | 191.6 ms           |
| 4GB   | 1.471 s      | 1.090 s                | 1.308 s           | 1.150 s                    | 807.1 ms           |

DOM (`xeno-dom` and `hexml-dom` benchmarks):

| size  |xeno threaded | improved xeno threaded | hexml-dom threaded | xeno unthreaded | improved xeno non-threaded | imp. xeno w/o copy | hexml-dom non-threaded |
| ----- | ------------ | ---------------------- | ------------------ | --------------- | -------------------------- | ------------------ | ---------------------- |
| 4KB   | 29.46 μs     | 40.96 μs               | 10.84 μs           | 9.023 μs        | 6.174 μs                   | 6.251 μs           | 3.290 μs               |
| 31KB  | 29.04 μs     | 167.7 μs               | 82.60 μs           | 4.020 μs        | 3.433 μs                   | 2.921 μs           | 2.813 μs               |
| 211KB | 3.297 ms     | 3.147 ms               | 525.8 μs           | 433.0 μs        | 280.1 μs                   | 303.6 μs           | 122.7 μs               |
| 46MB  | 28.42 ms     | 20.11 ms               | 14.90 ms           | 26.91 ms        | 21.18 ms                   | 19.04 ms           | 14.55 ms               |
| 624MB | 8.385 μs     | 6.263 μs               | 2.328 μs           | 8.032 μs        | 5.769 μs                   | 5.270 μs           | 2.228 μs               |
| 921MB | 3.292 s      | 1.692 s                | 793.8 ms           | 3.201 s         | 1.732 s                    | 2.188 s            | 789.0 ms               |
| 1.6GB | 537.9 ms     | 442.2 ms               | 314.7 ms           | 490.9 ms        | 435.2 ms                   | 364.9 ms           | 353.9 ms               |
| 4GB   | 2.492 s      | 1.737 s                | n/a                | 2.245 s         | 2.120 s                    | 1.735 s            | n/a                    |

("imp. xeno w/o copy" means improved xeno without adding zero at end of bytestring)

[image](/uploads/227aa39e028f9dd02cc8197636b20e61/image.png)

## Note about GHC version

TODO GHC 8.6.5 has problems with speed :(

## Hardware & GHC
* CPU: AMD Ryzen 9 3900X 12-Core Processor with 4200MHz
* Memory: 32 Gb (4 banks),  2400MHz
* Motherboard: ASRock X570 Phantom Gaming X
* GHC: 8.6.5

# Benchmark with small files

In this section we present results of original `xeno` benchmarks, runned in two modes: with threaded support and without
threaded support.

## Threaded environment

Compiled with `ghc-options: -O2 -threaded -rtsopts "-with-rtsopts=-N"`.

Please note **huge** variance introduced by outliers and low R².

### Current benchmarks

Memory:

```
Case                          Allocated  GCs
4kb_hexml_dom                     3,808    0
4kb_xeno_sax                      3,856    0
4kb_xeno_dom                     12,416    0
4kb_xeno_dom-with-recovery       18,360    0
31kb_hexml_dom                   30,608    0
31kb_xeno_sax                    30,440    0
31kb_xeno_dom                    62,320    0
31kb_xeno_dom-with-recovery      41,488    0
211kb_hexml_dom                 211,752    0
211kb_xeno_sax                  247,640    0
211kb_xeno_dom                  930,160    0
211kb_xeno_dom-with-recovery  1,549,408    0
```

Speed:

```
benchmarking 4KB/hexml-dom
time                 10.19 μs   (9.804 μs .. 10.60 μs)
                     0.977 R²   (0.967 R² .. 0.988 R²)
mean                 10.84 μs   (10.46 μs .. 11.42 μs)
std dev              1.594 μs   (1.274 μs .. 1.989 μs)
variance introduced by outliers: 93% (severely inflated)

benchmarking 4KB/xeno-sax
time                 16.39 μs   (14.89 μs .. 18.51 μs)
                     0.950 R²   (0.908 R² .. 0.993 R²)
mean                 15.68 μs   (15.14 μs .. 16.71 μs)
std dev              2.260 μs   (1.354 μs .. 4.080 μs)
variance introduced by outliers: 92% (severely inflated)

benchmarking 4KB/xeno-dom
time                 40.96 μs   (31.52 μs .. 50.74 μs)
                     0.766 R²   (0.640 R² .. 0.944 R²)
mean                 45.40 μs   (39.90 μs .. 49.05 μs)
std dev              13.42 μs   (8.675 μs .. 18.47 μs)
variance introduced by outliers: 98% (severely inflated)

benchmarking 4KB/xeno-dom-with-recovery
time                 45.04 μs   (42.00 μs .. 47.86 μs)
                     0.966 R²   (0.943 R² .. 0.980 R²)
mean                 46.39 μs   (44.30 μs .. 50.30 μs)
std dev              9.778 μs   (6.792 μs .. 16.08 μs)
variance introduced by outliers: 96% (severely inflated)

benchmarking 4KB/hexpat-sax
time                 308.9 μs   (272.6 μs .. 349.5 μs)
                     0.920 R²   (0.887 R² .. 0.948 R²)
mean                 296.4 μs   (276.4 μs .. 321.1 μs)
std dev              65.71 μs   (56.56 μs .. 79.00 μs)
variance introduced by outliers: 95% (severely inflated)

benchmarking 4KB/hexpat-dom
time                 1.816 ms   (1.782 ms .. 1.856 ms)
                     0.994 R²   (0.988 R² .. 0.998 R²)
mean                 1.843 ms   (1.818 ms .. 1.928 ms)
std dev              130.9 μs   (70.29 μs .. 258.9 μs)
variance introduced by outliers: 53% (severely inflated)

benchmarking 4KB/xml-dom
time                 11.13 ms   (10.64 ms .. 11.75 ms)
                     0.978 R²   (0.955 R² .. 0.992 R²)
mean                 10.72 ms   (10.41 ms .. 11.08 ms)
std dev              858.4 μs   (624.1 μs .. 1.174 ms)
variance introduced by outliers: 41% (moderately inflated)

benchmarking 31KB/hexml-dom
time                 68.87 μs   (63.22 μs .. 77.79 μs)
                     0.903 R²   (0.849 R² .. 0.948 R²)
mean                 82.60 μs   (75.79 μs .. 88.88 μs)
std dev              22.69 μs   (15.91 μs .. 28.95 μs)
variance introduced by outliers: 97% (severely inflated)

benchmarking 31KB/xeno-sax
time                 121.1 μs   (110.9 μs .. 140.8 μs)
                     0.782 R²   (0.660 R² .. 0.926 R²)
mean                 172.1 μs   (123.1 μs .. 351.9 μs)
std dev              278.6 μs   (49.70 μs .. 579.8 μs)
variance introduced by outliers: 99% (severely inflated)

benchmarking 31KB/xeno-dom
time                 167.7 μs   (106.5 μs .. 207.6 μs)
                     0.658 R²   (0.433 R² .. 0.777 R²)
mean                 223.8 μs   (206.4 μs .. 247.0 μs)
std dev              65.90 μs   (46.09 μs .. 107.8 μs)
variance introduced by outliers: 98% (severely inflated)

benchmarking 31KB/xeno-dom-with-recovery
time                 106.3 μs   (74.99 μs .. 136.8 μs)
                     0.719 R²   (0.504 R² .. 0.895 R²)
mean                 134.9 μs   (122.7 μs .. 142.2 μs)
std dev              28.38 μs   (18.69 μs .. 42.59 μs)
variance introduced by outliers: 95% (severely inflated)

benchmarking 31KB/hexpat-sax
time                 1.853 ms   (1.775 ms .. 1.994 ms)
                     0.967 R²   (0.947 R² .. 0.988 R²)
mean                 1.802 ms   (1.731 ms .. 1.876 ms)
std dev              211.6 μs   (140.0 μs .. 337.4 μs)
variance introduced by outliers: 76% (severely inflated)

benchmarking 31KB/hexpat-dom
time                 1.648 ms   (1.397 ms .. 1.925 ms)
                     0.865 R²   (0.776 R² .. 0.974 R²)
mean                 1.833 ms   (1.739 ms .. 1.899 ms)
std dev              232.5 μs   (162.3 μs .. 342.9 μs)
variance introduced by outliers: 79% (severely inflated)

benchmarking 31KB/xml-dom
time                 65.17 ms   (40.68 ms .. 81.04 ms)
                     0.849 R²   (0.498 R² .. 0.999 R²)
mean                 76.56 ms   (63.22 ms .. 80.48 ms)
std dev              10.73 ms   (2.420 ms .. 17.58 ms)
variance introduced by outliers: 48% (moderately inflated)

benchmarking 211KB/hexml-dom
time                 461.9 μs   (414.8 μs .. 513.5 μs)
                     0.904 R²   (0.851 R² .. 0.958 R²)
mean                 525.8 μs   (480.6 μs .. 567.9 μs)
std dev              153.7 μs   (125.8 μs .. 187.8 μs)
variance introduced by outliers: 97% (severely inflated)

benchmarking 211KB/xeno-sax
time                 760.0 μs   (707.2 μs .. 800.4 μs)
                     0.912 R²   (0.842 R² .. 0.955 R²)
mean                 748.3 μs   (726.9 μs .. 783.9 μs)
std dev              90.89 μs   (65.18 μs .. 129.7 μs)
variance introduced by outliers: 81% (severely inflated)

benchmarking 211KB/xeno-dom
time                 3.147 ms   (2.923 ms .. 3.586 ms)
                     0.912 R²   (0.869 R² .. 0.949 R²)
mean                 2.973 ms   (2.903 ms .. 3.046 ms)
std dev              227.7 μs   (189.7 μs .. 284.2 μs)
variance introduced by outliers: 53% (severely inflated)

benchmarking 211KB/xeno-dom-with-recovery
time                 3.752 ms   (3.465 ms .. 4.080 ms)
                     0.963 R²   (0.950 R² .. 0.980 R²)
mean                 3.789 ms   (3.645 ms .. 3.917 ms)
std dev              421.4 μs   (356.7 μs .. 512.5 μs)
variance introduced by outliers: 68% (severely inflated)

benchmarking 211KB/hexpat-sax
time                 88.23 ms   (86.61 ms .. 90.64 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 87.93 ms   (87.04 ms .. 88.91 ms)
std dev              1.640 ms   (1.179 ms .. 2.323 ms)

benchmarking 211KB/hexpat-dom
time                 65.85 ms   (44.53 ms .. 94.58 ms)
                     0.815 R²   (0.449 R² .. 0.996 R²)
mean                 83.69 ms   (73.19 ms .. 90.30 ms)
std dev              13.71 ms   (7.782 ms .. 20.26 ms)
variance introduced by outliers: 58% (severely inflated)

benchmarking 211KB/xml-dom
time                 525.2 ms   (462.2 ms .. 616.4 ms)
                     0.997 R²   (0.990 R² .. 1.000 R²)
mean                 496.9 ms   (484.3 ms .. 511.4 ms)
std dev              16.85 ms   (6.239 ms .. 21.98 ms)
variance introduced by outliers: 19% (moderately inflated)
```

### Before speed improvements

Memory:

```
Case                          Allocated  GCs
4kb_hexml_dom                     3,808    0
4kb_xeno_sax                      6,120    0
4kb_xeno_dom                     13,720    0
4kb_xeno_dom-with-recovery       20,664    0
31kb_hexml_dom                   30,608    0
31kb_xeno_sax                     2,232    0
31kb_xeno_dom                    10,336    0
31kb_xeno_dom-with-recovery      13,392    0
211kb_hexml_dom                 211,496    0
211kb_xeno_sax                  343,488    0
211kb_xeno_dom                1,369,408    0
211kb_xeno_dom-with-recovery  1,663,416    0
```

Speed:

```
benchmarking 4KB/hexml-dom
time                 10.49 μs   (9.851 μs .. 11.25 μs)
                     0.977 R²   (0.966 R² .. 0.987 R²)
mean                 10.82 μs   (10.48 μs .. 11.30 μs)
std dev              1.265 μs   (1.056 μs .. 1.516 μs)
variance introduced by outliers: 89% (severely inflated)

benchmarking 4KB/xeno-sax
time                 24.44 μs   (20.72 μs .. 27.23 μs)
                     0.915 R²   (0.808 R² .. 0.989 R²)
mean                 25.48 μs   (23.35 μs .. 26.74 μs)
std dev              5.119 μs   (2.788 μs .. 7.819 μs)
variance introduced by outliers: 96% (severely inflated)

benchmarking 4KB/xeno-dom
time                 29.46 μs   (28.74 μs .. 30.15 μs)
                     0.994 R²   (0.991 R² .. 0.996 R²)
mean                 29.25 μs   (28.70 μs .. 29.89 μs)
std dev              2.083 μs   (1.782 μs .. 2.567 μs)
variance introduced by outliers: 73% (severely inflated)

benchmarking 4KB/xeno-dom-with-recovery
time                 50.43 μs   (48.71 μs .. 52.65 μs)
                     0.979 R²   (0.967 R² .. 0.991 R²)
mean                 52.50 μs   (50.24 μs .. 60.33 μs)
std dev              10.27 μs   (6.881 μs .. 19.47 μs)
variance introduced by outliers: 95% (severely inflated)

benchmarking 4KB/hexpat-sax
time                 326.4 μs   (289.5 μs .. 369.1 μs)
                     0.919 R²   (0.884 R² .. 0.958 R²)
mean                 356.6 μs   (335.1 μs .. 374.8 μs)
std dev              71.59 μs   (61.24 μs .. 83.25 μs)
variance introduced by outliers: 94% (severely inflated)

benchmarking 4KB/hexpat-dom
time                 1.846 ms   (1.812 ms .. 1.899 ms)
                     0.997 R²   (0.996 R² .. 0.999 R²)
mean                 1.829 ms   (1.810 ms .. 1.849 ms)
std dev              63.45 μs   (52.82 μs .. 83.24 μs)
variance introduced by outliers: 20% (moderately inflated)

benchmarking 4KB/xml-dom
time                 9.287 ms   (6.621 ms .. 10.57 ms)
                     0.673 R²   (0.399 R² .. 0.920 R²)
mean                 9.334 ms   (7.749 ms .. 10.23 ms)
std dev              3.305 ms   (2.410 ms .. 4.209 ms)
variance introduced by outliers: 96% (severely inflated)

benchmarking 31KB/hexml-dom
time                 69.12 μs   (60.10 μs .. 74.93 μs)
                     0.918 R²   (0.880 R² .. 0.951 R²)
mean                 65.81 μs   (60.41 μs .. 71.72 μs)
std dev              18.49 μs   (16.04 μs .. 22.04 μs)
variance introduced by outliers: 97% (severely inflated)

benchmarking 31KB/xeno-sax
time                 13.01 μs   (12.57 μs .. 13.27 μs)
                     0.983 R²   (0.971 R² .. 0.990 R²)
mean                 12.43 μs   (12.04 μs .. 12.79 μs)
std dev              1.126 μs   (882.8 ns .. 1.373 μs)
variance introduced by outliers: 83% (severely inflated)

benchmarking 31KB/xeno-dom
time                 29.04 μs   (27.77 μs .. 30.37 μs)
                     0.986 R²   (0.978 R² .. 0.993 R²)
mean                 28.44 μs   (27.30 μs .. 29.51 μs)
std dev              3.356 μs   (2.735 μs .. 4.341 μs)
variance introduced by outliers: 89% (severely inflated)

benchmarking 31KB/xeno-dom-with-recovery
time                 26.83 μs   (22.70 μs .. 29.62 μs)
                     0.872 R²   (0.774 R² .. 0.938 R²)
mean                 29.38 μs   (27.37 μs .. 30.73 μs)
std dev              4.979 μs   (2.314 μs .. 7.940 μs)
variance introduced by outliers: 94% (severely inflated)

benchmarking 31KB/hexpat-sax
time                 1.856 ms   (1.818 ms .. 1.891 ms)
                     0.997 R²   (0.994 R² .. 0.998 R²)
mean                 1.826 ms   (1.795 ms .. 1.844 ms)
std dev              75.90 μs   (62.50 μs .. 104.9 μs)
variance introduced by outliers: 27% (moderately inflated)

benchmarking 31KB/hexpat-dom
time                 1.613 ms   (1.289 ms .. 1.927 ms)
                     0.839 R²   (0.652 R² .. 0.996 R²)
mean                 1.873 ms   (1.753 ms .. 1.933 ms)
std dev              280.7 μs   (65.05 μs .. 474.2 μs)
variance introduced by outliers: 84% (severely inflated)

benchmarking 31KB/xml-dom
time                 84.01 ms   (76.93 ms .. 98.24 ms)
                     0.962 R²   (0.871 R² .. 0.998 R²)
mean                 74.92 ms   (64.68 ms .. 79.51 ms)
std dev              11.18 ms   (3.451 ms .. 16.74 ms)
variance introduced by outliers: 48% (moderately inflated)

benchmarking 211KB/hexml-dom
time                 428.8 μs   (346.1 μs .. 504.5 μs)
                     0.826 R²   (0.771 R² .. 0.911 R²)
mean                 482.2 μs   (444.6 μs .. 524.7 μs)
std dev              148.5 μs   (115.6 μs .. 174.1 μs)
variance introduced by outliers: 97% (severely inflated)

benchmarking 211KB/xeno-sax
time                 965.9 μs   (672.4 μs .. 1.243 ms)
                     0.695 R²   (0.523 R² .. 0.869 R²)
mean                 1.153 ms   (1.053 ms .. 1.246 ms)
std dev              354.6 μs   (254.3 μs .. 487.2 μs)
variance introduced by outliers: 97% (severely inflated)

benchmarking 211KB/xeno-dom
time                 3.297 ms   (3.207 ms .. 3.383 ms)
                     0.961 R²   (0.895 R² .. 0.993 R²)
mean                 3.244 ms   (3.054 ms .. 3.318 ms)
std dev              357.4 μs   (249.6 μs .. 559.7 μs)
variance introduced by outliers: 69% (severely inflated)

benchmarking 211KB/xeno-dom-with-recovery
time                 3.143 ms   (2.931 ms .. 3.381 ms)
                     0.958 R²   (0.895 R² .. 0.985 R²)
mean                 3.085 ms   (2.950 ms .. 3.186 ms)
std dev              381.1 μs   (299.4 μs .. 559.8 μs)
variance introduced by outliers: 74% (severely inflated)

benchmarking 211KB/hexpat-sax
time                 98.40 ms   (80.36 ms .. 126.7 ms)
                     0.917 R²   (0.850 R² .. 0.982 R²)
mean                 66.50 ms   (50.68 ms .. 80.20 ms)
std dev              28.32 ms   (20.67 ms .. 34.71 ms)
variance introduced by outliers: 90% (severely inflated)

benchmarking 211KB/hexpat-dom
time                 89.94 ms   (86.98 ms .. 95.16 ms)
                     0.996 R²   (0.987 R² .. 0.999 R²)
mean                 89.93 ms   (87.84 ms .. 92.01 ms)
std dev              3.456 ms   (2.122 ms .. 5.345 ms)

benchmarking 211KB/xml-dom
time                 493.9 ms   (278.2 ms .. 604.3 ms)
                     0.976 R²   (0.943 R² .. 1.000 R²)
mean                 485.7 ms   (436.0 ms .. 517.9 ms)
std dev              48.76 ms   (22.23 ms .. 63.11 ms)
variance introduced by outliers: 23% (moderately inflated)
```

## Nonthreaded environment

Compiled with `ghc-options: -O2 -rtsopts`.

Please note almost lack of variance introduced by outliers and high R².

### Current benchmarks

Memory:

```
Case                          Allocated  GCs
4kb_hexml_dom                     3,808    0
4kb_xeno_sax                      4,216    0
4kb_xeno_dom                     12,416    0
4kb_xeno_dom-with-recovery       18,720    0
31kb_hexml_dom                   30,608    0
31kb_xeno_sax                    30,440    0
31kb_xeno_dom                    61,992    0
31kb_xeno_dom-with-recovery      41,848    0
211kb_hexml_dom                 211,496    0
211kb_xeno_sax                  248,000    0
211kb_xeno_dom                  930,160    0
211kb_xeno_dom-with-recovery  1,549,480    0
```

Speed:

```
benchmarking 4KB/hexml-dom
time                 3.292 μs   (3.275 μs .. 3.306 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 3.290 μs   (3.279 μs .. 3.302 μs)
std dev              36.54 ns   (31.80 ns .. 43.45 ns)

benchmarking 4KB/xeno-sax
time                 2.866 μs   (2.831 μs .. 2.924 μs)
                     0.994 R²   (0.983 R² .. 1.000 R²)
mean                 2.888 μs   (2.832 μs .. 3.047 μs)
std dev              264.5 ns   (14.17 ns .. 488.9 ns)
variance introduced by outliers: 86% (severely inflated)

benchmarking 4KB/xeno-dom
time                 6.174 μs   (6.148 μs .. 6.201 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 6.158 μs   (6.150 μs .. 6.171 μs)
std dev              33.54 ns   (25.43 ns .. 50.89 ns)

benchmarking 4KB/xeno-dom-with-recovery
time                 8.076 μs   (8.051 μs .. 8.099 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 8.052 μs   (8.027 μs .. 8.073 μs)
std dev              74.41 ns   (59.93 ns .. 98.45 ns)

benchmarking 4KB/hexpat-sax
time                 46.75 μs   (46.59 μs .. 47.00 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 47.46 μs   (47.21 μs .. 47.72 μs)
std dev              851.3 ns   (731.6 ns .. 1.156 μs)
variance introduced by outliers: 14% (moderately inflated)

benchmarking 4KB/hexpat-dom
time                 132.7 μs   (132.0 μs .. 133.3 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 131.4 μs   (130.8 μs .. 131.9 μs)
std dev              1.838 μs   (1.685 μs .. 2.039 μs)

benchmarking 4KB/xml-dom
time                 1.002 ms   (994.3 μs .. 1.012 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 990.3 μs   (984.2 μs .. 998.5 μs)
std dev              22.58 μs   (16.96 μs .. 30.52 μs)
variance introduced by outliers: 12% (moderately inflated)

benchmarking 31KB/hexml-dom
time                 2.811 μs   (2.805 μs .. 2.818 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 2.813 μs   (2.807 μs .. 2.819 μs)
std dev              19.98 ns   (15.83 ns .. 25.29 ns)

benchmarking 31KB/xeno-sax
time                 1.947 μs   (1.941 μs .. 1.953 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.949 μs   (1.945 μs .. 1.952 μs)
std dev              12.48 ns   (10.75 ns .. 14.36 ns)

benchmarking 31KB/xeno-dom
time                 3.433 μs   (3.422 μs .. 3.443 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 3.430 μs   (3.424 μs .. 3.436 μs)
std dev              19.86 ns   (16.08 ns .. 24.60 ns)

benchmarking 31KB/xeno-dom-with-recovery
time                 4.566 μs   (4.555 μs .. 4.582 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 4.585 μs   (4.574 μs .. 4.598 μs)
std dev              40.27 ns   (33.66 ns .. 52.72 ns)

benchmarking 31KB/hexpat-sax
time                 165.7 μs   (164.5 μs .. 167.0 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 165.3 μs   (164.6 μs .. 166.0 μs)
std dev              2.472 μs   (2.048 μs .. 3.225 μs)

benchmarking 31KB/hexpat-dom
time                 172.2 μs   (170.8 μs .. 173.3 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 170.7 μs   (170.2 μs .. 171.3 μs)
std dev              1.932 μs   (1.630 μs .. 2.629 μs)

benchmarking 31KB/xml-dom
time                 6.749 ms   (6.551 ms .. 6.923 ms)
                     0.996 R²   (0.993 R² .. 0.998 R²)
mean                 6.433 ms   (6.363 ms .. 6.521 ms)
std dev              241.8 μs   (196.7 μs .. 319.9 μs)
variance introduced by outliers: 18% (moderately inflated)

benchmarking 211KB/hexml-dom
time                 121.7 μs   (121.4 μs .. 122.1 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 122.7 μs   (122.4 μs .. 123.1 μs)
std dev              1.270 μs   (980.1 ns .. 1.666 μs)

benchmarking 211KB/xeno-sax
time                 139.3 μs   (138.9 μs .. 139.5 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 138.4 μs   (138.1 μs .. 138.7 μs)
std dev              1.105 μs   (951.2 ns .. 1.247 μs)

benchmarking 211KB/xeno-dom
time                 280.1 μs   (278.5 μs .. 282.3 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 283.3 μs   (282.3 μs .. 284.1 μs)
std dev              3.129 μs   (2.480 μs .. 4.007 μs)

benchmarking 211KB/xeno-dom-with-recovery
time                 395.7 μs   (393.9 μs .. 398.5 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 395.4 μs   (394.5 μs .. 397.0 μs)
std dev              4.219 μs   (2.674 μs .. 6.378 μs)

benchmarking 211KB/hexpat-sax
time                 12.10 ms   (11.80 ms .. 12.41 ms)
                     0.996 R²   (0.994 R² .. 0.998 R²)
mean                 12.54 ms   (12.37 ms .. 12.76 ms)
std dev              516.3 μs   (415.2 μs .. 664.9 μs)
variance introduced by outliers: 14% (moderately inflated)

benchmarking 211KB/hexpat-dom
time                 11.90 ms   (11.53 ms .. 12.24 ms)
                     0.991 R²   (0.983 R² .. 0.996 R²)
mean                 12.60 ms   (12.28 ms .. 12.93 ms)
std dev              833.5 μs   (696.0 μs .. 1.022 ms)
variance introduced by outliers: 32% (moderately inflated)

benchmarking 211KB/xml-dom
time                 60.34 ms   (57.27 ms .. 66.33 ms)
                     0.989 R²   (0.977 R² .. 0.998 R²)
mean                 64.81 ms   (62.83 ms .. 66.78 ms)
std dev              3.643 ms   (2.833 ms .. 4.701 ms)
variance introduced by outliers: 16% (moderately inflated)

Benchmark xeno-speed-bench: FINISH
```

### Before speed improvements

Memory:

```
Case                          Allocated  GCs
4kb_hexml_dom                     3,808    0
4kb_xeno_sax                      6,120    0
4kb_xeno_dom                     14,728    0
4kb_xeno_dom-with-recovery       21,024    0
31kb_hexml_dom                   30,608    0
31kb_xeno_sax                     2,232    0
31kb_xeno_dom                    10,696    0
31kb_xeno_dom-with-recovery      13,752    0
211kb_hexml_dom                 211,496    0
211kb_xeno_sax                  343,848    0
211kb_xeno_dom                1,369,768    0
211kb_xeno_dom-with-recovery  1,663,776    0
```

Speed:

```
benchmarking 4KB/hexml-dom
time                 3.375 μs   (3.342 μs .. 3.417 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 3.428 μs   (3.405 μs .. 3.450 μs)
std dev              75.84 ns   (63.24 ns .. 90.55 ns)
variance introduced by outliers: 25% (moderately inflated)

benchmarking 4KB/xeno-sax
time                 5.638 μs   (5.617 μs .. 5.663 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 5.654 μs   (5.636 μs .. 5.680 μs)
std dev              69.15 ns   (54.69 ns .. 100.9 ns)

benchmarking 4KB/xeno-dom
time                 9.023 μs   (8.986 μs .. 9.053 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 8.969 μs   (8.948 μs .. 8.988 μs)
std dev              64.36 ns   (53.90 ns .. 80.12 ns)

benchmarking 4KB/xeno-dom-with-recovery
time                 11.78 μs   (11.75 μs .. 11.82 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 11.81 μs   (11.78 μs .. 11.84 μs)
std dev              105.5 ns   (89.45 ns .. 130.4 ns)

benchmarking 4KB/hexpat-sax
time                 48.36 μs   (48.29 μs .. 48.47 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 48.59 μs   (48.45 μs .. 48.74 μs)
std dev              485.2 ns   (390.5 ns .. 685.1 ns)

benchmarking 4KB/hexpat-dom
time                 133.3 μs   (132.8 μs .. 133.9 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 132.8 μs   (132.4 μs .. 133.2 μs)
std dev              1.446 μs   (1.263 μs .. 1.721 μs)

benchmarking 4KB/xml-dom
time                 1.026 ms   (1.009 ms .. 1.041 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 1.016 ms   (1.011 ms .. 1.024 ms)
std dev              21.37 μs   (16.57 μs .. 30.56 μs)
variance introduced by outliers: 11% (moderately inflated)

benchmarking 31KB/hexml-dom
time                 2.821 μs   (2.813 μs .. 2.828 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 2.807 μs   (2.799 μs .. 2.816 μs)
std dev              28.15 ns   (23.56 ns .. 33.72 ns)

benchmarking 31KB/xeno-sax
time                 2.477 μs   (2.464 μs .. 2.491 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 2.477 μs   (2.470 μs .. 2.489 μs)
std dev              32.72 ns   (23.35 ns .. 50.93 ns)
variance introduced by outliers: 11% (moderately inflated)

benchmarking 31KB/xeno-dom
time                 4.020 μs   (4.014 μs .. 4.025 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 4.011 μs   (4.002 μs .. 4.019 μs)
std dev              28.51 ns   (20.23 ns .. 45.33 ns)

benchmarking 31KB/xeno-dom-with-recovery
time                 5.325 μs   (5.278 μs .. 5.398 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 5.286 μs   (5.267 μs .. 5.314 μs)
std dev              82.13 ns   (47.60 ns .. 153.7 ns)
variance introduced by outliers: 13% (moderately inflated)

benchmarking 31KB/hexpat-sax
time                 166.7 μs   (166.5 μs .. 166.9 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 166.5 μs   (166.2 μs .. 167.0 μs)
std dev              1.185 μs   (867.9 ns .. 1.857 μs)

benchmarking 31KB/hexpat-dom
time                 172.6 μs   (172.0 μs .. 173.4 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 173.0 μs   (172.4 μs .. 173.6 μs)
std dev              2.022 μs   (1.523 μs .. 2.779 μs)

benchmarking 31KB/xml-dom
time                 6.944 ms   (6.707 ms .. 7.146 ms)
                     0.994 R²   (0.991 R² .. 0.997 R²)
mean                 6.677 ms   (6.559 ms .. 6.961 ms)
std dev              483.6 μs   (293.8 μs .. 846.9 μs)
variance introduced by outliers: 43% (moderately inflated)

benchmarking 211KB/hexml-dom
time                 118.3 μs   (117.6 μs .. 119.2 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 118.1 μs   (117.7 μs .. 118.7 μs)
std dev              1.679 μs   (1.172 μs .. 2.467 μs)

benchmarking 211KB/xeno-sax
time                 274.0 μs   (273.2 μs .. 275.0 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 273.2 μs   (272.6 μs .. 274.3 μs)
std dev              2.724 μs   (2.035 μs .. 4.049 μs)

benchmarking 211KB/xeno-dom
time                 433.0 μs   (431.2 μs .. 435.3 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 434.9 μs   (433.3 μs .. 437.0 μs)
std dev              6.016 μs   (4.833 μs .. 7.945 μs)

benchmarking 211KB/xeno-dom-with-recovery
time                 566.5 μs   (558.7 μs .. 575.9 μs)
                     0.998 R²   (0.998 R² .. 0.999 R²)
mean                 554.9 μs   (550.7 μs .. 560.2 μs)
std dev              16.23 μs   (12.47 μs .. 19.34 μs)
variance introduced by outliers: 21% (moderately inflated)

benchmarking 211KB/hexpat-sax
time                 11.26 ms   (11.20 ms .. 11.31 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 11.20 ms   (11.06 ms .. 11.27 ms)
std dev              259.3 μs   (121.2 μs .. 461.2 μs)

benchmarking 211KB/hexpat-dom
time                 11.31 ms   (11.19 ms .. 11.39 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 11.51 ms   (11.43 ms .. 11.67 ms)
std dev              265.5 μs   (152.2 μs .. 460.7 μs)

benchmarking 211KB/xml-dom
time                 57.30 ms   (56.28 ms .. 58.74 ms)
                     0.998 R²   (0.996 R² .. 1.000 R²)
mean                 59.24 ms   (58.32 ms .. 60.63 ms)
std dev              2.064 ms   (1.462 ms .. 2.875 ms)
```


# Benchmark with big files

Benchmarks ran with `--time-limit 60` to eliminate variance.

## Nonthreaded environment

Compiled with `ghc-options: -O2 -rtsopts "-with-rtsopts=-H10G -AL1G -A256m -M25G"`.

### Current benchmarks


```
benchmarking 46MB/hexml-dom
time                 14.29 ms   (14.25 ms .. 14.36 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 14.55 ms   (14.51 ms .. 14.59 ms)
std dev              161.9 μs   (137.5 μs .. 199.9 μs)

benchmarking 46MB/xeno-sax
time                 14.26 ms   (14.01 ms .. 14.55 ms)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 14.04 ms   (13.96 ms .. 14.12 ms)
std dev              339.4 μs   (299.2 μs .. 444.3 μs)
variance introduced by outliers: 13% (moderately inflated)

benchmarking 46MB/xeno-dom
time                 21.52 ms   (21.20 ms .. 21.85 ms)
                     0.999 R²   (0.999 R² .. 0.999 R²)
mean                 21.18 ms   (21.06 ms .. 21.30 ms)
std dev              487.5 μs   (443.6 μs .. 537.0 μs)
variance introduced by outliers: 11% (moderately inflated)

benchmarking 46MB/xeno-dom-with-recovery
time                 34.83 ms   (34.65 ms .. 35.00 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 34.83 ms   (34.76 ms .. 34.90 ms)
std dev              245.5 μs   (216.3 μs .. 283.6 μs)

benchmarking 624MB/hexml-dom
time                 2.266 μs   (2.258 μs .. 2.271 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 2.228 μs   (2.221 μs .. 2.235 μs)
std dev              35.03 ns   (30.76 ns .. 45.90 ns)
variance introduced by outliers: 18% (moderately inflated)

benchmarking 624MB/xeno-sax
time                 3.388 μs   (3.379 μs .. 3.397 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 3.376 μs   (3.368 μs .. 3.384 μs)
std dev              37.73 ns   (32.70 ns .. 43.64 ns)

benchmarking 624MB/xeno-dom
time                 5.983 μs   (5.949 μs .. 6.008 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 5.769 μs   (5.739 μs .. 5.803 μs)
std dev              152.3 ns   (133.5 ns .. 171.7 ns)
variance introduced by outliers: 36% (moderately inflated)

benchmarking 624MB/xeno-dom-with-recovery
time                 8.230 μs   (8.189 μs .. 8.264 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 8.020 μs   (7.993 μs .. 8.050 μs)
std dev              137.2 ns   (115.2 ns .. 160.0 ns)
variance introduced by outliers: 19% (moderately inflated)

benchmarking 921MB/hexml-dom
time                 778.4 ms   (775.9 ms .. 783.2 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 789.0 ms   (782.7 ms .. 803.7 ms)
std dev              15.17 ms   (1.021 ms .. 24.21 ms)

benchmarking 921MB/xeno-sax
time                 1.022 s    (1.013 s .. 1.026 s)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.023 s    (1.019 s .. 1.030 s)
std dev              8.569 ms   (4.558 ms .. 12.04 ms)

benchmarking 921MB/xeno-dom
time                 1.698 s    (1.665 s .. 1.731 s)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 1.732 s    (1.716 s .. 1.759 s)
std dev              30.31 ms   (14.45 ms .. 43.77 ms)
variance introduced by outliers: 12% (moderately inflated)

benchmarking 921MB/xeno-dom-with-recovery
time                 2.835 s    (2.812 s .. 2.861 s)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 2.887 s    (2.865 s .. 2.912 s)
std dev              28.72 ms   (16.82 ms .. 39.08 ms)
variance introduced by outliers: 16% (moderately inflated)

benchmarking 1.6Gb/hexml-dom
time                 358.3 ms   (357.0 ms .. 360.0 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 353.9 ms   (352.0 ms .. 355.3 ms)
std dev              3.588 ms   (2.775 ms .. 5.135 ms)

benchmarking 1.6Gb/xeno-sax
time                 296.4 ms   (293.9 ms .. 298.2 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 299.3 ms   (297.7 ms .. 304.2 ms)
std dev              5.538 ms   (1.036 ms .. 10.24 ms)

benchmarking 1.6Gb/xeno-dom
time                 422.7 ms   (411.6 ms .. 427.9 ms)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 435.2 ms   (428.8 ms .. 466.0 ms)
std dev              24.62 ms   (1.125 ms .. 49.13 ms)
variance introduced by outliers: 18% (moderately inflated)

benchmarking 1.6Gb/xeno-dom-with-recovery
time                 661.4 ms   (658.9 ms .. 663.1 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 663.3 ms   (661.9 ms .. 665.6 ms)
std dev              3.289 ms   (1.822 ms .. 5.586 ms)

benchmarking 4Gb/xeno-sax
time                 1.087 s    (1.070 s .. 1.113 s)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 1.150 s    (1.114 s .. 1.223 s)
std dev              78.01 ms   (36.11 ms .. 117.7 ms)
variance introduced by outliers: 20% (moderately inflated)

benchmarking 4Gb/xeno-dom
time                 2.078 s    (2.027 s .. 2.129 s)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 2.120 s    (2.100 s .. 2.145 s)
std dev              34.26 ms   (25.58 ms .. 45.89 ms)
variance introduced by outliers: 12% (moderately inflated)

benchmarking 4Gb/xeno-dom-with-recovery
time                 2.561 s    (2.429 s .. 2.699 s)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 2.810 s    (2.680 s .. 3.073 s)
std dev              241.4 ms   (7.854 ms .. 327.5 ms)
variance introduced by outliers: 16% (moderately inflated)
```

### Before speed improvements

```
benchmarking 46MB/xeno-sax
time                 14.49 ms   (14.45 ms .. 14.52 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 14.48 ms   (14.45 ms .. 14.54 ms)
std dev              162.5 μs   (100.4 μs .. 254.5 μs)

benchmarking 46MB/xeno-dom
time                 26.67 ms   (26.53 ms .. 26.78 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 26.91 ms   (26.80 ms .. 27.05 ms)
std dev              481.0 μs   (407.5 μs .. 559.0 μs)

benchmarking 46MB/xeno-dom-with-recovery
time                 34.16 ms   (33.96 ms .. 34.32 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 33.75 ms   (33.66 ms .. 33.84 ms)
std dev              322.8 μs   (277.0 μs .. 398.9 μs)

benchmarking 624MB/xeno-sax
time                 4.769 μs   (4.729 μs .. 4.812 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 4.763 μs   (4.749 μs .. 4.776 μs)
std dev              69.56 ns   (61.48 ns .. 80.52 ns)
variance introduced by outliers: 15% (moderately inflated)

benchmarking 624MB/xeno-dom
time                 8.033 μs   (7.947 μs .. 8.106 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 8.032 μs   (7.993 μs .. 8.083 μs)
std dev              227.8 ns   (197.2 ns .. 275.6 ns)
variance introduced by outliers: 39% (moderately inflated)

benchmarking 624MB/xeno-dom-with-recovery
time                 9.347 μs   (9.270 μs .. 9.411 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 9.519 μs   (9.458 μs .. 9.581 μs)
std dev              305.4 ns   (268.5 ns .. 350.5 ns)
variance introduced by outliers: 45% (moderately inflated)

benchmarking 921MB/xeno-sax
time                 2.168 s    (2.099 s .. 2.224 s)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 2.189 s    (2.171 s .. 2.213 s)
std dev              26.88 ms   (14.05 ms .. 38.93 ms)
variance introduced by outliers: 14% (moderately inflated)

benchmarking 921MB/xeno-dom
time                 3.220 s    (3.144 s .. 3.318 s)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 3.201 s    (3.177 s .. 3.235 s)
std dev              33.20 ms   (27.21 ms .. 35.74 ms)
variance introduced by outliers: 16% (moderately inflated)

benchmarking 921MB/xeno-dom-with-recovery
time                 3.611 s    (3.556 s .. 3.645 s)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 3.641 s    (3.624 s .. 3.652 s)
std dev              17.77 ms   (9.617 ms .. 26.87 ms)
variance introduced by outliers: 16% (moderately inflated)

benchmarking 1.6Gb/xeno-sax
time                 287.4 ms   (286.7 ms .. 288.3 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 288.0 ms   (287.5 ms .. 288.5 ms)
std dev              1.005 ms   (677.9 μs .. 1.519 ms)

benchmarking 1.6Gb/xeno-dom
time                 492.5 ms   (490.7 ms .. 494.6 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 490.9 ms   (489.6 ms .. 491.9 ms)
std dev              2.400 ms   (1.611 ms .. 3.445 ms)

benchmarking 1.6Gb/xeno-dom-with-recovery
time                 620.7 ms   (615.2 ms .. 629.3 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 611.0 ms   (605.1 ms .. 616.0 ms)
std dev              10.46 ms   (7.512 ms .. 13.16 ms)

benchmarking 4Gb/xeno-sax
time                 1.309 s    (1.306 s .. 1.312 s)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.308 s    (1.307 s .. 1.309 s)
std dev              2.114 ms   (1.591 ms .. 2.696 ms)

benchmarking 4Gb/xeno-dom
time                 2.204 s    (2.082 s .. 2.300 s)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 2.245 s    (2.210 s .. 2.304 s)
std dev              62.12 ms   (23.07 ms .. 89.62 ms)
variance introduced by outliers: 14% (moderately inflated)

benchmarking 4Gb/xeno-dom-with-recovery
time                 2.739 s    (2.674 s .. 2.795 s)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 2.812 s    (2.769 s .. 2.864 s)
std dev              63.86 ms   (43.18 ms .. 82.15 ms)
variance introduced by outliers: 14% (moderately inflated)
```

## Threaded environment

Compiled with `ghc-options: -O2 -rtsopts -threaded "-with-rtsopts=-N -H10G -AL1G -A256m -M25G"`.

### Current benchmarks

```
benchmarking 46MB/hexml-dom
time                 14.99 ms   (14.97 ms .. 15.01 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 14.90 ms   (14.88 ms .. 14.95 ms)
std dev              144.7 μs   (91.07 μs .. 225.0 μs)

benchmarking 46MB/xeno-sax
time                 13.29 ms   (13.03 ms .. 13.44 ms)
                     0.991 R²   (0.982 R² .. 0.996 R²)
mean                 14.72 ms   (14.30 ms .. 15.33 ms)
std dev              2.142 ms   (1.503 ms .. 2.782 ms)
variance introduced by outliers: 84% (severely inflated)

benchmarking 46MB/xeno-dom
time                 20.01 ms   (19.96 ms .. 20.05 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 20.11 ms   (20.05 ms .. 20.21 ms)
std dev              336.5 μs   (198.2 μs .. 498.6 μs)

benchmarking 46MB/xeno-dom-with-recovery
time                 31.40 ms   (31.36 ms .. 31.43 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 31.44 ms   (31.40 ms .. 31.54 ms)
std dev              218.9 μs   (142.4 μs .. 358.0 μs)

benchmarking 624MB/hexml-dom
time                 2.346 μs   (2.333 μs .. 2.359 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 2.328 μs   (2.317 μs .. 2.343 μs)
std dev              61.87 ns   (38.71 ns .. 83.36 ns)
variance introduced by outliers: 39% (moderately inflated)

benchmarking 624MB/xeno-sax
time                 3.336 μs   (3.326 μs .. 3.348 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 3.315 μs   (3.310 μs .. 3.326 μs)
std dev              36.64 ns   (25.27 ns .. 65.29 ns)

benchmarking 624MB/xeno-dom
time                 6.454 μs   (6.419 μs .. 6.483 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 6.263 μs   (6.238 μs .. 6.295 μs)
std dev              142.9 ns   (126.2 ns .. 159.1 ns)
variance introduced by outliers: 30% (moderately inflated)

benchmarking 624MB/xeno-dom-with-recovery
time                 7.597 μs   (7.546 μs .. 7.638 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 7.390 μs   (7.361 μs .. 7.415 μs)
std dev              149.1 ns   (127.4 ns .. 168.2 ns)
variance introduced by outliers: 25% (moderately inflated)

benchmarking 921MB/hexml-dom
time                 794.2 ms   (789.2 ms .. 797.5 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 793.8 ms   (791.8 ms .. 798.1 ms)
std dev              5.548 ms   (2.540 ms .. 9.008 ms)

benchmarking 921MB/xeno-sax
time                 989.7 ms   (986.5 ms .. 992.8 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 994.8 ms   (992.5 ms .. 998.4 ms)
std dev              4.712 ms   (3.148 ms .. 6.898 ms)

benchmarking 921MB/xeno-dom
time                 1.642 s    (1.632 s .. 1.653 s)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.692 s    (1.672 s .. 1.718 s)
std dev              32.64 ms   (18.64 ms .. 46.58 ms)
variance introduced by outliers: 12% (moderately inflated)

benchmarking 921MB/xeno-dom-with-recovery
time                 2.701 s    (2.688 s .. 2.712 s)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 2.733 s    (2.721 s .. 2.750 s)
std dev              18.96 ms   (13.56 ms .. 26.10 ms)
variance introduced by outliers: 14% (moderately inflated)

benchmarking 1.6Gb/hexml-dom
time                 315.9 ms   (311.7 ms .. 321.4 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 314.7 ms   (312.8 ms .. 317.2 ms)
std dev              4.749 ms   (3.107 ms .. 6.952 ms)

benchmarking 1.6Gb/xeno-sax
time                 289.6 ms   (288.3 ms .. 290.6 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 293.5 ms   (291.8 ms .. 297.8 ms)
std dev              5.826 ms   (2.143 ms .. 10.26 ms)

benchmarking 1.6Gb/xeno-dom
time                 425.4 ms   (415.4 ms .. 433.9 ms)
                     0.998 R²   (0.996 R² .. 0.999 R²)
mean                 442.2 ms   (435.6 ms .. 453.3 ms)
std dev              17.67 ms   (12.17 ms .. 26.14 ms)
variance introduced by outliers: 12% (moderately inflated)

benchmarking 1.6Gb/xeno-dom-with-recovery
time                 604.8 ms   (601.4 ms .. 608.8 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 613.3 ms   (610.2 ms .. 617.9 ms)
std dev              7.289 ms   (3.513 ms .. 11.54 ms)

benchmarking 4Gb/xeno-sax
time                 1.028 s    (953.7 ms .. 1.069 s)
                     0.994 R²   (0.983 R² .. 1.000 R²)
mean                 1.090 s    (1.049 s .. 1.210 s)
std dev              125.0 ms   (5.987 ms .. 204.9 ms)
variance introduced by outliers: 38% (moderately inflated)

benchmarking 4Gb/xeno-dom
time                 1.664 s    (1.563 s .. 1.733 s)
                     0.996 R²   (0.987 R² .. 1.000 R²)
mean                 1.737 s    (1.683 s .. 1.836 s)
std dev              118.3 ms   (21.76 ms .. 186.1 ms)
variance introduced by outliers: 13% (moderately inflated)

benchmarking 4Gb/xeno-dom-with-recovery
time                 2.828 s    (2.703 s .. 2.975 s)
                     0.997 R²   (0.989 R² .. 1.000 R²)
mean                 2.762 s    (2.671 s .. 2.823 s)
std dev              100.4 ms   (53.00 ms .. 154.0 ms)
variance introduced by outliers: 14% (moderately inflated)
```

### Before speed improvements


```
benchmarking 46MB/xeno-sax
time                 16.29 ms   (16.21 ms .. 16.35 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 16.27 ms   (16.22 ms .. 16.32 ms)
std dev              231.9 μs   (196.1 μs .. 279.4 μs)

benchmarking 46MB/xeno-dom
time                 28.33 ms   (28.22 ms .. 28.45 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 28.42 ms   (28.33 ms .. 28.55 ms)
std dev              415.1 μs   (292.0 μs .. 549.4 μs)

benchmarking 46MB/xeno-dom-with-recovery
time                 34.52 ms   (34.22 ms .. 34.75 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 33.92 ms   (33.82 ms .. 34.03 ms)
std dev              439.4 μs   (379.9 μs .. 523.7 μs)

benchmarking 624MB/xeno-sax
time                 5.092 μs   (5.057 μs .. 5.130 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 5.089 μs   (5.070 μs .. 5.108 μs)
std dev              103.7 ns   (81.65 ns .. 131.2 ns)
variance introduced by outliers: 26% (moderately inflated)

benchmarking 624MB/xeno-dom
time                 8.453 μs   (8.424 μs .. 8.492 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 8.385 μs   (8.363 μs .. 8.411 μs)
std dev              112.0 ns   (97.12 ns .. 143.9 ns)
variance introduced by outliers: 13% (moderately inflated)

benchmarking 624MB/xeno-dom-with-recovery
time                 9.851 μs   (9.808 μs .. 9.885 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 9.808 μs   (9.776 μs .. 9.850 μs)
std dev              170.5 ns   (140.2 ns .. 208.5 ns)
variance introduced by outliers: 19% (moderately inflated)

benchmarking 921MB/xeno-sax
time                 2.290 s    (2.241 s .. 2.339 s)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 2.304 s    (2.292 s .. 2.320 s)
std dev              19.09 ms   (11.00 ms .. 26.20 ms)
variance introduced by outliers: 14% (moderately inflated)

benchmarking 921MB/xeno-dom
time                 3.238 s    (3.180 s .. 3.286 s)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 3.292 s    (3.261 s .. 3.323 s)
std dev              40.30 ms   (31.73 ms .. 47.24 ms)
variance introduced by outliers: 16% (moderately inflated)

benchmarking 921MB/xeno-dom-with-recovery
time                 3.692 s    (3.635 s .. 3.754 s)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 3.682 s    (3.666 s .. 3.690 s)
std dev              15.03 ms   (6.116 ms .. 20.90 ms)
variance introduced by outliers: 16% (moderately inflated)

benchmarking 1.6Gb/xeno-sax
time                 317.6 ms   (313.4 ms .. 321.8 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 321.5 ms   (319.6 ms .. 322.9 ms)
std dev              3.502 ms   (2.465 ms .. 4.933 ms)

benchmarking 1.6Gb/xeno-dom
time                 537.3 ms   (533.3 ms .. 539.4 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 537.9 ms   (536.2 ms .. 540.4 ms)
std dev              3.809 ms   (1.744 ms .. 6.103 ms)

benchmarking 1.6Gb/xeno-dom-with-recovery
time                 617.2 ms   (610.4 ms .. 625.9 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 615.8 ms   (611.8 ms .. 619.9 ms)
std dev              7.619 ms   (5.534 ms .. 12.26 ms)

benchmarking 4Gb/xeno-sax
time                 1.465 s    (1.447 s .. 1.493 s)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 1.471 s    (1.464 s .. 1.477 s)
std dev              10.52 ms   (7.115 ms .. 14.78 ms)
variance introduced by outliers: 11% (moderately inflated)

benchmarking 4Gb/xeno-dom
time                 2.448 s    (2.345 s .. 2.572 s)
                     0.998 R²   (0.992 R² .. 1.000 R²)
mean                 2.492 s    (2.448 s .. 2.547 s)
std dev              66.66 ms   (44.21 ms .. 81.71 ms)
variance introduced by outliers: 14% (moderately inflated)

benchmarking 4Gb/xeno-dom-with-recovery
time                 2.837 s    (2.807 s .. 2.859 s)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 2.840 s    (2.821 s .. 2.855 s)
std dev              21.73 ms   (12.26 ms .. 30.64 ms)
variance introduced by outliers: 14% (moderately inflated)
```
