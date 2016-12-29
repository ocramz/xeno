# xeno

A fast event-based XML parser.

## Example

Quickly dumping XML:

``` haskell
> let input = "Before<x k=''>You<y>Hello, <strong>Xeno!</strong></y>go</x>Try <z><i>this!</i></z>"
> dump
<x>
  <y>
    <strong>
    </strong>
  </y>
</x>
<z>
  <i>
  </i>
</z>
```

Folding over XML:

``` haskell
> fold (\m _ -> m+1) const 0 input
5
```

Most general XML processor:

``` haskell
> process print print input
"x"
"y"
"strong"
"strong"
"y"
"x"
"z"
"i"
"i"
"z"
```

## Features

* It's a SAX-style/fold parser which triggers events for open/close
  tags, text, etc.
* It does not process attributes yet.
* It handles comments.
* It runs in constant or practically constant space (see the memory
  benchmarks below).
* It currently is faster than Hexml.

## Performance goals

The [hexml](https://github.com/ndmitchell/hexml) Haskell library uses
an XML parser written in C, so that is the baseline we're trying to
beat or match roughly.

Memory benchmarks for Xeno:

    Case        Bytes  GCs  Check
    4kb parse   1,160    0  OK
    42kb parse  1,160    0  OK
    52kb parse  1,160    0  OK

Speed benchmarks:

    benchmarking 4KB/hexml
    time                 6.190 μs   (6.159 μs .. 6.230 μs)
                         1.000 R²   (1.000 R² .. 1.000 R²)
    mean                 6.216 μs   (6.195 μs .. 6.257 μs)
    std dev              94.07 ns   (83.83 ns .. 105.7 ns)
    variance introduced by outliers: 13% (moderately inflated)

    benchmarking 4KB/xeno
    time                 4.215 μs   (4.175 μs .. 4.247 μs)
                         0.999 R²   (0.998 R² .. 0.999 R²)
    mean                 4.246 μs   (4.189 μs .. 4.311 μs)
    std dev              224.9 ns   (164.1 ns .. 289.2 ns)
    variance introduced by outliers: 66% (severely inflated)

    benchmarking 31KB/hexml
    time                 9.519 μs   (9.252 μs .. 9.795 μs)
                         0.995 R²   (0.992 R² .. 0.998 R²)
    mean                 9.643 μs   (9.436 μs .. 9.882 μs)
    std dev              735.3 ns   (541.3 ns .. 905.4 ns)
    variance introduced by outliers: 78% (severely inflated)

    benchmarking 31KB/xeno
    time                 2.440 μs   (2.415 μs .. 2.463 μs)
                         0.999 R²   (0.999 R² .. 0.999 R²)
    mean                 2.439 μs   (2.416 μs .. 2.473 μs)
    std dev              96.87 ns   (79.85 ns .. 118.0 ns)
    variance introduced by outliers: 53% (severely inflated)

    benchmarking 211KB/hexml
    time                 258.6 μs   (258.1 μs .. 259.1 μs)
                         1.000 R²   (1.000 R² .. 1.000 R²)
    mean                 258.3 μs   (258.1 μs .. 258.5 μs)
    std dev              712.5 ns   (564.2 ns .. 873.5 ns)

    benchmarking 211KB/xeno
    time                 200.9 μs   (199.0 μs .. 202.7 μs)
                         0.999 R²   (0.999 R² .. 0.999 R²)
    mean                 201.0 μs   (199.3 μs .. 203.7 μs)
    std dev              7.278 μs   (5.819 μs .. 10.53 μs)
    variance introduced by outliers: 34% (moderately inflated)
