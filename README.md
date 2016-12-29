# xeno

A fast event-based XML parser.

## Features

* It's a SAX-style/fold parser which triggers events for open/close
  tags, text, etc.
* It does not process attributes yet.
* It handles comments.
* It runs in constant or practically constant space (see the memory
  benchmarks below).
* It currently is faster than Hexml.

## Example

Quickly dumping XML:

``` haskell
> let input = "Before<x k=''>You<y>Hello, <strong>Xeno!</strong></y>go</x>Try <z><i>this!</i></z>"
> dump
"Before... "
<x>
  "You..."
  <y>
    "Hello, "
    <strong>
      "Xeno!"
    </strong>
  </y>
  " go..."
</x>
"Try "
<z>
  <i>
    "this!"
  </i>
</z>
" Woo!"
```

Folding over XML:

``` haskell
> fold (\m _ -> m + 1) const const 0 input
5
```

Most general XML processor:

``` haskell
> process print (const (return ())) print
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

## Performance goals

The [hexml](https://github.com/ndmitchell/hexml) Haskell library uses
an XML parser written in C, so that is the baseline we're trying to
beat or match roughly.

Memory benchmarks for Xeno:

    Case           Bytes  GCs  Check
    4kb validate   1,224    0  OK
    42kb validate  1,536    0  OK
    52kb validate  1,536    0  OK

Speed benchmarks:

    benchmarking 4KB/hexml
    time                 6.121 μs   (6.106 μs .. 6.143 μs)
                         1.000 R²   (0.999 R² .. 1.000 R²)
    mean                 6.191 μs   (6.147 μs .. 6.387 μs)
    std dev              241.3 ns   (82.38 ns .. 561.7 ns)
    variance introduced by outliers: 50% (moderately inflated)

    benchmarking 4KB/xeno
    time                 4.670 μs   (4.610 μs .. 4.732 μs)
                         0.999 R²   (0.998 R² .. 0.999 R²)
    mean                 4.683 μs   (4.630 μs .. 4.743 μs)
    std dev              194.5 ns   (161.3 ns .. 237.1 ns)
    variance introduced by outliers: 53% (severely inflated)

    benchmarking 31KB/hexml
    time                 9.968 μs   (9.713 μs .. 10.24 μs)
                         0.994 R²   (0.991 R² .. 0.997 R²)
    mean                 10.12 μs   (9.824 μs .. 10.38 μs)
    std dev              887.5 ns   (712.6 ns .. 1.062 μs)
    variance introduced by outliers: 83% (severely inflated)

    benchmarking 31KB/xeno
    time                 2.728 μs   (2.693 μs .. 2.759 μs)
                         0.999 R²   (0.998 R² .. 0.999 R²)
    mean                 2.720 μs   (2.687 μs .. 2.756 μs)
    std dev              110.3 ns   (89.33 ns .. 133.6 ns)
    variance introduced by outliers: 54% (severely inflated)

    benchmarking 211KB/hexml
    time                 256.2 μs   (255.9 μs .. 256.5 μs)
                         1.000 R²   (1.000 R² .. 1.000 R²)
    mean                 256.0 μs   (255.7 μs .. 256.3 μs)
    std dev              861.9 ns   (699.5 ns .. 1.062 μs)

    benchmarking 211KB/xeno
    time                 209.6 μs   (206.5 μs .. 213.1 μs)
                         0.998 R²   (0.997 R² .. 0.999 R²)
    mean                 208.6 μs   (206.3 μs .. 210.9 μs)
    std dev              7.096 μs   (6.141 μs .. 8.274 μs)
    variance introduced by outliers: 30% (moderately inflated)
