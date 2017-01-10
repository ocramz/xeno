# xeno

A fast event-based XML parser.

## Features

* It's a SAX-style/fold parser which triggers events for open/close
  tags, attributes, text, etc.
* It handles comments.
* It uses very low memory (see memory benchmarks below).
* It's very fast (see speed benchmarks below).
* It's written in pure Haskell.

## Example

Quickly dumping XML:

``` haskell
> let input = "Text<tag prop='value'>Hello, World!</tag><x><y prop=\"x\">Content!</y></x>Trailing."
> dump input
"Text"
<tag prop="value">
  "Hello, World!"
</tag>
<x>
  <y prop="x">
    "Content!"
  </y>
</x>
"Trailing."
```

Folding over XML:

``` haskell
> fold const (\m _ _ -> m + 1) const const const 0 input -- Count elements.
2
```

``` haskell
> fold (\m _ -> m + 1) (\m _ _ -> m) const const const 0 input -- Count attributes.
3
```

Most general XML processor:

``` haskell
process
  :: Monad m
  => (ByteString -> m ())               -- ^ Open tag.
  -> (ByteString -> ByteString -> m ()) -- ^ Tag attribute.
  -> (ByteString -> m ())               -- ^ End open tag.
  -> (ByteString -> m ())               -- ^ Text.
  -> (ByteString -> m ())               -- ^ Close tag.
  -> ByteString                         -- ^ Input string.
  -> m ()
```

You can use any monad you want. IO, State, etc. For example, `fold` is
implemented like this:

``` haskell
fold openF attrF endOpenF textF closeF s str =
  execState
    (process
       (\name -> modify (\s' -> openF s' name))
       (\key value -> modify (\s' -> attrF s' key value))
       (\name -> modify (\s' -> endOpenF s' name))
       (\text -> modify (\s' -> textF s' text))
       (\name -> modify (\s' -> closeF s' name))
       str)
    s
```

The `process` is marked as INLINE, which means use-sites of it will
inline, and your particular monad's type will be potentially erased
for great performance.

## Performance goals

The [hexml](https://github.com/ndmitchell/hexml) Haskell library uses
an XML parser written in C, so that is the baseline we're trying to
beat or match roughly.

It currently is faster than Hexml for simply walking the
document. Hexml actually does more work, by allocating a big vector
and writing nodes and attributes to it, which, when I tested out
implementing with Xeno, brought the speed benchmarks to the same speed
as Hexml more or less.

Memory benchmarks for Xeno:

    Case                Bytes  GCs  Check
    4kb validate        2,376    0  OK
    31kb validate       1,824    0  OK
    211kb validate     56,832    0  OK
    4kb                10,880    0  OK
    31kb               10,256    0  OK
    211kb           1,078,136    0  OK

Speed benchmarks:

    benchmarking 4KB/hexml/tree
    time                 6.264 μs   (6.196 μs .. 6.352 μs)
                         0.998 R²   (0.994 R² .. 1.000 R²)
    mean                 6.311 μs   (6.256 μs .. 6.523 μs)
    std dev              291.9 ns   (144.5 ns .. 599.2 ns)
    variance introduced by outliers: 58% (severely inflated)

    benchmarking 4KB/xeno/sax
    time                 4.947 μs   (4.905 μs .. 5.002 μs)
                         0.999 R²   (0.997 R² .. 0.999 R²)
    mean                 5.042 μs   (4.982 μs .. 5.129 μs)
    std dev              248.0 ns   (172.4 ns .. 332.2 ns)
    variance introduced by outliers: 61% (severely inflated)

    benchmarking 4KB/xeno/tree
    time                 10.90 μs   (10.85 μs .. 10.97 μs)
                         0.999 R²   (0.999 R² .. 1.000 R²)
    mean                 11.13 μs   (11.00 μs .. 11.40 μs)
    std dev              656.4 ns   (263.0 ns .. 1.042 μs)
    variance introduced by outliers: 68% (severely inflated)

    benchmarking 4KB/hexpat/hexpat-sax
    time                 266.2 μs   (256.5 μs .. 275.8 μs)
                         0.993 R²   (0.989 R² .. 0.998 R²)
    mean                 260.5 μs   (256.5 μs .. 266.4 μs)
    std dev              15.28 μs   (11.50 μs .. 21.05 μs)
    variance introduced by outliers: 55% (severely inflated)

    benchmarking 4KB/hexpat/hexpat-tree
    time                 330.4 μs   (328.4 μs .. 333.4 μs)
                         0.999 R²   (0.999 R² .. 1.000 R²)
    mean                 330.4 μs   (328.8 μs .. 333.0 μs)
    std dev              7.107 μs   (5.066 μs .. 10.32 μs)
    variance introduced by outliers: 14% (moderately inflated)

    benchmarking 31KB/hexml/tree
    time                 10.12 μs   (9.843 μs .. 10.40 μs)
                         0.996 R²   (0.992 R² .. 0.999 R²)
    mean                 9.791 μs   (9.684 μs .. 9.977 μs)
    std dev              498.0 ns   (343.5 ns .. 793.1 ns)
    variance introduced by outliers: 61% (severely inflated)

    benchmarking 31KB/xeno/sax
    time                 2.637 μs   (2.627 μs .. 2.649 μs)
                         1.000 R²   (0.999 R² .. 1.000 R²)
    mean                 2.661 μs   (2.645 μs .. 2.691 μs)
    std dev              71.86 ns   (46.10 ns .. 106.3 ns)
    variance introduced by outliers: 34% (moderately inflated)

    benchmarking 31KB/xeno/tree
    time                 6.093 μs   (5.981 μs .. 6.211 μs)
                         0.998 R²   (0.997 R² .. 0.999 R²)
    mean                 5.971 μs   (5.909 μs .. 6.066 μs)
    std dev              247.6 ns   (181.5 ns .. 344.4 ns)
    variance introduced by outliers: 53% (severely inflated)

    benchmarking 31KB/hexpat/hexpat-sax
    time                 339.4 μs   (320.8 μs .. 365.9 μs)
                         0.986 R²   (0.973 R² .. 0.998 R²)
    mean                 333.1 μs   (328.0 μs .. 341.3 μs)
    std dev              20.04 μs   (13.67 μs .. 31.33 μs)
    variance introduced by outliers: 55% (severely inflated)

    benchmarking 31KB/hexpat/hexpat-tree
    time                 393.9 μs   (392.1 μs .. 396.0 μs)
                         0.997 R²   (0.992 R² .. 1.000 R²)
    mean                 408.9 μs   (402.9 μs .. 426.0 μs)
    std dev              35.58 μs   (14.67 μs .. 63.35 μs)
    variance introduced by outliers: 72% (severely inflated)

    benchmarking 211KB/hexml/tree
    time                 259.4 μs   (257.7 μs .. 261.7 μs)
                         0.999 R²   (0.999 R² .. 1.000 R²)
    mean                 258.4 μs   (257.4 μs .. 259.9 μs)
    std dev              3.822 μs   (1.964 μs .. 5.775 μs)

    benchmarking 211KB/xeno/sax
    time                 234.1 μs   (233.4 μs .. 234.9 μs)
                         1.000 R²   (0.999 R² .. 1.000 R²)
    mean                 239.1 μs   (236.8 μs .. 243.2 μs)
    std dev              9.988 μs   (6.549 μs .. 14.26 μs)
    variance introduced by outliers: 39% (moderately inflated)

    benchmarking 211KB/xeno/tree
    time                 562.3 μs   (556.9 μs .. 568.0 μs)
                         0.999 R²   (0.998 R² .. 1.000 R²)
    mean                 563.1 μs   (559.4 μs .. 570.5 μs)
    std dev              17.26 μs   (12.24 μs .. 28.09 μs)
    variance introduced by outliers: 22% (moderately inflated)

    benchmarking 211KB/hexpat/hexpat-sax
    time                 23.37 ms   (23.25 ms .. 23.51 ms)
                         1.000 R²   (1.000 R² .. 1.000 R²)
    mean                 23.72 ms   (23.56 ms .. 24.03 ms)
    std dev              520.0 μs   (256.9 μs .. 867.8 μs)

    benchmarking 211KB/hexpat/hexpat-tree
    time                 29.13 ms   (28.94 ms .. 29.42 ms)
                         1.000 R²   (0.999 R² .. 1.000 R²)
    mean                 29.54 ms   (29.36 ms .. 29.83 ms)
    std dev              490.0 μs   (305.5 μs .. 686.4 μs)
