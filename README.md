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
  -> ByteString -> m ()
```

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

    Case             Bytes  GCs  Check
    4kb validate     2,304    0  OK
    31kb validate    1,728    0  OK
    211kb validate  38,400    0  OK

Speed benchmarks:

    benchmarking 4KB/hexml
    time                 6.107 μs   (6.096 μs .. 6.120 μs)
                         1.000 R²   (1.000 R² .. 1.000 R²)
    mean                 6.163 μs   (6.138 μs .. 6.201 μs)
    std dev              102.3 ns   (75.47 ns .. 132.4 ns)
    variance introduced by outliers: 15% (moderately inflated)

    benchmarking 4KB/xeno
    time                 4.737 μs   (4.720 μs .. 4.757 μs)
                         1.000 R²   (1.000 R² .. 1.000 R²)
    mean                 4.741 μs   (4.718 μs .. 4.772 μs)
    std dev              92.87 ns   (72.96 ns .. 116.0 ns)
    variance introduced by outliers: 20% (moderately inflated)

    benchmarking 31KB/hexml
    time                 9.779 μs   (9.734 μs .. 9.845 μs)
                         1.000 R²   (0.999 R² .. 1.000 R²)
    mean                 9.957 μs   (9.874 μs .. 10.07 μs)
    std dev              345.4 ns   (260.1 ns .. 514.1 ns)
    variance introduced by outliers: 42% (moderately inflated)

    benchmarking 31KB/xeno
    time                 2.720 μs   (2.696 μs .. 2.742 μs)
                         0.999 R²   (0.998 R² .. 0.999 R²)
    mean                 2.727 μs   (2.696 μs .. 2.764 μs)
    std dev              116.0 ns   (91.92 ns .. 144.4 ns)
    variance introduced by outliers: 56% (severely inflated)

    benchmarking 211KB/hexml
    time                 254.4 μs   (253.6 μs .. 255.5 μs)
                         1.000 R²   (1.000 R² .. 1.000 R²)
    mean                 255.7 μs   (254.8 μs .. 256.9 μs)
    std dev              3.587 μs   (2.553 μs .. 5.137 μs)

    benchmarking 211KB/xeno
    time                 229.4 μs   (227.9 μs .. 230.7 μs)
                         1.000 R²   (1.000 R² .. 1.000 R²)
    mean                 227.9 μs   (227.2 μs .. 228.7 μs)
    std dev              2.659 μs   (2.292 μs .. 3.246 μs)
