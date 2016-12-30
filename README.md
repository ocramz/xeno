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

    Case             Bytes  GCs  Check
    4kb validate     2,304    0  OK
    31kb validate    1,728    0  OK
    211kb validate  38,400    0  OK

Speed benchmarks:

    benchmarking 4KB/hexml
    time                 6.152 μs   (6.134 μs .. 6.170 μs)
                         1.000 R²   (1.000 R² .. 1.000 R²)
    mean                 6.137 μs   (6.127 μs .. 6.150 μs)
    std dev              40.77 ns   (32.13 ns .. 57.46 ns)

    benchmarking 4KB/xeno
    time                 4.886 μs   (4.863 μs .. 4.915 μs)
                         1.000 R²   (1.000 R² .. 1.000 R²)
    mean                 4.892 μs   (4.867 μs .. 4.919 μs)
    std dev              87.78 ns   (65.58 ns .. 118.9 ns)
    variance introduced by outliers: 18% (moderately inflated)

    benchmarking 31KB/hexml
    time                 9.872 μs   (9.810 μs .. 9.942 μs)
                         1.000 R²   (0.999 R² .. 1.000 R²)
    mean                 9.875 μs   (9.825 μs .. 9.966 μs)
    std dev              206.0 ns   (166.7 ns .. 281.0 ns)
    variance introduced by outliers: 21% (moderately inflated)

    benchmarking 31KB/xeno
    time                 2.614 μs   (2.602 μs .. 2.625 μs)
                         1.000 R²   (1.000 R² .. 1.000 R²)
    mean                 2.617 μs   (2.608 μs .. 2.635 μs)
    std dev              39.65 ns   (31.27 ns .. 53.55 ns)
    variance introduced by outliers: 14% (moderately inflated)

    benchmarking 211KB/hexml
    time                 255.8 μs   (254.4 μs .. 257.3 μs)
                         1.000 R²   (1.000 R² .. 1.000 R²)
    mean                 255.5 μs   (254.9 μs .. 256.7 μs)
    std dev              2.845 μs   (1.973 μs .. 4.409 μs)

    benchmarking 211KB/xeno
    time                 239.7 μs   (238.0 μs .. 241.1 μs)
                         1.000 R²   (0.999 R² .. 1.000 R²)
    mean                 239.6 μs   (238.7 μs .. 240.8 μs)
    std dev              3.965 μs   (2.998 μs .. 5.581 μs)
