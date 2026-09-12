# Flatten a captured condition message

Drops cli bullet glyphs and console-width wrapping from a condition
message so the stored `warnings` text reads as plain sentences.

## Usage

``` r
clean_cnd_message(cnd)
```

## Arguments

- cnd:

  A condition object.

## Value

A single character string.
