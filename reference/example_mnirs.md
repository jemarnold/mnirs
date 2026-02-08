# Get path to *mnirs* example files

Get path to *mnirs* example files

## Usage

``` r
example_mnirs(file = NULL)
```

## Arguments

- file:

  Name of file as character string. If `NULL`, returns a vector of all
  available file names.

## Value

A file path character string for selected example files stored in this
package.

## Examples

``` r
## lists all files
example_mnirs()
#> [1] "artinis_intervals.xlsx"  "moxy_intervals.csv"     
#> [3] "moxy_ramp.xlsx"          "portamon-oxcap.xlsx"    
#> [5] "train.red_intervals.csv" "vo2master.csv"          

## partial matching will error if matches multiple
try(example_mnirs("moxy"))
#> Error in example_mnirs("moxy") : 
#>   ✖ Multiple files match "moxy":
#> ℹ Matching files: "moxy_intervals.csv" and "moxy_ramp.xlsx"

example_mnirs("moxy_ramp")
#> [1] "/home/runner/work/_temp/Library/mnirs/extdata/moxy_ramp.xlsx"
```
