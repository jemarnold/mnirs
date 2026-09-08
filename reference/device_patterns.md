# Known channel names and detection patterns for supported mNIRS devices

Per device: `pattern` strings which must all match one header row
(`fixed` regex flag); default `time_channel` and `event_channel` names;
`extra_channels` companion columns (e.g. sample index, numeric tag)
returned with `keep_all = TRUE`.

## Usage

``` r
device_patterns
```
