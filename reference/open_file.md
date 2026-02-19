# Open a file or directory

`open_file()` opens the file at the specified path using the default
program for that file type. `open_location()` opens the file explorer at
the location of the specified file.

`file.open()` and `dir.open()` are aliases, in line with
[`base::file.create`](https://rdrr.io/r/base/files.html), `file.exists`,
`dir.create`, `dir.exists`, etc.

## Usage

``` r
open_file(path)

open_location(path)

file.open(path)

dir.open(path)
```

## Arguments

- path:

  The path to the file or directory.
