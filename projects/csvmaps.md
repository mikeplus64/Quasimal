---
title: csvmaps
published: January 9, 2019
author: Mike Ledger
---

This is a command-line tool for manipulating map-like CSV data, where the first
column is treated as a "key" for that row. Please see the README for the most
up-to-date documentation: https://gitlab.com/transportengineering/csvmaps

## Installation 

Currently requires stack because of a dependency on an unreleased (or rather,
also not on hackage) package `cassava-streaming`.

```shell
$ git clone git@gitlab.com:transportengineering/csvmaps.git
$ cd csvmaps
$ stack install
```

## Usage

```shell
$ csvmaps --help
Usage: csvmaps [-i|--infiles STRING]... [-o|--outfile STRING]
               [--has-header BOOL] [--op MAPOP]
               [--expr MAPEXPR] [--save-labels BOOL]
               [--verbose BOOL]

Available options:
  -h,--help                Show this help text
  -i,--infiles STRING...   Files to combine. The first column of each csv will
                           be used as a key. For non-csv files ( iles that don't
                           end in .csv), each line will be treated as a key.
  -o,--outfile STRING      .csv file to write output to. If omitted, use stdout.
  --has-header BOOL        whether to ignore header in csv files
  --op MAPOP               The operation to use to combine csv files (default:
                           union)
  --expr MAPEXPR
                           The expression to use to combine csv files. Reference
                           the inputs with $N for the Nth document in the
                           "infiles". The operations available are: 
                           1. Union with the + operator; 
                           2. Difference with the - operator; 
                           3. Intersection with the * operator; 
                           4. Union combining all columns with the +. operator; 
                           5. Intersection combining all columns with the *. 
                              operator 
                           6. Labels for expressions with the syntax
                              '"LABEL": EXPR'.
```

## Examples

Take the rows that exist in both `v1`, in addition (preferring `v1`) to the rows
that exist in both `v1` and `v2` (preferring `v2`).

```shell
$ csvmaps -i v1.csv -i v2.csv --expr '(($1 *| $2) +| ($1 - $2))'
```

### MAPEXPR syntax

`$N`
: References the Nth document specified, starting at 1.

`keys A`
: Discards the values of all rows.

`nulls A`
: Discards the rows that have non-empty values.

`non-nulls A`
: Discards the rows that have empty values.

`A : "label"`
: Assigns a label to the expression `A`. When used with the `--save-labels`
  option, this will also result in a file called `label.csv` being created using
  `A`.

`const ["Col_1","Col_2",...] A`
: Replaces all values of `A` with the columns given in the first argument.

`pad N A`
: Ensures the number of columns after each key is at least N; fills ones that
  don't exist with empty strings.
  
`col N A`
: Takes the Nth column.

`col [N1,N2,...] A`
: Uses the columns given.

#### Unions

`A + B`
: Left-biased union of A and B. Same as `A + B`. When keys exist in both maps,
  the values at those keys are taken from the left operand.

`A |+ B`
: Left-biased union of A and B. Same as `A + B`. When keys exist in both maps,
  the left operand's values at those keys are used, unless they are empty.

`A +| B`
: Right-biased union of A and B. Same as `A +| B`. When keys exist in both maps,
  the right operand's values at those keys are used, unless they are empty.

`A |+| B`
: Union of A and B that concatenates the values of all keys that both operands
  have in common.

#### Intersections

`A * B`
: Left-biased intersection of A and B. Same as `A * B`. When keys exist in both
  maps, the values at those keys are taken from the left operand.

`A |* B`
: Left-biased intersection of A and B. Same as `A * B`. When keys exist in both
  maps, the left operand's values at those keys are used, unless they are empty.

`A *| B`
: Right-biased intersection of A and B. Same as `A *| B`. When keys exist in both
  maps, the right operand's values at those keys are used, unless they are empty.

`A |*| B`
: Intersection of A and B that concatenates the values of all keys that both
  operands have in common.
  
#### Difference

`A - B`
: Returns the rows of A whose keys are not in B.

