# Module 11 – Debugging & Defensive Programming in R

Assignment #11 for USF Information Science.

## What's here
| File | Purpose |
|---|---|
| `module11_debug.R` | Reproduces the bug, fixes it, validates the fix, and adds a defensive version with input checks |

## The bug
`tukey_multiple()` used `&&` (scalar operator) to combine two length‑10 logical vectors. In R ≥ 4.3 that throws:

```
Error in outliers[, j] && tukey.outlier(x[, j]) :
  'length = 10' in coercion to 'logical(1)'
```

## The fix
Swap `&&` for the element‑wise `&`:

```r
outliers[, j] <- outliers[, j] & tukey.outlier(x[, j])
```

## Run it
```r
source("module11_debug.R")
```
Tested on R 4.3.3.
