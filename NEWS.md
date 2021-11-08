# productivity 1.1.0
Changes in 'productivity' version 1.1.0 (2018-03-28)

## NEW FEATURES

* A new productivity index is now proposed:
	- Hicks-Moorsteen index (`hicksmoorsteen()`), with its components (malmquist-hs and malmquist-it)

## BUG FIXES

* A bug is corrected addressing `MP`, `TFPE`, `ROSE`, `RISE`, `ROSE.RISE`, `RME` values computed with `lowe()` function.

## DEPRECATED & DEFUNCT
None

## OTHER USER-VISIBLE CHANGES

* {Rglpk} package is not used any more. All DEA linear programs are now implemented using the {lpSolveAPI} package.

* The argument `scaled` in `fareprim()`, `fisher()`, `laspeyres()`, `lowe()`, `malm()`and `paasche()` is now set to `TRUE` by default to avoid extreme cases where models cannot be optimized. Note that it is still possible to specify `scaled = FALSE` in functions' call.

* An argument `shadow` is introduced in `fareprim()`, `fisher()`, `laspeyres()`, `lowe()`, and `paasche()` functions if one wants to retrieve shadow prices.

* Dependency to `plm` package is removed.

* Updated functions' documentation.

* Updated `DESCRIPTION` file.

* Updated startup message.

***
# productivity 1.0.0
Changes in 'productivity' version 1.0.0 (2017-07-20)

## NEW FEATURES

* New productivity indices are now proposed:
	- Laspeyres index(`laspeyres()`)
	- Paasche index (`paasche()`)
	- Fisher index (`fisher()`)

* New functions are now included:
	- `Levels()`, to extract productivity and profitability levels created by `fareprim()`, `fisher()`, `laspeyres()`, `lowe()`, `malm()`, or `paasche()` functions
	- `Changes()`, to extract productivity and profitability change indices created by `fareprim()`, `fisher()`, `laspeyres()`, `lowe()`, `malm()`, or `paasche()` functions
	- `Shadowp()`, to extract input and output deflated shadow prices used in productivity and profitability computations

* The `lowe()` function now returns also deflated shadow prices of input and output variables (`Shadowp`).

* `fareprim()`, `lowe()` and `malm()` functions are now enriched with a `parallel` argument to be set to `TRUE` to enable parallel computation.

* `cores` arguments in `fareprim()`, `lowe()` and `malm()` functions is now only considered if `parallel = TRUE`.

* A 'progress message' is printed to the console while running `fareprim()`, `lowe()` and `malm()` functions (only if `parallel == FALSE`).

## BUG FIXES

* `fareprim()` and `lowe()` functions return correct `OSME.ISME` values (i.e. Geometric mean of Output-oriented and Input-oriented scale-mix efficiency scores when `orientation = "in-out"`).

* `lowe()` function, when `tech.change = FALSE` and `orientation = "in-out"`, does not return an error message any more.

## DEPRECATED & DEFUNCT

* Function argument `indices` is no longer proposed in `fareprim()`, `lowe()` and `malm()` functions. Changes indices are now automatically computed.

* Function arguments `out.levels` and `out.indices` are no longer proposed in `fareprim()`, `lowe()` and `malm()` functions.

## OTHER USER-VISIBLE CHANGES

* Default value of `cores` argument in `fareprim()`, `lowe()` and `malm()` functions is now set to `max(1, detectCores() - 1)` (used only if `parallel = TRUE`).
In the previous version of the package (v 0.2.0), `cores = detectCores() - 1` (default).

* `fareprim()`, `lowe()` and `malm()` functions now return summarised productivity results, to then be extracted using `Levels()`, `Changes()` or `Shadowp` functions

* Updated `fareprim()` documentation.

* Updated `lowe()` documentation.

* Updated `malm()` documentation.

* Updated `DESCRIPTION` file.

* Updated startup message.

***
# productivity 0.2.0
Changes in 'productivity' version 0.2.0 (2017-05-11)

## NEW FEATURES

* The `fareprim()` function now returns also shadow prices of input and output variables (`Shadowp`).

* Added a `NEWS.md` file to track changes to the package.

* Added a `CITATION` file.

## BUG FIXES

* In `fareprim()` function, arguments `by.year` and `by.id` can now be independently specified.
In the previous version of the package (v 0.1.0), specifying only one of these two arguments was generating an error (`object 'id.vec' not found`).

* In `lowe()` function, arguments `by.year` and `by.id` can now be independently specified.
In the previous version of the package (v 0.1.0), specifying only one of these two arguments was generating an error (`object 'id.vec' not found`).

## DEPRECATED & DEFUNCT
None

## OTHER USER-VISIBLE CHANGES

* Updated `DESCRIPTION` file.

* Updated `frameprim()` documentation to reflect the new `Shadowp` feature in `fareprim()`.

* Updated `frameprim()` documentation to correct minor typos.

* Updated `lowe()` documentation to correct minor typos.

* Updated startup message.

***
# productivity 0.1.0
First public release (2016-11-14)