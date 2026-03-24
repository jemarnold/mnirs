# CLAUDE.md

## Package Overview

`{mnirs}` is an R package for reading, processing, and analysing muscle near-infrared spectroscopy (mNIRS) data. It imports raw data from CSV/XLS(X) files and provides a standardised pipeline of cleaning, filtering, and analysis functions. It is intended for mNIRS researchers and practitioners in exercise physiology, sports science, and clinical rehabilitation.

## Review package functionality

- Review full .R files associated with mentioned functions, including documentation.
- Review `*_helpers.R` files associated with target functions.
- Review tests in `test-*.R` files under the headers associated with target functions.

## Formatting

- Enforce per-line 80-character limit, closer is better. Adhere to `Air` formatting.
- Comments in the form: `## lower case comment`.
- Comments should briefly explain the "why" of the operation.
- Comments should not describe refactoring changes; describe the new functionality
- Use explicit `return()` expression at the end of custom functions.

## Architecture

...

### Messaging

- Use `cli_abort()`, `cli_warn()`, or `cli_inform()` from `{cli}` for all user-facing messages. 
- The global option `mnirs.verbose` controls whether informational messages are shown.

### Validation

- Input validation lives in [R/validate_mnirs.R](R/validate_mnirs.R).
- Only review validation functions if necessary to understan core functionality associated with user request. Otherwise simply remind user about validations.

### Testing

- Unit tests live in `tests/testthat/`. 
- Tests should not be edited without asking confirmation.
- Write core functionality and remind the user to investigate changes to tests.

### Devtools & git commands

- Don't run devtool commands, inform the user which commands to run themselves.
- Don't check previous git commits, only look at current state.

### Documentation

- Roxygen2 with markdown enabled (`Roxygen: list(markdown = TRUE)`). 
- Roxygen2 commented with `#'`
- pkgdown site config is in [_pkgdown.yml](_pkgdown.yml)

### Example Data

- Example files in `inst/extdata/` cover supported device formats (Artinis, Moxy, Portamon, Train.Red, VO2 Master). 
- Access via e.g.`example_mnirs("moxy_ramp")` with appropriate column names and event times.
