# CLAUDE.md

## Review package functionality

- Review .R files associated with target functions mentioned by the user.
- Review `*_helpers.R` files associated with target functions.
- Unit tests live in `tests/testthat/`. Review tests associated with target functions.
- Test files should not be modified without asking user confirmation first.
- Input validation lives in [R/validate_mnirs.R](R/validate_mnirs.R).
- Use `cli_abort()`, `cli_warn()`, or `cli_inform()` for user-facing messages.
- Don't run devtool commands.
- Don't check previous git commits, only look at current state.

## Formatting

- Loose 80-character per line limit.
- Comments: `## lower case comment`.
- Comments should briefly explain the purpose of the operation
- Comments should not describe refactoring changes or reference prompt decisions.
- Use explicit `return()` for custom functions.

### Documentation

- Roxygen2 with markdown enabled (`Roxygen: list(markdown = TRUE)`). 
- Roxygen2 commented with `#'`
- pkgdown site config is in [_pkgdown.yml](_pkgdown.yml)