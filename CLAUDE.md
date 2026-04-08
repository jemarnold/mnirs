# CLAUDE.md

## Review package functionality

- Review .R files for target functions mentioned by the user.
- Review `*_helpers.R` files associated with target functions.
- Review tests associated with target functions. Unit tests live in `tests/testthat/`.
- Test files should not be modified without asking user confirmation first.
- Use `cli_abort()`, `cli_warn()`, or `cli_inform()` for user-facing messages.
- Input validation lives in [R/validate_mnirs.R](R/validate_mnirs.R).
- Don't run devtool commands.
- Don't search for or read previous git commits, only look at current state.
- Don't run git commands which will change the current state.

## Formatting

- 80-character per line limit.
- Comments: `## lower case comment`.
- Comments: briefly explain the purpose of the operation
- Comments: don't describe refactoring changes or reference prompt decisions.
- Use explicit `return()` for custom functions.

### Documentation

- Roxygen2 with markdown enabled (`Roxygen: list(markdown = TRUE)`). 
- Roxygen2 commented with `#'`
- pkgdown site config is in [_pkgdown.yml](_pkgdown.yml)