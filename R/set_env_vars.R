#' Set Environment Variables Persistently for R
#'
#' Sets one or more environment variables for the current R session and,
#' optionally, persists them to the user's \code{~/.Renviron} file. Existing
#' entries in \code{~/.Renviron} are safely replaced rather than duplicated.
#'
#' Any variables supplied with \code{NULL} values will automatically prompt
#' the user for input. Prompting is only possible in interactive sessions.
#'
#' @param ... Named character arguments specifying environment variables and
#'   their values. Names correspond to variable names. Values must be character
#'   strings or \code{NULL}. \code{NULL} values trigger a prompt.
#' @param persist Logical; if \code{TRUE} (default), variables are written to
#'   \code{~/.Renviron} for use in future R sessions.
#'
#' @return Invisibly returns a named character vector of environment variables
#'   that were set.
#'
#' @details
#' Environment variables are set immediately for the current R session using
#' \code{Sys.setenv()}. When \code{persist = TRUE}, values are written in
#' plain text to \code{~/.Renviron}. Existing entries with the same variable
#' names are replaced; unrelated entries are preserved.
#'
#' This function intentionally avoids modifying \code{.Rprofile}, which should
#' not be used to store secrets or credentials.
#'
#' Users should ensure that \code{~/.Renviron} has appropriate file permissions
#' (e.g. \code{chmod 600 ~/.Renviron}) to prevent unintended access.
#'
#' @examples
#' \dontrun{
#' # Prompt for secrets
#' set_env_vars(
#'   DHIS2_USERNAME = NULL,
#'   DHIS2_PASSWORD = NULL
#' )
#'
#' # Non-interactive usage
#' set_env_vars(
#'   GITHUB_TOKEN = "ghp_xxx"
#' )
#'
#' # Session-only
#' set_env_vars(
#'   TEMP_API_KEY = "temp",
#'   persist = FALSE
#' )
#' }
#'
#' @export
set_env_vars <- function(..., persist = TRUE) {
  vars <- list(...)

  if (!length(vars)) {
    stop("At least one environment variable must be supplied.", call. = FALSE)
  }

  if (is.null(names(vars)) || any(names(vars) == "")) {
    stop("All environment variables must be named.", call. = FALSE)
  }

  # Prompt for NULL values
  for (nm in names(vars)) {
    if (is.null(vars[[nm]])) {
      if (!interactive()) {
        stop("Cannot prompt for value of '",
             nm,
             "' in a non-interactive session.",
             call. = FALSE)
      }

      vars[[nm]] <- readline(paste0("Enter value for ", nm, ": "))
    }
  }

  # Check for empty strings
  missing <- vapply(vars, identical, logical(1), "")
  if (any(missing)) {
    stop("Missing values for: ", paste(names(vars)[missing], collapse = ", "), call. = FALSE)
  }

  vars <- vapply(vars, as.character, character(1))

  # Set variables for the current session
  do.call(Sys.setenv, as.list(vars))

  # Persist to ~/.Renviron with whitespace-insensitive replacement
  if (persist) {
    renviron <- path.expand("~/.Renviron")
    new_lines <- paste0(names(vars), "=", shQuote(vars))

    if (file.exists(renviron)) {
      existing_lines <- readLines(renviron, warn = FALSE)

      # Extract variable names robustly (ignore whitespace)
      existing_names <- ifelse(
        grepl("^\\s*[A-Za-z_][A-Za-z0-9_]*\\s*=", existing_lines),
        sub(
          "^\\s*([A-Za-z_][A-Za-z0-9_]*)\\s*=.*$",
          "\\1",
          existing_lines
        ),
        NA_character_
      )

      keep <- is.na(existing_names) |
        !existing_names %in% names(vars)

      final_lines <- c(existing_lines[keep], new_lines)

    } else {
      final_lines <- new_lines
    }

    writeLines(final_lines, renviron)
  }

  invisible(vars)
}
