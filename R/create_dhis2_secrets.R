#' Create DHIS2 Credentials as Keyring Secrets
#'
#' Interactively prompts for a DHIS2 username and password and stores them
#' securely using the \pkg{keyring} package.
#'
#' The secrets are saved under the keys \code{"dhis2_username"} and
#' \code{"dhis2_password"} in the default keyring.
#'
#' @details
#' This function is interactive and intended for local use only.
#' It should not be called in non-interactive sessions such as CI pipelines.
#'
#' @return
#' Invisibly returns \code{NULL}.
#'
#' @seealso
#' \code{\link[keyring]{key_set}}
#'
#' @examples
#' \dontrun{
#' create_dhis2_secrets()
#' }
#'
#' @export
create_dhis2_secrets <- function() {
  keyring::key_set("dhis2_username", prompt = "Enter DHIS 2 user name: ")
  keyring::key_set("dhis2_password", prompt = "Enter DHIS 2 password: ")
}