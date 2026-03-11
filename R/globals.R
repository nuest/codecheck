#' @importFrom stats median setNames
#' @keywords internal
"_PACKAGE"

#' Custom HTTP GET with proper User-Agent header
#'
#' Wraps \code{httr::GET()} with a descriptive User-Agent to avoid being
#' blocked by services like Figshare that reject default libcurl requests,
#' especially from CI runner IPs (e.g., GitHub Actions).
#'
#' @param url The URL to request
#' @param ... Additional arguments passed to \code{httr::GET()}
#' @return An \code{httr} response object
codecheck_GET <- function(url, ...) {
  ua <- paste0("codecheck/", utils::packageVersion("codecheck"),
               " (https://codecheck.org.uk; mailto:daniel.nuest@tu-dresden.de)")
  httr::GET(url,
    httr::user_agent(ua),
    ...
  )
}

# Declare global variables used in NSE (non-standard evaluation)
# to avoid R CMD check NOTEs about "no visible binding for global variable"
utils::globalVariables(c(
  # Column names used in data.frame/dplyr operations
  "Certificate",
  "Certificate ID",
  "Check date",
  "codechecker_name",
  "venue_label"
))
