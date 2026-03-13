#' Function for rendering the register into different view
#'
#' NOTE: You should put a GitHub API token in the environment variable `GITHUB_PAT` to fix rate limits. Acquire one at see https://github.com/settings/tokens.
#'
#' - `.html`
#' - `.md``
#'
#' @param register A `data.frame` with all required information for the register's view
#' @param filter_by The filter or list o filters (if applicable)
#' @param outputs The output formats to create
#' @param config A list of configuration files to be sourced at the beginning of the rending process
#' @param venues_file Path to the venues.csv file containing venue names and labels
#' @param codecheck_repo_path Optional path to the codecheck package repository for build metadata (default: NULL)
#' @param from The first register entry to check
#' @param to The last register entry to check
#' @param parallel Logical; if TRUE, renders certificates in parallel using multiple cores. Defaults to FALSE.
#' @param ncores Integer; number of CPU cores to use for parallel rendering. If NULL, automatically detects available cores minus 1. Defaults to NULL.
#' @param verbose Logical; if TRUE, shows detailed output including pandoc commands from rmarkdown::render(). Defaults to FALSE.
#'
#' @return A `data.frame` of the register enriched with information from the configuration files of respective CODECHECKs from the online repositories
#'
#' @author Daniel Nuest
#' @importFrom parsedate parse_date
#' @importFrom rmarkdown render
#' @importFrom knitr kable
#' @importFrom utils capture.output read.csv tail packageVersion
#' @importFrom cli cli_h1 cli_h2 cli_alert_info cli_alert_success cli_alert_warning cli_alert_danger cli_progress_bar cli_progress_update cli_progress_done
#' @import     jsonlite
#' @import     dplyr
#'
#' @export
register_render <- function(register = read.csv("register.csv", as.is = TRUE, comment.char = '#'),
                            filter_by = c("venues", "codecheckers"),
                            outputs = c("html", "md", "json"),
                            config = c(system.file("extdata", "config.R", package = "codecheck")),
                            venues_file = "venues.csv",
                            codecheck_repo_path = NULL,
                            from = 1,
                            to = nrow(register),
                            parallel = FALSE,
                            ncores = NULL,
                            verbose = FALSE) {
  cli::cli_h1("CODECHECK Register Rendering")
  cli::cli_alert_info("codecheck v{utils::packageVersion('codecheck')} | entries {from} to {to}")

  # Capture all warnings so they can be shown as structured log entries at the
  # end, rather than R's default "There were N warnings" prompt.
  captured_warnings <- character(0)

  register_table <- withCallingHandlers(
    {
      # Loading config.R files (creates CONFIG environment)
      for (i in seq(length(config))) {
        source(config[i])
      }

      # Store verbosity setting in CONFIG for use by all rendering functions
      # (must be after config sourcing, which creates the CONFIG environment)
      CONFIG$VERBOSE <- verbose

      # Load venues configuration
      load_venues_config(venues_file)

      # Setup external libraries locally (Bootstrap, Font Awesome, Academicons, etc.)
      setup_external_libraries()

      # Copy package JavaScript files (citation.js, cert-utils.js, etc.)
      copy_package_javascript()

      cli::cli_alert_info("Cache path: {.path {R.cache::getCacheRootPath()}}")

      # Get build metadata for footer and meta tags
      build_metadata <- get_build_metadata(".", codecheck_repo_path)
      CONFIG$BUILD_METADATA <- build_metadata

      register <- register[(from:to),]

      register_table <- preprocess_register(register, filter_by)
      # Setting number of codechecks now for later use. This is done to avoid double counting codechecks
      # done by multiple authors.
      CONFIG$NO_CODECHECKS <- nrow(register_table)

      if("html" %in% outputs) {
        render_cert_htmls(register_table, force_download = FALSE, parallel = parallel, ncores = ncores)
      }

      create_filtered_reg_csvs(register_table, filter_by)
      create_register_files(register_table, filter_by, outputs)
      create_non_register_files(register_table, filter_by)

      # Generate redirect pages for codecheckers with ORCID
      if ("codecheckers" %in% filter_by) {
        generate_codechecker_redirects(register_table)
      }

      # Generate redirect page for /certs/ (without certificate ID)
      generate_certs_redirect()

      # Write build metadata JSON file
      write_meta_json(build_metadata, "docs")

      # Generate SEO files (sitemap.xml and robots.txt)
      generate_sitemap(register_table, filter_by, output_dir = "docs")
      generate_robots_txt(output_dir = "docs")

      register_table
    },
    warning = function(w) {
      captured_warnings <<- c(captured_warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  # Display captured warnings as structured log entries
  if (length(captured_warnings) > 0) {
    # Deduplicate with counts: show each unique warning once, with [xN] prefix if repeated
    warn_table <- sort(table(captured_warnings), decreasing = TRUE)
    cli::cli_h2("{length(captured_warnings)} warning{?s} ({length(warn_table)} unique)")
    for (msg in names(warn_table)) {
      count <- warn_table[[msg]]
      if (count > 1) {
        cli::cli_alert_warning("[x{count}] {msg}")
      } else {
        cli::cli_alert_warning("{msg}")
      }
    }
  }

  cli::cli_alert_success("Register rendering complete")
  invisible(register_table)
}

#' Regenerate all stats.json files from existing register.json files
#'
#' Fast alternative to a full re-render when only the stats computation has
#' changed. Reads the already-generated register.json files under `docs/` and
#' rewrites every stats.json with up-to-date statistics (including annual and
#' cumulative breakdowns).
#'
#' @param docs_dir Path to the docs output directory (default: "docs")
#' @param config Path to the config.R file
#'
#' @author Daniel Nuest
#' @export
register_update_stats <- function(docs_dir = "docs",
                                  config = system.file("extdata", "config.R", package = "codecheck")) {
  cli::cli_h1("CODECHECK Stats Update")
  source(config)

  main_json_path <- file.path(docs_dir, "register.json")
  if (!file.exists(main_json_path)) {
    stop("No register.json found at ", main_json_path, ". Run register_render() first.")
  }

  # --- Main register stats ---
  main_data <- jsonlite::fromJSON(main_json_path)
  stats_data <- list(
    source = paste0(CONFIG$HREF_DETAILS$json$base_url, "register", CONFIG$HREF_DETAILS$json$ext),
    cert_count = nrow(main_data)
  )
  annual <- compute_annual_stats(main_data)
  stats_data <- c(stats_data, annual)

  jsonlite::write_json(stats_data, auto_unbox = TRUE,
                       path = file.path(docs_dir, "stats.json"), pretty = TRUE)
  cli::cli_alert_success("Updated {.path {file.path(docs_dir, 'stats.json')}} ({nrow(main_data)} certs)")

  # --- Sub-register stats (venues, codecheckers, etc.) ---
  sub_jsons <- list.files(docs_dir, pattern = "^register\\.json$",
                          recursive = TRUE, full.names = TRUE)
  # Exclude the main register.json already handled above (normalize both sides)
  main_norm <- normalizePath(main_json_path, mustWork = FALSE)
  sub_jsons <- sub_jsons[normalizePath(sub_jsons, mustWork = FALSE) != main_norm]

  updated <- 0L
  for (json_path in sub_jsons) {
    sub_dir <- dirname(json_path)
    sub_data <- tryCatch(jsonlite::fromJSON(json_path), error = function(e) NULL)
    if (is.null(sub_data) || nrow(sub_data) == 0) next

    # Reconstruct the source URL from the relative path under docs/
    rel_path <- gsub(paste0("^", gsub("([.\\\\])", "\\\\\\1", docs_dir), "/?"), "", sub_dir)
    source_url <- paste0(CONFIG$HREF_DETAILS$json$base_url, rel_path, "/register.json")

    sub_stats <- list(
      source = source_url,
      cert_count = nrow(sub_data)
    )

    jsonlite::write_json(sub_stats, auto_unbox = TRUE,
                         path = file.path(sub_dir, "stats.json"), pretty = TRUE)
    updated <- updated + 1L
  }

  cli::cli_alert_success("Updated {updated} sub-register stats.json files")
}

#' Function for checking all entries in the register
#'
#' This functions starts of a `data.frame` read from the local register file.
#'
#' **Note**: The validation of `codecheck.yml` files happens in function `validate_codecheck_yml()`.
#'
#' Further test ideas:
#'
#' - Does the repo have a LICENSE?
#'
#' @param register A `data.frame` with all required information for the register's view
#' @param from The first register entry to check
#' @param to The last register entry to check
#'
#' @author Daniel Nuest
#' @importFrom R.cache getCacheRootPath
#' @importFrom utils packageVersion
#' @importFrom gh gh
#' @export
register_check <- function(register = read.csv("register.csv", as.is = TRUE, comment.char = '#'),
                           from = 1,
                           to = nrow(register)) {
  cli::cli_h1("CODECHECK Register Check")
  cli::cli_alert_info("codecheck v{utils::packageVersion('codecheck')} | entries {from} to {to}")

  # Loading config.R file
  source(system.file("extdata", "config.R", package = "codecheck"))

  cli::cli_alert_info("Cache path: {.path {R.cache::getCacheRootPath()}}")
  
  for (i in seq(from = from, to = to)) {
    cat("Checking", toString(register[i, ]), "\n")
    entry <- register[i, ]

    # check certificate IDs if there is a codecheck.yml
    codecheck_yaml <- get_codecheck_yml(entry$Repository)
    check_certificate_id(entry, codecheck_yaml)
    check_issue_status(entry)
    cat("Completed checking registry entry", toString(register[i, "Certificate"]), "\n")
  }
}
