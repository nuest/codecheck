#' Function for adding repository links in the register table for the creation of the json file.
#' 
#' @param register_table The register table
#' @return Register table with adjusted repository links
add_repository_links_json <- function(register_table) {
  register_table$`Repository Link` <- sapply(
    X = register_table$Repository,
    FUN = function(repository) {
      # ! Needs refactoring
      spec <- parse_repository_spec(repository)
      if (spec[["type"]] == "github") {
        paste0(CONFIG$HYPERLINKS[["github"]], spec[["repo"]])
      } else if (spec[["type"]] == "osf") {
        paste0(CONFIG$HYPERLINKS[["osf"]], spec[["repo"]])
      } else if (spec[["type"]] == "gitlab") {
        paste0(CONFIG$HYPERLINKS[["gitlab"]], spec[["repo"]])
      } else if (spec[["type"]] == "zenodo") {
        paste0(CONFIG$HYPERLINKS[["zenodo"]], spec[["repo"]])
      } else {
        repository
      }
    }
  )
  return(register_table)
}

#' Set "Title" and "Paper reference" columns and values to the register_table
#' 
#' @param register_table The register table
#' @return Updated register table including "Title" and "Paper reference" columns
set_paper_title_references <- function(register_table){
  titles <- c()
  references <- c()
  for (i in seq_len(nrow(register_table))) {
    config_yml <- get_codecheck_yml(register_table[i, ]$Repository)

    title <- NA
    reference <- NA
    if (!is.null(config_yml)) {
      title <- config_yml$paper$title
      reference <- config_yml$paper$reference
    }

    titles <- c(titles, title)
    references <- c(references, reference)
  }
  register_table$Title <- stringr::str_trim(titles)
  register_table$`Paper reference` <- stringr::str_trim(references)

  return(register_table)
}

#' Add certificate PDF download URLs to the register table
#'
#' Resolves report DOIs to direct PDF download URLs using get_cert_link().
#'
#' @param register_table The register table (must have "Report" and "Certificate ID" columns)
#' @return Register table with added "Certificate PDF" column
add_cert_pdf_links <- function(register_table) {
  register_table$`Certificate PDF` <- sapply(
    seq_len(nrow(register_table)),
    function(i) {
      report <- register_table[i, "Report"]
      cert_id <- register_table[i, "Certificate ID"]
      if (is.na(report) || is.null(report) || nchar(report) == 0) {
        return(NA_character_)
      }
      tryCatch(
        {
          link <- get_cert_link(report, cert_id)
          if (is.null(link)) NA_character_ else link
        },
        error = function(e) {
          warning(cert_id, " | Could not resolve certificate PDF URL: ", e$message)
          NA_character_
        }
      )
    }
  )
  return(register_table)
}

#' Detect the publication platform from a report URL
#'
#' Extracts a human-readable platform name from the report URL's domain.
#' For doi.org URLs, resolves the DOI to determine the actual hosting platform.
#'
#' @param url A report URL string
#' @return A lowercase platform name (e.g., "zenodo", "osf", "researchequals")
detect_report_platform <- function(url) {
  # Known patterns (checked before any HTTP request)
  if (grepl("zenodo", url, ignore.case = TRUE)) return("zenodo")
  if (grepl("osf\\.io", url, ignore.case = TRUE)) return("osf")
  if (grepl("researchequals", url, ignore.case = TRUE)) return("researchequals")

  # For doi.org URLs, follow the redirect to find the actual platform
  if (grepl("doi\\.org/", url, ignore.case = TRUE)) {
    resolved <- tryCatch({
      resp <- codecheck_GET(url, httr::config(followlocation = FALSE))
      loc <- httr::headers(resp)[["location"]]
      if (!is.null(loc)) loc else url
    }, error = function(e) url)
    # Re-check known patterns on the resolved URL
    if (grepl("zenodo", resolved, ignore.case = TRUE)) return("zenodo")
    if (grepl("osf\\.io", resolved, ignore.case = TRUE)) return("osf")
    if (grepl("researchequals", resolved, ignore.case = TRUE)) return("researchequals")
    # Extract domain name as platform
    domain <- gsub("^https?://([^/]+).*", "\\1", resolved)
    domain <- gsub("^www\\.", "", domain)
    return(domain)
  }

  # Fallback: extract domain name
  domain <- gsub("^https?://([^/]+).*", "\\1", url)
  domain <- gsub("^www\\.", "", domain)
  return(domain)
}

#' Compute annual statistics from the register table
#'
#' Computes per-year breakdowns of checks, venues, codecheckers, and report
#' platforms. Used to enrich stats.json (addresses register#144).
#'
#' @param register_table The full preprocessed register table (before column filtering)
#' @return A list of annual statistics
compute_annual_stats <- function(register_table) {
  stats <- list()

  if (!("Check date" %in% names(register_table))) {
    return(stats)
  }

  dates <- as.Date(register_table$`Check date`)
  years <- format(dates, "%Y")
  valid <- !is.na(years)
  all_years <- sort(unique(years[valid]))

  # --- Total venue count and per-type breakdown (addresses register#76) ---
  if ("Venue" %in% names(register_table)) {
    stats$venue_count <- length(unique(register_table$Venue[valid]))
    if ("Type" %in% names(register_table)) {
      type_vec <- register_table$Type[valid]
      venue_vec <- register_table$Venue[valid]
      # Count unique venues per type
      type_venue_df <- data.frame(type = type_vec, venue = venue_vec, stringsAsFactors = FALSE)
      venues_per_type <- tapply(type_venue_df$venue, type_venue_df$type, function(v) length(unique(v)))
      stats$venues_per_type <- as.list(venues_per_type[order(names(venues_per_type))])
    }
  }

  # --- Checks per year (annual + cumulative) ---
  checks_tbl <- table(years[valid])
  checks_sorted <- checks_tbl[all_years]
  stats$checks_per_year <- as.list(checks_sorted)

  cumulative_checks <- cumsum(as.integer(checks_sorted))
  names(cumulative_checks) <- all_years
  stats$checks_cumulative <- as.list(cumulative_checks)

  # --- Unique venues per year (annual + cumulative) ---
  if ("Venue" %in% names(register_table)) {
    venue_vec <- register_table$Venue[valid]
    yr_vec <- years[valid]

    venues_by_year <- tapply(venue_vec, yr_vec, function(v) length(unique(v)))
    stats$venues_per_year <- as.list(venues_by_year[all_years])

    # Cumulative: count unique venues seen up to and including each year
    venues_cumulative <- setNames(integer(length(all_years)), all_years)
    seen_venues <- character(0)
    for (y in all_years) {
      seen_venues <- unique(c(seen_venues, venue_vec[yr_vec == y]))
      venues_cumulative[y] <- length(seen_venues)
    }
    stats$venues_cumulative <- as.list(venues_cumulative)
  }

  # --- Unique codecheckers per year (annual + cumulative, list column) ---
  if ("Codechecker" %in% names(register_table)) {
    checkers <- register_table$Codechecker[valid]
    yr <- years[valid]
    pairs <- do.call(rbind, lapply(seq_along(checkers), function(i) {
      cc <- checkers[[i]]
      if (is.null(cc) || all(is.na(cc))) return(NULL)
      data.frame(year = yr[i], codechecker = cc, stringsAsFactors = FALSE)
    }))
    if (!is.null(pairs) && nrow(pairs) > 0) {
      codecheckers_by_year <- tapply(pairs$codechecker, pairs$year, function(cc) length(unique(cc)))
      stats$codecheckers_per_year <- as.list(codecheckers_by_year[all_years[all_years %in% names(codecheckers_by_year)]])

      # Cumulative unique codecheckers
      cc_years <- sort(unique(pairs$year))
      cc_cumulative <- setNames(integer(length(cc_years)), cc_years)
      seen_cc <- character(0)
      for (y in cc_years) {
        seen_cc <- unique(c(seen_cc, pairs$codechecker[pairs$year == y]))
        cc_cumulative[y] <- length(seen_cc)
      }
      stats$codecheckers_cumulative <- as.list(cc_cumulative)
    }
  }

  # --- Checks per platform per year (annual + cumulative) ---
  if ("Report" %in% names(register_table)) {
    report_vec <- register_table$Report[valid]
    yr_vec <- years[valid]
    has_report <- !is.na(report_vec) & nchar(report_vec) > 0

    platforms <- sapply(report_vec[has_report], function(url) {
      detect_report_platform(url)
    }, USE.NAMES = FALSE)
    plat_years <- yr_vec[has_report]

    # Per-year breakdown by platform
    plat_df <- data.frame(year = plat_years, platform = platforms, stringsAsFactors = FALSE)
    platform_by_year <- list()
    for (y in all_years) {
      sub <- plat_df$platform[plat_df$year == y]
      if (length(sub) > 0) {
        platform_by_year[[y]] <- as.list(table(sub)[order(names(table(sub)))])
      }
    }
    stats$platform_per_year <- platform_by_year

    # Cumulative by platform
    all_plats <- sort(unique(platforms))
    platform_cumulative <- list()
    running <- setNames(rep(0L, length(all_plats)), all_plats)
    for (y in all_years) {
      sub <- plat_df$platform[plat_df$year == y]
      if (length(sub) > 0) {
        yr_tbl <- table(sub)
        for (p in names(yr_tbl)) {
          running[p] <- running[p] + as.integer(yr_tbl[p])
        }
      }
      platform_cumulative[[y]] <- as.list(running)
    }
    stats$platform_cumulative <- platform_cumulative
  }

  return(stats)
}

#' Renders register json for a single register_table
#'
#' @param register_table The register table
#' @param table_details List containing details such as the table name, subcat name.
#' @param filter The filter
#' @param full_register_table Optional full preprocessed register table for computing
#'   annual statistics (only used for the main register stats.json)
render_register_json <- function(register_table, table_details, filter, full_register_table = NULL) {
  register_table_json <- add_repository_links_json(register_table)

  # Set paper titles and references
  register_table_json <- set_paper_title_references(register_table_json)

  # Add certificate PDF download URLs (resolves report DOIs, addresses register#87)
  register_table_json <- add_cert_pdf_links(register_table_json)

  output_dir <- table_details[["output_dir"]]

  # Keeping only those columns that are mentioned in the json columns and those that
  # register table already has
  columns_to_keep <- intersect(CONFIG$JSON_COLUMNS, names(register_table_json))

  # Main register.json: sorted by certificate identifier (done in render_register())
  jsonlite::write_json(
    register_table_json[, columns_to_keep],
    path = file.path(output_dir, "register.json"),
    pretty = TRUE,
    na = "null"
  )

  # featured.json: sorted by check date (most recent first)
  # (addresses codecheckers/register#160)
  featured_table <- register_table_json
  if ("Check date" %in% names(featured_table)) {
    featured_table <- featured_table %>% arrange(desc(`Check date`))
  }
  jsonlite::write_json(
    utils::head(featured_table, CONFIG$FEATURED_COUNT)[, columns_to_keep],
    path = file.path(output_dir, "featured.json"),
    pretty = TRUE,
    na = "null"
  )

  stats_data <- list(
    source = generate_href(filter, table_details, "json"),
    cert_count = nrow(register_table_json)
  )

  # Add annual statistics for the main register (addresses register#144)
  if (!is.null(full_register_table)) {
    annual <- compute_annual_stats(full_register_table)
    stats_data <- c(stats_data, annual)
  }

  jsonlite::write_json(
    stats_data,
    auto_unbox = TRUE,
    path = file.path(output_dir, "stats.json"),
    pretty = TRUE
  )
}
