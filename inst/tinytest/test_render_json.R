tinytest::using(ttdo)

test_path <- "register/short.csv"
test_register <- read.csv(test_path)
venues_path <- "register/venues.csv"

capture.output(
  suppressWarnings(
    table <- register_render(register = test_register, filter_by = c(), outputs = c("json"),
                            venues_file = venues_path)
  ),
  type = "message"
)

# file generation ----
expect_true(file.exists(file.path("docs/register.json")))
expect_true(file.exists(file.path("docs/featured.json")))
expect_true(file.exists(file.path("docs/stats.json")))

# stats ----
stats <- jsonlite::read_json("docs/stats.json")
expect_equal(stats$cert_count, nrow(test_register))
expect_equal(stats$source, paste0(CONFIG$HREF_DETAILS$json$base_url, "register.json"))

# venue counts (addresses register#76) ----
expect_true(!is.null(stats$venue_count), info = "venue_count present")
expect_true(stats$venue_count > 0, info = "venue_count is positive")
expect_true(!is.null(stats$venues_per_type), info = "venues_per_type present")

# annual stats (addresses register#144) ----
expect_true(!is.null(stats$checks_per_year), info = "checks_per_year present")
expect_true(is.list(stats$checks_per_year), info = "checks_per_year is a named list")
expect_equal(sum(unlist(stats$checks_per_year)), stats$cert_count,
             info = "checks_per_year values sum to cert_count")
expect_true(all(grepl("^\\d{4}$", names(stats$checks_per_year))),
            info = "checks_per_year keys are years")
expect_true(!is.null(stats$venues_per_year), info = "venues_per_year present")
expect_true(!is.null(stats$platform_per_year), info = "platform_per_year present")

# cumulative stats ----
expect_true(!is.null(stats$checks_cumulative), info = "checks_cumulative present")
# Last cumulative value should equal total cert count
cumulative_checks <- unlist(stats$checks_cumulative)
expect_equal(unname(cumulative_checks[length(cumulative_checks)]), stats$cert_count,
             info = "last checks_cumulative equals cert_count")
# Cumulative values should be non-decreasing
expect_true(all(diff(cumulative_checks) >= 0), info = "checks_cumulative is non-decreasing")

expect_true(!is.null(stats$venues_cumulative), info = "venues_cumulative present")
cumulative_venues <- unlist(stats$venues_cumulative)
expect_true(all(diff(cumulative_venues) >= 0), info = "venues_cumulative is non-decreasing")

expect_true(!is.null(stats$platform_cumulative), info = "platform_cumulative present")

# featured ----
featured <- jsonlite::read_json("docs/featured.json")
expect_true(length(featured) < CONFIG$FEATURED_COUNT)
expect_equal(stats$cert_count, length(featured))
expect_equal(names(featured[[1]]), CONFIG$JSON_COLUMNS)
# Featured certificates should be sorted (possibly by date), so sort both before comparison
expect_equal(sort(sapply(featured, "[[", "Certificate ID")), sort(test_register$Certificate))

# register ----
register <- jsonlite::read_json("docs/register.json")
expect_equal(length(register), length(featured))
expect_equal(names(register[[1]]), CONFIG$JSON_COLUMNS)
expect_equal(sapply(register, "[[", "Certificate ID"), test_register$Certificate)

# register-full (addresses register#57) ----
expect_true(file.exists("docs/register-full.json"), info = "register-full.json exists")
expect_true(file.exists("docs/register-full.csv"), info = "register-full.csv exists")

full_json <- jsonlite::fromJSON("docs/register-full.json")
expect_equal(nrow(full_json), nrow(test_register), info = "register-full.json has all entries")
# Should be sorted by Certificate ID
expect_equal(full_json$`Certificate ID`, sort(full_json$`Certificate ID`),
             info = "register-full.json sorted by Certificate ID")
# Should have extended columns beyond the regular register
expected_cols <- c("Certificate ID", "Repository", "Repository Link", "Type", "Venue",
                   "Check date", "Report", "Title", "Paper reference",
                   "Paper authors", "Paper author ORCIDs",
                   "Codecheckers", "Codechecker ORCIDs", "Summary", "Source")
expect_true(all(expected_cols %in% names(full_json)),
            info = "register-full.json has all expected columns")

full_csv <- read.csv("docs/register-full.csv", as.is = TRUE)
expect_equal(nrow(full_csv), nrow(test_register), info = "register-full.csv has all entries")

# clean up
expect_equal(unlink("docs", recursive = TRUE), 0)
