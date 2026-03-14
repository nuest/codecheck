# Test register_render_cert() - single certificate rendering

tinytest::using(ttdo)

test_path <- "register/short.csv"
test_register <- read.csv(test_path)
venues_path <- "register/venues.csv"

# --- Test 1: renders a single certificate HTML and JSON ---
register_render_cert(
  cert_id = "2024-017",
  register = test_register,
  config = c(system.file("extdata", "config.R", package = "codecheck"),
             "config/render_html.R"),
  venues_file = venues_path,
  download_and_convert = FALSE
)

expect_true(
  file.exists("docs/certs/2024-017/index.html"),
  info = "Certificate HTML page should be created"
)
expect_true(
  file.exists("docs/certs/2024-017/index.json"),
  info = "Certificate JSON metadata should be created"
)

# --- Test 2: only the targeted certificate is rendered, not others ---
expect_false(
  file.exists("docs/certs/2021-010/index.html"),
  info = "Other certificates should not be rendered"
)
expect_false(
  file.exists("docs/certs/2022-018/index.html"),
  info = "Other certificates should not be rendered"
)

# --- Test 3: no index pages are created ---
expect_false(
  file.exists("docs/index.html"),
  info = "Main index page should not be created"
)

# --- Test 4: no stray temporary files left behind ---
cert_dir <- "docs/certs/2024-017"
expect_false(
  file.exists(file.path(cert_dir, "index_header.html")),
  info = "Temporary index_header.html should be cleaned up"
)
expect_false(
  file.exists(file.path(cert_dir, "index_prefix.html")),
  info = "Temporary index_prefix.html should be cleaned up"
)
expect_false(
  file.exists(file.path(cert_dir, "index_postfix.html")),
  info = "Temporary index_postfix.html should be cleaned up"
)
expect_false(
  file.exists(file.path(cert_dir, "html_document.yml")),
  info = "Temporary html_document.yml should be cleaned up"
)
expect_false(
  file.exists(file.path(cert_dir, "temp.md")),
  info = "Temporary temp.md should be cleaned up"
)

# --- Test 5: invalid cert_id raises an error ---
expect_error(
  register_render_cert(
    cert_id = "9999-999",
    register = test_register,
    config = c(system.file("extdata", "config.R", package = "codecheck"),
               "config/render_html.R"),
    venues_file = venues_path,
    download_and_convert = FALSE
  ),
  pattern = "not found in register",
  info = "Non-existent certificate ID should raise an error"
)

# --- Test 6: register can be passed as a file path ---
register_render_cert(
  cert_id = "2021-010",
  register = test_path,
  config = c(system.file("extdata", "config.R", package = "codecheck"),
             "config/render_html.R"),
  venues_file = venues_path,
  download_and_convert = FALSE
)

expect_true(
  file.exists("docs/certs/2021-010/index.html"),
  info = "Should work when register is passed as a file path"
)

# --- Test 7: invalid register file path raises an error ---
expect_error(
  register_render_cert(
    cert_id = "2024-017",
    register = "nonexistent.csv",
    config = c(system.file("extdata", "config.R", package = "codecheck"),
               "config/render_html.R"),
    venues_file = venues_path,
    download_and_convert = FALSE
  ),
  pattern = "not found",
  info = "Non-existent register file should raise an error"
)

# clean up
expect_equal(unlink("docs", recursive = TRUE), 0)
