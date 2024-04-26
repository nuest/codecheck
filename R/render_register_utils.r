#' Function for rendering the register markdown file.
#' 
#' @param register_table The register table
#' @param md_columns_widths The column widths for the markdown file
#' @return None

render_register_md <- function(register_table, md_columns_widths) {
  capture.output(
    cat("---\ntitle: CODECHECK Register\n---"),
    knitr::kable(register_table, format = "markdown"),
    file = "register.md"
  )
  # hack to reduce column width of 4th column
  md_table <- readLines("register.md")
  md_table[6] <- md_columns_widths
  writeLines(md_table, "docs/register.md")
  file.remove("register.md")
  # TODO: fix table column width, e.g. via using a register.Rm
}

#' Function for rendering the register html file. 
#' The html file is rendered from the markdown file.
#' 
#' @param register_table The register table
#' @param md_columns_widths The column widths for the markdown file
#' @return None

render_register_html <- function(register_table, register, md_columns_widths) {
  # add icons to the Repository column for HTML output, use a copy of the register.md
  # so the inline HTML is not in the .md output
  register_table$Repository <- sapply(
    X = register$Repository,
    FUN = function(repository) {
      spec <- parse_repository_spec(repository)

      if (!any(is.na(spec))) {
        urrl <- "#"

        if (spec[["type"]] == "github") {
          urrl <- paste0("https://github.com/", spec[["repo"]])
          paste0("<i class='fa fa-github'></i>&nbsp;[", spec[["repo"]], "](", urrl, ")")
        } else if (spec[["type"]] == "osf") {
          urrl <- paste0("https://osf.io/", spec[["repo"]])
          paste0("<i class='ai ai-osf'></i>&nbsp;[", spec[["repo"]], "](", urrl, ")")
        } else if (spec[["type"]] == "gitlab") {
          urrl <- paste0("https://gitlab.com/", spec[["repo"]])
          paste0("<i class='fa fa-gitlab'></i>&nbsp;[", spec[["repo"]], "](", urrl, ")")
        } else {
          repository
        }
      } else {
        repository
      }
    }
  )
  capture.output(
    cat("---\ntitle: CODECHECK Register\n---"),
    knitr::kable(register_table, format = "markdown"),
    file = "docs/register-icons.md"
  )
  md_table <- readLines("docs/register-icons.md")
  file.remove("docs/register-icons.md")
  md_table[6] <- md_columns_widths
  writeLines(md_table, "docs/register-icons.md")

  rmarkdown::render(
    input = "docs/register-icons.md",
    # next paths are relative to input file
    output_yaml = "html_document.yml",
    output_file = "index.html"
  )
  file.remove("docs/register-icons.md")
}

#' Function for rendering the register json file. 
#' 
#' @param register_table The register table
#' @param register The register from the register.csv file
#' @return None

render_register_json <- function(register_table, register) {
  # Get paper titles and references
  titles <- c()
  references <- c()

  for (i in seq_len(nrow(register))) {
    config_yml <- get_codecheck_yml(register[i, ]$Repo)

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
  register_table$`Repository Link` <- sapply(
    X = register$Repository,
    FUN = function(repository) {
      spec <- parse_repository_spec(repository)
      if (spec[["type"]] == "github") {
        paste0("https://github.com/", spec[["repo"]])
      } else if (spec[["type"]] == "osf") {
        paste0("https://osf.io/", spec[["repo"]])
      } else if (spec[["type"]] == "gitlab") {
        paste0("https://gitlab.com/", spec[["repo"]])
      } else {
        repository
      }
    }
  )

  jsonlite::write_json(
    register_table[, c(
      "Certificate",
      "Repository Link",
      "Type",
      "Report",
      "Title",
      "Paper reference",
      "Check date"
    )],
    path = "docs/register.json",
    pretty = TRUE
  )

  jsonlite::write_json(
    utils::tail(register_table, 10)[, c(
      "Certificate",
      "Repository Link",
      "Type",
      "Report",
      "Title",
      "Paper reference",
      "Check date"
    )],
    path = "docs/featured.json",
    pretty = TRUE
  )

  jsonlite::write_json(
    list(
      source = "https://codecheck.org.uk/register/register.json",
      cert_count = nrow(register_table)
      # TODO count conferences, preprints,
      # journals, etc.
    ),
    auto_unbox = TRUE,
    path = "docs/stats.json",
    pretty = TRUE
  )
}
