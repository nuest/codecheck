# Look up a work's publication date on OpenAlex

Look up a work's publication date on OpenAlex

## Usage

``` r
get_openalex_publication_date_result(openalex_id)
```

## Arguments

- openalex_id:

  An OpenAlex work URL or ID, e.g. "https://openalex.org/W3014157798"

## Value

A list with \`status\` ("found", "absent" or "failed") and \`value\`,
the publication date as a character string or \`NA_character\_\`
