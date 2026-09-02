# One Action API request, repeated while the server asks us to come back

The single place both reads and writes go through, so the User-Agent,
the retry rule and the JSON parsing are decided once.

## Usage

``` r
wikibase_request(method, params, handle = NULL, what = "the API", attempts = 4)
```

## Arguments

- method:

  \`httr::GET\` or \`httr::POST\`

- params:

  the request parameters

- handle:

  an \`httr\` handle, or \`NULL\` for an anonymous request

- what:

  what is being requested, for messages

- attempts:

  how many times to try in total

## Value

the parsed response
