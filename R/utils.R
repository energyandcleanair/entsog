# Inspired by https://github.com/krose/entsoeapi
api_req <- function(url, params=list(), limit=-1){

  params['limit'] <- limit
  message(url, " ...")
  req <- httr::GET(url, query=params)

  if(httr::status_code(req) != "200"){
    print(sprintf("Failed (Code %s) for: ", httr::status_code(req)))
    print(url)
    print(params)
    warning(httr::content(req, encoding = "UTF-8"))
    return(NULL)
  }

  en_cont <- httr::content(req, as="text", encoding = "UTF-8")
  res <- jsonlite::fromJSON(en_cont)

  if(res$meta$total > res$meta$count * 1.01){ #+2: for some reason, sometimes total is +1 or +2
    warning(sprintf("More data available (%s/%s). Increase limit or implement a loop here...", res$meta$count, res$meta$total))
  }

  return(res)
}

api_req_safe <- function(...) {
  purrr:::capture_error(api_req(...), otherwise, quiet)
}
