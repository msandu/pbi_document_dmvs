library(RDCOMClient)
library(dplyr)
library(magrittr)
library(data.table)
library(jsonlite)

get_value <- function(x) {
  get_v1 <- function (x) {
    result <- x[[1]] # kinda retarded, but works
    return(result) 
  }
  result <- sapply(x, get_v1)
  return(result)
}

rowset_to_DT <- function(x, headers) {
  result <- x %>%
    sapply(get_value) %>%
    as.data.table() %>%
    t() %>%
    as.data.table() %>%
    mutate(across(where(is.list), as.character))
  
  colnames(result) <- headers
  return(result)
}

build_query <- function(view, headers) {
  result <- paste(
    'select',
    paste0('[', headers, ']', collapse = ","),
    paste0('from $system.', view)
  )
  return(result)
}

get_dmv <- function(dmv) {
  
  expected_headers <- dmv_queries[dmv_queries$view == dmv,]$headers[[1]]
  
  result <- con$Execute(
    build_query(dmv, expected_headers)
  )$GetRows() %>%
    rowset_to_DT(headers = expected_headers)
  return(result)
  
}

get_sample <- function(table_name, table_id, columns) {
  expected_headers <- columns[columns$TableID == table_id & columns$IsHidden == 'FALSE', ExplicitName]
  dax_query <- paste0("EVALUATE SAMPLE (5, '",table_name,"', 1)")
  result <- con$Execute(
    dax_query
  )$GetRows() %>%
    rowset_to_DT(headers = expected_headers)
  return(result)
}