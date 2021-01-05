library(RDCOMClient)
library(dplyr)
library(magrittr)
library(data.table)
library(jsonlite)

get_value <- function (x) {
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

get_sample <- function(table_name) {
  
  table_id <- tables[tables$Name == table_name, ID]
  
  expected_headers <- columns[
    columns$TableID == table_id 
    & columns$IsHidden == 'FALSE', c('ExplicitName', 'Conversion')]
  
  expected_headers$Conversion <- as.character(expected_headers$Conversion)
  
  headers_to_format <- expected_headers[expected_headers$Conversion != 'NULL']
  
  dax_query <- paste0("EVALUATE SAMPLE (5, '",table_name,"', 1)")
  result <- con$Execute(
    dax_query
  )$GetRows() %>%
    rowset_to_DT(headers = expected_headers$ExplicitName)
  
  # TODO: fix this convert deconvert crap
  result <- as.data.frame(result)
  
  for (header_to_format in headers_to_format$ExplicitName) {
    
    format_string <- as.character( headers_to_format[headers_to_format$ExplicitName == header_to_format, Conversion])
    
    for (i in 1:nrow(result)) {
      
      value_to_convert <- result[i, header_to_format]
      result[i, header_to_format] <- convert_datetime(value_to_convert, format = format_string)
      
    }
    
  }
  
  result <- as.data.table(result)

  return(result)
  
}

convert_excel_date <- function(excel_date) {
  return(
    as.POSIXlt((excel_date * 86400), origin='1899-12-30', tz='GMT')
  )
}

convert_datetime <- function(excel_date, format = '') {
  return(
    format( convert_excel_date( as.numeric(excel_date) ),  format = format )
  )
}

convert_dax_format <- function(string) {
  # Absolutely disgusting
  initial_string <- string
  string <- gsub('\\\\', '', string)
  string <- gsub(' AM/PM', '', string)
  string <- gsub('dddd', '%A', string)
  string <- gsub('mmmm', '%B', string)
  string <- gsub('yyyy', '%Y', string)
  string <- gsub('ddd', '%a', string)
  string <- gsub('mmm', '%b', string)
  string <- gsub('dd', '%d', string)
  string <- gsub('mm', '%m', string)
  string <- gsub('yy', '%y', string)
  string <- gsub('d', '%d', string)
  string <- gsub('hh', '%H', string)
  string <- gsub('h', '%H', string)
  string <- gsub('nn', '%M', string)
  string <- gsub('ss', '%S', string)
  string <- gsub('General Date', '%Y-%m-%d %H:%M:%S', string)
  
  if (string == initial_string) {
    return(NULL)
  } else {
    return(string)
  }
}

DT_to_HTML <- function(DT, table_id, display) {
  
  headers <- colnames(DT)
  
  template <- readr::read_file('sample_tables_template/template.htm')
  th <- strsplit(
    strsplit(
      strsplit(template, "}}")[[1]][1], "\\{\\{")[[1]][2], "@")[[1]]
  tr <- strsplit(
    strsplit(template, "]]")[[1]][1], "\\[\\[")[[1]][2]
  td <- strsplit(
    strsplit(
      strsplit(template, "))")[[1]][1], "\\(\\(")[[1]][2], "@")[[1]]
  
  th_filled <- paste(th[1], headers, th[2], collapse = '')
  
  tr_filled <- ''
  
  for (i in 1:nrow(DT)) {
    tds_filled <- paste(td[1], DT[i], td[2], collapse = '')
    local_tr_filled <- gsub(
      paste0("\\(\\(", paste0(td, collapse = "@") , "))"), 
      tds_filled, 
      tr)
    tr_filled <- paste0(tr_filled, local_tr_filled)
  }
  
  table_start <- gsub(
    "\\?display\\?",
    display,
    gsub(
      "\\?id\\?", 
      table_id, 
      strsplit(template, "\\{\\{")[[1]][1]))
  table_end <- strsplit(strsplit(template, "\\{\\{")[[1]][2], "]]")[[1]][2]
  
  table_html <- paste0(
    table_start,
    th_filled,
    "\r\n</tr>\r\n</thead>\r\n<tbody>\r\n",
    tr_filled,
    table_end
  )
  return(table_html)
}