#' @import magrittr
NULL


####################################### API MINING ######################################

#' Get Unpaywall data from indicated source
#'
#' This function either kickstarts mining the Unpaywall API using the
#' doi columnn in a provided data frame, or loads saved Unpaywall data.
#'
#' @param df data frame with a doi column
#' @param source either "api" or the path of a saved unpaywall result
#' @param email email address of user (required by Unpaywall)
#' @param save_results Do you want to save the resulting data frame?
#' @return data frame with unpaywall results
#' @export
get_upw <- function(df, source="api", email="", save_results=F){
  if(source=="api"){
    if(email == ""){
      warning("Provide a valid email address to mine the Unpaywall API.")
    }
  df <- df %>%
    api_to_df("upw", email)
  if(save_results == T){
    save_df(df, "upw")
  }
  } else if(file.exists(source)){
    #TODO add a check if the dataframe provided matches the saved results
    #TODO rename df, this is confusing
    df <- readr::read_csv(source)
  } else{
    warning("Not sure what Unpaywall data to use. Please either use source='api' for use of the Unpaywall API,
or provide the path of saved data that was previously mined from the Unpaywall API.")
    stop
  }
  df <- df %>% dplyr::filter(!duplicated(doi)) # just in case any dois are inadvertently duplicated
  return(df)
}


#' Mining the Unpaywall API
#'
#' Collecting DOI results from Unpaywall using their REST API
#' (at http://api.unpaywall.org). The entry for the DOI in the
#' Unpaywall database is returned.
#'
#' @param doi DOI of paper that needs to be checked
#' @param email email address of user (required by Unpaywall)
#' @return list with Unpaywall results
#' @export
upw_api <- function(doi,email){
  # compile query to send to unpaywall
  api <- "http://api.unpaywall.org/"
  email <- paste0("?email=",email)
  query <- paste0(api,doi,email)
  result <- httr::GET(query)
  # resolve query results and transform to a line that can be added to a df
  result_txt <- httr::content(result, as="text",encoding="UTF-8")
  result_line <- jsonlite::fromJSON(result_txt, flatten=T)$results
  #result_parsed <- httr::content(result, as="parsed",encoding="UTF-8")
  return(result_line)
}

#IN PROGRESS
upw_api_v2 <- function(doi,email){
  # compile query to send to unpaywall
  api <- "https://api.unpaywall.org/v2/"
  email <- paste0("?email=",email)
  query <- paste0(api,doi,email)
  result2 <- httr::GET(query)
  # resolve query results and transform to a line that can be added to a df
  result_txt2 <- httr::content(result2, as="text",encoding="UTF-8")
  #output of API v2 is single level. FROMJSON does not transform into df. WHYYYY
  result_line2 <- jsonlite::fromJSON(result_txt2)

  #alternative approach
  result_parsed2 <- httr::content(result2, as="parsed",encoding="UTF-8")
  result_df <- enframe(result_parsed2) %>%
    tidyr::pivot_wider(names_from = name, values_from = value)
  #PROBLEM: columns are still lists - would need to convert of find other approach


  #add column 'oa_color' as column to extract from upw data later
  #'oa_color' = 'oa_status' unless oa_status is bronze, and green version is available
  result_line2 <- result_line %>%
    dplyr::mutate(oa_color = case_when(
    (oa_status == "bronze" & has_repository_copy == TRUE) ~ "green",
    TRUE ~ oa_status))

    return(result_line)
}

email = "bianca.kramer@gmail.com"
doi = "10.1038/nature12373"


