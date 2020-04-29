#' @import magrittr
NULL

############################## GENERIC CLASSIFICATION TOOLS ###############################

remove_na <- function(column){
  column <- column[!is.na(column)]
  return(column)
}

#' Extract unique values without NAs
#'
#' Uses unique and NA removal to retrieve
#' a vector of unique entries in a column.
#' This is useful when mining an API, trying
#' to minimize the number of calls.
#' @param column a vector from which unique values need to be extracted
#' @return a vector of unique values
#' @export
extract_uniques <- function(column){
  all_entries <- column %>% unique() %>% remove_na()
  return(all_entries)
}

#' Save dataframes in data/clean
#'
#' Save three kinds of clean data: results from APIs and total data after classification.
#' @param df data frame to save
#' @param which_info either 'doaj' for DOAJ mining results, 'upw', for Unpaywall, or 'all', for all data
save_df <- function(df, which_info){
  if (!file.exists(here::here("data/clean"))){
    dir.create(here::here("data/clean"))
  }
  # remove list columns so the data can be saved
  df <- df %>% dplyr::select_if(is.atomic)
  basename <- dplyr::case_when(
    which_info == "doaj" ~ "doaj_from_issn_",
    which_info == "upw" ~ "upw_from_doi_",
    which_info == "all" ~ "complete_dataframe_",
    TRUE ~ "unknown_info_")
  filename <- paste0("data/clean/", basename, lubridate::today(), ".csv")
  readr::write_csv(df, filename)
}


####################################### API MINING ######################################

#' Get DOAJ data from indicated source
#'
#' This function either kickstarts mining the DOAJ API using the
#' issn columnn in a provided data frame, or loads saved DOAJ data.
#'
#' @param df data frame with an ISSN column
#' @param source either "api" or the path of a saved DOAJ result
#' @return data frame with DOAJ results
#' @export
doaj_pipeline <- function(df, source){
  if(source=="api"){
  df <- df %>%
    api_to_df("doaj") %>%
    process_doaj()
  save_df(df, "doaj")
  } else if(file.exists(source)){
    #TODO add a check if the dataframe provided matches the saved results
    #TODO rename df, this is confusing
    df <- readr::read_csv(source)
  } else{
    warning("Not sure what DOAJ data to use. Please either use source='api' for use of the DOAJ API,
or provide the path of saved data that was previously mined from the DOAJ API.")
    stop
  }
  return(df)
}

#' Get Unpaywall data from indicated source
#'
#' This function either kickstarts mining the Unpaywall API using the
#' doi columnn in a provided data frame, or loads saved Unpaywall data.
#'
#' @param df data frame with a doi column
#' @param source either "api" or the path of a saved unpaywall result
#' @param email email address of user (required by Unpaywall)
#' @return data frame with unpaywall results
#' @export
upw_pipeline <- function(df, source="api", email){
  if(source=="api"){
  df <- df %>%
    api_to_df("upw", email)
  save_df(df, "upw")
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

#' Process DOAJ API result
#'
#' Specifically used to process the data frame that results from a doaj mining query.
#' ISSN numbers are in a nested format, they need to be un-nested.
#' For the ease of future processing, the ISSN columns are renamed.
#'
#' @param df the data frame resulting from a DOAJ API mining
#' @return the cleaned up data frame
process_doaj <- function(df){
  df <- df %>%
    # unnest the issn information in the bibjson.identifier column
    tidyr::unnest(bibjson.identifier, keep_empty = T, names_sep="_") %>%
    rename(issn = bibjson.identifier_id,
           issn_type = bibjson.identifier_type) %>%
    dplyr::filter(lubridate::year(created_date) <= report_year)
  return(df)
}

#' Mining the DOAJ API
#'
#' This function uses an issn to mine the
#' DOAJ API (at doaj.org/api/v1/).
#' The entry for this ISSN in the DOAJ is returned.
#'
#' @param issn ISSN for journal that needs to be checked
#' @return list with DOAJ results
#' @export
doaj_api <- function(issn){
  Sys.sleep(0.6) # requests for this api are limited at 2 per second, so the request is slowed down.
  api <- "https://doaj.org/api/v1/search/journals/issn:"
  query <- paste0(api,issn)
  result <- httr::GET(query) %>%
    httr::content(as="text",encoding="UTF-8")
  result_line <- jsonlite::fromJSON(result,flatten=T)$results
  return(result_line)
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
  result_line <- jsonlite::fromJSON(result_txt,flatten=T)$results
  return(result_line)
}


#' Mine an API and save the results in a data frame
#'
#' Using an API mining  function, query all unique
#' entries in a column, and return a data frame
#' with their results.
#'
#' @param df Source data frame that contains DOI and/or ISSN columns
#' @param which_info What api will be mined? Either "doaj" or "upw" (unpaywall).
#' @param email email address of user (required for the Unpaywall API; otherwise can be left empty)
#' @return a data frame
api_to_df <- function(df, which_info, email = ""){
  # extract unique values before mining the api
  if(which_info == "doaj"){
    all_entries <- extract_uniques(c(df$issn,df$eissn))
  }else if(which_info == "upw"){
    all_entries <- extract_uniques(df$doi)
  }

  collect <- list()

  cat(paste("Mining the", which_info, "api on",length(all_entries),"items.\n"))
  cat(paste("This will take around",round(length(all_entries)/90,0),"minutes."))
  cat(paste0(" (Current time is ",lubridate::now(),".)\n"))
  for(i in seq_along(all_entries)){
    entry <- all_entries[i]
    if(which_info == "doaj"){
      collect[[i]] <- doaj_api(entry)
    } else if(which_info == "upw"){
      tryCatch({
        collect[[i]] <- upw_api(entry, email=email)
      }, error = function(e){
        cat(paste0("There is a problem with DOI ",entry,". Pausing briefly, then trying again.\n"))
        Sys.sleep(2)
        collect[[i]] <- upw_api(entry, email=email)
      })
    }
  }
  collectdf <- dplyr::bind_rows(collect)
  cat(paste0("Done mining the ",which_info," api at ",lubridate::now(),".\n"))
  return(collectdf)
}


################################# SOURCE PROCESSING ##########################################

#' Read and clean VSNU data
#'
#' @param path path of VSNU data
#' @return data frame with VSNU data
#' @export
get_vsnu <- function(path){
  # get all VSNU DOIs
  vsnu <- read_ext(path, "")
  vsnu <- vsnu %>% mutate(doi = clean_doi(DOI))
  return(vsnu)
}


