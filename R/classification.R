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


save_df <- function(df, which_info){
  # remove list columns so the data can be saved
  df <- df %>% select_if(is.atomic)
  basename <- case_when(
    which_info == "doaj" ~ "doaj_from_issn_",
    which_info == "upw" ~ "upw_from_doi_",
    which_info == "all" ~ "complete_dataframe_",
    TRUE ~ "unknown_info_")
  filename <- paste0("data/clean/", basename, lubridate::today(), ".csv")
  write_csv(df, filename)
}


####################################### API MINING ######################################

doaj_pipeline <- function(df){
  if(use_doaj=="api"){
  df <- df %>%
    api_to_df("doaj") %>%
    process_doaj()
  save_df(df, "doaj")
  } else if(use_doaj=="saved"){
    df <- read_csv(path_doaj)
  } else{
    warning("Not sure what DOAJ data to use.
Please indicate this in the config.R file, using the option 'api' for use of the DOAJ API,
or 'saved' to use saved data that was previously mined from the DOAJ API.")
    stop
  }
  return(df)
}

upw_pipeline <- function(df){
  if(use_upw=="api"){
  df <- df %>%
    api_to_df("upw")
  save_df(df, "upw")
  } else if(use_upw=="saved"){
    df <- read_csv(path_upw)
  } else{
    warning("Not sure what Unpaywall data to use.
Please indicate this in the config.R file, using the option 'api' for use of the Unpaywall API,
or 'saved' to use saved data that was previously mined from the Unpaywall API.")
    stop
  }
  df <- df %>% filter(!duplicated(doi)) # just in case any dois are inadvertently duplicated
  return(df)
}

#' This function uses an issn to mine the
#' DOAJ API (at doaj.org/api/v1/).
#' The entry for this ISSN in the DOAJ is returned.
doaj_api <- function(issn){
  Sys.sleep(0.6) # requests for this api are limited at 2 per second, so the request is slowed down.
  api <- "https://doaj.org/api/v1/search/journals/issn:"
  query <- paste0(api,issn)
  result <- GET(query) %>%
    content(as="text",encoding="UTF-8")
  result_line <- fromJSON(result,flatten=T)$results
  return(result_line)
}

#' collecting DOI results from Unpaywall using their REST API
upw_api <- function(doi,email = email_address){
  # compile query to send to unpaywall
  api <- "http://api.unpaywall.org/"
  email <- paste("?email=",email,sep="")
  query <- paste0(api,doi,email)
  result <- GET(query)
  # resolve query results and transform to a line that can be added to a df
  result_txt <- content(result, as="text",encoding="UTF-8")
  result_line <- fromJSON(result_txt,flatten=T)$results
  return(result_line)
}

#' Using an API mining  function, query all unique
#' entries in a column, and return a data frame
#' with their results.
#'
#' @param df
#' @param which_info What api will be mined? Either "doaj" or "upw" (unpaywall).
api_to_df <- function(df, which_info){
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
        collect[[i]] <- upw_api(entry)
      }, error = function(e){
        cat(paste0("There is a problem with DOI ",entry,". Pausing briefly, then trying again.\n"))
        Sys.sleep(2)
        collect[[i]] <- upw_api(entry)
      })
    }
  }
  collectdf <- bind_rows(collect)
  cat(paste0("Done mining the ",which_info," api at ",lubridate::now(),".\n"))
  return(collectdf)
}


################################# SOURCE PROCESSING ##########################################
get_vsnu <- function(path){
  # get all VSNU DOIs
  vsnu <- read_ext(path, "")
  vsnu <- vsnu %>% mutate(doi = clean_doi(DOI))
  return(vsnu)
}

#' Specifically used to process the data frame
#' that results from a doaj mining query.
#' ISSN numbers are in a nested format,
#' they need to be un-nested.
#' For the ease of future processing, the ISSN
#' columns are renamed.
process_doaj <- function(df){
  df <- df %>%
    # unnest the issn information in the bibjson.identifier column
    unnest(bibjson.identifier, keep_empty = T, names_sep="_") %>%
    rename(issn = bibjson.identifier_id,
           issn_type = bibjson.identifier_type) %>%
    filter(lubridate::year(created_date) <= report_year)
  return(df)
}


################################## APPLYING SOURCE TESTS ###################################

apply_upw <- function(df){
  # extract the relevant column from the unpaywall df
  # and place it in the main df.
  if("oa_color"%in%colnames(df)){
    # it is possible that oa_color already exists. This is the crucial column from Unpaywall,
    # so we ensure here that the column is not duplicated (prompting suffix naming, causing confusion)
    # by removing it from the df.
    df <- select(df, -oa_color)
  }
  df_with_upw <- left_join(df, upwdf, by="doi")
  df <- df %>%
    mutate(upw = df_with_upw$oa_color)
  return(df)
}

apply_vsnu <- function(df){
  # match with the VSNU document
  vsnu_doi_strip <- remove_na(vsnudf$doi)
  df <- df %>% mutate(
    vsnu = doi%in%vsnu_doi_strip)
  return(df)
}

apply_doaj <- function(df){
  # match issns with their existence in DOAJ
  doaj_issn_strip <- remove_na(doajdf$issn)
  df <- df %>% mutate(
    doaj = issn%in%doaj_issn_strip|eissn%in%doaj_issn_strip)
  return(df)
}

#' add matches info to the dataframe
apply_matches <- function(df){
  df <- df %>%
    apply_doaj() %>%
    apply_vsnu() %>%
    apply_upw()
  return(df)
}

###################################### CUSTOM LABELS ######################################
apply_custom <- function(df){
  if(customized == FALSE){
    return(df)
  }
  custom_list <- get_custom(path_custom)
  df <- df %>% mutate(
    custom_label = custom_label(system_id,custom_list)
  )
  return(df)
}


get_custom <- function(path){
  #TODO confirm that custom path exists
  # get the dataframe with custom IDS
  custom <- read_ext(path, "")
  custom_list <- list()
  customlabels <- colnames(custom)
  for(label in customlabels){
    ids <- custom %>% pull(label) %>% remove_na()
    custom_list[[label]] <- ids
  }
  return(custom_list)
}

custom_label <- function(column,custom_list){
  nlabels <- custom_list %>% names() %>% length()

  # Make a dataframe from the IDs in the column
  # and determine whether they are labeled in any of
  # the custom columns.
  label_list <- list()
  for(label in names(custom_list)){
    outlist <- NULL
    for(id in column){
      if(id%in%custom_list[[label]]){
        outlist <- c(outlist,label)
      } else{
        outlist <- c(outlist, NA)
      }
      label_list[[label]] <- outlist
    }
  }
  label_df <- bind_rows(label_list)

  # Process this dataframe, to have an ID and
  # a T/F column that checks if any of the labels are
  # filled out.
  label_df <- label_df %>%
    mutate(ID = 1:n(),
           custom_green = rowSums(is.na(label_df)),
           custom_green = custom_green < nlabels)
  custom_return <- label_df %>% pull(custom_green)

  # Gather all labels in a single column. With a right
  # join back, this will ensure each label is in the right place.
  label_df <- label_df %>%
    gather(label_header, label, -ID, -custom_green, na.rm=T) %>%
    right_join(label_df, by="ID")

  # Multiple labels will make the dataframe
  # unusable to merge back to the original. In this case,
  # the previously generated custom_green column will be used.
  if(nrow(label_df) != length(column)){
    warning("Duplicate IDs exist in the Custom ID data. No specific labels can therefore be assigned.")
    return(custom_return)
  } else{
    label_df %>% pull(label) %>% return()
  }
}

################################### CLASSIFICATION ######################################

#' @title Classification of papers
#'
#' classify based on the information acquired
#' All publications are classified according to their presence in check lists. In sequence:
#' 1. match the journal ISSN with a list from the Directory of Open Access Journals (DOAJ).
#'    If the journal matches, the publication is Gold OA
#' 2. match the DOI with a list obtained from VSNU.
#'    If the journal matches, the publication is Hybrid OA
#' 3. obtain the OA status from Unpaywall.
#'    If the status is 'gold' or 'hybrid', the publication is Hybrid OA
#'    If the status is 'green', the publication is Green OA
#' NB in the classification pipeline these labels will be applied in sequence
#' Thus, e.g. if ISSN matches DOAJ but Unpaywall says 'green', the label will still be Gold OA.
classify_oa <- function(df){
  df <- df %>%
    apply_matches() %>%
    mutate(
      OA_label = case_when(
        doaj ~ "GOLD",
        vsnu ~ "HYBRID",
        upw == "bronze" ~ "CLOSED",
        upw == "gold" ~ "HYBRID", # indeed, we choose to label gold only confirmed DOAJ ISSN
        upw == "hybrid" ~ "HYBRID",
        upw == "green" ~ "GREEN",
        upw == "closed" ~ "CLOSED",
        TRUE ~ "CLOSED"),
      OA_label_explainer = case_when(
        doaj ~ "DOAJ",
        vsnu ~ "VSNU",
        upw == "bronze" ~ "UPW (bronze)",
        upw == "gold" ~ "UPW (gold)",
        upw == "hybrid" ~ "UPW (hybrid)",
        upw == "green" ~ "UPW (green)",
        upw == "closed" ~ "UPW (closed)",
        TRUE ~ "NONE")
    )

  # following additions are only done in case customization is required
  if(customized){
    df <- df %>%
      apply_custom() %>%
      mutate(
        OA_label = case_when(
          OA_label_explainer %in% c("UPW (green)","UPW (closed)", "NONE") & !is.na(custom_label) ~ "GREEN",
          TRUE ~ OA_label
        ),
        OA_label_explainer = case_when(
          OA_label_explainer %in% c("UPW (green)","UPW (closed)", "NONE") & !is.na(custom_label) ~ paste0("CUSTOM (", custom_label,")"),
          TRUE ~ OA_label_explainer
        )
      )
  }
  save_df(df, "all")
  return(df)
}
