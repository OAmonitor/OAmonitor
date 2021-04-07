#' @import magrittr
NULL

#is it necessary to call the scripts here?
#source("R/get_doaj.R")
#source("R/get_upw.R")
#source("R/get_vsnu.R")

############################## GENERIC CLASSIFICATION TOOLS ###############################


all_labels <- function(type = "label"){
  if(type == "label"){
    return(c("GOLD","HYBRID","GREEN","CLOSED"))
  } else if(type == "explainer"){
    return(c("DOAJ","VSNU","UPW"))
  }
}

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
#' @param which_info either 'doaj' for DOAJ mining results, 'upw', for Unpaywall, 'all', for all data, or 'check' for publications that need to be verified.
save_df <- function(df, which_info){
  # generate necessary folders
  if (!file.exists(here::here("data"))){
    dir.create(here::here("data"))
  }
  if (!file.exists(here::here("data/clean"))){
    dir.create(here::here("data/clean"))
  }
  # remove list columns so the data can be saved
  df <- df %>% dplyr::select_if(is.atomic)
  basename <- dplyr::case_when(
    which_info == "doaj" ~ "doaj_from_issn_",
    which_info == "upw" ~ "upw_from_doi_",
    which_info == "all" ~ "complete_dataframe_",
    which_info == "check" ~ "check_these_publications_",
    TRUE ~ "unknown_info_")
  filename <- paste0("data/clean/", basename, lubridate::today(), ".csv")
  readr::write_csv(df, filename)
}


####################################### API MINING ######################################

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


################################## APPLYING SOURCE TESTS ###################################


#' Add Unpaywall data to publications data frame
#'
#' This function conflates the publications data frame with the results of mining
#' the Unpaywall API. The column containing the content of Unpaywall's `oa_color`
#' is added to the data frame and labeled `upw`.
#'
#' @param df Source data frame containing a doi column
#' @param upwdf Data frame resulting from unpaywall API mining (see `get_upw()`)
#' @return Data frame with a column `upw` added
#' @export
apply_upw <- function(df, upwdf){
  # extract the relevant column from the unpaywall df
  # and place it in the main df.
  if("oa_color"%in%colnames(df)){
    # it is possible that oa_color already exists. This is the crucial column from Unpaywall,
    # so we ensure here that the column is not duplicated (prompting suffix naming, causing confusion)
    # by removing it from the df.
    df <- dplyr::select(df, -oa_color)
  }
  df_with_upw <- dplyr::left_join(df, upwdf, by="doi")
  df <- df %>%
    dplyr::mutate(upw = df_with_upw$oa_color)
  return(df)
}

#' Add VSNU data to publications data frame
#'
#' This function checks DOIs in the publications data frame for their appearance in
#' the VSNU report, and adds a column called `vsnu` containing True/False values
#' that indicate that publications were published under the VSNU deal.
#'
#' @param df Source data frame containing a doi column
#' @param vsnudf VSNU data (see `get_vsnu()`)
#' @return Data frame with a column `vsnu` added
#' @export
apply_vsnu <- function(df, vsnudf){
  # match with the VSNU document
  vsnu_doi_strip <- remove_na(vsnudf$doi)
  df <- df %>% dplyr::mutate(
    vsnu = doi%in%vsnu_doi_strip)
  return(df)
}

#' Add DOAJ data to publications data frame
#'
#' This function checks ISSNs in the publications data frame for their appearance in
#' the VSNU report, and adds a column called `doaj` containing True/False values
#' that indicate that a journal is present in the DOAJ.
#'
#' @param df Source data frame containing an ISSN column
#' @param doajdf Data frame resulting from DOAJ API mining (see `get_doaj()`)
#' @param max_year The journal must have been registered in the DOAJ before or during this year
#' @return Data frame with a column `doaj` added
#' @export
apply_doaj <- function(df, doajdf, max_year="previous"){
  if(max_year == "previous"){
    max_year = lubridate::year(lubridate::today()) - 1
  }
  doajdf <- doajdf %>%
    dplyr::filter(lubridate::year(created_date) <= max_year)
  # match issns with their existence in DOAJ
  doaj_issn_strip <- remove_na(doajdf$issn)
  df <- df %>% dplyr::mutate(
    doaj = issn%in%doaj_issn_strip|eissn%in%doaj_issn_strip)
  return(df)
}

#' Add all VSNU, UPW, DOAJ matches to the data frame
#'
#' Combination function that applies all DOAJ, UPW, and VSNU data to the
#' data frame.
#' @param df Source data frame containing doi and issn columns
#' @param doajdf Data frame resulting from DOAJ API mining (see `get_doaj()`)
#' @param vsnudf VSNU data (see `get_vsnu()`)
#' @param upwdf Data frame resulting from unpaywall API mining (see `get_upw()`)
#' @param max_year The journal must have been registered in the DOAJ before or during this year
#' @return data frame with three extra columns: doaj, vsnu, upw
#' @export
apply_matches <- function(df, doajdf, vsnudf, upwdf, max_year = "previous"){
  df <- df %>%
    apply_doaj(doajdf, max_year) %>%
    apply_vsnu(vsnudf) %>%
    apply_upw(upwdf)
  return(df)
}

###################################### CUSTOM LABELS ######################################

#' Apply custom labels to a data frame
#'
#' Using a data file with columns with system IDs, this function
#' can be used to apply custom labels to publications.
#' Column headers are used as labels and applied to publications in
#' the data frame.
#' @param df Source data frame containing a system_id column
#' @param path The location of the custom file
#' @return Data frame with an added column `custom_label`
#' @export
apply_custom <- function(df, path){
  custom_list <- get_custom(path)
  df <- df %>% dplyr::mutate(
    custom_label = custom_label(system_id,custom_list)
  )
  return(df)
}

#' Read the custom file
#'
#' @param path Location of file with custom labels
#' @return list with the contents of the custom label file
get_custom <- function(path){
  #TODO confirm that custom path exists
  # get the dataframe with custom IDS
  custom <- read_ext(path)
  custom_list <- list()
  customlabels <- colnames(custom)
  for(label in customlabels){
    ids <- custom %>% dplyr::pull(label) %>% remove_na()
    custom_list[[label]] <- ids
  }
  return(custom_list)
}

#' Generate the custom label column
#'
#' @param column the column of system IDs that needs to be verified
#' @param custom_list the list generated by `get_custom`
#' @return a vector with the length of `column` containing labels if applicable
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
  label_df <- dplyr::bind_rows(label_list)

  # Process this dataframe, to have an ID and
  # a T/F column that checks if any of the labels are
  # filled out.
  label_df <- label_df %>%
    dplyr::mutate(ID = 1:dplyr::n(),
                  custom_green = rowSums(is.na(label_df)),
                  custom_green = custom_green < nlabels)
  custom_return <- label_df %>% dplyr::pull(custom_green)

  # Gather all labels in a single column. With a right
  # join back, this will ensure each label is in the right place.
  label_df <- label_df %>%
    tidyr::gather(label_header, label, -ID, -custom_green, na.rm=T) %>%
    dplyr::right_join(label_df, by="ID") %>%
    #sorted on ID to force same order as in original dataframe
    arrange(ID)

  # Multiple labels will make the dataframe
  # unusable to merge back to the original. In this case,
  # the previously generated custom_green column will be used.
  if(nrow(label_df) != length(column)){
    warning("Duplicate IDs exist in the Custom ID data. No specific labels can therefore be assigned.")
    single_label <- dplyr::case_when(custom_return ~ "multiple") # only label when TRUE; NA is used later to determine label
    return(single_label)
  } else{
    label_df %>% dplyr::pull(label) %>% return()
  }
}

################################### CLASSIFICATION ######################################

#' Classification of papers
#'
#' Publications are classified based on the information acquired
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
#'
#' @param df Source data frame containing doi and issn columns
#' @param doajdf Data frame resulting from DOAJ API mining (see `get_doaj()`)
#' @param vsnudf VSNU data (see `get_vsnu()`)
#' @param upwdf Data frame resulting from unpaywall API mining (see `get_upw()`)
#' @param max_year The journal must have been registered in the DOAJ before or during this year
#' @param custom Is a custom label applicable?
#' @param custom_path Path to the excel file with custom labels
#' @param save_results Do you want to save the resulting data frame?
#' @return data frame with Open Access classification (OA_label) and explainer (OA_label_explainer)
#' @export
classify_oa <- function(df, doajdf, vsnudf, upwdf, max_year="previous", custom=F, custom_path="", save_results=F){
  #TODO if data columns do not exist: add them, and remove apply_matches
  df <- df %>%
    apply_matches(doajdf=doajdf, vsnudf=vsnudf, upwdf=upwdf, max_year) %>%
    dplyr::mutate(
      OA_label = dplyr::case_when(
        doaj ~ "GOLD",
        vsnu ~ "HYBRID",
        upw == "bronze" ~ "CLOSED",
        upw == "gold" ~ "HYBRID", # indeed, we choose to label gold only confirmed DOAJ ISSN
        upw == "hybrid" ~ "HYBRID",
        upw == "green" ~ "GREEN",
        upw == "closed" ~ "CLOSED",
        TRUE ~ "CLOSED"),
      OA_label_explainer = dplyr::case_when(
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
  if(custom){
    df <- df %>%
      apply_custom(path = custom_path) %>% #TODO this function requires a path
      dplyr::mutate(
        OA_label = dplyr::case_when(
          #added "UPW (bronze)"as this is classified as CLOSED
          OA_label_explainer %in% c("UPW (green)","UPW (closed)", "UPW (bronze)", "NONE") & !is.na(custom_label) ~ "GREEN",
          TRUE ~ OA_label
        ),
        OA_label_explainer = dplyr::case_when(
          #added "UPW (bronze)"as this is classified as CLOSED
          OA_label_explainer %in% c("UPW (green)","UPW (closed)", "UPW (bronze)", "NONE") & !is.na(custom_label) ~ paste0("CUSTOM (", custom_label,")"),
          TRUE ~ OA_label_explainer
        )
      )
  }
  if(save_results == T){
    save_df(df, "all")
  }
  return(df)
}

