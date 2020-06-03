#' @import magrittr
#' @import ggplot2
#' @import ggalluvial
#' @import grDevices
NULL

################################## DEDUPLICATION #########################################
#' Deduplication of publication entries
#'
#' Deduplication function that uses a single dataframe and applies a variety
#' of deduplication functions to subsets of the dataframe.
#' Required to use before reporting on a subset of publications, so that no bias is created
#' in the results if a publication is entered multiple times by different groups, eg.
#' @param df The dataframe that needs to be deduplicated.
#' @return The deduplicated dataframe.
#' @export
deduplicate <- function(df){
  if(!("doi" %in% names(df))|!("system_id" %in% names(df))|!("source" %in% names(df))){
    stop("Required columns are not present. Deduplication requires system_id, source, and doi.")
  }
  # first determine whether there is a mix of multiple source files
  sourcefiles <- df$source %>% as.factor() %>% levels()
  # if a single source file has been used, deduplication can be performed on system ID
  if(length(sourcefiles) < 2){
    df <- dplyr::distinct(df,system_id,.keep_all = T)
    return(df)
  }
  # ensure there are only atomic columns in the dataset
  df <- df %>% dplyr::select_if(is.atomic)
  # use existing information to add DOIs to those with unknown dois
  # separate between entries with and without doi
  df_nondoi <- df %>% dplyr::filter(is.na(doi))
  df_doi <- df %>% dplyr::filter(!is.na(doi))
  # combine so that doi-having entries are all first
  df <- dplyr::bind_rows(df_doi,df_nondoi)
  # deduplicate by system_ID/source combination
  df <- dplyr::distinct(df,system_id,source,.keep_all = T)
  # separate between entries with and without doi
  df_nondoi <- df %>% dplyr::filter(is.na(doi))
  df_doi <- df %>% dplyr::filter(!is.na(doi))
  # deduplicate the doi-having entries
  df_doi <- dplyr::distinct(df_doi,doi,.keep_all = T)
  # combine
  df <- dplyr::bind_rows(df_doi,df_nondoi)
  # deduplicate by system_ID/source combination
  df <- dplyr::distinct(df,system_id,source,.keep_all = T)
  return(df)
}


############################### REQUEST CUSTOMIZATION ###############################


#' Screen for sufficient information
#'
#' Screen the dataframe to see whether there is sufficient information
#' in each reporting unit to determine whether
#'
#' @param df The dataframe to check
#' @param cutoff The accepted proportion of "NONE" per organization unit
#' @param save Should the results be saved?
#' @return Tibble with publications to check manually.
#' @export
check_info <- function(df, cutoff = 0.05, save = F){
  if(!"OA_label_explainer" %in% names(df)){
    stop("Before checking the information status, please run the classification pipeline (`classify_oa()`).")
  }

  # extract the org_units for which the percentage "NONE" exceeds the cutoff
  org_units_to_check <- df %>%
    dplyr::group_by(org_unit) %>%
    dplyr::summarise(no_label = sum(OA_label_explainer=="NONE")/dplyr::n()) %>%
    dplyr::filter(no_label > cutoff) %>%
    dplyr::pull(org_unit)

  # extract individual publications to check
  pubs_to_check <- df %>%
    dplyr::filter(org_unit %in% org_units_to_check & OA_label_explainer=="NONE") %>%
    dplyr::group_by(source) %>% # to ensure that all system_ids will be represented
    deduplicate()

  # save — if opted
  if(save){
    save_df(pubs_to_check, "check")
    cat("A csv with publications for manual screening has been stored in data/clean.")
    }

  return(pubs_to_check)
}



###################################### REPORTING #####################################

#' Generate a report
#'
#' Turn a dataframe with classification into a report summarizing the different kinds of
#' access to publications.
#' @param df The dataframe with classification label (OA_label; result of `classify_oa`)
#' @param title the name of the reporting unit
#' @param save show the image (F) or save it (in `output`)
#' @return summary of the results in a dataframe
#' @export
report_to_dataframe <- function(df, title="all", save=F){
  if(!"OA_label" %in% names(df)){
    stop("Before extracting a report, please run the classification pipeline (`classify_oa()`).")
  }
  ## Write a general report for the entire dataset
  df <- df %>%
    dplyr::group_by(org_unit) %>%
    deduplicate()
  df_report <- df %>%
    dplyr::group_by(org_unit, OA_label) %>%
    dplyr::summarise(n_papers = dplyr::n())
  # deduplicate the dataset and score irrespective of org_unit
  df_all <- df %>%
    deduplicate() %>%
    dplyr::group_by(OA_label) %>%
    dplyr::summarise(n_papers = dplyr::n()) %>%
    dplyr::mutate(org_unit = "all")
  # add all columns to the report
  df_report <- dplyr::bind_rows(df_report,df_all)
  # transform the data
  df_report <- df_report %>% tidyr::pivot_wider(names_from=OA_label,values_from=n_papers)
  # add percentages
  df_report <- df_report %>%
    dplyr::mutate(
      Total_papers = sum(CLOSED,GOLD,GREEN,HYBRID, na.rm=T),
      gold_percent = round(GOLD/Total_papers*100,1),
      hybrid_percent = round(HYBRID/Total_papers*100,1),
      green_percent = round(GREEN/Total_papers*100,1),
      total_OA_percent = round((1 - CLOSED/Total_papers)*100,1)
    )
  # generate a report for explainer column
  df_explainer <- df %>%
    dplyr::group_by(org_unit, OA_label_explainer) %>%
    dplyr::summarise(n_papers = dplyr::n())
  # deduplicate the dataset and score irrespective of org_unit - for explainer
  df_explainer_all <- df %>%
    deduplicate() %>%
    dplyr::group_by(OA_label_explainer) %>%
    dplyr::summarise(n_papers = dplyr::n()) %>%
    dplyr::mutate(org_unit = "all")
  # combine to single dataframe
  df_report_explainer <- dplyr::bind_rows(df_explainer,df_explainer_all)
  # transform the data
  df_report_explainer <- df_report_explainer %>%
    tidyr::pivot_wider(names_from=OA_label_explainer,values_from=n_papers)
  # join both reports
  df_report <- dplyr::left_join(df_report,df_report_explainer,by="org_unit")


  if(save){
    # generate output folder if this does not yet exist
    if (!file.exists(here::here("output"))){
      dir.create(here::here("output"))
    }
    title_slug <- stringr::str_replace(title," ","_")
    outfile <- paste0("output/results_",title_slug,"_",lubridate::today(),".csv")
    readr::write_csv(df_report, outfile)
  }

  return(df_report)
}


##################################### IMAGING ###############################################

#' Turn classification data into a barplot
#'
#' Turn a summary report into a barplot; either proportional or with total numbers.
#' @param df The dataframe with classification label (OA_label; result of `classify_oa`)
#' @param title the name of the reporting unit
#' @param type type of barplot: "prop" (proportional) or "total" (absolute numbers)
#' @param save show the image (F) or save it (in `figures/`)
#' @export
report_to_image <- function(df, title = "all", type = "prop", save = F){
  if(!"OA_label" %in% names(df)){
    stop("Before extracting a report, please run the classification pipeline (`classify_oa()`).")
  }
  if(!(type == "prop" | type == "total")){
    stop("Define the output type as 'prop', or 'total'.")
  }

  # generate figures folder if this does not yet exist
  if (save & !file.exists(here::here("figures"))){
    dir.create(here::here("figures"))
  }

  oacols <- c("gray88","chartreuse3","orange3","gold1")
  title_slug <-stringr::str_replace(title," ","_")
  outfile <- paste0("figures/plot_",title_slug)
  out_prop <- paste0(outfile,"_prop_",lubridate::today(),".png")
  out_num <- paste0(outfile,"_number_",lubridate::today(),".png")

  # ensure levels of df are in order: closed/green/hybrid/gold
  df$OA_label <- factor(df$OA_label, levels = c("CLOSED","GREEN","HYBRID","GOLD"))

  p <- ggplot(df, aes(x = org_unit, fill = OA_label)) +
    scale_fill_manual(values = oacols) +
    theme_bw() +
    labs(title = paste("Accessibility for",title,"publications"),
      x = "",
      fill = "Access type") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

  # PLOT PROPORTION
  if(type == "prop"){
    plot_prop <- p +
      ylab("proportion of papers") +
      geom_bar(position="fill")

    if(save){
      ggsave(filename = out_prop, plot = plot_prop, device=png())
      dev.off()
    } else{
      plot_prop
    }
  }

  # PLOT ACTUAL NUMBER
  if(type == "total"){
    plot_num <- p +
      ylab("number of papers") +
      geom_bar()

    if(save){
      ggsave(filename = out_num, plot = plot_num, device=png())
      dev.off()
    } else{
      plot_num
    }
  }
}

#' Turn classification data into an alluvial diagram
#'
#' Turn a summary report into an alluvial diagram clarifying OA strategy.
#' @param df The dataframe with classification label (OA_label; result of `classify_oa`)
#' @param title the name of the reporting unit
#' @param save show the image (F) or save it (in `figures/`)
#' @export
report_to_alluvial <- function(df,title="all", save=F){
  if(!"OA_label" %in% names(df)){
    stop("Before extracting a report, please run the classification pipeline (`classify_oa()`).")
  }
  # generate figures folder if this does not yet exist
  if (save & !file.exists(here::here("figures"))){
    dir.create(here::here("figures"))
  }

  oacols <- c("gray88","chartreuse3","orange3","gold1")
  StatStratum <- ggalluvial::StatStratum

  df_sum <- df %>%
    reduce_categories() %>%
    deduplicate() %>%
    dplyr::group_by(org_unit,OA_label,OA_label_explainer_short) %>%
    dplyr::summarise(n_papers = dplyr::n()) %>%
    # ensure levels of df are in order: closed/green/hybrid/gold
    as.data.frame() %>%
    dplyr::mutate(
      OA_label = forcats::fct_relevel(OA_label, "CLOSED","GREEN","HYBRID","GOLD"),
      OA_label_explainer_short = forcats::fct_relevel(OA_label_explainer_short, "VSNU","DOAJ",after=Inf),
      OA_label_explainer_short = forcats::fct_relevel(OA_label_explainer_short,"NONE")
    )

  plt_alluv <- ggplot(df_sum,
         aes(y = n_papers,
             axis1 = OA_label_explainer_short, axis2 = OA_label)) +
    geom_alluvium(aes(fill = OA_label),
                  width = 0, knot.pos = 0, reverse = FALSE) +
    guides(fill = FALSE) +
    geom_stratum(width = 1/8, reverse = FALSE) +
    geom_text(stat = "stratum", infer.label = TRUE, reverse = FALSE) +
    scale_x_continuous(breaks = 1:2, labels = c("OA Strategy", "OA Status")) +
    scale_fill_manual(values = oacols) +
    theme_bw() +
    labs(title = paste("Open Access publication strategies for", title),
         y = "Number of papers")

  if(save){
    title_slug <-stringr::str_replace(title," ","_")
    outfile <- paste0("figures/alluvial_",title_slug,"_",lubridate::today(),".png")
    ggsave(filename = outfile, plot = plt_alluv, device=png())
    dev.off()
  } else{
    plt_alluv
  }
}


#################################### DATA REFORMATTING FOR ALLUVIAL ###########################
get_first_word <- function(sentence){
  words <-stringr::str_split(sentence, " ")
  first_word <- words[[1]][1]
  return(first_word)
}

reduce_categories <- function(df){
  df <- df %>% dplyr::mutate(
    OA_label_explainer_short = mapply(get_first_word,OA_label_explainer)
  )
  return(df)
}

################### CUSTOMIZED REPORTS ######################

#' Generate a full report
#'
#' Function that runs all individual reporting functions, and generates
#' a report dataframe in `output`, and images in `figures`.
#' For individual reports that directly return results: use `report_to_dataframe`,
#' `report_to_image`, or `report_to_alluvial`.
#' @param df The dataframe with classification label (OA_label; result of `classify_oa`)
#' @param title the name of the reporting unit
#' @export
full_report <- function(df,title="all"){
  if(!"OA_label" %in% names(df)){
    stop("Before extracting reports, please run the classification pipeline (`classify_oa()`).")
  }
  commandline_report(title)
  report_to_dataframe(df,title,save = T)
  report_to_image(df,title,save = T)
  report_to_alluvial(df,title,save=T)
}

commandline_report <- function(name){
  name_upper <- stringr::str_to_upper(name)
  message <- paste(
    "\n\n#### GENERATING REPORT FOR",
    name_upper,
    "####\n\n"
  )
  cat(message)
}

#' Generate many individual reports
#'
#' The custom-made reporting sheet is used to
#' write individual reports and figures for
#' each set of units in the reporting sheet.
#' @param df The dataframe with classification label (OA_label; result of `classify_oa`)
#' @param path_report Path to the custom excel file that contains reporting information
#' @export
individual_reports <- function(df,path_report){
  reporting <- open_reporting_file(path_report)
  for(r in seq_along(reporting)){
    name <- colnames(reporting)[r]
    col <- dplyr::pull(reporting, name)
    units <- col[!is.na(col)]
    df_r <- df %>% dplyr::filter(org_unit%in%units)
    full_report(df_r,name)
  }
}

open_reporting_file <- function(path){
  reporting <- readxl::read_excel(path)
  reporting <- reporting[2:ncol(reporting)]
  return(reporting)
}


################################### HOOP AREAS #######################################

#' Report for HOOP areas
#'
#' Go over HOOP areas that were filled out with existing organization units
#' and generate reports on the areas overall.
#' @param df The dataframe with classification label (OA_label; result of `classify_oa`)
#' @param path_hoop Path to the custom excel file that contains hoop information
#' @export
hoop_report <- function(df, path_hoop){
  hoopfile <- read_ext(path_hoop)
  for(h in seq_along(hoopfile)){
    name <- colnames(hoopfile)[h]
    col <- dplyr::pull(hoopfile, name)
    units <- col[!is.na(col)]
    if(length(units) < 1){next}
    df <- df %>% dplyr::mutate(
      org_unit = dplyr::case_when(
        org_unit %in% units ~ name,
        TRUE ~ org_unit
      )
    )
  }
  df <- df %>%
    # remove any remaining org_unit entries that were not replaced
    dplyr::filter(org_unit %in% colnames(hoopfile))
  full_report(df,title="HOOP")
}



