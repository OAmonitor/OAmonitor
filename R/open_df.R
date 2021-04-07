#' Open a single file to
#'
#' This function can be used in the package OAmonitor to
#' open all files on which information is filled out in an
#' excel template.
#' The files will have to be stored in the data folder
#' and formatted either as tsv, csv, xls, or xlsx.
#' @param file The (entire!) path to the (filled out) excel template file.
#' @param dir The path to the folder in which all data content files are located.
#' @return a data frame combining all file content
#' @export
open_df <- function(file, dir=""){
  allfiles <- readxl::read_excel(file)

}
