#' @import magrittr
NULL


#' Read and clean VSNU data
#'
#' @param path path of VSNU data
#' @return data frame with VSNU data
#' @export
get_vsnu <- function(path){
  # get all VSNU DOIs
  vsnu <- read_ext(path)
  vsnu <- vsnu %>% dplyr::mutate(doi = clean_doi(DOI))
  return(vsnu)
}
