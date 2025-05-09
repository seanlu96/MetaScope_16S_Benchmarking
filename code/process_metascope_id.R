library(purrr)
library(dplyr)

#' Processes MetaScope ID counts tables
#'
#' This is a helper function process metascope_id.csv files
#'
#' @param meta_id_path A character path where the .metascope_id.csv file is located
#' @return A dataframe with cleaned results
#' @export

process_metascope_id <- function(meta_id_path) {
  sample_name <- sub('\\..*$', '', basename(meta_id_path))
  df <- read.csv(meta_id_path)
  df <- df |>
    dplyr::select(TaxonomyID, read_count) |>
    dplyr::rename(!!sample_name := read_count)
  return(df)
}

#' Generates counts and taxonomy tables
#'
#' Generates merged counts and taxonomy tables
#'
#' @param meta_id_path A vector of paths where all samples .metascope_id.csv files are located
#' @param accession_path A character path to where taxonomizr accession_path files
#' @return A list containing the merged counts table and corresponding taxonomy table
#' @export

generate_counts_tax_tables <- function(meta_id_path, accession_path) {
  counts_table <- purrr::reduce(lapply(meta_id_path, process_metascope_id), dplyr::full_join, by = 'TaxonomyID')
  counts_table[is.na(counts_table)] <- 0
  
  tax_table <- taxonomizr::getTaxonomy(counts_table$TaxonomyID, accession_path) |> as.data.frame()
  tax_table <- tax_table |>
    tibble::rownames_to_column(var = "TaxonomyID") |>
    dplyr::mutate(TaxonomyID = as.numeric(TaxonomyID)) |>
    dplyr::relocate(TaxonomyID)

  return(list(counts_table, tax_table))  
}