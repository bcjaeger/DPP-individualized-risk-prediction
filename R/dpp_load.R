

#' Load all of the non-form data files
#'
#' @author Byron C Jaeger
#'
#' @param format used to determine whether you want csv files or sas ones.
#'   Note that you need to be specific in saying "\\.csv$" or "\\.sas7bdat$".
#'   If you say something simpler like "csv" the function won't work.
#'
#' @return a list of data frames
dpp_load <- function(format = '\\.csv$'){

 fpath <- file.path(
  "\\\\medctr.ad.wfubmc.edu",
  "dfs",
  "phs_research$",
  "bancks_2022ada",
  "DPP",
  "Data_original",
  "DPP_Data_2008",
  "Non-Form_Data",
  "Data"
 )

 files <- list.files(fpath, pattern = format)
 data_names <- str_remove(files, format)

 reader_fun <- switch(format,
                      "\\.csv$" = readr::read_csv,
                      "\\.sas7bdat$" = haven::read_sas)

 files_nonform <- file.path(fpath, files) %>%
  set_names(data_names) %>%
  map(reader_fun) %>%
  map(clean_names)

 fpath <- file.path(
  "\\\\medctr.ad.wfubmc.edu",
  "dfs",
  "phs_research$",
  "bancks_2022ada",
  "DPP",
  "Data_original",
  "DPP_Data_2008",
  "Form_Data",
  "Data"
 )

 files <- list.files(fpath, pattern = format)
 data_names <- str_remove(files, format)

 files_form <- file.path(fpath, files) %>%
  set_names(data_names) %>%
  map(reader_fun) %>%
  map(clean_names)

 list(non_form = files_nonform,
      form = files_form)

}
