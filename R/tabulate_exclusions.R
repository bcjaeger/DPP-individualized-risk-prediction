#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param dpp_excluded
#' @param mesa_excluded
tabulate_exclusions <- function(dpp_excluded, mesa_excluded) {

  bind_rows(DPP = attr(dpp_excluded, 'exclusions'),
            MESA = attr(mesa_excluded, 'exclusions'),
            .id = 'study') %>%
    mutate(value = table_value(as.integer(value))) %>%
    pivot_wider(names_from = study, values_from = value)

}
