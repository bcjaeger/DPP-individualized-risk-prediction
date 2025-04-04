

#' Rows for the reference group
#'
#' @param tidy_model A model that has been tidied using the `tidy` function
#'   from the `broom` package.
#' @param data the dataframe used to fit `tidy_model`
#' @param ref_value a numeric value indicating what a referent value is.
#'   For example, if odds ratios are the estimate of interest, the
#'   referent value is 1. If log-odds are the estimate of interest,
#'   the referent value is 0.
#' @param delim a character value indicating how factor levels are
#'   appended to factor variable names.
#'
#' @return a [tibble][tibble::tibble-package] with additional rows
#'   representing the reference category.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' mdl <- lm(Sepal.Length ~ Species, data = iris)
#' add_ref_rows(broom::tidy(mdl), data = iris)
#' }
#'
add_ref_rows <- function(tidy_model, data, ref_value = 0, delim = ''){

  data_factors <- sapply(data, is.factor) %>%
    which() %>%
    names()

  if(is_empty(data_factors)){

    warning('No factors identified in data. Are they character vectors?',
            call. = FALSE)

    return(tidy_model %>%
             dplyr::mutate(variable = term, level = NA_character_) %>%
             dplyr::select(variable, level, dplyr::everything())
    )

  }


  data_factor_levels <- data_factors %>%
    lapply(function(x) levels(data[[x]])) %>%
    magrittr::set_names(data_factors)

  data_factor_terms <- data_factor_levels

  for(i in seq_along(data_factor_terms)){
    data_factor_terms[[i]] <- paste(
      names(data_factor_terms)[i],
      data_factor_terms[[i]],
      sep = delim
    )
  }

  tbl_factor_terms <- tibble::tibble(
    variable = rep(names(data_factor_terms), sapply(data_factor_terms, length)),
    term = unlist(data_factor_terms, use.names = FALSE),
    level = unlist(data_factor_levels, use.names = FALSE)
  ) %>%
    dplyr::group_by(variable) %>%
    dplyr::mutate(num_in_model = sum(term %in% tidy_model$term)) %>%
    dplyr::filter(num_in_model > 0) %>%
    dplyr::select(-num_in_model)

  if(nrow(tbl_factor_terms) == 0)
    return(tidy_model %>%
             dplyr::mutate(variable = term, level = NA_character_) %>%
             dplyr::select(variable, level, dplyr::everything())
    )

  data_factor_reference <- data_factor_terms %>%
    sapply(function(x) x[1]) %>%
    tibble::enframe(name = NULL, value = 'term') %>%
    dplyr::mutate(estimate = ref_value, std.error = 0) %>%
    dplyr::filter(term %in% tbl_factor_terms$term)

  if(all(c('conf.low', 'conf.high') %in% names(tidy_model))){
    data_factor_reference$conf.low <- ref_value
    data_factor_reference$conf.high <- ref_value
  }

  output <- dplyr::bind_rows(tidy_model, data_factor_reference) %>%
    dplyr::left_join(tbl_factor_terms, by = 'term') %>%
    dplyr::mutate(
      variable = dplyr::if_else(
        condition = is.na(variable),
        true = term,
        false =  variable
      )
    ) %>%
    dplyr::mutate(variable = factor(variable, levels = unique(variable))) %>%
    base::split(f = .$variable) %>%
    base::lapply(top_to_bottom) %>%
    dplyr::bind_rows() %>%
    dplyr::select(variable, level, dplyr::everything())

  output

}

top_to_bottom <- function(data){
  if(nrow(data)==1) return(data)
  data[c(nrow(data), 1:(nrow(data)-1)), ]
}
