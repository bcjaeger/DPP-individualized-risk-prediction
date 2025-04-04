parse_model_eqn_terms <- function(data, labels){

  term_type <- data$interaction[1]

  res <- data %>%
    mutate(
      variable = recode(variable, !!!labels),
      estimate = format(round(estimate, 8), nsmall = 8),
    )

  if(term_type == 'main'){
    out <- res %>%
      mutate(
        eqn_term = case_when(
          is.na(level) ~ glue("({variable}) * {estimate}"),
          TRUE ~ glue("({variable} = {level}) * {estimate}")
        )) %>%
      pull(eqn_term) %>%
      glue_collapse(sep = ' + ')
  } else {
    out <- res %>%
      mutate(
        eqn_term = case_when(
          level == 'base' ~ glue("{variable} * [{estimate}"),
          level == 'metformin' ~ glue(" + ({variable} = {level}) * {estimate}) + "),
          TRUE ~ glue("({variable} = {level}) * {estimate})]")
        )
      ) %>%
      pull(eqn_term) %>%
      glue_collapse(sep = '')
  }

}
