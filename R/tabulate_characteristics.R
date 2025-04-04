#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param dpp_excluded
#' @param mesa_excluded
tabulate_characteristics <- function(dpp_excluded, mesa_excluded, labels) {

  # deal with the extra rows in mesa.

  # mesa_excluded$excluded <- mesa_excluded$excluded %>%
  #   arrange(id, visit) %>%
  #   group_by(id) %>%
  #   slice(1) %>%
  #   filter(!(id %in% mesa_excluded$included$id))

  map2(
    .x = dpp_excluded,
    .y = mesa_excluded,
    .f = tabulate_characteristics_worker,
    labels = labels
  )

}

# dpp_data <- tar_read(dpp_excluded)$excluded
# mesa_data <- tar_read(mesa_excluded)$excluded
# labels <- tar_read(labels)

tabulate_characteristics_worker <- function(dpp_data, mesa_data, labels){

  tbl_data <- bind_rows(DPP = dpp_data,
                        MESA = mesa_data,
                        .id = 'study') %>%
    select(
      study,
      age_yrs,
      sex,
      race_ethnicity,
      education,
      glucose_fasting_mgdl,
      hba1c_percent,
      homa_ir,
      homa_beta,
      bmi,
      chol_trig_mgdl,
      chol_ldlc_mgdl,
      chol_hdl_mgdl
    )

  cat_vars <- labels$levels

  for(i in names(cat_vars)){

    tbl_data[[i]] <- factor(tbl_data[[i]],
                            levels = names(labels$levels[[i]]),
                            labels = labels$levels[[i]])

  }

  .label <- labels$variables

  ctns_vars <- labels$units

  for(i in names(ctns_vars)){

    .label[[i]] <- paste(.label[[i]], labels$units[[i]], sep = ", ")

  }

  out <- tbl_summary(data = tbl_data,
                     label = as.list(.label),
                     by = 'study',
                     missing = 'no',
                     statistic = list(age_yrs = "{mean} ({sd})",
                                      glucose_fasting_mgdl = "{mean} ({sd})",
                                      hba1c_percent = "{mean} ({sd})",
                                      bmi = "{mean} ({sd})",
                                      homa_ir = "{median} ({p25}, {p75})",
                                      homa_beta = "{median} ({p25}, {p75})",
                                      chol_trig_mgdl = "{median} ({p25}, {p75})",
                                      chol_ldlc_mgdl = "{mean} ({sd})",
                                      chol_hdl_mgdl = "{mean} ({sd})")) %>%
    modify_footnote(c(all_stat_cols()) ~ NA)

  out$table_body$stat_2 %<>% str_replace_all(pattern = "NA \\(NA, NA\\)",
                                             replacement = "---")

  out$table_body$stat_2 %<>% str_replace_all(pattern = "NA \\(NA\\)",
                                             replacement = "---")

  out

}
