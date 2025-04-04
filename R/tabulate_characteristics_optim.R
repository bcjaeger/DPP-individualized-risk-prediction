#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param dpp_excluded
#' @param mesa_excluded
#' @param labels
#' @param analysis
#' @return
#' @author bcjaeger
#' @export
tabulate_characteristics_optim <- function(data_analysis,
                                           labels,
                                           analysis) {


  preds_met <- predict(analysis$fits$individualized,
                       new_data = mutate(data_analysis,
                                         treatment = "metformin"),
                       type = 'survival',
                       eval_time = 3) %>%
    unnest(.pred)

  preds_ili <- predict(analysis$fits$individualized,
                       new_data = mutate(data_analysis,
                                         treatment = "lifestyle"),
                       type = 'survival',
                       eval_time = 3) %>%
    unnest(.pred)

  inc_data <- data_analysis %>%
    mutate(pred_risk_met = 1 - preds_met$.pred_survival,
           pred_risk_ili = 1 - preds_ili$.pred_survival,
           tx_optim = if_else(pred_risk_met < pred_risk_ili,
                              "metformin", "lifestyle"))

  incidence_tx <- cuminc(ftime = inc_data$time_diabetes,
                         fstatus = inc_data$status_diabetes,
                         group = inc_data$treatment)

  inc_data_tx_optim <- filter(inc_data, tx_optim == treatment)

  incidence_tx_optim <- cuminc(ftime = inc_data_tx_optim$time_diabetes,
                               fstatus = inc_data_tx_optim$status_diabetes)

  nnt_data <- incidence_tx[1:3] %>%
    c(incidence_tx_optim[1]) %>%
    map_dfr(~bind_cols(est=.x$est, time=.x$time), .id = 'group') %>%
    group_by(group) %>%
    mutate(tdiff = time - 3) %>%
    filter(tdiff > 0) %>%
    arrange(tdiff) %>%
    slice(1) %>%
    ungroup() %>%
    select(group, est) %>%
    mutate(group = str_remove(group, " 1$"),
           group = recode(group, "1" = "model"),
           arr = est[group == 'placebo'] - est,
           nnt = 1 / arr) %>%
    filter(group != 'placebo') %>%
    select(group, nnt)

  tbl_data <- inc_data %>%
    select(
      study,
      tx_optim,
      age_yrs,
      sex,
      race_ethnicity,
      education,
      pred_risk_met,
      pred_risk_ili,
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

  .label %<>% c(
    pred_risk_ili = "Predicted 3-year risk under lifestyle intervention",
    pred_risk_met = "Predicted 3-year risk under metformin intervention"
  )

  out <- tbl_summary(data = tbl_data,
                     label = as.list(.label),
                     by = 'tx_optim',
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
    modify_footnote(c(all_stat_cols()) ~ NA) %>%
    as_flex_table()

  list(tbl = out,
       nnt = nnt_data)


}
