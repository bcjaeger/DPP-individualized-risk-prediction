#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data_analysis
tabulate_cuminc <- function(data_analysis, pred_horizon = 3) {

  data_cr <- data_analysis %>%
    mutate(
      status = if_else(
        time_death < time_diabetes & status_death == 1,
        2,
        status_diabetes
      ),
      time = if_else(
        time_death < time_diabetes & status_death == 1,
        time_death,
        time_diabetes
      ),
      .status = if_else(time > pred_horizon, 0, status),
      .time = pmin(pred_horizon, time)
    ) %>%
    mutate(status = factor(status)) %>%
    select(study, time, status, .time, .status)

  # there were no all-cause mortality events, but the code above
  # would catch them if they were there.

  cuminc_object <- cuminc(ftime = data_cr$time,
                          fstatus = data_cr$status,
                          group = data_cr$study)

  cuminc_stats <- cuminc_object[c("DPP 1", "MESA 1")] %>%
    map(as_tibble) %>%
    bind_rows(.id = 'study') %>%
    mutate(timediff = abs(time - 3)) %>%
    arrange(timediff) %>%
    group_by(study) %>%
    slice(1) %>%
    transmute(
      study = str_remove(study, " 1$"),
      cuminc = table_glue(
        "{100 * est} ({100 * (est-sqrt(var)*1.96)}, {100 * (est+sqrt(var)*1.96)})"
      )
    )

  sf <- survfit(Surv(time, status==0)~study, data= data_cr)

  sf_stats <- survival:::survmean(sf, rmean = 'none')$matrix %>%
    as_tibble(rownames = 'study') %>%
    mutate(study = str_remove(study, "^study=")) %>%
    transmute(
      study,
      fup_median = table_glue(
        "{median} ({`0.95LCL`}, {`0.95UCL`})"
      )
    )

  data_tbl <- data_cr %>%
    group_by(study) %>%
    summarize(n_obs = n(),
              n_events = sum(.status==1),
              inc_100 = 100 * sum(.status==1) / sum(.time),
              fup_total = sum(time)) %>%
    left_join(sf_stats) %>%
    left_join(cuminc_stats) %>%
    mutate(across(where(is.numeric), table_value)) %>%
    select(study, n_obs, fup_total, fup_median, n_events, inc_100, cuminc)%>%
    pivot_longer(cols = -study) %>%
    pivot_wider(names_from = study, values_from = value) %>%
    mutate(
      group = recode(
        name,
        n_obs = "Over all follow-up time",
        fup_total = "Over all follow-up time",
        fup_median = "Over all follow-up time",
        n_events = "Up to three years after baseline",
        cuminc = "Up to three years after baseline",
        inc_100 = "Up to three years after baseline"
      ),
      name = recode(
        name,
        n_obs = "Number at risk",
        fup_total = "Total follow-up time, person-years",
        fup_median = "Median (95% CI) follow-up time, years",
        n_events = "Number of incident cases",
        cuminc = "Cumulative incidence (95% CI), %",
        inc_100 = "Incidence rate, per 100 years"
      )
    )

  data_tbl %>%
    as_grouped_data(groups = 'group') %>%
    as_flextable(hide_grouplabel = TRUE) %>%
    set_header_labels(name = "") %>%
    width(j = 1, width = 3) %>%
    width(j = c(2, 3), width = 1.25) %>%
    padding(i = ~ is.na(group), padding.left = 15)



}
