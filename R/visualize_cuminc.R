#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data_analysis
visualize_cuminc <- function(data_analysis) {

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
      )
    ) %>%
    mutate(status = factor(status)) %>%
    select(study, time, status)

  cuminc_object <- cuminc(ftime = data_cr$time,
                          fstatus = data_cr$status,
                          group = data_cr$study)

  pval <- table.glue::table_pvalue(cuminc_object$Tests[,'pv'])

  data_gg <- cuminc_object[c("DPP 1", "MESA 1")] %>%
    map(as_tibble) %>%
    bind_rows(.id = 'study') %>%
    mutate(study = str_remove(study, " 1$")) %>%
    filter(time < 5)

  ggplot(data_gg) +
    aes(x = time, y = est) +
    geom_line(aes(color = study)) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          legend.position = 'right',
          legend.justification = 'top') +
    scale_y_continuous(labels = scales::percent) +
    labs(x = "Time, years since baseline",
         y = "Cumulative incidence, type 2 diabetes",
         color = "") +
    annotate(
      geom = 'text',
      x = 4,
      y = 0.1,
      label = glue(
        "Gray's test for equivalent\ncumulative incidence: P = {pval}"
      )
    )

}
