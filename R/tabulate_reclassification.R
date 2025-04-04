


tabulate_reclassification <- function(analysis) {


  risk_cuts = c(0, 0.1, 0.2, 1)

  data <- bind_rows(internal = analysis$data_internal,
                    external = analysis$data_external,
                    .id = 'set') %>%
    mutate(
      across(
        .cols = c(individualized, standard),
        .fns = ~ cut(.x,
                     breaks = risk_cuts,
                     include.lowest = TRUE,
                     right = FALSE,
                     labels = c("0 to < 10%",
                                "10% to < 20%",
                                "\u2265 20%"))
      ),
      case = if_else(time < 3 & status == 1, 'yes', 'no'),
      set = factor(set, levels = c("internal", "external"))
    )

  by_set <- data %>%
    group_by(set) %>%
    count(individualized, standard) %>%
    mutate(n = table_glue("{n} ({100 * n / sum(n)}%)")) %>%
    pivot_wider(names_from = individualized, values_from = n,
                values_fill = "0")

  by_sex <- data %>%
    group_by(sex) %>%
    count(individualized, standard) %>%
    mutate(n = table_glue("{n} ({100 * n / sum(n)}%)")) %>%
    pivot_wider(names_from = individualized, values_from = n,
                values_fill = "0")

  by_race <- data %>%
    group_by(race_ethnicity) %>%
    count(individualized, standard) %>%
    mutate(n = table_glue("{n} ({100 * n / sum(n)}%)")) %>%
    pivot_wider(names_from = individualized, values_from = n,
                values_fill = "0")

  list(set = by_set,
       sex = by_sex,
       race = by_race)

}
