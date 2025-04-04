
#' @author bcjaeger

mesa_exclude <- function(mesa_init) {


  # drop_na(time_diabetes, status_diabetes) %>%

  # don't use nrow because the mesa data were pre-filtered
  n_participants <- length(unique(mesa_init$id))

  e1 <- mesa_init %>%
    drop_na(time_diabetes, status_diabetes) %>%
    # drop those without pre-diabetes
    filter(prediabetes == 1) %>%
    group_by(id) %>%
    slice(1) %>%
    ungroup()

  stopifnot(all(e1$diabetes==0))

  n_participants <- c(n_participants, nrow(e1), NA_integer_)

  exclusions <- enframe(n_participants) %>%
    mutate(name = c("Study participants",
                    "Pre-diabetic at baseline",
                    "Randomized to placebo, metformin, or lifestyle"))


  out <- list(included = e1,
              excluded = mesa_init %>%
                group_by(id) %>%
                slice(1) %>%
                filter(!id%in%e1$id))

  # assert no loss of participants
  stopifnot(n_participants[1] == nrow(out$excluded) + nrow(out$included))

  attr(out, 'exclusions') <- exclusions

  out

}
