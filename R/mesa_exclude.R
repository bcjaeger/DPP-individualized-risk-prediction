
#' @author bcjaeger

mesa_exclude <- function(mesa_init) {

  # don't use nrow because the mesa data were pre-filtered
  n_participants <- 6814

  e1 <- mesa_init %>%
      # drop those without pre-diabetes
    filter(prediabetes == 1) %>%
    group_by(id) %>%
    slice(1) %>%
    ungroup()

  n_participants <- c(n_participants, nrow(e1), NA_integer_)

  exclusions <- enframe(n_participants) %>%
    mutate(name = c("Study participants",
                    "Pre-diabetic at baseline",
                    "Randomized to placebo, metformin, or lifestyle"))


  out <- list(included = e1,
              excluded = setdiff(mesa_init, e1))

  # assert no loss of participants
  stopifnot(nrow(bind_rows(out)) == nrow(mesa_init))

  attr(out, 'exclusions') <- exclusions

  out

}
