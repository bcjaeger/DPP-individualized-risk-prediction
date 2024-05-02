#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param dpp_formatted
dpp_exclude <- function(dpp_formatted) {

 n_participants <- nrow(dpp_formatted)

 e1 <- dpp_formatted %>%
  filter(
   # drop those who have diabetes already (according to current defn)
   glucose_fasting_mgdl < 126,
   hba1c_percent < 6.5 | is.na(hba1c_percent)
  )

 n_participants <- c(n_participants, nrow(e1))

 e2 <- e1 %>%
  filter(treatment != 'troglitazone') %>%
  droplevels()

 n_participants <- c(n_participants, nrow(e2))

 exclusions <- enframe(n_participants) %>%
  mutate(dropped = abs(c(0, diff(value))),
         name = c("Study participants",
                   "Pre-diabetic at baseline",
                   "Randomized to placebo, metformin, or lifestyle"))


 out <- list(included = e2,
             excluded = setdiff(dpp_formatted, e2))

 # assert no loss of participants
 stopifnot(nrow(bind_rows(out)) == nrow(dpp_formatted))

 attr(out, 'exclusions') <- exclusions

 out

}
