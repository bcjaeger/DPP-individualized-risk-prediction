#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param mesa_visit_one
#' @param mesa_visit_two
mesa_bind_rows <- function(mesa_visit_one, mesa_visit_two) {

  bind_rows(mesa_visit_one,
            mesa_visit_two) %>%
  arrange(id, visit) %>%
  group_by(desc(id)) %>%
  slice(1)

}
