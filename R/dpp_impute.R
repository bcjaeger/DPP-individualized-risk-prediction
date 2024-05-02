#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param dpp_data
dpp_impute_miceranger <- function(dpp_data,
                                  n_impute = 3,
                                  n_iter = 5,
                                  n_neighbors = 10,
                                  n_trees = 250) {


 preds <- setdiff(
  x = names(dpp_data),
  # don't use the outcome in imputation models b/c
  # we can safely assume that the outcome won't be
  # known when we impute missing values in testing data
  y = c("time", "status")
 )

 vars <- vector(mode = 'list', length = length(preds))
 names(vars) <- preds

 for(v in seq_along(vars)) vars[[v]] <- setdiff(preds, names(vars)[v])


 miceRanger(dpp_data,
            m = n_impute,
            maxiter = n_iter,
            meanMatchCandidates = n_neighbors,
            vars = vars,
            num.trees = n_trees,
            returnModels = TRUE)

}
