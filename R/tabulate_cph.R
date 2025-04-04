
tabulate_cph <- function(analysis, labels, pred_horizon = 3) {

  rspec <- round_spec() %>%
    round_using_decimal(digits = 2)

  data_fit <- analysis$data_internal

  data_recipe <- recipe(data_fit, time + status ~ .) %>%
    step_rm(individualized, standard) %>%
    update_role(race_ethnicity, new_role = 'id') %>%
    step_impute_mean(all_numeric_predictors()) %>%
    step_impute_mode(all_nominal_predictors()) %>%
    step_scale(all_numeric_predictors())

  data_prepped <- prep(data_recipe)

  train <- juice(data_prepped)

  cph <- coxph(
    formula = Surv(time, status) ~ sex + hba1c_percent + chol_trig_mgdl +
      treatment * (age_yrs + glucose_fasting_mgdl + bmi),
    data = train,
    model = TRUE,
    x = TRUE
  )

  betas <- coef(cph)

  lpm <- cph$means

  bhaz <- suppressWarnings(basehaz(cph)) %>%
    filter(time <= pred_horizon) %>%
    slice(n()) %>%
    pull(hazard)

  continuous_pred_sds <- data_prepped$steps[[4]]$sds

  lp_mean <- lpm %*% betas

  # assert that the prediction equation we use will give the same answer
  # as the predictRisk function from riskRegression
  test_data <- bake(data_prepped, new_data = analysis$data_external)

  pred_vars <- setdiff(all.vars(cph$formula), c("time", "status"))
  pred_form <- as.formula( paste("~", as.character(terms(cph$formula))[3]) )
  lp_data <-  select(test_data, all_of(pred_vars))

  lp <-  model.matrix(pred_form, data = lp_data)[, -1L] %*% betas

  preds_eqn <- 1 - exp(exp(lp-as.numeric(lp_mean)) * -bhaz)

  preds_reg <- predictRisk(cph,
                           newdata = test_data,
                           times = pred_horizon)

  # throws an error if any test predictions don't match prediction equation
  stopifnot(
    all(abs(as.numeric(preds_reg) - as.numeric(preds_eqn)) < 1e-10)
  )

  cph_eqn <- tidy(cph) %>%
    separate(term, into = c("term", "interaction"),
             sep = '\\:',
             fill = 'right') %>%
    add_ref_rows(data = train) %>%
    filter(estimate != 0) %>%
    mutate(
      interaction = if_else(
        is.na(interaction) & variable %in% .$interaction,
        variable,
        interaction
      ),
      level = if_else(
        !is.na(interaction) & is.na(level),
        'base',
        level
      ),
      interaction = replace(interaction,
                            is.na(interaction),
                            "main")
    ) %>%
    arrange(interaction, level, variable) %>%
    split(.$interaction) %>%
    map(parse_model_eqn_terms, labels = labels$variables) %>%
    .[c("main", setdiff(names(.), "main"))] %>%
    reduce(paste, collapse = " + ")

  names(continuous_pred_sds) %<>% recode(!!!labels$variables)

  sds <- paste("- divide", tolower(names(continuous_pred_sds)), 'by',
               map_dbl(continuous_pred_sds, round, 8),
               collapse = '\n')

  cph_instructions <- glue(
    "Instructions for computing {pred_horizon}-year predicted risk:\\
  \n\n Step 1: Scale predictors:\\
  \n\n {sds} \\
  \n\n Step 2: Compute linear predictor (LP):\\
  \n\n LP = {tolower(cph_eqn)}\\
  \n\n Step 3: Plug LP into the risk formula:\\
  \n\n Risk = 1 - exp({-round(bhaz, 6)} * exp(LP - {round(lp_mean,6)}))"
  )

  final_cph_tidy <- Publish::publish(cph, print=FALSE) %>%
    getElement('rawTable') %>%
    as_tibble() %>%
    select(-Pvalue) %>%
    separate(Variable, into = c("variable", "level"),
             sep = "\\: ", fill = 'right') %>%
    mutate(
      level = coalesce(level, Units),
      level = str_remove(level, "treatment\\("),
      level = str_remove(level, "\\)"),
      level = if_else(level == "", NA_character_, level),
      variable = if_else(
        variable == "" & level %in% c("male", "female"),
        "sex",
        variable
      )
    ) %>%
    select(-Units) %>%
    rename(est = HazardRatio,
           lwr = Lower,
           upr = Upper) %>%
    mutate(
      variable = recode(variable, !!!labels$variables),
      level = Hmisc::capitalize(level),
      temp_variable = variable,
      variable = if_else(level %in% levels(train$treatment),
                         level,
                         variable),
      level = if_else(level %in% levels(train$treatment),
                      temp_variable,
                      level)
    ) %>%
    arrange(variable, level)

  tbl_data <- final_cph_tidy %>%
    transmute(variable, level,
              tbl_value = table_glue("{est} ({lwr}, {upr})",
                                     rspec = rspec)) %>%
    mutate(tbl_value = str_replace(tbl_value,
                                   '1\\.00, 1\\.00',
                                   "Reference"),
           level = if_else(is.na(level), variable, level),
           variable = if_else(variable == level, NA, variable))


  tbl_inline <- tbl_data %>%
    as_inline(tbl_variables = c('variable', 'level'),
              tbl_values = c('tbl_value'))

  list(tbl_inline = tbl_inline,
       tbl_data = tbl_data,
       instructions = cph_instructions)

}
