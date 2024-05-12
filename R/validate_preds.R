#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data_analysis
validate_preds <- function(data_analysis,
                           pred_horizon = 3,
                           risk_cut = 0.125) {

  initial_split <- data_analysis %>%
    mutate(surv = Surv(time_diabetes, status_diabetes == 1)) %>%
    select(
      study,
      surv,
      age_yrs,
      race_ethnicity,
      sex,
      treatment,
      glucose_fasting_mgdl,
      hba1c_percent,
      chol_trig_mgdl,
      bmi
    ) %>%
    split(.$study) %>%
    map(select, -study)

  derivation <- initial_split$DPP
  validation <- initial_split$MESA

  derivation_folds <- vfold_cv(derivation)

  rec_standard <- recipe(surv ~ ., data = derivation) %>%
    step_impute_mean(all_numeric_predictors()) %>%
    step_impute_mode(all_nominal_predictors()) %>%
    update_role(race_ethnicity, new_role = 'id')

  rec_interaction <- rec_standard %>%
    step_interact(terms = ~treatment:matches("age_yrs|bmi|^glucose"))

  cph_spec <- proportional_hazards() %>%
    set_engine("survival") %>%
    set_mode("censored regression")

  wf_standard <- workflow() %>%
    add_recipe(rec_standard) %>%
    add_model(cph_spec)

  wf_individualized <- workflow() %>%
    add_recipe(rec_interaction) %>%
    add_model(cph_spec)

  wf <- list(
    standard = wf_standard,
    individualized = wf_individualized
  )

  # internal validation ----

  preds_out <- list()
  test_out <- list()

  for(i in seq(nrow(derivation_folds))){

    train <- training(derivation_folds$splits[[i]])
    test <- testing(derivation_folds$splits[[i]])

    fits <- map(wf, .f = fit, data = train)

    preds_out[[i]] <- map_dfr(fits,
                              .f = predict,
                              .id = 'model',
                              new_data = test,
                              eval_time = pred_horizon,
                              type = 'survival') %>%
      group_by(model) %>%
      mutate(.id = seq(n()))

    test_out[[i]] <- tibble(time = test$surv[,1],
                            status = test$surv[,2],
                            sex = test$sex,
                            race_ethnicity = test$race_ethnicity)

  }

  pred_mats <- bind_rows(preds_out, .id = 'fold') %>%
    unnest(everything()) %>%
    pivot_wider(names_from = .eval_time, values_from = .pred_survival) %>%
    select(-fold, -.id) %>%
    ungroup() %>%
    split(.$model) %>%
    map(~select(.x, -model) %>%
          as.matrix() %>%
          set_colnames(NULL))

  preds_internal <- map(pred_mats, ~1-.x)

  test_internal <- bind_rows(test_out)

  data_internal <-  preds_internal %>%
    map(as.numeric) %>%
    bind_cols(test_internal)


  conseq_internal <- list()

  conseq_internal$overall <- data_internal %>%
    prediction_validate_consequences(pred_horizon, risk_cut) %>%
    select(label, test_pos_rate:spec) %>%
    mutate(level = 'overall', .before = label)

  conseq_internal$race <- data_internal %>%
    filter(race_ethnicity != "other") %>%
    droplevels() %>%
    split(.$race_ethnicity) %>%
    map_dfr(
      ~ .x %>%
        prediction_validate_consequences(pred_horizon, risk_cut) %>%
        select(label, test_pos_rate:spec),
      .id = 'level'
    )

  conseq_internal$sex <- data_internal %>%
    split(.$sex) %>%
    map_dfr(
      ~ .x %>%
        prediction_validate_consequences(pred_horizon, risk_cut) %>%
        select(label, test_pos_rate:spec),
      .id = 'level'
    )


  fairness_internal_out <- conseq_internal[c("race", "sex")] %>%
    bind_rows(.id = 'group') %>%
    pivot_longer(test_pos_rate:spec) %>%
    group_by(group, label, name) %>%
    mutate(parity = min(value / max(value))) %>%
    select(-value, -level) %>%
    distinct() %>%
    pivot_wider(names_from = name, values_from = parity) %>%
    # this converts sens to equalized odds
    mutate(spec = pmin(spec, sens)) %>%
    rename(demo_parity = test_pos_rate,
           equal_oppo = sens,
           equal_odds = spec) %>%
    pivot_longer(cols = c(demo_parity, equal_oppo, equal_odds)) %>%
    pivot_wider(names_from = label, values_from = value) %>%
    rename(stat = name) %>%
    unite(col = 'stat', group, stat, sep = '..') %>%
    mutate(across(.cols = c(individualized, standard),
                  .fns = ~ table_value(100 * .x)))

  sc_internal <- Score(object = preds_internal,
                       data = test_internal,
                       formula = Surv(time, status) ~ 1,
                       times = pred_horizon,
                       summary = 'IPA')

  nri_internal <- nricens(time = test_internal$time,
                          event = test_internal$status,
                          p.std = as.vector(preds_internal$standard),
                          p.new = as.vector(preds_internal$individualized),
                          t0 = pred_horizon,
                          cut = risk_cut)

  nri_internal_out <- tidy_nri(nri_internal)

  sc_internal_out <- tidy_score(sc_internal) %>%
    select(-pval)

  internal_out <- bind_rows(nri_internal_out,
                            sc_internal_out,
                            fairness_internal_out) %>%
    relocate(standard, .before = individualized)

  # external validation ----

  fits <- map(wf, .f = fit, data = derivation)

  validation_list <- list(
    v1 = mutate(validation, treatment = 'lifestyle'),
    v2 = mutate(validation, treatment = 'metformin'),
    v3 = mutate(validation, treatment = 'placebo')
  )

  preds <- map(
    .x = validation_list,
    .f = ~ map_dfr(fits,
                   predict,
                   .id = 'model',
                   new_data = .x,
                   eval_time = pred_horizon,
                   type = 'survival') %>%
      unnest(everything()) %>%
      group_by(model) %>%
      mutate(.id = seq(n())) %>%
      pivot_wider(names_from = .eval_time, values_from = .pred_survival) %>%
      select(-.id) %>%
      ungroup() %>%
      split(.$model) %>%
      map(~select(.x, -model) %>%
            as.matrix() %>%
            set_colnames(NULL))
  )

  preds_external <- list(
    individualized =
      preds[[1]]$individualized +
      preds[[2]]$individualized +
      preds[[3]]$individualized,
    standard =
      preds[[1]]$standard +
      preds[[2]]$standard +
      preds[[3]]$standard
  ) %>%
    map(~ 1 - (.x / 3))


  data_external <-  preds_external %>%
    map(as.numeric) %>%
    bind_cols(
      transmute(validation,
                time = surv[,1],
                status = surv[,2],
                race_ethnicity, sex)
    )


  conseq_external <- list()

  conseq_external$overall <- data_external %>%
    prediction_validate_consequences(pred_horizon, risk_cut) %>%
    select(label, test_pos_rate:spec) %>%
    mutate(level = 'overall', .before = label)

  conseq_external$race <- data_external %>%
    filter(race_ethnicity != "other") %>%
    droplevels() %>%
    split(.$race_ethnicity) %>%
    map_dfr(
      ~ .x %>%
        prediction_validate_consequences(pred_horizon, risk_cut) %>%
        select(label, test_pos_rate:spec),
      .id = 'level'
    )

  conseq_external$sex <- data_external %>%
    split(.$sex) %>%
    map_dfr(
      ~ .x %>%
        prediction_validate_consequences(pred_horizon, risk_cut) %>%
        select(label, test_pos_rate:spec),
      .id = 'level'
    )

  fairness_external_out <- conseq_external[c("race", "sex")] %>%
    bind_rows(.id = 'group') %>%
    pivot_longer(test_pos_rate:spec) %>%
    group_by(group, label, name) %>%
    mutate(parity = min(value / max(value))) %>%
    select(-value, -level) %>%
    distinct() %>%
    pivot_wider(names_from = name, values_from = parity) %>%
    # this converts sens to equalized odds
    mutate(spec = pmin(spec, sens)) %>%
    rename(demo_parity = test_pos_rate,
           equal_oppo = sens,
           equal_odds = spec) %>%
    pivot_longer(cols = c(demo_parity, equal_oppo, equal_odds)) %>%
    pivot_wider(names_from = label, values_from = value) %>%
    rename(stat = name) %>%
    unite(col = 'stat', group, stat, sep = '..') %>%
    mutate(across(.cols = c(individualized, standard),
                  .fns = ~ table_value(100 * .x)))


  sc_external <- Score(object = preds_external,
                       data = tibble(time = validation$surv[,1],
                                     status = validation$surv[,2]),
                       formula = Surv(time, status) ~ 1,
                       times = pred_horizon,
                       summary = 'IPA',
                       plots = 'Calibration')

  nri_external <- nricens(time = validation$surv[,1],
                          event = validation$surv[,2],
                          p.std = as.vector(preds_external$standard),
                          p.new = as.vector(preds_external$individualized),
                          t0 = pred_horizon,
                          cut = c(.10))

  nri_external_out <- tidy_nri(nri_external)

  sc_external_out <- tidy_score(sc_external) %>%
    select(-pval)

  external_out <- bind_rows(nri_external_out,
                            sc_external_out,
                            fairness_external_out) %>%
    relocate(standard, .before = individualized)


  eval_out <- bind_rows(internal = internal_out,
                        external = external_out,
                        .id = 'evaluation') %>%
    relocate(standard, .before = individualized)

  list(eval = eval_out,
       fits = fits)

}


tidy_score <- function(x){

  rspec <- round_spec() %>%
    round_using_decimal(digits = 1)

  auc <- x$AUC$score %>%
    transmute(model,
              auc = table_glue("{100*AUC} ({100*lower} {100*upper})", rspec = rspec)) %>%
    pivot_wider(names_from = model, values_from = auc) %>%
    mutate(pval = x$AUC$contrasts$p[1])

  bri <- x$Brier$score %>%
    filter(model != "Null model") %>%
    transmute(model, ipa = table_glue("{100*IPA}", rspec = rspec)) %>%
    pivot_wider(names_from = model, values_from = ipa) %>%
    mutate(pval = x$Brier$contrasts$p[3])

  bind_rows(AUC = auc, IPA = bri, .id = 'stat') %>%
    mutate(pval = table_pvalue(pval))

}

tidy_nri <- function(x){

  as_tibble(x$nri, rownames = 'stat') %>%
    filter(str_detect(stat, "^NRI")) %>%
    transmute(stat,
              individualized = table_glue("{100 * Estimate} ({100*Lower}, {100*Upper})"),
              standard = "0 (ref)")

}



prediction_validate_consequences <- function(predictions,
                                             pred_horizon,
                                             risk_cut){

  cal_time <- max(pred_horizon)

  dcurve_data <- predictions

  test_consequences(Surv(time, status) ~ individualized + standard,
                    data = dcurve_data,
                    time = cal_time,
                    thresholds = risk_cut,
                    statistics = c("test_pos_rate",
                                   "sens",
                                   "spec"))


}
