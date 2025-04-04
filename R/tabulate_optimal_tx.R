
tabulate_optimal_tx <- function(analysis, pred_horizon = 3) {

  data_internal <- map(
    .x = set_names(levels(analysis$data_internal$treatment)),
    ~ mutate(analysis$data_internal, treatment = .x,
             id_tmp = paste0('internal_', seq(n())))
  )

  data_external <- map(
    .x = set_names(levels(analysis$data_external$treatment)),
    ~ mutate(analysis$data_external, treatment = .x,
             id_tmp = paste0('external_', seq(n())))
  )

  data <- map2(data_internal,
               data_external,
               ~bind_rows(internal = .x,
                          external = .y,
                          .id = 'type'))


  results <- map(
    .x = data,
    ~ augment(analysis$fits$individualized,
              new_data = .x,
              type = 'survival',
              eval_time = pred_horizon) %>%
      unnest(cols = .pred) %>%
      select(type, id_tmp, .pred_survival, treatment)
  )

  preds <- bind_rows(results) %>%
    pivot_wider(names_from = treatment, values_from = .pred_survival) %>%
    rowwise() %>%
    mutate(tx_opt = which.max(c(lifestyle, metformin))) %>%
    ungroup() %>%
    mutate(tx_opt = factor(tx_opt,
                           levels = c(1, 2, 3),
                           labels = c('lifestyle', 'metformin', 'placebo')))

  optim_risk <- preds %>%
    pivot_longer(cols = c(lifestyle, metformin, placebo), names_to = 'tx_cf') %>%
    group_by(type, tx_opt, tx_cf) %>%
    summarize(risk_mean = mean(100 * (1-value)),
              risk_sd = sd(100 * (1-value))) %>%
    mutate(tbv = table_glue("{risk_mean} ({risk_sd})")) %>%
    select(-starts_with("risk")) %>%
    pivot_wider(names_from = tx_cf, values_from = tbv)

  optim_counts <- preds %>%
    group_by(type) %>%
    count(tx_opt) %>%
    mutate(p = n / sum(n)) %>%
    transmute(type, tx_opt, tbn = table_glue("{n} ({100 * p}%)"))

  left_join(optim_counts, optim_risk)


}
