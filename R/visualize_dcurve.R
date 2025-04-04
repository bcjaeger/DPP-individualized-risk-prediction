
visualize_dcurve <- function(analysis, pred_horizon = 3) {

  library(dcurves)

  dca_objects <- map(
    .x = analysis[c('data_internal', 'data_external')],
    .f = ~ {

      init <- dca(formula = Surv(time, status) ~ individualized + standard,
                  data = .x,
                  time = 3,
                  thresholds = seq(0.01, 0.50, length.out = 100))

      gg_data <- init$dca %>%
        mutate(label = str_to_title(label))

      net_ppl <- gg_data %>%
        filter(variable != 'all') %>%
        filter(threshold > 0.19,
               threshold < 0.21) %>%
        group_by(label) %>%
        summarize(net_ppl = round(100 * mean(net_benefit), digits = 0)) %>%
        filter(label == "Individualized") %>%
        deframe()

      sl_label <- glue(
        "At 20%, the individualized model
        identifies {net_ppl} patients who will
        develop T2 diabetes in the next 3
        years per 100 in target population."
      )

      arrow_end <- gg_data %>%
        filter(threshold > 0.19,
               threshold < 0.21,
               variable == "individualized") %>%
        group_by(variable) %>%
        summarize(threshold = mean(threshold),
                  net_benefit = mean(net_benefit))

      fig <- ggplot(gg_data) +
        aes(x = threshold,
            y = net_benefit,
            color = label) +
        geom_line(linewidth = 3/4,
                  show.legend = c(color = TRUE, alpha = FALSE)) +
        coord_cartesian(ylim = c(0, 0.125)) +
        theme_bw() +
        theme(panel.grid = element_blank(),
              legend.position = 'top') +
        labs(x = "Threshold, %",
             y = "Net benefit",
             color = "") +
        scale_x_continuous(labels = scales::percent) +
        scale_color_manual(
          values = c("deeppink",
                     "seagreen",
                     "black",
                     "grey80")
        ) +
        annotate('text',
                 label = sl_label,
                 x = 0.375,
                 y = 0.095) +
        annotate(
          'curve',
          x = .25, # Play around with the coordinates until you're satisfied
          y = 0.08,
          yend = arrow_end$net_benefit + 0.001,
          xend = arrow_end$threshold,
          linewidth = 1/2,
          arrow = arrow(length = unit(0.5, 'cm'))
        )

      fig

    }
  )


  dca_objects

}
