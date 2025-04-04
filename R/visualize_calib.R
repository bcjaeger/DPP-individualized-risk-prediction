
visualize_calib <- function(analysis,
                            pred_horizon = 3,
                            type = 'external') {

  data_calib <- switch(type,
                       'internal' = analysis$data_internal,
                       'external' = analysis$data_external)

  groups <- list(
    overall = data_calib,
    men = filter(data_calib, sex == 'male'),
    women = filter(data_calib, sex == 'female'),
    black = filter(data_calib, race_ethnicity == 'african_american'),
    white = filter(data_calib, race_ethnicity == 'caucasian'),
    hispanic = filter(data_calib, race_ethnicity == "hispanic"),
    other = filter(data_calib, race_ethnicity == "other")
  )

  output <- map(
    groups,

    .f = ~ {

      .scalib <-
        scalib(pred_risk = .x$individualized,
               pred_horizon = pred_horizon,
               event_status = .x$status,
               event_time = .x$time)


      # keeping max dimension low to reduce computation time
      .scalib_slope <- scalib_hare(scalib_object = .scalib,
                                   hare_linear_time = TRUE,
                                   hare_linear_risk = TRUE)


      # use 7 risk groups. default is 10, but 7 looks nicer on the final plot

      .scalib_test <- scalib_gnd(scalib_object = .scalib,
                                 group_count_init = 10)

      data_pointrange <- .scalib_test %>%
        getElement('data_outputs') %>%
        unnest(gnd_data)


      gg_data <- .scalib_slope %>%
        getElement("data_outputs") %>%
        select(._id_., hare_data_plot) %>%
        unnest(hare_data_plot)

      fig_cal_slope <- ggplot(gg_data) +
        aes(x = predicted, y = observed) +
        geom_pointrange(
          data = data_pointrange,
          mapping = aes(x = percent_expected,
                        y = percent_observed,
                        ymin = percent_observed - 1.96 * sqrt(variance),
                        ymax = percent_observed + 1.96 * sqrt(variance)),
          color = 'grey') +
        geom_line() +
        geom_abline(col = 'grey', linetype = 2, intercept = 0, slope = 1) +
        scale_x_continuous(limits = c(0,1),
                           # expand = c(0,0),
                           breaks = seq(0, 1, by = 0.2),
                           labels = paste0(seq(0, 100, by = 20),"%")) +
        scale_y_continuous(limits = c(-0.1, 1),
                           breaks = seq(0, 1, by = 0.2),
                           labels = paste0(seq(0, 100, by = 20),"%")) +
        theme_bw() +
        theme(panel.grid = element_blank())


      data_segment <- predrisk_bin_segments(.scalib_slope,
                                            bin_count = 100,
                                            bin_yintercept = -0.1,
                                            bin_length = 1)


      fig_final <- fig_cal_slope +
        geom_segment(data = data_segment,
                     inherit.aes = FALSE,
                     linewidth = 1.5,
                     color = 'grey',
                     mapping = aes(x = x,
                                   y = y,
                                   xend = xend,
                                   yend = yend))


      fig_final

    })




}
