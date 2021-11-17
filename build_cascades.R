# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Build the cascade of care charts overlaid with text
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 1. TB overall
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

output$tb_cascade_chart <-  renderPlot({

  # Make sure there are data to plot
  req(pdata()$profile_estimates$e_inc_num)

  incidence <- pdata()$profile_estimates$e_inc_num
  notified <- pdata()$profile_data$c_newinc
  success_pct <- pdata()$profile_data$c_new_tsr

  # create a proxy of number successfully treated to show in bar chart
  # (based on data from 2 years ago)
  success_num <- notified * success_pct / 100

  # Create dataframe to plot cascade
  cascade_tb <- data.frame(cat = c("estimated", "notified", "success"),
                           num = c(incidence, notified, success_num))

  cascade_tb %>%
    ggplot(aes(x=cat, y=num)) +
    geom_col(fill="#a2cfde", alpha=0.15) +
    annotate("text",
             x=1,
             y=incidence/2,
             size=8,
             label = paste0(int_spacer(incidence),
                            relatable_num(incidence),
                            "\nfell ill\nwith TB\nin 2020")) +

    annotate("text",
             x=2,
             y=incidence/2,
             size=ifelse(!is.na(notified),
                         8,
                         6),
             label = ifelse(!is.na(notified),
                            paste0(int_spacer(notified),
                                   relatable_num(notified),
                                   "\nnotified\nwith TB\nin 2020"),
                            "notified\nwith TB\nin 2020\nnot\navailable"
             )
    ) +

    annotate("text",
             x=3,
             y=incidence/2,
             size=ifelse(!is.na(success_pct),
                         8,
                         6),
             label = ifelse(!is.na(success_pct),
                            paste0(success_pct, "%\nsuccessfully\ntreated\nin 2019"),
                            "%\nsuccessfully\ntreated\nin 2019\nnot\navailable")
    ) +

    # Remove all axes, legends etc
    theme_void()

})

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 2. TB/HIV
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

output$tbhiv_cascade_chart <-  renderPlot({

  # Make sure there are data to plot
  req(pdata()$profile_estimates$e_inc_tbhiv_num)

  incidence <- pdata()$profile_estimates$e_inc_tbhiv_num
  notified <- pdata()$profile_data$newrel_hivpos
  success_pct <- pdata()$profile_data$c_tbhiv_tsr

  # create a proxy of number successfully treated to show in bar chart
  # (based on data from 2 years ago)
  success_num <- notified * success_pct / 100

  # Create dataframe to plot cascade
  cascade_tb <- data.frame(cat = c("estimated", "notified", "success"),
                           num = c(incidence, notified, success_num))

  cascade_tb %>%
    ggplot(aes(x=cat, y=num)) +
    geom_col(fill="#fc1e1e", alpha=0.08) +
    annotate("text",
             x=1,
             y=incidence/2,
             size=8,
             label = paste0(int_spacer(incidence), "\nfell ill\nwith TB\nin 2020")) +

    annotate("text",
             x=2,
             y=incidence/2,
             size=ifelse(!is.na(notified),
                         8,
                         6),
             label = ifelse(!is.na(notified),
                            paste0(int_spacer(notified), "\nnotified\nwith TB\nin 2020"),
                            "notified\nwith TB\nin 2020\nnot\navailable"
             )
    ) +

    annotate("text",
             x=3,
             y=incidence/2,
             size=ifelse(!is.na(success_pct),
                         8,
                         6),
             label = ifelse(!is.na(success_pct),
                            paste0(success_pct, "%\nsuccessfully\ntreated\nin 2019"),
                            "%\nsuccessfully\ntreated\nin 2019\nnot\navailable")
    ) +

    # Remove all axes, legends etc
    theme_void()

})


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 2. DR-TB
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

output$drtb_cascade_chart <-  renderPlot({

  # Make sure there are data to plot
  req(pdata()$profile_data$conf_rrmdr)

  detected <- pdata()$profile_data$conf_rrmdr
  enrolled <- pdata()$profile_data$mdr_tx
  success_pct <- pdata()$profile_data$c_mdr_tsr

  # create a proxy of number successfully treated to show in bar chart
  # (based on data from 2 years ago)
  success_num <- enrolled * success_pct / 100

  # Create dataframe to plot cascade
  cascade_tb <- data.frame(cat = c("detected", "enrolled", "success"),
                           num = c(detected, enrolled, success_num))

  cascade_tb %>%
    ggplot(aes(x=cat, y=num)) +
    geom_col(fill="#f1c70c", alpha=0.08) +
    annotate("text",
             x=1,
             y=detected/2,
             size=8,
             label = paste0(int_spacer(detected), "\ndetected\nin 2020")) +

    annotate("text",
             x=2,
             y=detected/2,
             size=ifelse(!is.na(enrolled),
                         8,
                         6),
             label = ifelse(!is.na(enrolled),
                            paste0(int_spacer(enrolled), "\nstarted\ntreatment\nin 2020"),
                            "started\ntreatment\nin 2020\nnot\navailable"
             )
    ) +

    annotate("text",
             x=3,
             y=detected/2,
             size=ifelse(!is.na(success_pct),
                         8,
                         6),
             label = ifelse(!is.na(success_pct),
                            paste0(success_pct, "%\nsuccessfully\ntreated\nin 2018"),
                            "%\nsuccessfully\ntreated\nin 2018\nnot\navailable")
    ) +

    # Remove all axes, legends etc
    theme_void()

})

