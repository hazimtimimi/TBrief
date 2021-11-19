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
  # Prefix numbers to the category names so that they appear in the reverse
  # vertical order (1 is at the bottom, 2 above it, ..) when axes flipped
  cascade_tb <- data.frame(cat = c("3_estimated", "2_notified", "1_success"),
                           num = c(incidence, notified, success_num))

  cascade_tb %>%
    ggplot(aes(x=cat, y=num)) +
    geom_col(fill="#a2cfde", alpha=0.15) +

    # Flip sideways and reverse the order of the bars
    coord_flip() +

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
  # Prefix numbers to the category names so that they appear in the reverse
  # vertical order (1 is at the bottom, 2 above it, ..) when axes flipped
  cascade_tbhiv <- data.frame(cat = c("3_estimated", "2_notified", "1_success"),
                              num = c(incidence, notified, success_num))

  cascade_tbhiv %>%
    ggplot(aes(x=cat, y=num)) +
    geom_col(fill="#fc1e1e", alpha=0.08) +

    # Flip sideways and reverse the order of the bars
    coord_flip() +

    # Remove all axes, legends etc
    theme_void()

})


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 3. DR-TB
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
  # Prefix numbers to the category names so that they appear in the reverse
  # vertical order (1 is at the bottom, 2 above it, ..) when axes flipped
  cascade_drtb <- data.frame(cat = c("3_detected", "2_enrolled", "1_success"),
                             num = c(detected, enrolled, success_num))

  cascade_drtb %>%
    ggplot(aes(x=cat, y=num)) +
    geom_col(fill="#f1c70c", alpha=0.08) +

    # Flip sideways and reverse the order of the bars
    coord_flip() +

    # Remove all axes, legends etc
    theme_void()

})

