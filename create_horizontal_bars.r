
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Build charts
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


output$incidence_milestone_bar <-  renderPlot({

  # Make sure there are data to plot
  req(pdata()$epi_timeseries)

  # The End TB strategy milestone is 20% incidence reduction compared to 2015
  inc_milestone <-  20

  # Calculate the actual change in incidence compared to 2015
  inc_achieved <- ifelse(NZ(pdata()$epi_timeseries[pdata()$epi_timeseries$year == 2015, "e_inc_100k"])==0,
                         # If estimate is not available pretend achievement is 0%
                         0,
                         (pdata()$epi_timeseries[pdata()$epi_timeseries$year == 2015, "e_inc_100k"] -
                            pdata()$epi_timeseries[pdata()$epi_timeseries$year == 2020, "e_inc_100k"]) * 100/
                           pdata()$epi_timeseries[pdata()$epi_timeseries$year == 2015, "e_inc_100k"])

  inc_achieved_text <- ifelse(is.na(pdata()$epi_timeseries[pdata()$epi_timeseries$year == 2015, "e_inc_100k"]),
                              "No data",
                              paste0(abs(signif(inc_achieved, 2)), "%")
                              )

  # Create the horizontal bar chart
  target_achieved_bar(achieved_val = inc_achieved,
                      target_val = inc_milestone,
                      achieved_text = inc_achieved_text)

})

output$deaths_milestone_bar <-  renderPlot({

  # Make sure there are data to plot
  req(pdata()$epi_timeseries)

  # The End TB strategy milestone is 35% reduction in total deaths compared to 2015
  deaths_milestone <-  35

  # Calculate the actual change in incidence compared to 2015
  deaths_achieved <- ifelse(NZ(pdata()$epi_timeseries[pdata()$epi_timeseries$year == 2015, "e_mort_num"])==0,
                            # If estimate is not available pretend achievement is 0%
                            0,
                            (pdata()$epi_timeseries[pdata()$epi_timeseries$year == 2015, "e_mort_num"] -
                               pdata()$epi_timeseries[pdata()$epi_timeseries$year == 2020, "e_mort_num"]) * 100/
                              pdata()$epi_timeseries[pdata()$epi_timeseries$year == 2015, "e_mort_num"])

  deaths_achieved_text <- ifelse(is.na(pdata()$epi_timeseries[pdata()$epi_timeseries$year == 2015, "e_mort_num"]),
                                 "No data",
                                 paste0(abs(signif(deaths_achieved, 2)), "%")
                                 )

  # Create the horizontal bar chart
  target_achieved_bar(achieved_val = deaths_achieved,
                      target_val = deaths_milestone,
                      achieved_text = deaths_achieved_text)

})

output$catastrophic_milestone_bar <- renderPlot({

  # Make sure there are data to plot
  req(pdata()$profile_data)

  # The End TB strategy milestone is 0% of patients to suffer catastrophic costs in 2020
  catastrophic_milestone <-  0

  # Get estimate of catastrophic costs if available (if not available pretend it is zero)
  catastrophic_achieved <- NZ(pdata()$profile_data[, "catast_pct"])

  catastrophic_achieved_text <- ifelse(is.na(pdata()$profile_data[, "catast_pct"]),
                                       "No data",
                                       paste0(abs(signif(catastrophic_achieved, 2)), "%")
                                       )

  # Create the horizontal bar chart
  target_achieved_bar(achieved_val = catastrophic_achieved,
                      target_val = catastrophic_milestone,
                      achieved_text = catastrophic_achieved_text)

})


target_achieved_bar <- function(achieved_val, target_val, achieved_text){


  output <- ggplot() +

    geom_col(aes(x=1, y=target_val),
             fill="lightblue",
             size=.0,
             width = 0.4,
             position="identity")   +

    geom_col(aes(x=1, y=achieved_val),
             fill="darkblue",
             size=.3,
             width = 0.4,
             position="identity")  +

    annotate("text",
             x=1.4,
             y=target_val,
             label=paste0("Target\n", target_val, "%")) +

    annotate("text",
             x=1,
             y=ifelse(achieved_val <= 0, achieved_val - 2, achieved_val)*1.2,
             label=paste0("Actual:\n", achieved_text, ifelse(achieved_val < 0, "\nincrease", "")),
             colour="darkblue") +

    annotate("segment",
             x = 0.5, y = 0,
             xend = 1.5, yend = 0,
             colour = "gray",
             size = 0.25) +

    # ensure there is enough space to show the text annotations
    xlim(0.5,1.5) +

    ylim(ifelse(achieved_val <= 0, (achieved_val - 3)*1.3, 0),
         max(ifelse(target_val==0, 3, target_val), achieved_val)*1.3) +

    coord_flip() +

    # Remove all axes, legends, etc.
    theme_void() +
    theme(legend.position = "none")

  if (NZ(achieved_val) > target_val) {

    # Add a dashed line at the target value on top of the achievement bar
    output <- output +

      annotate("segment",
               x = 0.7, y = target_val,
               xend = 1.3, yend = target_val,
               colour = "lightblue",
               size = 0.25,
               linetype = 2)

  }

  return(output)

}

