
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Build charts
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


output$incidence_milestone_chart <-  renderPlot({

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
                              "missing!",
                              ifelse(inc_achieved < 0,
                                     paste0("+", signif(abs(inc_achieved), 2), "%"),
                                     paste0(signif(inc_achieved, 2), "%")
                                     )
                              )

  # Create the doughnut chart
  target_doughnut_text(achieved_val = inc_achieved,
                       target_val = inc_milestone,
                       palette_val = "BuGn",
                       target_text = paste0(inc_milestone, "%"),
                       achieved_text = inc_achieved_text)

})


output$deaths_milestone_chart <-  renderPlot({

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
                                 "missing!",
                                 ifelse(deaths_achieved < 0,
                                        paste0("+", signif(abs(deaths_achieved), 2), "%"),
                                        paste0(signif(deaths_achieved, 2), "%")
                                        )
                                 )


  # Create the doughnut chart
  target_doughnut_text(achieved_val = deaths_achieved,
                       target_val = deaths_milestone,
                       palette_val = "Greens",
                       target_text = paste0(deaths_milestone, "%"),
                       achieved_text = deaths_achieved_text)

})

# Generate a doughnut chart
# Adapted from https://www.r-graph-gallery.com/128-ring-or-donut-plot.html
target_doughnut <- function(achieved_val, target_val, palette_val){

  data <- data.frame(
    category=c("achieved", "to_do"),
    val=c(achieved_val, target_val - achieved_val)
  )

  # Compute percentages
  data$fraction = data$val / sum(data$val)

  # Compute the cumulative percentages (top of each rectangle)
  data$ymax = cumsum(data$fraction)

  # Compute the bottom of each rectangle
  data$ymin = c(0, head(data$ymax, n=-1))

  # Make the plot
  ggplot(data, aes(ymax=ymax, ymin=ymin,
                   # Determine the width of the donut
                   xmax=2, xmin=1.2,
                   fill=category)) +

    # Plot each category as a rectangle
    geom_rect() +

    # Switch to polar coordinates to make a circular plot
    coord_polar(theta="y") +

    # Limit the range of x to get a doughnut instead of filled circle (pie)
    # difference between the two numbers determines the radius of the doughnut
    xlim(c(0, 2))  +

    scale_fill_brewer(palette = palette_val,
                      direction = -1) +

    # Remove all axes, legends, etc.
    theme_void() +
    theme(legend.position = "none")

}

# Adapted from https://www.r-graph-gallery.com/128-ring-or-donut-plot.html
target_doughnut_text <- function(achieved_val, target_val, palette_val, target_text, achieved_text){

  if (achieved_val >= 0) {

    # If have exceeded the target then cap achieved value at the target value
    achieved_val <- ifelse(achieved_val < target_val,
                           achieved_val,
                           target_val)

    data <- data.frame(category=c("1_achieved", "2_to_do"),
                       val=c(achieved_val, target_val - achieved_val))

    up_down_desc <- "reduction\n2015-2020"

  } else {

    # If have exceeded the target in the opposite then cap achieved value at minus the target value
    achieved_val <- ifelse(abs(achieved_val) < target_val,
                           achieved_val,
                           -1 * target_val)

        # Reverse the dataframe because trend is in the opposite direction from the target
    data <- data.frame(category=c("1_todo", "2_achieved"),
                       val=c(target_val - abs(achieved_val), abs(achieved_val)))

    up_down_desc <- "*increase*\n2015-2020"

    # Change the palette to red and yellow to highlight problem
    palette_val <- "Spectral"
  }

  # Compute percentages
  data$fraction = data$val / sum(data$val)

  # Compute the cumulative percentages (top of each rectangle)
  data$ymax = cumsum(data$fraction)

  # Compute the bottom of each rectangle
  data$ymin = c(0, head(data$ymax, n=-1))

  # Make the plot
  ggplot(data, aes(ymax=ymax, ymin=ymin,
                   # Determine the width of the donut
                   xmax=1.9, xmin=1.25,
                   fill=category)) +

    # Plot each category as a rectangle
    geom_rect() +

    # Switch to polar coordinates to make a circular plot
    coord_polar(theta="y") +

    # Limit the range of x to get a doughnut instead of filled circle (pie)
    # difference between the two numbers determines the radius of the doughnut
    xlim(c(0, 3))  +

    scale_fill_brewer(palette = palette_val,
                      direction = -1) +

    # Remove all axes, legends, etc.
    theme_void() +
    theme(legend.position = "none",
          plot.margin = margin(-3,1,0,-5, unit="cm")) +

    # add dashed line on the right
    annotate("segment",
             x = 1.9,
             y = 0.25,
             xend = 3,
             yend = 0.25,
             colour = "darkred",
             linetype = "11", #1 unit on, 1 unit off to give dots
             size = 1.2) +

    # add text
    annotate("text",
             x = 2.62,
             y = 0.2,
             size = 5,
             colour = "#333333",
             label = "Actual:") +

    annotate("text",
             x = 2.6,
             y = 0.23,
             size = 10,
             fontface = "bold",
             colour = "darkred",
             label = achieved_text) +

    annotate("text",
             x = 2.65,
             y = 0.278,
             size = 5,
             colour = "#333333",
             label = up_down_desc) +

    annotate("text",
             x = 0.8,
             y = 1,
             size = 5,
             colour = "#333333",
             label = "Target:") +

    annotate("text",
             x = 0.32,
             y = 1,
             size = 10,
             fontface = "bold",
             colour = "#333333",
             label = target_text) +

    annotate("text",
             x = 0,
             y = 0,
             size = 5,
             colour = "#333333",
             label = "\n\nreduction\n2015-2020")

}
