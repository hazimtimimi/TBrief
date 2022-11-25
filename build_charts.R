# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Build charts
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# A. Horizontal progress bars for the End TB Strategy milestones
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Generic function to generate a horizontal bar chart

target_achieved_bar <- function(achieved_val, target_val, achieved_text){


  output <- ggplot() +

    geom_col(aes(x=1, y=target_val),
             fill="lightblue",
             width = 0.4,
             colour = "darkblue",
             position="identity")   +

    geom_col(aes(x=1, y=achieved_val),
             fill="darkblue",
             width = 0.4,
             colour = "darkblue",
             position="identity")  +

    annotate("text",
             x=1.4,
             y=target_val,
             label=paste0("Target\n", target_val, "%")) +

    annotate("text",
             x=1,
             y=ifelse(achieved_val <= 0, achieved_val - 2, achieved_val)*1.2,
             label=paste0(achieved_text, ifelse(achieved_val < 0, "\nincrease", "")),
             colour="darkblue") +

    annotate("segment",
             x = 0.7, y = 0,
             xend = 1.3, yend = 0,
             colour = "gray",
             size = 1) +

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
                            pdata()$epi_timeseries[pdata()$epi_timeseries$year == dcyear-1, "e_inc_100k"]) * 100/
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
                               pdata()$epi_timeseries[pdata()$epi_timeseries$year == dcyear-1, "e_mort_num"]) * 100/
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

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# B. Indicator charts
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 1. Mortality 2000 - latest year
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

output$mortality_chart <-  renderPlot({

  # Make sure there are data to plot
  req(pdata()$epi_timeseries)

  # Calculate the End TB strategy milestone of 35% total deaths reduction compared to 2015
  mort_milestone <- pdata()$epi_timeseries[pdata()$epi_timeseries$year == 2015, "e_mort_num"] * 0.65

  pdata()$epi_timeseries %>%
    ggplot(aes(x=year, y=e_mort_num, ymin=0)) +
    geom_line(size=3,
              colour=gtbreport::palette_gtb("mort")) +

    geom_ribbon(aes(x=year, ymin=e_mort_num_lo, ymax=e_mort_num_hi),
                fill=gtbreport::palette_gtb("mort"),
                alpha=0.1) +

    # Add End TB strategy milestone as dashed line
    geom_hline(mapping=aes(yintercept = mort_milestone, linetype = I(2))) +

    annotate(geom='text',
             label='2020 milestone',
             x=2016,
             y=mort_milestone*1.15) +

    scale_x_continuous(name=element_blank(), breaks = c(2000, dcyear-1)) +

    scale_y_continuous(name = "Number per year",
                       limits = c(0, NA),
                       labels = function(x){ifelse(x %% 1 == 0, int_spacer(x),"")}) +

    gtbreport::theme_gtb() +

    # Get rid of annoying x-axis line and ticks and reduce bottom margin
    theme(axis.line.x = ggplot2::element_blank(),
          axis.ticks.x = element_blank(),
          plot.margin = ggplot2::margin(1, 5, 1, 5))


})


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 2. Incidence and notification 2000 - latest year
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


output$incidence_chart <-  renderPlot({

  # Make sure there are data to plot
  req(pdata()$epi_timeseries)

  # Calculate the End TB strategy milestone of 20% incidence reduction compared to 2015
  inc_milestone <- pdata()$epi_timeseries[pdata()$epi_timeseries$year == 2015, "e_inc_100k"] * 0.8

  pdata()$epi_timeseries %>%

    ggplot() +

    geom_ribbon(mapping=aes(x=year, ymin=e_inc_100k_lo, ymax=e_inc_100k_hi),
                fill=gtbreport::palette_gtb("inc"),
                alpha=0.1) +

    geom_line(mapping=aes(x=year, y=e_inc_100k, colour="TB incidence"),
              size=3) +

    # Add End TB strategy milestone as dashed line
    geom_hline(mapping=aes(yintercept = inc_milestone, linetype = I(2))) +

    annotate(geom='text',
             label='2020 milestone',
             x=2016,
             y=inc_milestone*1.15) +

    geom_line(mapping=aes(x=year, y=c_newinc_100k, colour="People notified with TB"),
              size=3) +

    scale_color_manual(values = c('TB incidence' = gtbreport::palette_gtb("inc"),
                                  'People notified with TB' = '#000000')) +

    scale_x_continuous(name=element_blank(), breaks = c(2000, dcyear-1)) +

    scale_y_continuous(name = "Number\nper 100 000 per year",
                       limits = c(0, NA),
                       labels = function(x){ifelse(x %% 1 == 0, int_spacer(x),"")}) +

    gtbreport::theme_gtb() +

    # Get rid of x-axis line and ticks and reduce bottom margin
    theme(axis.line.x = element_blank(),
          axis.ticks.x = element_blank(),
          plot.margin = ggplot2::margin(1, 5, 1, 5),
          # reduce space between x-axis and bottom legend
          legend.box.margin = margin(-30)
    )

})

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 3. Attributable cases - latest year
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


output$rf_chart <-  renderPlot({

  # Make sure there are data to plot
  req(is.data.frame(pdata()$attributable_cases))

  # Create a dataframe of display labels
  df_labs <- data.frame(risk_factor=c("alc",
                                      "dia",
                                      "hiv",
                                      "smk",
                                      "und"),
                        label_text=c("Alcohol use disorder",
                                     "Diabetes",
                                     "HIV",
                                     "Smoking",
                                     "Undernutrition"))

  pdata()$attributable_cases %>%

    # Get the labels for each risk factor
    right_join(df_labs, by = "risk_factor") %>%

    # Order by descending number of attributable cases
    arrange(desc(best)) %>%

    # Preserve the order by setting risk_factor as a factor variable
    mutate(label_text = factor(label_text, levels = rev(label_text))) %>%

    # Plot
    ggplot(aes(y=label_text,
               x=best)) +

    geom_point() +
    # Some records have null entries for lo and hi which triggers an error so
    # set them to zero using NZ() because this only happens when best is zero.
    geom_pointrange(aes(xmin=NZ(lo), xmax=NZ(hi)),
                    size=1.2,
                    colour='Darkgreen') +

    expand_limits(x=0) +

    ylab(element_blank()) +

    # Use space separators to label large numbers; don't display fractions
    scale_x_continuous(name = "Number falling ill with TB",
                       labels = function(x){ifelse(x %% 1 == 0, int_spacer(x),"")},
                       expand = expansion(mult = c(0.05, 0.15))) +


    gtbreport::theme_gtb() +

    # Reduce bottom margin and increase label size for y axis
    theme(plot.margin = ggplot2::margin(1, 5, 1, 5),
          axis.text.y = element_text(size = 14))


})

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 4. Treatment success rate - latest year
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

output$tsr_chart <-  renderPlot({

  # Make sure there are data to plot
  req(pdata()$profile_data$c_new_tsr)

  # Create simple dataframe
  tx_out <- data.frame(cat=c("TB", "TB", "DRTB", "DRTB"),
                       out=c("tsr", "rest", "tsr", "rest"),
                       val=c(pdata()$profile_data$c_new_tsr,
                             100 - NZ(pdata()$profile_data$c_new_tsr),
                             pdata()$profile_data$c_mdr_tsr,
                             100 - NZ(pdata()$profile_data$c_mdr_tsr)))


  # Create simple horizontal bar chart
  tx_out %>%

    ggplot(aes(x=cat, y=val, fill=out)) +
    geom_col(alpha=0.4,
             show.legend = FALSE) +

    scale_fill_manual(name=element_blank(),
                      values = c("lightblue",
                                 "#1D91D1")) +

    ylim(0,100) +

    # Flip sideways and reverse the order of the bars
    coord_flip() +

    # Remove all axes, legends etc
    theme_void() +
    theme(plot.margin = ggplot2::margin(-20,0,0,-20))


})


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 5. TPT provision 2015 - latest year
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


output$tpt_chart <- renderPlot({

  # Make sure there are data to plot
  req(pdata()$tpt_timeseries)

  #Make sure there are some non-null values in the dataframe
  req(sum(!is.na(pdata()$tpt_timeseries$hiv)) +
        sum(!is.na(pdata()$tpt_timeseries$contact_04)) +
        sum(!is.na(pdata()$tpt_timeseries$contact_5plus)) > 0)

  pdata()$tpt_timeseries %>%

    # flip to long format for plotting
    pivot_longer(cols = hiv:contact_5plus,
                 names_to = "TPT_category",
                 values_to = "how_many") %>%

    # Set the order of the TPT categories so that it is reflected in the chart
    mutate(TPT_category = factor(TPT_category,
                                 levels = c("hiv", "contact_04", "contact_5plus"))) %>%

    ggplot(aes(x=year, y=how_many, fill = TPT_category)) +

    geom_col(position = position_stack(reverse = TRUE)) +

    scale_x_continuous(name=element_blank(),
                       breaks = c(2015, dcyear-1)) +

    scale_y_continuous(name = "Number",
                       labels = function(x){ifelse(x %% 1 == 0, int_spacer(x),"")}) +

    scale_fill_manual(name=element_blank(),
                      breaks = c("hiv", "contact_04", "contact_5plus"),
                      labels = c("People living with HIV", "Contacts aged under 5", "Contacts aged 5 and over"),
                      values = c("hiv"="#ffc425",
                                 "contact_04"="#9fcd25",
                                 "contact_5plus"="dark green")) +

    theme_gtb() +

    # Get rid of x-axis line and ticks and reduce bottom margin
    theme(axis.line.x = element_blank(),
          axis.ticks.x = element_blank(),
          plot.margin = ggplot2::margin(1, 5, 1, 5),
          # reduce space between x-axis and bottom legend
          legend.box.margin = margin(-30)
    )


})



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 6. Budget most recent 5 years
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

output$funding_chart <-  renderPlot({
  # Make sure there are data to plot
  req(pdata()$funding_timeseries)

  # First make sure there are some data to display
  # There will only be the year column if no data, so check number of columns

  ndata_cols <- ncol(pdata()$funding_timeseries) - 1

  # Only plot the data if have at least one year with data

  if (ndata_cols > 0) {
    plotobj <- pdata()$funding_timeseries %>%

      # Convert to long format
      pivot_longer(
        cols = -year,
        names_to = "funding",
        values_to = "fund_amount",
        # drop empty values
        values_drop_na = TRUE
      ) %>%

      ggplot(aes(x = year, y = fund_amount, fill = funding)) +

      geom_col(position = position_stack(reverse = TRUE)) +

      scale_fill_manual(
        "",
        values = c(
          "a_domestic_funds" =  "#7DAAD4",
          "b_international_funds" =  "#E9C04F"
        ),
        labels = c(
          "a_domestic_funds" =  "Domestic funding",
          "b_international_funds" =  "International funding"
        )
      ) +

      scale_x_continuous(name = element_blank(), breaks = c(dcyear - 5, dcyear - 1)) +

      scale_y_continuous(
        name = "US$ millions",
        labels = function(x) {
          ifelse(x %% 1 == 0, int_spacer(x), "")
        }
      ) +

      gtbreport::theme_gtb() +

      # Get rid of annoying x-axis line and ticks and reduce bottom margin
      theme(
        axis.line.x = ggplot2::element_blank(),
        axis.ticks.x = element_blank(),
        plot.margin = ggplot2::margin(1, 5, 1, 5),
        # reduce space between x-axis and bottom legend
        legend.box.margin = margin(-30)
      )

  } else {
    plotobj <- NA
  }

  return(plotobj)

})
