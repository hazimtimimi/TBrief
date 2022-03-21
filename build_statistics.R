# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Build outputs showing numbers
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

output$population <- renderInfoBox(

  infoBox(title = paste("Population", dcyear-1),
          value = paste(ftb_na(pdata()$profile_estimates[, "e_pop_num"]/1e6), "million"),
          icon = icon("users"),
          color = "purple"
  )
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# End TB Strategy milestones
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

output$milestone_deaths <- renderInfoBox(

  infoBox(title = "TB deaths 2020 vs 2015",
          subtitle = "(Target is 35% reduction)",
          value = pct_change_description(pdata()$epi_timeseries[pdata()$epi_timeseries$year == 2015, "e_mort_num"],
                                         pdata()$epi_timeseries[pdata()$epi_timeseries$year == 2020, "e_mort_num"]),
          icon = icon("people-carry")
          )
)

output$milestone_incidence <- renderInfoBox(

  infoBox(title = "TB incidence 2020 vs 2015",
          subtitle = "(Target is 20% reduction)",
          value = pct_change_description(pdata()$epi_timeseries[pdata()$epi_timeseries$year == 2015, "e_inc_100k"],
                                         pdata()$epi_timeseries[pdata()$epi_timeseries$year == 2020, "e_inc_100k"]),
          icon = icon("head-side-cough"),
          color = "green"
          )
)

output$milestone_catast <- renderInfoBox(

  infoBox(title = "Catastrophic costs",
          subtitle = "(Target is 0% of people with TB facing catastrophic costs by 2020)",
          value = ifelse(is.na(pdata()$profile_data[, "catast_pct"]),
                         "No data",
                         paste0(signif(pdata()$profile_data[, "catast_pct"], 2), "%")
                         ),
          icon = icon("balance-scale-left"),
          color = "orange"
          )
)

output$achieved_deaths <- renderText(

  HTML(paste("<h4>Achieved: <span style='font-size:120%; font-weight:bold;'>",
        pct_change_description(pdata()$epi_timeseries[pdata()$epi_timeseries$year == 2015, "e_mort_num"],
                               pdata()$epi_timeseries[pdata()$epi_timeseries$year == 2020, "e_mort_num"]),
        "</span></h4>"))
)

output$achieved_incidence <- renderText(

  HTML(paste("<h4>Achieved: <span style='font-size:120%; font-weight:bold;'>",
             pct_change_description(pdata()$epi_timeseries[pdata()$epi_timeseries$year == 2015, "e_inc_100k"],
                                    pdata()$epi_timeseries[pdata()$epi_timeseries$year == 2020, "e_inc_100k"]),
             "</span></h4>"))
)

output$achieved_catast <- renderText(

  HTML(paste("<h4>Achieved: <span style='font-size:120%; font-weight:bold;'>",
             ifelse(is.na(pdata()$profile_data[, "catast_pct"]),
                    "No data",
                    paste0(signif(pdata()$profile_data[, "catast_pct"], 2), "%")
             ),
             "</span></h4>"))
)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Indicators
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# TB deaths
output$tb_deaths_num <- renderInfoBox(

  infoBox(title = "Number of TB deaths 2020",

          value = paste0(ftb(pdata()$epi_timeseries[pdata()$epi_timeseries$year == 2020, "e_mort_num"]),
                        " (",
                        relatable_number(pdata()$epi_timeseries[pdata()$epi_timeseries$year == 2020, "e_mort_num"]),
                        ")"
                        ),

          subtitle = HTML(paste0("Range ",
                                 ftb(pdata()$epi_timeseries[pdata()$epi_timeseries$year == 2020, "e_mort_num_lo"]),
                                 "-",
                                 ftb(pdata()$epi_timeseries[pdata()$epi_timeseries$year == 2020, "e_mort_num_hi"]),
                                 ". (",
                                 ftb(pdata()$epi_timeseries[pdata()$epi_timeseries$year == 2019, "e_mort_num"]),
                                 " in 2019 ",
                                 pct_change_arrow(pdata()$epi_timeseries[pdata()$epi_timeseries$year == 2019, "e_mort_num"],
                                                  pdata()$epi_timeseries[pdata()$epi_timeseries$year == 2020, "e_mort_num"]),
                                 ")")
                          ),

          icon = icon("people-carry")
  )
)

output$tb_deaths_rate <- renderInfoBox(

  infoBox(title = "TB death rate 2020",

          value = paste0(ftb(pdata()$profile_estimates$e_mort_100k),
                         " per 100 000 population"),

          subtitle = paste0("Range ",
                            ftb(pdata()$profile_estimates$e_mort_100k_lo),
                            "-",
                            ftb(pdata()$profile_estimates$e_mort_100k_hi)),

          icon = icon("people-carry")
  )
)

# TB incidence
output$tb_incidence_rate <- renderInfoBox(

  infoBox(title = "TB incidence rate 2020",

          value = paste0(ftb(pdata()$epi_timeseries[pdata()$epi_timeseries$year == 2020, "e_inc_100k"]),
                         " per 100 000 population"),

          subtitle = HTML(paste0("Range ",
                                 ftb(pdata()$epi_timeseries[pdata()$epi_timeseries$year == 2020, "e_inc_100k_lo"]),
                                 "-",
                                 ftb(pdata()$epi_timeseries[pdata()$epi_timeseries$year == 2020, "e_inc_100k_hi"]),
                                 ". (",
                                 ftb(pdata()$epi_timeseries[pdata()$epi_timeseries$year == 2019, "e_inc_100k"]),
                                 " in 2019 ",
                                 pct_change_arrow(pdata()$epi_timeseries[pdata()$epi_timeseries$year == 2019, "e_inc_100k"],
                                                  pdata()$epi_timeseries[pdata()$epi_timeseries$year == 2020, "e_inc_100k"]),
                                 ")")
          ),

          icon = icon("head-side-cough"),

          color = "green"
  )
)

output$tb_incidence_num <- renderInfoBox(

  infoBox(title = "Number falling ill with TB (incidence number) 2020",

          value = paste0(ftb(pdata()$profile_estimates$e_inc_num),
                         " (",
                         relatable_number(pdata()$profile_estimates$e_inc_num),
                         ")"
          ),

          subtitle = paste0("Range ",
                            ftb(pdata()$profile_estimates$e_inc_num_lo),
                            "-",
                            ftb(pdata()$profile_estimates$e_inc_num_hi)),

          icon = icon("head-side-cough"),

          color = "green"
  )
)

# Notification
output$tb_notified_num <- renderInfoBox(

  infoBox(title = "People newly diagnosed and reported (notified) with TB 2020",

          value = ftb_na(pdata()$profile_data$c_newinc, int_spacer),

          subtitle = HTML(paste0("(",
                                 ftb_na(pdata()$profile_data$c_newinc_ym1, int_spacer),
                          " in 2019 ",
                          pct_change_arrow(pdata()$profile_data$c_newinc_ym1,
                                           pdata()$profile_data$c_newinc),
                          ")")
                      ),

          icon = icon("bed"),

          color = "teal"
  )
)

output$diagnosed_wrd <- renderInfoBox(

  infoBox(title = "WHO-recommended rapid diagnostics 2020",

          value = display_cap_pct(pdata()$profile_data$newinc_rdx,
                                  pdata()$profile_data$c_newinc),

          subtitle = "Percentage of people newly diagnosed with TB using WHO-recommended rapid diagnostics",

          icon = icon("desktop"),

          color = "teal"
  )
)

output$rrmdr_num <- renderInfoBox(

  infoBox(title = "People diagnosed with drug-resistant TB 2020",

          value = ftb_na(pdata()$profile_data$conf_rrmdr, int_spacer),

          subtitle = HTML(paste0("(",
                                 ftb_na(pdata()$profile_data$conf_rrmdr_ym1, int_spacer),
                                 " in 2019 ",
                                 pct_change_arrow(pdata()$profile_data$conf_rrmdr_ym1,
                                                  pdata()$profile_data$conf_rrmdr),
                                 ")")
          ),

          icon = icon("stethoscope"),

          color = "red"
  )
)

output$mdr_tx_num <- renderInfoBox(

  infoBox(title = "People started on treatment for drug-resistant TB 2020",

          value = ftb_na(pdata()$profile_data$mdr_tx, int_spacer),

          subtitle = HTML(paste0("(",
                                 ftb_na(pdata()$profile_data$mdr_tx_ym1, int_spacer),
                                 " in 2019 ",
                                 pct_change_arrow(pdata()$profile_data$mdr_tx_ym1,
                                                  pdata()$profile_data$mdr_tx),
                                 ")")
          ),

          icon = icon("bed"),

          color = "red"
  )
)


output$mdr_tx_short_num <- renderInfoBox(

  infoBox(title = "WHO-recommended shorter treatment regimens 2020",

          value = display_cap_pct(pdata()$profile_data$mdr_alloral_short_tx,
                                  pdata()$profile_data$mdr_tx),

          subtitle = "Percentage of people started on WHO-recommended shorter regimens for drug-resistant TB",

          icon = icon("pills"),

          color = "red"
  )
)

# Treatment success rates
output$tsr <- renderText(

  HTML(
    paste0(
      "<span style='font-size:150%;'><b>",
      pdata()$profile_data$c_new_tsr,
      "%</b></span> of those started TB treatment in 2019<br /><br /><br />",
       "<span style='font-size:150%;'><b>",
      pdata()$profile_data$c_mdr_tsr,
      "%</b></span> of those started drug-resistant TB treatment in 2018"
    )
  )

)


# TPT
output$tpt_num <- renderInfoBox(

  infoBox(title = "People started on TB preventive treatment 2020",

          value = ftb_na(sum(c(pdata()$tpt_timeseries[pdata()$tpt_timeseries$year==2020, "hiv"],
                               pdata()$tpt_timeseries[pdata()$tpt_timeseries$year==2020, "contact_04"],
                               pdata()$tpt_timeseries[pdata()$tpt_timeseries$year==2020, "contact_5plus"]),
                             na.rm = TRUE),
                         int_spacer),

          subtitle = HTML(paste0("(",
                                 ftb_na(sum(c(pdata()$tpt_timeseries[pdata()$tpt_timeseries$year==2019, "hiv"],
                                              pdata()$tpt_timeseries[pdata()$tpt_timeseries$year==2019, "contact_04"],
                                              pdata()$tpt_timeseries[pdata()$tpt_timeseries$year==2019, "contact_5plus"]),
                                            na.rm = TRUE),
                                        int_spacer),
                                 " in 2019",
                                 pct_change_arrow(sum(c(pdata()$tpt_timeseries[pdata()$tpt_timeseries$year==2019, "hiv"],
                                                        pdata()$tpt_timeseries[pdata()$tpt_timeseries$year==2019, "contact_04"],
                                                        pdata()$tpt_timeseries[pdata()$tpt_timeseries$year==2019, "contact_5plus"]),
                                                      na.rm = TRUE),
                                                  sum(c(pdata()$tpt_timeseries[pdata()$tpt_timeseries$year==2020, "hiv"],
                                                        pdata()$tpt_timeseries[pdata()$tpt_timeseries$year==2020, "contact_04"],
                                                        pdata()$tpt_timeseries[pdata()$tpt_timeseries$year==2020, "contact_5plus"]),
                                                      na.rm = TRUE)
                                                  ),
                                 ")")),

          icon = icon("tablets"),

          color = "green"
  )
)

# Budget
output$tb_budget <- renderInfoBox(

  infoBox(title = "National TB budget 2021",

          value = paste0(ftb(pdata()$profile_data$tot_req),
                         " US$ million"),

          subtitle = paste0(ftb(pdata()$profile_data$tot_gap),
                            " US$ million (",
                            display_cap_pct(pdata()$profile_data$tot_gap,
                                            pdata()$profile_data$tot_req),
                            ") of the budget was not funded"),

          icon = icon("coins"),

          color = "orange"
  )
)

