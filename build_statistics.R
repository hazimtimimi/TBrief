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

  infoBox(title = paste("TB deaths", dcyear-1, "vs 2015"),
          subtitle = "(Target is 35% reduction by 2020)",
          value = pct_change_description(pdata()$epi_timeseries[pdata()$epi_timeseries$year == 2015, "e_mort_num"],
                                         pdata()$epi_timeseries[pdata()$epi_timeseries$year == dcyear-1, "e_mort_num"]),
          icon = icon("people-carry")
          )
)

output$milestone_incidence <- renderInfoBox(

  infoBox(title = paste("TB incidence", dcyear-1, "vs 2015"),
          subtitle = "(Target is 20% reduction by 2020)",
          value = pct_change_description(pdata()$epi_timeseries[pdata()$epi_timeseries$year == 2015, "e_inc_100k"],
                                         pdata()$epi_timeseries[pdata()$epi_timeseries$year == dcyear-1, "e_inc_100k"]),
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


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Indicators
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# TB deaths
output$tb_deaths_num <- renderInfoBox(

  infoBox(title = paste("Number of TB deaths", dcyear-1),

          value = paste0(ftb(pdata()$epi_timeseries[pdata()$epi_timeseries$year == dcyear-1, "e_mort_num"]),
                        " (",
                        relatable_number(pdata()$epi_timeseries[pdata()$epi_timeseries$year == dcyear-1, "e_mort_num"]),
                        ")"
                        ),

          subtitle = HTML(paste0("Range ",
                                 ftb(pdata()$epi_timeseries[pdata()$epi_timeseries$year == dcyear-1, "e_mort_num_lo"]),
                                 "-",
                                 ftb(pdata()$epi_timeseries[pdata()$epi_timeseries$year == dcyear-1, "e_mort_num_hi"]),
                                 ". (",
                                 ftb(pdata()$epi_timeseries[pdata()$epi_timeseries$year == dcyear-2, "e_mort_num"]),
                                 " in ", dcyear-2, " ",
                                 pct_change_arrow(pdata()$epi_timeseries[pdata()$epi_timeseries$year == dcyear-2, "e_mort_num"],
                                                  pdata()$epi_timeseries[pdata()$epi_timeseries$year == dcyear-1, "e_mort_num"]),
                                 ")")
                          ),

          icon = icon("people-carry")
  )
)

output$tb_deaths_rate <- renderInfoBox(

  infoBox(title = paste("TB death rate", dcyear-1),

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

  infoBox(title = paste("TB incidence rate", dcyear-1),

          value = paste0(ftb(pdata()$epi_timeseries[pdata()$epi_timeseries$year == dcyear-1, "e_inc_100k"]),
                         " per 100 000 population"),

          subtitle = HTML(paste0("Range ",
                                 ftb(pdata()$epi_timeseries[pdata()$epi_timeseries$year == dcyear-1, "e_inc_100k_lo"]),
                                 "-",
                                 ftb(pdata()$epi_timeseries[pdata()$epi_timeseries$year == dcyear-1, "e_inc_100k_hi"]),
                                 ". (",
                                 ftb(pdata()$epi_timeseries[pdata()$epi_timeseries$year == dcyear-2, "e_inc_100k"]),
                                 " in ", dcyear-2, " ",
                                 pct_change_arrow(pdata()$epi_timeseries[pdata()$epi_timeseries$year == dcyear-2, "e_inc_100k"],
                                                  pdata()$epi_timeseries[pdata()$epi_timeseries$year == dcyear-1, "e_inc_100k"]),
                                 ")")
          ),

          icon = icon("head-side-cough"),

          color = "green"
  )
)

output$tb_incidence_num <- renderInfoBox(

  infoBox(title = paste("Number falling ill with TB (incidence number)", dcyear-1),

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

  infoBox(title = paste("People newly diagnosed and reported (notified) with TB", dcyear-1),

          value = ftb_na(pdata()$profile_data$c_newinc, int_spacer),

          subtitle = HTML(paste0("(",
                                 ftb_na(pdata()$profile_data$c_newinc_ym1, int_spacer),
                          " in ", dcyear-2, " ",
                          pct_change_arrow(pdata()$profile_data$c_newinc_ym1,
                                           pdata()$profile_data$c_newinc),
                          ")")
                      ),

          icon = icon("bed"),

          color = "teal"
  )
)

output$diagnosed_wrd <- renderInfoBox(

  infoBox(title = paste("WHO-recommended rapid diagnostics", dcyear-1),

          value = display_cap_pct(pdata()$profile_data$newinc_rdx,
                                  pdata()$profile_data$c_newinc),

          subtitle = "Percentage of people newly diagnosed with TB using WHO-recommended rapid diagnostics",

          icon = icon("desktop"),

          color = "teal"
  )
)

output$dr_num <- renderInfoBox(

  infoBox(title = paste("People diagnosed with drug-resistant TB", dcyear-1),

          value = ftb_na(sum(c(pdata()$profile_data$conf_rr_nfqr,
                               pdata()$profile_data$conf_rr_fqr),
                             na.rm = TRUE),
                         int_spacer),

          subtitle = HTML(paste0("(",
                                 ftb_na(sum(c(pdata()$profile_data$conf_rr_nfqr_ym1,
                                              pdata()$profile_data$conf_rr_fqr_ym1),
                                            na.rm = TRUE), int_spacer),
                                 " in ", dcyear-2," ",
                                 pct_change_arrow(sum(c(pdata()$profile_data$conf_rr_nfqr_ym1,
                                                        pdata()$profile_data$conf_rr_fqr_ym1),
                                                      na.rm = TRUE),
                                                  sum(c(pdata()$profile_data$conf_rr_nfqr,
                                                        pdata()$profile_data$conf_rr_fqr),
                                                      na.rm = TRUE)
                                                  ),
                                 ")")
          ),

          icon = icon("stethoscope"),

          color = "red"
  )
)

output$dr_tx_num <- renderInfoBox(

  infoBox(title = paste("People started on treatment for drug-resistant TB", dcyear-1),

          value = ftb_na(sum(c(pdata()$profile_data$unconf_rr_nfqr_tx,
                               pdata()$profile_data$conf_rr_nfqr_tx,
                               pdata()$profile_data$conf_rr_fqr_tx),
                             na.rm = TRUE),
                         int_spacer),

          subtitle = HTML(paste0("(",
                                 ftb_na(sum(c(pdata()$profile_data$unconf_rr_nfqr_tx_ym1,
                                              pdata()$profile_data$conf_rr_nfqr_tx_ym1,
                                              pdata()$profile_data$conf_rr_fqr_tx_ym1),
                                            na.rm = TRUE),
                                        int_spacer),
                                 " in ", dcyear-2, " ",
                                 pct_change_arrow(sum(c(pdata()$profile_data$unconf_rr_nfqr_tx_ym1,
                                                        pdata()$profile_data$conf_rr_nfqr_tx_ym1,
                                                        pdata()$profile_data$conf_rr_fqr_tx_ym1),
                                                      na.rm = TRUE),
                                                  sum(c(pdata()$profile_data$unconf_rr_nfqr_tx,
                                                        pdata()$profile_data$conf_rr_nfqr_tx,
                                                        pdata()$profile_data$conf_rr_fqr_tx),
                                                      na.rm = TRUE)),
                                 ")")
          ),

          icon = icon("bed"),

          color = "red"
  )
)


output$mdr_tx_short_num <- renderInfoBox(

  infoBox(title = paste("WHO-recommended shorter treatment regimens", dcyear-1),

          value = display_cap_pct(pdata()$profile_data$mdr_alloral_short_tx,
                                  sum(c(pdata()$profile_data$unconf_rr_nfqr_tx,
                                        pdata()$profile_data$conf_rr_nfqr_tx,
                                        pdata()$profile_data$conf_rr_fqr_tx),
                                      na.rm = TRUE)),

          subtitle = "Percentage of people started on WHO-recommended shorter regimens for drug-resistant TB",

          icon = icon("pills"),

          color = "red"
  )
)

# Treatment success rates
output$tsr <- renderText(

  HTML(
    ifelse(isTruthy(pdata()$profile_data$c_new_tsr),
           paste0(
             "<span style='font-size:150%;'><b>",
             pdata()$profile_data$c_new_tsr,
             "%</b></span> of those started TB treatment in ",
             dcyear-2,
             "<br /><br /><br />",
             "<span style='font-size:150%;'><b>",
             pdata()$profile_data$c_mdr_tsr,
             "%</b></span> of those started drug-resistant TB treatment in ",
             dcyear-3
           ),
           "")
    )

)


# TPT
output$tpt_num <- renderInfoBox(

  infoBox(title = paste("People started on TB preventive treatment", dcyear-1),

          value = ftb_na(sum(c(pdata()$tpt_timeseries[pdata()$tpt_timeseries$year==dcyear-1, "hiv"],
                               pdata()$tpt_timeseries[pdata()$tpt_timeseries$year==dcyear-1, "contact_04"],
                               pdata()$tpt_timeseries[pdata()$tpt_timeseries$year==dcyear-1, "contact_5plus"]),
                             na.rm = TRUE),
                         int_spacer),

          subtitle = HTML(paste0("(",
                                 ftb_na(sum(c(pdata()$tpt_timeseries[pdata()$tpt_timeseries$year==dcyear-2, "hiv"],
                                              pdata()$tpt_timeseries[pdata()$tpt_timeseries$year==dcyear-2, "contact_04"],
                                              pdata()$tpt_timeseries[pdata()$tpt_timeseries$year==dcyear-2, "contact_5plus"]),
                                            na.rm = TRUE),
                                        int_spacer),
                                 " in ", dcyear-2, " ",
                                 pct_change_arrow(sum(c(pdata()$tpt_timeseries[pdata()$tpt_timeseries$year==dcyear-2, "hiv"],
                                                        pdata()$tpt_timeseries[pdata()$tpt_timeseries$year==dcyear-2, "contact_04"],
                                                        pdata()$tpt_timeseries[pdata()$tpt_timeseries$year==dcyear-2, "contact_5plus"]),
                                                      na.rm = TRUE),
                                                  sum(c(pdata()$tpt_timeseries[pdata()$tpt_timeseries$year==dcyear-1, "hiv"],
                                                        pdata()$tpt_timeseries[pdata()$tpt_timeseries$year==dcyear-1, "contact_04"],
                                                        pdata()$tpt_timeseries[pdata()$tpt_timeseries$year==dcyear-1, "contact_5plus"]),
                                                      na.rm = TRUE)
                                                  ),
                                 ")")),

          icon = icon("tablets"),

          color = "green"
  )
)

# Funding
output$tb_funding <- renderInfoBox(

  infoBox(title = paste("Funding for TB", dcyear-1),

          value = paste0(ftb_na(sum(c(pdata()$funding_timeseries[pdata()$funding_timeseries$year == dcyear-1, "a_domestic_funds"],
                                      pdata()$funding_timeseries[pdata()$funding_timeseries$year == dcyear-1, "b_international_funds"]),
                                    na.rm = TRUE),
                                int_spacer),
                         " US$ million"),

          subtitle = HTML(paste0("(",
                                 ftb_na(sum(c(pdata()$funding_timeseries[pdata()$funding_timeseries$year == dcyear-2, "a_domestic_funds"],
                                              pdata()$funding_timeseries[pdata()$funding_timeseries$year == dcyear-2, "b_international_funds"]),
                                            na.rm = TRUE),
                                        int_spacer),
                                 " US$ million in ", dcyear-2, " ",
                                 pct_change_arrow(sum(c(pdata()$funding_timeseries[pdata()$funding_timeseries$year == dcyear-2, "a_domestic_funds"],
                                                        pdata()$funding_timeseries[pdata()$funding_timeseries$year == dcyear-2, "b_international_funds"]),
                                                      na.rm = TRUE),
                                                  sum(c(pdata()$funding_timeseries[pdata()$funding_timeseries$year == dcyear-1, "a_domestic_funds"],
                                                        pdata()$funding_timeseries[pdata()$funding_timeseries$year == dcyear-1, "b_international_funds"]),
                                                      na.rm = TRUE)
                                 ),
                                 ")")),

          icon = icon("coins"),

          color = "orange"
  )
)
