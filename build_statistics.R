# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Build outputs showing numbers
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# End TB Strategy milestones
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

output$milestone_deaths <- renderInfoBox(

  infoBox(title = "TB deaths 2020 vs 2015",
          subtitle = "(Target is 35% reduction)",
          value = pct_change_description(pdata()$epi_timeseries[pdata()$epi_timeseries$year == 2015, "e_mort_num"],
                                         pdata()$epi_timeseries[pdata()$epi_timeseries$year == 2020, "e_mort_num"]),
          icon = icon("skull")
          )
)

output$milestone_incidence <- renderInfoBox(

  infoBox(title = "TB incidence 2020 vs 2015",
          subtitle = "(Target is 20% reduction)",
          value = pct_change_description(pdata()$epi_timeseries[pdata()$epi_timeseries$year == 2015, "e_inc_100k"],
                                         pdata()$epi_timeseries[pdata()$epi_timeseries$year == 2020, "e_inc_100k"]),
          icon = icon("head-side-cough")
          )
)

output$milestone_catast <- renderInfoBox(

  infoBox(title = "Catastrophic costs",
          subtitle = "(Target is 0% of people with TB facing catastrophic costs by 2020)",
          value = ifelse(is.na(pdata()$profile_data[, "catast_pct"]),
                         "No data",
                         paste0(signif(pdata()$profile_data[, "catast_pct"], 2), "%")
                         ),
          icon = icon("balance-scale-left")
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

          icon = icon("skull")
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

          icon = icon("skull")
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

          subtitle = HTML("(yyy yyyy in 2019 <b>&darr;</b> -z%)"),

          icon = icon("bed"),

          color = "black"
  )
)

output$diagnosed_wrd <- renderInfoBox(

  infoBox(title = "WHO-recommended rapid diagnostics 2020",

          value = display_cap_pct(pdata()$profile_data$newinc_rdx,
                                  pdata()$profile_data$c_newinc),

          subtitle = "Percentage of people newly diagnosed with TB using WHO-recommended rapid diagnostics",

          icon = icon("desktop"),

          color = "black"
  )
)

output$rrmdr_num <- renderInfoBox(

  infoBox(title = "People diagnosed with drug-resistant TB 2020",

          value = ftb_na(pdata()$profile_data$conf_rrmdr, int_spacer),

          subtitle = HTML("(yyy in 2019 <b>&darr;</b> -z%)"),

          icon = icon("stethoscope"),

          color = "red"
  )
)

output$mdr_tx_num <- renderInfoBox(

  infoBox(title = "People started on treatment for drug-resistant TB 2020",

          value = ftb_na(pdata()$profile_data$mdr_tx, int_spacer),

          subtitle = HTML("(yyy in 2019 <b>&darr;</b> -z%)"),

          icon = icon("bed"),

          color = "red"
  )
)


output$mdr_tx_short_num <- renderInfoBox(

  infoBox(title = "WHO-recommended shorter treatment regimens 2020",

          value = "xx%",

          subtitle = "Percentage of people started on WHO-recommended shorter regimens for drug-resistant TB",

          icon = icon("pills"),

          color = "purple"
  )
)

# TB cascade
output$tb_cascade_text <- renderText(

  paste0("<span class='stats'>",
         ftb_na(pdata()$profile_estimates$e_inc_num, int_spacer),
         "</span>",
         relatable_num(pdata()$profile_estimates$e_inc_num),
         " fell ill with TB in 2020<br /><br />",

         "<span class='stats'>",
         ftb_na(pdata()$profile_data$c_newinc, int_spacer),
         "</span>",
         ifelse(!is.na(pdata()$profile_data$c_newinc),
                relatable_num(pdata()$profile_data$c_newinc),
                ""),
         " notified with TB in 2020<br /><br />",

         "<span class='stats'>",
         ftb_na(pdata()$profile_data$c_new_tsr, fn=function(x) paste0(x, "%")),
         "</span> successfully treated in 2019")
)


# TB/HIV cascade
output$tbhiv_cascade_text <- renderText(

  paste0("<span class='stats'>",
         ftb_na(pdata()$profile_estimates$e_inc_tbhiv_num, int_spacer),
         "</span> fell ill with TB in 2020<br /><br />",

         "<span class='stats'>",
         ftb_na(pdata()$profile_data$newrel_hivpos, int_spacer),
         "</span> notified with TB in 2020<br /><br />",

         "<span class='stats'>",
         ftb_na(pdata()$profile_data$c_tbhiv_tsr, fn=function(x) paste0(x, "%")),
         "</span> successfully treated in 2019")
)


# DR-TB cascade
output$drtb_cascade_text <- renderText(

  paste0("<span class='stats'>",
         ftb_na(pdata()$profile_data$conf_rrmdr, int_spacer),
         "</span> detected in 2020<br /><br />",

         "<span class='stats'>",
         ftb_na(pdata()$profile_data$mdr_tx, int_spacer),
         "</span> started treatment in 2020<br /><br />",

         "<span class='stats'>",
         ftb_na(pdata()$profile_data$c_mdr_tsr, fn=function(x) paste0(x, "%")),
         "</span> successfully treated in 2018")
)


# Number of deaths
output$deaths_text <- renderText(

  paste0("<span class='stats'>",
         ftb_na(pdata()$profile_estimates$e_mort_exc_tbhiv_num, int_spacer),
         "</span>",
         ftb_na(pdata()$profile_estimates$e_mort_exc_tbhiv_num, relatable_num),
         " in people without HIV<br/><br/>",

         "<span class='stats'>",
         ftb_na(pdata()$profile_estimates$e_mort_tbhiv_num, int_spacer),
         "</span>",
         ftb_na(pdata()$profile_estimates$e_mort_tbhiv_num, relatable_num),
         " in people living with HIV")
)


# Notifications by age group and sex
output$notifs_text <- renderText(

  paste0("<span class='stats'>",
         ftb_na(pdata()$profile_data$c_newrel_014, int_spacer),
         "</span> children aged under 15 years<br /><br />",

         "<span class='stats'>",
         ftb_na(pdata()$profile_data$c_newrel_women, int_spacer),
         "</span> women aged 15 years and over<br /><br />",

         "<span class='stats'>",
         ftb_na(pdata()$profile_data$c_newrel_men, int_spacer),
         "</span> men aged 15 years and over<br /><br />")
)


# Cases attributable to 5 risk factors
output$attributable_cases <- renderText(

  paste0("<br/><span class='stats'>",
         ftb_na(pdata()$attributable_cases[pdata()$attributable_cases$risk_factor=="alc", "best"]),
         "</span> due to harmful use of alcohol<br /><br />",

         "<span class='stats'>",
         ftb_na(pdata()$attributable_cases[pdata()$attributable_cases$risk_factor=="dia", "best"]),
         "</span> due to diabetes<br /><br />",

         "<span class='stats'>",
         ftb_na(pdata()$attributable_cases[pdata()$attributable_cases$risk_factor=="hiv", "best"]),
         "</span> due to HIV<br /><br />",

         "<span class='stats'>",
         ftb_na(pdata()$attributable_cases[pdata()$attributable_cases$risk_factor=="smk", "best"]),
         "</span> due to smoking<br /><br />",

         "<span class='stats'>",
         ftb_na(pdata()$attributable_cases[pdata()$attributable_cases$risk_factor=="und", "best"]),
         "</span> due to undernourishment<br /><br />")
)



# Provision of TB preventive treatment
output$tpt_text <- renderText(

  paste0("<span class='stats'>",
         ftb_na(pdata()$profile_data$hiv_tpt, int_spacer),
         "</span> to people living with HIV<br /><br />",

         "<span class='stats'>",
         ftb_na(pdata()$profile_data$contacts_04_tpt, int_spacer),
         "</span> to household contacts (aged 0-4 years) of people with TB<br /><br />",

         "<span class='stats'>",
         ftb_na(pdata()$profile_data$contacts_5plus_tpt, int_spacer),
         "</span> to household contacts (aged 5 years and above) of people with TB")
)


# National TB budget
output$finance_text <- renderText(

  paste0("<span class='stats'>",
         ftb_na(pdata()$profile_data$tot_req, int_spacer),
         "</span> million US$ required<br /><br />",

         "<span class='stats'>",
         ftb_na(pdata()$profile_data$tot_domestic, int_spacer),
         "</span> million US$ available from domestic funds<br /><br />",

         "<span class='stats'>",
         ftb_na(pdata()$profile_data$tot_international, int_spacer),
         "</span> million US$ available from international funds<br /><br />",

         "<span class='stats'>",
         ftb_na(pdata()$profile_data$tot_gap, int_spacer),
         "</span> million US$ funding gap")
)


output$pcs_text <- renderText(

  ifelse(!is.na(pdata()$profile_data[, "catast_pct"]),

         paste0("<span class='stats'>",
                pdata()$profile_data$catast_pct,
                "%</span> of TB patients experienced catastrophic total costs (survey conducted in ",
                pdata()$profile_data$catast_survey_year,
                ")"),

         "No survey results available")

)
