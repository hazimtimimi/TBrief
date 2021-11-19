# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Build outputs showing numbers
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# TB cascade
# - - - - - -

output$tb_inc <- renderText( ftb_na(pdata()$profile_estimates$e_inc_num, int_spacer) )

output$tb_inc_txt <- renderText(

  paste0(relatable_num(pdata()$profile_estimates$e_inc_num),
         " fell ill with TB in 2020")
)

output$tb_notif <- renderText( ftb_na(pdata()$profile_data$c_newinc, int_spacer) )

output$tb_notif_txt <- renderText(

  ifelse(!is.na(pdata()$profile_data$c_newinc),
         relatable_num(pdata()$profile_data$c_newinc),
         "")
)

output$tb_tsr_pct <- renderText( ftb_na(pdata()$profile_data$c_new_tsr, fn=function(x) paste0(x, "%")) )


# TB/HIV cascade
# - - - - - - - -

output$tbhiv_inc <- renderText( ftb_na(pdata()$profile_estimates$e_inc_tbhiv_num, int_spacer) )
output$tbhiv_notif <- renderText( ftb_na(pdata()$profile_data$newrel_hivpos, int_spacer) )
output$tbhiv_tsr_pct <- renderText( ftb_na(pdata()$profile_data$c_tbhiv_tsr, fn=function(x) paste0(x, "%")) )


# DR-TB cascade
# - - - - - - - -

output$drtb_detect <- renderText( ftb_na(pdata()$profile_data$conf_rrmdr, int_spacer) )
output$drtb_enrol <- renderText( ftb_na(pdata()$profile_data$mdr_tx, int_spacer) )
output$drtb_tsr_pct <- renderText( ftb_na(pdata()$profile_data$c_mdr_tsr, fn=function(x) paste0(x, "%")) )


# Number of deaths
output$deaths_nh <- renderText( ftb_na(pdata()$profile_estimates$e_mort_exc_tbhiv_num, int_spacer) )
output$deaths_nh_relatable <- renderText( ftb_na(pdata()$profile_estimates$e_mort_exc_tbhiv_num, relatable_num) )

output$deaths_h  <- renderText( ftb_na(pdata()$profile_estimates$e_mort_tbhiv_num, int_spacer) )
output$deaths_h_relatable <- renderText( ftb_na(pdata()$profile_estimates$e_mort_tbhiv_num, relatable_num) )



# Notifications by age group and sex
output$notifs_women <- renderText( ftb_na(pdata()$profile_data$c_newrel_women, int_spacer) )
output$notifs_men <- renderText( ftb_na(pdata()$profile_data$c_newrel_men, int_spacer) )
output$notifs_kids <- renderText( ftb_na(pdata()$profile_data$c_newrel_014, int_spacer) )


# Cases attributable to 5 risk factors
# Round the numbers

output$alc <- renderText(
  ftb_na(pdata()$attributable_cases[pdata()$attributable_cases$risk_factor=="alc", "best"])
)
output$dia <- renderText(
  ftb_na(pdata()$attributable_cases[pdata()$attributable_cases$risk_factor=="dia", "best"])
)
output$hiv <- renderText(
  ftb_na(pdata()$attributable_cases[pdata()$attributable_cases$risk_factor=="hiv", "best"])
)
output$smk <- renderText(
  ftb_na(pdata()$attributable_cases[pdata()$attributable_cases$risk_factor=="smk", "best"])
)
output$und <- renderText(
  ftb_na(pdata()$attributable_cases[pdata()$attributable_cases$risk_factor=="und", "best"])
)

# Provision of TB preventive treatment
output$hiv_tpt <- renderText( ftb_na(pdata()$profile_data$hiv_tpt, int_spacer) )
output$contacts_04_tpt <- renderText( ftb_na(pdata()$profile_data$contacts_04_tpt, int_spacer) )
output$contacts_5plus_tpt <- renderText( ftb_na(pdata()$profile_data$contacts_5plus_tpt, int_spacer) )

# National TB budget
output$tot_req <- renderText( ftb_na(pdata()$profile_data$tot_req, int_spacer) )
output$tot_domestic <- renderText( ftb_na(pdata()$profile_data$tot_domestic, int_spacer) )
output$tot_international <- renderText( ftb_na(pdata()$profile_data$tot_international, int_spacer) )
output$tot_gap <- renderText( ftb_na(pdata()$profile_data$tot_gap, int_spacer) )

# Patient cost survey results
output$catast <- renderText(

  ifelse(!is.na(pdata()$profile_data[, "catast_pct"]),
         paste0(pdata()$profile_data$catast_pct,
                "%"),
         "")
)

output$catast_description <- renderText(

  ifelse(!is.na(pdata()$profile_data[, "catast_pct"]),
         paste0("of TB patients experienced catastrophic total costs (survey conducted in ",
                pdata()$profile_data$catast_survey_year,
                ")"),
         "No survey completed yet")
)
