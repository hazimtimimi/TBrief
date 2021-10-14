# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Build outputs showing numbers
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Notifications by age group and sex

output$notifs_women <- renderText( ftb_na(pdata()$profile_data$c_newrel_women, int_spacer) )
output$notifs_men <- renderText( ftb_na(pdata()$profile_data$c_newrel_men, int_spacer) )
output$notifs_kids <- renderText( ftb_na(pdata()$profile_data$c_newrel_014, int_spacer) )


# Number of deaths
output$deaths_nh <- renderText( ftb_na(pdata()$profile_estimates$e_mort_exc_tbhiv_num, int_spacer) )
output$deaths_h  <- renderText( ftb_na(pdata()$profile_estimates$e_mort_tbhiv_num, int_spacer) )

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
