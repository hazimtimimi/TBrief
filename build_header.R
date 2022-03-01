# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Build header to show country name and population
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

output$main_heading <- renderText({

    req(pdata()$profile_properties)

    paste("Tuberculosis in",
          ifelse(check_entity_type(input$entity_type) == "group",
          sub("Global", "the world", pdata()$profile_properties[, "group_description"]),
          # Default is country
          pdata()$profile_properties[, "country"]))

} )

output$population <- renderText({

    # Make sure data are loaded
    req(pdata()$profile_estimates)

    paste0("Population ",
           dcyear - 1,
           ": ",
           int_spacer(pdata()$profile_estimates[, "e_pop_num"]/1e6),
           " million")
})

output$detailed_profile <- renderText({

    base_url <- "https://worldhealthorg.shinyapps.io/tb_profiles/"

    if (check_entity_type(input$entity_type) == "group") {
        url <- paste0(base_url, '?_inputs_&lan="EN"&entity_type="group"&group_code="', input$group_code, '"')
    } else {
        url <- paste0(base_url, '?_inputs_&entity_type="country"&lan="EN"&iso2="', input$iso2, '"')
    }

    paste("<p>See also <a href='",
          url,
          "' target='_blank'>the detailed TB profile</a></p>")

})
