# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Display TB country and group (regional/global) summary data on TB using JSON data
# retrieved from the WHO global tuberculosis database
# Hazim Timimi, November 2021 - May 2022
#               Updated August 2025 to change country code input paramater from iso2 tp
#                     iso3 code as was done for the main profiles shiny app(Version 2.0)
#               Updated October 2025 to produce compact profiles for entities using the compact
#                     data collection form and for which disaggregated estimates are not produced.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

app_version <- "Version 3.1"

library(shiny)
library(shinydashboard)
library(jsonlite)
library(dplyr)
library(tidyr)
library(gtbreport)
library(ggplot2)


# get the data collection year and list of countries and groups. This is only called once (not reactive)
# and is shared across all sessions.
# Make sure to specify the data being read are UTF-8 encoded
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

json_url <- "https://extranet.who.int/tme/generateJSON.asp"

year_countries <- fromJSON(readLines(paste0(json_url, "?ds=countries"), warn = FALSE, encoding = "UTF-8"))

dcyear <- year_countries$dcyear
countries <- year_countries$countries
rm(year_countries)

# Get the groups
aggregate_groups <- fromJSON(readLines(paste0(json_url, "?ds=groups"), warn = FALSE, encoding = "UTF-8"))
aggregate_groups <- aggregate_groups$groups

# Load general, non-reactive functions
source("general_functions.R")


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Web interface code
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

ui <- dashboardPage(

    dashboardHeader(title = "Summary of tuberculosis data",
                    titleWidth = 300),
    dashboardSidebar(disable = TRUE),
    dashboardBody(


        fluidPage(
            title = "Summary of tuberculosis data",

            # ADD a link to a print-specific CSS to hide selectors and metadata and to
            # retain two columns when printing
            # and a tbrief-specific sylesheet for named elements

            tags$head(
                tags$link(rel = "stylesheet", type = "text/css", href = "tbrief.css"),
                tags$link(rel = "stylesheet", type = "text/css", href = "tbrief_print.css", media = "print")
            ),

            fluidRow(id = "selectors",

                     column(
                         width = 2,
                             uiOutput(outputId = "entitytypes")
                     ),

                     # Choose whether to show the list of countries or the list of groups
                     # based on the choice made above

                     column(
                         width = 10,
                             uiOutput(outputId = "entities")
                     )
                ),


            fluidRow(
                id = "header",
                #Add a full width column so that contents align with those of subsequent rows
                column(
                    width = 12,

                    # Header  ----
                    textOutput(outputId = "main_heading", container = h1),
                    infoBoxOutput(outputId = "population"),
                    htmlOutput(outputId = "detailed_profile")
                )
            ),

            fluidRow(
                id = "milestones",
                #Add a full width column so that contents align with those of subsequent rows
                column(
                    width = 12,

                    # Milestones  ----

                    htmlOutput(outputId = "end_tb_strategy_milestones"),

                    infoBoxOutput(outputId = "milestone_deaths"),
                    infoBoxOutput(outputId = "milestone_incidence"),
                    infoBoxOutput(outputId = "milestone_catast")
                )
            ),

                # Indicators section ----

                # Mortality and incidence row ----
                fluidRow(
                    column(
                        width = 6,
                        box(
                            title = "TB deaths (including in people living with HIV)",
                            solidHeader = TRUE,
                            status = "info",
                            width = 12,
                            plotOutput(outputId = "mortality_chart", height = "200px")
                        ),

                        infoBoxOutput(outputId = "tb_deaths_num", width = 12),

                        infoBoxOutput(outputId = "tb_deaths_rate", width = 12)
                    ),
                    column(
                        width = 6,
                        box(
                            title = "People falling ill with TB (including drug-resistant TB)",
                            solidHeader = TRUE,
                            # Weird that we need to use a status of "success" instead
                            # of simply specifying a colour. Of course could use CSS
                            # but this is quicker ... just don;t read anything into the
                            # idea of "success"!
                            status = "success",
                            width = 12,
                            plotOutput(outputId = "incidence_chart", height = "200px")
                        ),

                        infoBoxOutput(outputId = "tb_incidence_num", width = 12),

                        infoBoxOutput(outputId = "tb_incidence_rate", width = 12)
                    )
                ),

                # Diagnosis and treatment row ----
                fluidRow(
                    column(
                        width = 6,

                        infoBoxOutput(outputId = "tb_notified_num", width = 12),

                        infoBoxOutput(outputId = "diagnosed_wrd", width = 12),

                        # Only show the other info boxes if the entity does not use the compact form
                        conditionalPanel(condition = "output.compact_form == 0",

                                         infoBoxOutput(outputId = "dr_num", width = 12),

                                         infoBoxOutput(outputId = "dr_tx_num", width = 12),

                                         infoBoxOutput(outputId = "mdr_tx_short_num", width = 12)
                        )

                    ),

                    column(
                        width = 6,

                        # The cases attributable to 5 risk factors estimates is not shown for all countries
                        conditionalPanel(condition = "output.show_attributable_cases == 1",
                                         box(
                                             title = paste0("Why did people fall ill with TB in ", dcyear-1, "?"),
                                             solidHeader = TRUE,
                                             status = "success",
                                             width = 12,
                                             plotOutput(outputId = "rf_chart", height = "200px")
                                         )
                        ),

                        # Hide treatment success rate chart if there are no data to show
                        conditionalPanel(condition = "output.show_outcomes == 1",
                                         box(
                                             title = "TB treatment success",
                                             solidHeader = TRUE,
                                             status = "primary",
                                             width = 12,

                                             tags$div(
                                                 style = "position: relative;",

                                                 plotOutput(outputId =  "tsr_chart", height = "170px"),

                                                 # Next DIV allows the text to appear over the chart image
                                                 tags$div(style = "position: absolute; top: 20px; left: 1em; font-size:120%;",

                                                          htmlOutput(outputId = "tsr", inline = TRUE))
                                             ),
                                         )
                        )
                    )
                ),

                # TPT and finance row ----
              # Only show TPT and finance row if the entity does not use the compact form
            conditionalPanel(condition = "output.compact_form == 0",
                             fluidRow(
                               column(
                                 width = 6,
                                 # Hide the TPT chart if there are no data to show
                                 conditionalPanel(condition = "output.show_tpt == 1",
                                                  box(
                                                    title = "People started on TB preventive treatment",
                                                    solidHeader = TRUE,
                                                    status = "success",
                                                    width = 12,
                                                    plotOutput(outputId = "tpt_chart", height = "200px")
                                                  )
                                 ),

                                 infoBoxOutput(outputId = "tpt_num", width = 12)

                               ),

                               column(
                                 width = 6,
                                 # The TB financing information is not shown for all countries
                                 conditionalPanel(condition = "output.show_finance == 1",
                                                  box(
                                                    title = "Funding for TB",
                                                    solidHeader = TRUE,
                                                    status = "warning",
                                                    width = 12,
                                                    plotOutput(outputId = "funding_chart", height = "200px")
                                                  ),

                                                  infoBoxOutput(outputId = "tb_funding", width = 12)
                                 )
                               )
                             )
            ),

                # Footer ----
                htmlOutput(outputId = "generation"),

                # Add an "About" bit of metadata
                HTML(paste0("<div id='metadata'>",
                            "<i>",
                            app_version,
                            ", Source code on <a href='https://github.com/hazimtimimi/TBrief' target='_blank'>Github</a>. ",
                            "Data are also available on the TB Report app for <a href='https://apps.apple.com/us/app/tb-report/id1483112411' target='_blank'>iOS</a> and
                      <a href='https://play.google.com/store/apps/details?id=uk.co.adappt.whotbreport&hl=en_us' target='_blank'>Android</a>
                      and as <a href='https://www.who.int/teams/global-tuberculosis-programme/data' target='_blank'>CSV files</a>.</i><br /><br /></div>"))


        )
    )
)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Backend server code (called each time a new session is initiated)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

server <- function(input, output, session) {


    # Include bookmarks in the URL
    source("bookmark_url.R", local = TRUE)

    # Make sure language choice is valid in case someone messes with URL
    # Note that Shiny will change input$lan to null if it doesn't match the
    # available choices, so use general function check_lan()

    # Create the entity type radio button selector using the chosen language
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    type_name_list <- reactive({

        country <- "Country"
        group <- "Group"

        # Return a list
        return(list(country, group))

    })

    # Build the entity type radio buttons
    output$entitytypes <- renderUI({

        radioButtons(inputId = "entity_type",
                     label = "",
                     choiceNames = type_name_list(),
                     choiceValues = list("country", "group"),

                     # next line needed to avoid losing the selected country when the language is changed
                     selected = input$entity_type,
                     inline = FALSE)
    })


    # Create the country or group selection lists in the selected language
    source("select_entity.R", local = TRUE)

    # Convert an iso3 code passed as a parameter to an iso2 code ----
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    iso2 <- reactive({
      req(input$iso3)

      iso2 <- countries |>
        filter(iso3==input$iso3) |>
        select(iso2) |>
        as.character()

      return(iso2)
    })


    # Get the profile data as a JSON file for the chosen country or group
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    pdata <- reactive({

        if (check_entity_type(input$entity_type) == "group") {
            url <- paste0(json_url, "?ds=group_data&group_code=", input$group_code)
        } else {
            url <- paste0(json_url, "?ds=data&iso2=", iso2())
        }

       json <- fromJSON(readLines(url, warn = FALSE, encoding = 'UTF-8'))
       return(json)
    })


    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # See if the entity only used the compact form introduced in 2025. In such cases
    # only need to show a few data items
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    output$compact_form <- reactive ({

      req(pdata()$profile_properties)

      if (pdata()$profile_properties[, "dc_form_description"] == "Compact form"){
        result <- 1
      } else {
        result <- 0
      }

      return(result)
    })

    # Need this to make sure browser can switch attributable cases  elements back on again if hidden
    outputOptions(output, "compact_form", suspendWhenHidden = FALSE)


    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # Define the 2025 End TB Strategy milestones ----
    #
    # - a 50% drop in incidence per 100,000 population compared to 2015
    # - a 75% drop in total TB deaths (HIV-negative + HIV-positive) compared to 2015
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    milestone_yr <- "2025"
    inc_milestone_vs_2015 <- 0.5     # a 50% drop in incidence rate compared to 2015
    mort_milestone_vs_2015 <- 0.25   # a 75% drop in total TB deaths compared to 2015

    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # Build all the output objects to display in the application
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    # Header: show country name and population
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    output$main_heading <- renderText({

        req(pdata()$profile_properties)

        paste("Tuberculosis in",
              ifelse(check_entity_type(input$entity_type) == "group",
                     sub("Global", "the world", pdata()$profile_properties[, "group_description"]),
                     # Default is country
                     pdata()$profile_properties[, "country"]))

    } )

    output$detailed_profile <- renderText({

        base_url <- "https://worldhealthorg.shinyapps.io/tb_profiles/"

        if (check_entity_type(input$entity_type) == "group") {
            url <- paste0(base_url, '?_inputs_&lan="EN"&entity_type="group"&group_code="', input$group_code, '"')
        } else {
            url <- paste0(base_url, '?_inputs_&entity_type="country"&lan="EN"&iso3="', input$iso3, '"')
        }

        paste("<h3><br />See also <a href='",
              url,
              "' target='_blank'>the detailed TB profile</a><br /><br />&nbsp;</h3>")
    })


    output$end_tb_strategy_milestones <- renderText({ paste("<h2>End TB Strategy milestones for", milestone_yr, "</h2>") })

    source("build_statistics.R", local = TRUE)

    source("build_charts.R", local = TRUE)

    # Show or hide the cases attributable to 5 risk factors
    # Some entities don't have these estimates and the attributable_cases
    # part of the profile data will be null rather than a data frame.
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    output$show_attributable_cases <- reactive ({

        req(pdata()$attributable_cases)

        if (is.data.frame(pdata()$attributable_cases)){
            result <- 1
        } else {
            result <- 0
        }

        return(result)
    })


    # Need this to make sure browser can switch attributable cases  elements back on again if hidden
    outputOptions(output, "show_attributable_cases", suspendWhenHidden = FALSE)

    # Hide the treatment success rates chart if there are no data to show
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    output$show_outcomes <- reactive ({

        req(pdata()$profile_data$c_new_tsr)

        if (!is.na(pdata()$profile_data$c_new_tsr)){
            result <- 1
        } else {
            result <- 0
        }

        return(result)
    })


    # Need this to make sure browser can switch success rate elements back on again if hidden
    outputOptions(output, "show_outcomes", suspendWhenHidden = FALSE)


    # Hide the TPT chart if there are no data to show
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    output$show_tpt <- reactive ({

        req(pdata()$tpt_timeseries)

        # Make sure total number on TPT is > 0

        tpt_tot <- pdata()$tpt_timeseries |>
            summarise(across(-year,  \(x) sum(x, na.rm = TRUE))) |>
            rowSums(na.rm = TRUE)

        if (tpt_tot > 0){
            result <- 1
        } else {
            result <- 0
        }

        return(result)
    })


    # Need this to make sure browser can switch the TPT chart back on again if hidden
    outputOptions(output, "show_tpt", suspendWhenHidden = FALSE)


    # Show or hide the finance data depending on the country selected
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    output$show_finance <- reactive({

        req(pdata()$profile_properties)

        if (isTRUE(pdata()$profile_properties[, "dc_finance_display"])){
            result <- 1

        } else {
            result <- 0
        }
        return(result)
    })

    # Need this to make sure browser can switch Finance elements back on again if hidden
    outputOptions(output, "show_finance", suspendWhenHidden = FALSE)



    # Footer: date and attribution
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    output$generation <- renderText({
        HTML(paste("Generated"),
             format(Sys.Date(), "%Y-%m-%d"),
             "by <a href='https://www.who.int/teams/global-tuberculosis-programme/data'>the World Health Organization</a>"
             )
        })
    }

# Run the application
shinyApp(ui = ui, server = server, enableBookmarking = "url")
