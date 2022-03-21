# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Display TB country and group (regional/global) summary data on TB using JSON data
# retrieved from the WHO global tuberculosis database
# Hazim Timimi, November 2021
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

app_version <- "Version 0.7"

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
                tags$link(rel = "stylesheet", type = "text/css", href = "boot_print.css", media = "print")
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

                # Header  ----
                textOutput(outputId = "main_heading", container = h1),
                infoBoxOutput(outputId = "population"),
                htmlOutput(outputId = "detailed_profile")
            ),

            fluidRow(
                id = "milestones",

                # Milestones  ----

                HTML("<h2>End TB Strategy milestones for 2020</h2>"),

                infoBoxOutput(outputId = "milestone_deaths"),
                infoBoxOutput(outputId = "milestone_incidence"),
                infoBoxOutput(outputId = "milestone_catast")
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

                        infoBoxOutput(outputId = "rrmdr_num", width = 12),

                        infoBoxOutput(outputId = "mdr_tx_num", width = 12),

                        infoBoxOutput(outputId = "mdr_tx_short_num", width = 12)

                    ),

                    column(
                        width = 6,

                        box(
                            title = "Why did people fall ill with TB in 2020?",
                            solidHeader = TRUE,
                            status = "success",
                            width = 12,
                            plotOutput(outputId = "rf_chart", height = "200px")
                        ),

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
                ),

                # TPT and finance row ----
                fluidRow(
                    column(
                        width = 6,
                        box(
                            title = "People started on TB preventive treatment",
                            solidHeader = TRUE,
                            status = "success",
                            width = 12,
                            plotOutput(outputId = "tpt_chart", height = "200px")
                        ),

                        infoBoxOutput(outputId = "tpt_num", width = 12)
                    ),

                    column(
                        width = 6,
                        box(
                            title = "TB budget",
                            solidHeader = TRUE,
                            status = "warning",
                            width = 12,
                            plotOutput(outputId = "budget_chart", height = "200px")
                        ),

                        infoBoxOutput(outputId = "tb_budget", width = 12)
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


    # Get the profile data as a JSON file for the chosen country or group
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    pdata <- reactive({

        if (check_entity_type(input$entity_type) == "group") {
            url <- paste0(json_url, "?ds=group_data&group_code=", input$group_code)
        } else {
            url <- paste0(json_url, "?ds=data&iso2=", input$iso2)
        }

       json <- fromJSON(readLines(url, warn = FALSE, encoding = 'UTF-8'))
       return(json)
    })


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
            url <- paste0(base_url, '?_inputs_&entity_type="country"&lan="EN"&iso2="', input$iso2, '"')
        }

        paste("<h3><br />See also <a href='",
              url,
              "' target='_blank'>the detailed TB profile</a><br /><br />&nbsp;</h3>")
    })

    source("build_statistics.R", local = TRUE)

    source("build_charts.R", local = TRUE)

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
