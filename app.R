# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Display TB country and group (regional/global) summary data on TB using JSON data
# retrieved from the WHO global tuberculosis database
# Hazim Timimi, November 2021
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

app_version <- "Version 0.5"

library(shiny)
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

ui <- function(request) {

    fixedPage(title = "Summary of tuberculosis data",

              # Add a link to the boostrap CSS Sandstone theme downloaded from
              # https://bootswatch.com/3/sandstone/bootstrap.css
              # and a link to a print-specific CSS to hide selectors and metadata and to
              # retain two columns when printing
              # and a tbrief-specific sylesheet for named elements

              tags$head(

                  tags$link(rel = "stylesheet", type = "text/css", href = "tbrief.css"),
                  tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css"),
                  tags$link(rel = "stylesheet", type = "text/css", href = "boot_print.css", media="print")
              ),

        fixedRow(id="selectors",

                   column(width = 2,
                          tags$div(class = "navbar navbar-inverse",
                                   style = "padding-left: 20px;",

                                   uiOutput(outputId = "entitytypes")

                                   )
                   ),

                 # Choose whether to show the list of countries or the list of groups
                 # based on the choice made above

                   column(width = 10,
                          tags$div(class = "navbar navbar-inverse",
                                   style = "padding-left: 20px;",

                                   uiOutput(outputId = "entities")

                                    )
                   )
                 ),


        fixedRow(id="main_content",

                 textOutput(outputId = "main_heading", container = h1),
                 textOutput(outputId = "population", container = h5),
                 htmlOutput(outputId = "detailed_profile"),

                 HTML("<h2>End TB Strategy: 2020 milestones</h2>
                      "),

                 column(width = 6,
                        HTML("<h3>TB incidence rate</h3>"),
                        plotOutput(outputId = "incidence_milestone_chart")
                        ),

                 column(width = 6,
                        HTML("<h3>Number of TB deaths</h3>"),
                        plotOutput(outputId = "deaths_milestone_chart")
                 ),


               # Footer that goes on every page
                htmlOutput(outputId = "generation"),

                # Add an "About" bit of metadata
                # HTML(paste0("<div id='metadata'>",
                #             "<i>",
                #             app_version,
                #             ", Source code on <a href='https://github.com/hazimtimimi/TBrief' target='_blank'>Github</a>. ",
                #             "Data are also available on the TB Report app for <a href='https://apps.apple.com/us/app/tb-report/id1483112411' target='_blank'>iOS</a> and
                #       <a href='https://play.google.com/store/apps/details?id=uk.co.adappt.whotbreport&hl=en_us' target='_blank'>Android</a>
                #       and as <a href='https://www.who.int/teams/global-tuberculosis-programme/data' target='_blank'>CSV files</a>.</i><br /><br /></div>"))

            )
        )

}


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

    source("build_header.R", local = TRUE)

    #source("build_cascades.R", local = TRUE)

    #source("build_statistics.R", local = TRUE)

    source("create_doughnuts.R", local = TRUE)


    # Add the footer that goes on every page
    output$generation <- renderText({
        HTML(paste("Generated"),
             format(Sys.Date(), "%Y-%m-%d"),
             "by <a href='https://www.who.int/teams/global-tuberculosis-programme/data'>the World Health Organization</a>"
             )
        })

    }

# Run the application
shinyApp(ui = ui, server = server, enableBookmarking = "url")
