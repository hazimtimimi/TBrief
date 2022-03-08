# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Display TB country and group (regional/global) summary data on TB using JSON data
# retrieved from the WHO global tuberculosis database
# Hazim Timimi, November 2021
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

app_version <- "Version 0.6"

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


    fluidPage(title = "Summary of tuberculosis data",

              # ADD a link to a print-specific CSS to hide selectors and metadata and to
              # retain two columns when printing
              # and a tbrief-specific sylesheet for named elements

              tags$head(

                  tags$link(rel = "stylesheet", type = "text/css", href = "tbrief.css"),
                  tags$link(rel = "stylesheet", type = "text/css", href = "boot_print.css", media="print")
              ),

              fluidRow(id="selectors",

                   column(width = 2,
                          tags$div(style = "padding-left: 20px;
                                            color: #ffffff;
                                            background-color: #367fa9;
                                            margin-bottom: 20px;
                                            border: 1px solid transparent;
                                            border-radius: 4px;",

                                   uiOutput(outputId = "entitytypes")

                                   )
                   ),

                 # Choose whether to show the list of countries or the list of groups
                 # based on the choice made above

                   column(width = 10,
                          tags$div(style = "padding-left: 20px;
                                            background-color: #367fa9;
                                            margin-bottom: 20px;
                                            border: 1px solid transparent;
                                            border-radius: 4px;",

                                   uiOutput(outputId = "entities")

                                    )
                   )
                 ),


        fluidRow(id="main_content",

                textOutput(outputId = "main_heading", container = h1),
                textOutput(outputId = "population", container = h3),
                htmlOutput(outputId = "detailed_profile"),


                tabsetPanel(

                    tabPanel("End TB Strategy milestones",

                             # Start of milestones tab! ---------------------

                             HTML("<h1>End TB Strategy</h1><h2>Milestones for 2020</h2>"),

                             fluidRow(
                               infoBoxOutput(outputId = "milestone_deaths"),
                               infoBoxOutput(outputId = "milestone_incidence"),
                               infoBoxOutput(outputId = "milestone_catast")
                             ),

                             HTML("<h1>Alternative</h1>"),

                             fluidRow(
                               box(title = "Number of TB deaths 2020 vs 2015",
                                   solidHeader = TRUE,
                                   status = "info",
                                   width = 4,
                                   htmlOutput(outputId = "achieved_deaths"),
                                   HTML("<h4>Target: 35% reduction<br />&nbsp;</h4>"),
                                   plotOutput(outputId = "deaths_milestone_bar", height = "200px")
                               ),

                               box(title = "TB incidence rate 2020 vs 2015",
                                   solidHeader = TRUE,
                                   status = "success",
                                   width = 4,
                                   htmlOutput(outputId = "achieved_incidence"),
                                   HTML("<h4>Target: 20% reduction<br />&nbsp;</h4>"),
                                   plotOutput(outputId = "incidence_milestone_bar", height = "200px")
                               ),

                               box(title = "Catastrophic costs",
                                   solidHeader = TRUE,
                                   status = "warning",
                                   width = 4,
                                   htmlOutput(outputId = "achieved_catast"),
                                   HTML("<h4>Target: 0% of people with TB facing catastrophic costs by 2020</h4>"),
                                   plotOutput(outputId = "catastrophic_milestone_bar", height = "200px")
                               )
                             )

                             # End of milestones tab! ---------------------

                             ),

                    tabPanel("Indicators",

                             # Start of indicators tab! ---------------------

                             fluidRow(column(
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
                             column(width = 6,
                      			         box(title = "People falling ill with TB (including drug-resistant TB)",
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
              									             tags$div(
              									                 style = "position: absolute; top: 20px; left: 1em; font-size:120%;",

              									                 htmlOutput(outputId = "tsr", inline = TRUE)
              									             )
              									         ),
              									     )


              									 )
              									 ),

              									 fluidRow(
              									     column(width = 6,
              									            box(title = "People started on TB preventive treatment",
              									                solidHeader = TRUE,
              									                status = "success",
              									                width = 12,
              									                plotOutput(outputId = "tpt_chart", height = "200px")
              									                ),

              									            infoBoxOutput(outputId = "tpt_num", width = 12)
              									     ),

              									     column(width = 6,

              									            box(title = "TB budget",
              									                solidHeader = TRUE,
              									                status = "warning",
              									                width = 12,
              									                plotOutput(outputId = "budget_chart", height = "200px")
              									                ),

              									            infoBoxOutput(outputId = "tb_budget", width = 12)

              									            )


              									 )

              									 # End of indicators tab! ---------------------

                    )


                    ),


                # Footer that goes on every page
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

    source("build_header.R", local = TRUE)

    source("build_statistics.R", local = TRUE)

    source("build_charts.R", local = TRUE)


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
