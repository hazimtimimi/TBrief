# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Display TB country and group (regional/global) summary data on TB using JSON data
# retrieved from the WHO global tuberculosis database
# Hazim Timimi, November 2021
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

app_version <- "Version 0.3"

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

# Add a function to create a standard block for the UI with an image in a column of size 2
# and text in a column of size 10, all within a column that will use up half of fixed row that
# spans the full width of the web page. The web page will be made up of manny such blocks

block_210 <- function(sub_title, image_name, text_name) {

    column(width = 6,

           HTML(sub_title),

           fixedRow(
               column(width = 2,
                      HTML(paste0("<img src='", image_name, "'>"))
               ),
               column(width = 10,

                      htmlOutput(outputId = text_name,
                                 inline = TRUE)
               )
           )
    )
}

# And a similar block, but now with a cascade of care horizontal bar chart placed in the background
# with text written over it

block_210_cascade <- function(sub_title, image_name, text_name, plot_name) {

    column(width = 6,

           HTML(sub_title),

           fixedRow(
               column(width = 2,
                      HTML(paste0("<img src='", image_name, "'>"))
               ),
               column(width = 10,

                      tags$div(style = "position: relative;",

                               plotOutput(outputId = plot_name, height = "140px"),

                               # Next DIV allows the text to appear over the chart image
                               tags$div(style = "position: absolute; top: 20px;",

                                        htmlOutput(outputId = text_name,
                                                   inline = TRUE)
                               )
                      )
               )
           )
    )
}



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
                                   style = "padding-left: 20px;
                                            background-color: #a2cfde;",

                                   uiOutput(outputId = "entitytypes")

                                   )
                   ),

                 # Choose whether to show the list of countries or the list of groups
                 # based on the choice made above

                   column(width = 10,
                          tags$div(class = "navbar navbar-inverse",
                                   style = "padding-left: 20px;
                                            background-image: url('gtbr_pattern.png');",

                                   uiOutput(outputId = "entities")

                                    )
                   )
                 ),


        fixedRow(id="main_content",

                textOutput(outputId = "main_heading", container = h1),
                textOutput(outputId = "population", container = h5),

                fixedRow(id="tb_cascade",

                    column(width = 6,
                           HTML("<h3>People with TB</h3>"),

                           fixedRow(
                               column(width = 2,
                                      HTML("<img src='tb@2x.png'>")),
                               column(width = 10,

                                      tags$div(style = "position: relative;",

                                               plotOutput(outputId = "tb_cascade_chart", height = "140px"),

                                               # Next DIV allows the text to appear over the chart image
                                               tags$div(style = "position: absolute;
                                                                 top: 20px;",

                                                        htmlOutput(outputId = "tb_cascade_text",
                                                                   inline = TRUE)
                                                        )
                                               )
                                      )
                           ),


                           HTML("<h3>People who died from TB in 2020</h3>"),

                           fixedRow(
                               column(width = 2,
                                      HTML("<img src='death@2x.png'>")),
                               column(width = 10,

                                      htmlOutput(outputId = "deaths_text",
                                                 inline = TRUE)
                               )
                           )
                    ),

                    block_210(sub_title = "<h3>Causes of people falling ill with TB in 2020</h3>",
                              image_name = "question@2x.png",
                              text_name = "attributable_cases"),

                ),

                fixedRow(id="tb_prevention",

                         block_210(sub_title = "<h3>People notified with TB in 2020</h3>",
                                   image_name = "people@2x.png",
                                   text_name = "notifs_text"),


                         block_210(sub_title = "<h3>Treatment to prevent TB provided in 2020</h3>",
                                   image_name = "medicines@2x.png",
                                   text_name = "tpt_text")

                ),

                fixedRow(id="drtbhiv_cascade",

                         block_210_cascade(sub_title = "<h3>People living with HIV</h3>",
                                           image_name = "ribbon@2x.png",
                                           text_name = "tbhiv_cascade_text",
                                           plot_name = "tbhiv_cascade_chart"),

                         block_210_cascade(sub_title = "<h3>People with drug-resistant TB</h3>",
                                           image_name = "clinical_a@2x.png",
                                           text_name = "drtb_cascade_text",
                                           plot_name = "drtb_cascade_chart")
                ),

                fixedRow(id="tb_finance",

                         block_210(sub_title = "<h3>National TB programme budget for 2021</h3>",
                                   image_name = "coins@2x.png",
                                   text_name = "finance_text"),

                         block_210(sub_title = "<h3>Survey of TB patient costs</h3>",
                                   image_name = "health_worker_form@2x.png",
                                   text_name = "pcs_text")
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

    source("build_cascades.R", local = TRUE)

    source("build_statistics.R", local = TRUE)


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
