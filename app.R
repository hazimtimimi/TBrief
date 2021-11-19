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


# json_url <- "https://extranet.who.int/tme/generateJSON.asp"
json_url <- "http://localhost/generateJSON.asp"

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

                                                        textOutput(outputId = "tb_inc",
                                                                   inline = TRUE),
                                                        textOutput(outputId = "tb_inc_txt",
                                                                   inline = TRUE),
                                                        HTML("<br /><br />"),
                                                        textOutput(outputId = "tb_notif",
                                                                   inline = TRUE),
                                                        textOutput(outputId = "tb_notif_txt",
                                                                   inline = TRUE),
                                                        HTML("notified with TB in 2020<br /><br />"),
                                                        textOutput(outputId = "tb_tsr_pct",
                                                                   inline = TRUE),
                                                        HTML("successfully treated in 2019")

                                                        )
                                               )

                                      )
                           ),


                           HTML("<h3>People who died from TB in 2020</h3>"),

                           fixedRow(
                               column(width = 2,
                                      HTML("<img src='death@2x.png'>")),
                               column(width = 10,
                                      textOutput(outputId = "deaths_nh",
                                                 inline = TRUE),
                                      textOutput(outputId = "deaths_nh_relatable",
                                                 inline = TRUE),
                                      HTML("people without HIV<br/><br/>"),
                                      textOutput(outputId = "deaths_h",
                                                 inline = TRUE),
                                      textOutput(outputId = "deaths_h_relatable",
                                                 inline = TRUE),
                                      HTML("people living with HIV")
                               )
                           )




                    ),
                    column(width = 6,


                           HTML("<h3>Causes of people falling ill with TB in 2020</h3>"),

                           fixedRow(
                               column(width = 2,
                                      HTML("<img src='question@2x.png'>")),
                               column(width = 10,
                                      HTML("<br/>"),
                                      textOutput(outputId = "alc",
                                                 inline = TRUE),
                                      HTML("due to harmful use of alcohol<br /><br />"),
                                      textOutput(outputId = "dia",
                                                 inline = TRUE),
                                      HTML("due to diabetes<br /><br />"),
                                      textOutput(outputId = "hiv",
                                                 inline = TRUE),
                                      HTML("due to HIV<br /><br />"),
                                      textOutput(outputId = "smk",
                                                 inline = TRUE),
                                      HTML("due to smoking<br /><br />"),
                                      textOutput(outputId = "und",
                                                 inline = TRUE),
                                      HTML("due to undernourishment")
                               )
                           )
                    ),

                ),

                fixedRow(id="tb_prevention",
                    column(width = 6,

                           HTML("<h3>People notified with TB in 2020</h3>"),

                           fixedRow(
                               column(width = 2,
                                      HTML("<img src='people@2x.png'>")
                               ),
                               column(width = 10,
                                      textOutput(outputId = "notifs_kids",
                                                 inline = TRUE),
                                      HTML("children aged under 15 years<br /><br />"),
                                      textOutput(outputId = "notifs_women",
                                                 inline = TRUE),
                                      HTML("women aged 15 years and over<br /><br />"),
                                      textOutput(outputId = "notifs_men",
                                                 inline = TRUE),
                                      HTML("men aged 15 years and over<br /><br />")
                               )
                           )
                           ),

                    column(width = 6,
                           HTML("<h3>Treatment to prevent TB provided in 2020</h3>"),

                           fixedRow(
                               column(width = 2,
                                      HTML("<img src='medicines@2x.png'>")),
                               column(width = 10,
                                      textOutput(outputId = "hiv_tpt",
                                                 inline = TRUE),
                                      HTML("people living with HIV<br /><br />"),
                                      textOutput(outputId = "contacts_04_tpt",
                                                 inline = TRUE),
                                      HTML("household contacts (aged 0-4 years) of people with TB<br /><br />"),
                                      textOutput(outputId = "contacts_5plus_tpt",
                                                 inline = TRUE),
                                      HTML("household contacts (aged 5 years and above) of people with TB")
                               )
                           )

                    )
                ),

                fixedRow(id="drtbhiv_cascade",
                    column(width = 6,
                           HTML("<h3>People living with HIV</h3>"),

                           fixedRow(
                               column(width = 2,
                                      HTML("<img src='ribbon@2x.png'>")),
                               column(width = 10,

                                      tags$div(style = "position: relative;",

                                               plotOutput(outputId = "tbhiv_cascade_chart", height = "140px"),

                                               # Next DIV allows the text to appear over the chart image
                                               tags$div(style = "position: absolute;
                                                                 top: 20px;",

                                                        textOutput(outputId = "tbhiv_inc",
                                                                   inline = TRUE),
                                                        HTML("fell ill with TB in 2020<br /><br />"),
                                                        textOutput(outputId = "tbhiv_notif",
                                                                   inline = TRUE),
                                                        HTML("notified with TB in 2020<br /><br />"),
                                                        textOutput(outputId = "tbhiv_tsr_pct",
                                                                   inline = TRUE),
                                                        HTML("successfully treated in 2019")

                                               )
                                      )

                               )
                           )
                           ),

                    column(width = 6,
                           HTML("<h3>People with drug-resistant TB</h3>"),

                           fixedRow(
                               column(width = 2,
                                      HTML("<img src='clinical_a@2x.png'>")),
                               column(width = 10,

                                             tags$div(style = "position: relative;",

                                                      plotOutput(outputId = "drtb_cascade_chart", height = "140px"),

                                                      # Next DIV allows the text to appear over the chart image
                                                      tags$div(style = "position: absolute;
                                                                 top: 20px;",

                                                               textOutput(outputId = "drtb_detect",
                                                                          inline = TRUE),
                                                               HTML("detected in 2020<br /><br />"),
                                                               textOutput(outputId = "drtb_enrol",
                                                                          inline = TRUE),
                                                               HTML("started treatment in 2020<br /><br />"),
                                                               textOutput(outputId = "drtb_tsr_pct",
                                                                          inline = TRUE),
                                                               HTML("successfully treated in 2018")

                                                      )
                                             )

                                      )
                               )
                           )
                ),

                fixedRow(id="tb_finance",
                    column(width = 6,
                           HTML("<h3>National TB programme budget for 2021</h3>"),

                           fixedRow(
                               column(width = 2,
                                      HTML("<img src='coins@2x.png'>")),
                               column(width = 10,
                                      textOutput(outputId = "tot_req",
                                                 inline = TRUE),
                                      HTML("million US$ required<br /><br />"),
                                      textOutput(outputId = "tot_domestic",
                                                 inline = TRUE),
                                      HTML("million US$ available from domestic funds<br /><br />"),
                                      textOutput(outputId = "tot_international",
                                                 inline = TRUE),
                                      HTML("million US$ available from international funds<br /><br />"),
                                      textOutput(outputId = "tot_gap",
                                                 inline = TRUE),
                                      HTML("million US$ funding gap<br /><br />")
                               )
                           )
                    ),

                    column(width = 6,
                           HTML("<h3>Survey of TB patient costs</h3>"),

                           fixedRow(
                               column(width = 2,
                                      HTML("<img src='health_worker_form@2x.png'>")),
                               column(width = 10,
                                      textOutput(outputId = "catast",
                                                 inline = TRUE),
                                      textOutput(outputId = "catast_description",
                                                 inline = TRUE)

                               )
                           )
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
