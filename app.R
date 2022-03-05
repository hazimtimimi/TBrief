# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Display TB country and group (regional/global) summary data on TB using JSON data
# retrieved from the WHO global tuberculosis database
# Hazim Timimi, November 2021
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

app_version <- "Version 0.3"

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

ui <- dashboardPage(

    dashboardHeader(title = "Summary of tuberculosis data",
                    titleWidth = 300),
    dashboardSidebar(disable = TRUE),
    dashboardBody(


    fluidPage(title = "Summary of tuberculosis data",

              # Add a link to the boostrap CSS Sandstone theme downloaded from
              # https://bootswatch.com/3/sandstone/bootstrap.css
              # and a link to a print-specific CSS to hide selectors and metadata and to
              # retain two columns when printing
              # and a tbrief-specific sylesheet for named elements

              tags$head(

                  tags$link(rel = "stylesheet", type = "text/css", href = "tbrief.css"),
                  #tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css"),
                  #tags$link(rel = "stylesheet", type = "text/css", href = "boot_print.css", media="print")
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
                textOutput(outputId = "population", container = h5),
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
                                   status = "info",
                                   width = 4,
                                   htmlOutput(outputId = "achieved_incidence"),
                                   HTML("<h4>Target: 20% reduction<br />&nbsp;</h4>"),
                                   plotOutput(outputId = "incidence_milestone_bar", height = "200px")
                               ),

                               box(title = "Catastrophic costs",
                                   solidHeader = TRUE,
                                   status = "info",
                                   width = 4,
                                   htmlOutput(outputId = "achieved_catast"),
                                   HTML("<h4>Target: 0% of people with TB facing catastrophic costs by 2020</h4>"),
                                   plotOutput(outputId = "catastrophic_milestone_bar", height = "200px")
                               )
                             )

                             # End of milestones tab! ---------------------

                             ),

                    tabPanel("Icon-tastic!",

                             # Start of icontastic tab! ---------------------

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
                             )

                             # End of icontastic tab! ---------------------

                             ),

                    tabPanel("Indicators",

                             # Start of indicators tab! ---------------------

                             fluidRow(
                                 box(title = "TB deaths",
                                     solidHeader = TRUE,
                                     status = "info",
                                     plotOutput(outputId = "mortality_chart_v2", height = "200px"),

                                        HTML("<p style='font-size:120%;'><b>2020 TB deaths</b><br />
                                     <span style='font-size:150%; color:#1D91D1;'><b>123 456</b></span> (one every 5 minutes)<br />
                                     <b>+z%</b> vs 2019  <span style='color:red'><b>&uarr;</b></span></p>
                                     <i>Range xxx - xxx</i>

                  									 <p style='font-size:120%;'>&nbsp;<br />
                                                       <span style='font-size:150%; color:#1D91D1;'><b>321</b></span> per 100 000 population<br />
                                                       <b>+z%</b> vs 2019  <span style='color:red'><b>&uarr;</b></span></p>
                                                       <i>Range xxx - xxx</i>

                  									 <i>Includes TB deaths in people living with HIV.</i>")
                  									 ),

                  									 box(title = "People ill with TB",
                  									     solidHeader = TRUE,
                  									     status = "info",
                  									     plotOutput(outputId = "incidence_chart_v2", height = "200px"),

              									        HTML("<p style='font-size:120%;'><b>2020 TB incidence</b><br />
                                                   <span style='font-size:150%; color:#91A93E;'><b>123 456</b></span> (one every 5 minutes)<br />
                                                   <b>-z%</b> vs 2019  <span style='color:#91A93E'>&darr;</span></p>

                                                   <i>Range xxx - xxx</i></p>

                          									 <p style='font-size:120%;'>&nbsp;<br />
                                                               <span style='font-size:150%; color:#91A93E;'><b>789</b></span> per 100 000 population<br />
                                                               <b>-z%</b> vs 2019  <span style='color:#91A93E;'>&darr;</span></p>
                                                               <i>Range xxx - xxx</i></p>

                          									 <i>Incidence and notifications include drug-resistant TB and TB in people living with HIV.</i>")
              									        )
                             ),


              							 fluidRow(
              							   box(title = "TB treatment",
              							       solidHeader = TRUE,
              							       status = "info",

              							            HTML("<p style='font-size:120%;'><b>TB treatment success</b></p>"),

              							            tags$div(style = "position: relative;",

              							                     plotOutput(outputId =  "tsr_chart_v2", height = "180px"),

              							                     # Next DIV allows the text to appear over the chart image
              							                     tags$div(style = "position: absolute; top: 20px; left: 1em; font-size:120%;",

              							                              HTML("<span style='font-size:150%;'><b>x%</b></span> of those treated for TB (2019)<br />
                                                                     <br /><br />
                                                                     <span style='font-size:150%;'> <b>x%</b></span> of those treated for drug-resistant TB (2018)")
              							                     )

              							            ),

              							            HTML("<p style='font-size:120%;'><b>2020 TB notifications</b><br /><br />
                                                   <span style='font-size:150%;'><b>98 765</b></span> people newly diagnosed and reported<br />
                                                   <b>-z%</b> vs 2019  <span style='color:red'>&darr;</span></p>

                                                   <p style='font-size:120%;'><span style='font-size:150%;'><b>xx%</b></span> diagnosed using WHO-recommended rapid diagnostics</p>

              									 <p style='font-size:120%;'>&nbsp;<br /><br />
                                                   <span style='font-size:150%;'><b>987</b></span> diagnosed with drug-resistant TB<br />
                                                   <b>-z%</b> vs 2019  <span style='color:#91A93E;'>&darr;</span></p>

                                                   <p style='font-size:120%;'><span style='font-size:150%;'><b>800</b></span> started on treatment for drug-resistant TB<br />
                                                   <span style='font-size:150%;'><b>xx%</b></span> of those on WHO-recommended shorter regimens</p>")

							            ),

							            box(title = "Why did people fall ill with TB?",
							                solidHeader = TRUE,
							                status = "info",
							                HTML("<h3>Contributing factors in 2020</h3>"),
							                plotOutput(outputId = "rf_chart_v2", height = "200px"),
							                HTML("<p style='font-size:120%;'>&nbsp;<br /><br />
                                     <span style='font-size:150%;'><b>XXXXXX</b></span> was the top contributory factor in 2020<br />
                                     accounting for <b>yyyy</b> people (range zzz - zzz) falling ill with TB</p>")
							            )

							 ),


							 fluidRow(

        							   box(title = "Preventive treatment",
        							       solidHeader = TRUE,
        							       status = "info",
        							       plotOutput(outputId = "tpt_chart_v2", height = "200px"),
        							       HTML("<p style='font-size:120%;'><b>2020 TB preventive treatment</b><br />
                                   <span style='font-size:150%;'><b>yyyy</b></span> people in total<br/>
                                   <b>+z%</b> vs 2019  <span style='color:#91A93E;'>&uarr;</span></p>

                                   <p style='font-size:120%;'>
                                   <span style='font-size:150%;color:#ffc425;'><b>aaa</b></span> (<b>xx%</b>) people living with HIV<br/>
                                   <span style='font-size:150%;color:#9fcd25;'><b>bbb</b></span> (<b>yy%</b>) household contacts aged under 5 years<br/>
                                   <span style='font-size:150%;color:dark green;'><b>ccc</b></span> (<b>zz%</b>) household contacts aged 5 years and over")
        							       ),

        							   box(title = "Financing",
        							       solidHeader = TRUE,
        							       status = "info",
        							       plotOutput(outputId = "budget_chart_v2", height = "200px"),

    							            HTML("<p style='font-size:120%;'><b>2021 National TB budget</b><br /><br />
                                     <span style='font-size:150%;color:#ffc425;'><b>zzz</b></span> of the xxx USD million national TB budget was funded<br />
                                     <span style='font-size:150%;color:#D84D3F;'><b>z%</b></span> (<b>xxx</b> USD million) was not funded</p>")
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

    source("build_cascades.R", local = TRUE)

    source("build_statistics.R", local = TRUE)

    source("build_charts.R", local = TRUE)

    source("create_horizontal_bars.r", local = TRUE)


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
