# Summary of the tuberculosis situation in countries or groups of countries
Show a summary of information about tuberculosis (TB) for a chosen country, area or group of countries. This is an abbreviated version of [TB profiles](https://github.com/hazimtimimi/tb_profiles), in English only.

## Features

* Choose a country or area (from a list of 215 countries and areas) or a group (from a list of 6 WHO regions and the global group of all 215 countries and areas)
* View the TB summary information for the selected entity.

## Components and data source

This is an app built using [Shiny](https://shiny.rstudio.com/) and hosted at [shinyapps.io](https://worldhealthorg.shinyapps.io/TBrief/). It uses data published by the World Health Organization's [Global Tuberculosis Programme](https://www.who.int/teams/global-tuberculosis-programme/data).

The app pulls data directly from the global TB database:

1. https://extranet.who.int/tme/generateJSON.asp?ds=countries. This returns a list of all countries and areas which report TB data annually to WHO. 

2. https://extranet.who.int/tme/generateJSON.asp?ds=groups. This returns a list of all groups for which profiles are available, currently six WHO regions countries and the global group of all countries and areas which report TB data annually to WHO. 


4. https://extranet.who.int/tme/generateJSON.asp?ds=data&iso2=XX (where XX is a country [ISO2 code](https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2)). This returns all the data that appear in the tables and charts for the chosen country or area.

5. https://extranet.who.int/tme/generateJSON.asp?ds=group_data&group_code=XXX (where XXX is a code for a WHO region or the global group). This returns all the data that appear in the tables and charts for the group of countries and areas.

## Icons

Thanks to [Health icons](https://healthicons.org/)!

## Data updates

WHO collects TB data annually from all countries and areas and publishes them in the  [Global Tuberculosis Report](https://www.who.int/teams/global-tuberculosis-programme/data), usually in October of each year.


