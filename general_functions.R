
# Validate inputs received via the URL: if an invalid choice is made, Shiny sets the input value to null
# So convert to a defaultvalue

# Replace a null entity_type with "country"   -- although invalid iso2 code doesn't seem to trigger an error
check_entity_type <- function(x){
  ifelse(is.null(x), "country", x)
}

# Replace a null group_code with "AFR"
check_group_code <- function(x){
  ifelse(is.null(x), "AFR", x)
}

# Replace an NA value with zero
NZ <- function(x){
  ifelse(is.na(x), 0, x)
}


# Format numbers taken from a list for display, but only after checking if they exist
# The second parameter is the formatting function to use
ftb_na <- function(x, fn=gtbreport::ftb) {
  ifelse(length(x)==0,
         "-",
         ifelse(is.na(x),
                "-",
                fn(x)
                )
         )
}

# Convert an annual number to number per day
relatable_number <- function(x){

  result <- ifelse(length(x)==0,
                   "",
                   ifelse(is.na(x),
                          "",
                          ifelse(x>=365*24*60,
                                 paste0("one person every ", round(365*24*60*60/x), " seconds"),
                                 ifelse(x>=365*24,
                                        paste0("one person every ", round(365*24*60/x), " minutes"),
                                        ifelse(x>=365,
                                               paste0("one person every ", round(365*24/x), " hours"),
                                               paste0("one person every ", round(365/x), " days")
                                        )
                                 )
                          )
                   )
            )

if (length(grep(" 1 ", result)) == 1) {
  # remove the number " 1 " and the final s of the result string
  # to avoid things like "one person every 1 days"
  result <- sub(" 1 ", " ", result)
  result <- substr(result, 1, nchar(result) - 1)
}

  return(result)
}


# Calculate % using numerator and denominator, format the output and cap at 100%
display_cap_pct <- function(numerator, denominator) {

  ifelse(is.na(numerator) | NZ(denominator) == 0,
                "",
                ifelse(numerator > denominator,
                       ">100%",
                       paste0(signif(numerator * 100 / denominator, 2), "%")
                       )
                )
}


# Calculate % change and describe it as increase or decrease
pct_change_description <- function(x1, x2){

  ifelse(NZ(x1)==0 | is.na(x2),
         "No data",
         ifelse(x1 == x2,
                "0% change",
                paste0(signif(abs(x2 - x1) * 100/x1,2),
                      "%",
                      ifelse(x2 <= x1,
                             " reduction",
                             " increase")
                      )
                )
         )
}

# Calculate % change and prefix with an HTM arrow entity
pct_change_arrow <- function(x1, x2){

  ifelse(NZ(x1)==0 | is.na(x2),
         "",
         ifelse(x1 == x2,
                "<b>&harr;</b> 0%",
                paste0(ifelse(x2 <= x1,
                              "<b>&darr;</b> -",
                              "<b>&uarr;</b> +"),
                       signif(abs(x2 - x1) * 100/x1,2),
                       "%")
                )
         )
}
