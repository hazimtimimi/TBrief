
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


# Calculate % using numerator and denominator, format the output and cap at 100%
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

display_cap_pct <- function(numerator, denominator) {

  pct <- ifelse(is.na(numerator) | is.na(denominator) | denominator == 0, "",
         ifelse(numerator > denominator, ">100%", paste0(signif(numerator * 100 / denominator, 2), "%")))

  return(pct)
}

# Calculate % using numerator and denominator, cap at 100, no formating
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

get_cap_pct <- function(numerator, denominator) {

  pct <- ifelse(is.na(numerator) | is.na(denominator) | denominator == 0, "",
         ifelse(numerator > denominator, 100, signif(numerator * 100 / denominator, 2)))

  return(pct)
}

