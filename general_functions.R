
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
relatable_num <- function(x){
  ifelse(length(x)==0,
         "",
         ifelse(is.na(x),
                "",
                ifelse(x>=365*24*60,
                       paste0("\n(one every\n", round(365*24*60*60/x), " seconds)"),
                       ifelse(x>=365*24,
                              paste0("\n(one every\n", round(365*24*60/x), " minutes)"),
                              ifelse(x>=365,
                                     paste0("\n(one every\n", round(8760/x), " hours)"),
                                     paste0("\n(one every\n", round(365/x), " days)")
                                     )
                              )
                       )
                )
         )

}

