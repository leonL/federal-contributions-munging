library("metrumrg")
library("stringr")

source("constants.R")

set_column_names <- function(data_set)
{
    colnames(data_set) <- column_names
    data_set
}

clean_char_columns <- function(data_set)
{
  within(data_set, {
    full_name <- titleCase(str_trim(full_name))
    full_name[full_name == ""] <- NA

    postal_code <- gsub("(-|\\s)","", postal_code) # remove hyphens and whitespace
    postal_code <- toupper(postal_code)
    postal_code[postal_code == ""] <- NA
  })
}

generate_federal_cols <- function(data_set, party_shorthand)
{
  within(data_set, {
    party_riding <- str_trim(party_riding)
    party_riding[party_riding == ""] <- NA

    # generate 'party' column
    party_name <- official_party_names[party_shorthand]

    # generate 'federal_contribution' column
    federal_contribution <- (party_riding == official_party_names[party_shorthand])
  })
}

adjust_errant_dates <- function(data_set, current_year)
{
  # var setup
  max_date <- as.Date(GetoptLong::qq("@{current_year}-12-31"))
  min_date <- as.Date(GetoptLong::qq("@{current_year}-01-01"))

  within(data_set, {
    contribution_date.ec <- str_trim(contribution_date.ec)

    contribution_date.ec <- as.Date(contribution_date.ec, format="%b %d, %Y")
    contribution_date <- contribution_date.ec

    invalid_years <- (
      !is.na(contribution_date) &
      (contribution_date > max_date |
       contribution_date < min_date)
    )

    contribution_date[invalid_years] <-
      strftime(contribution_date[invalid_years],
      GetoptLong::qq("@{current_year}-%m-%d"))
  })
}