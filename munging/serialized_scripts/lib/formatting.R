library("metrumrg")
library("stringr")

column_names <-
  c("party_riding", "ec_id", "full_name", "contribution_date",
    "contribution_amount", "city", "province", "postal_code")
official_party_names <-
  c(Bloc="Bloc Québécois",
    Conservative="Conservative Party of Canada",
    Green="Green Party of Canada",
    Liberal="Liberal Party of Canada",
    NDP="New Democratic Party")
province_levels <-
  c("AB", "BC", "MB", "NB", "NL", "NS", "NT", "NU", "ON", "PE", "QC", "SK", "YT")

set_column_names <- function(data_set)
{
    colnames(data_set) <- column_names
    data_set
}

clean_char_columns <- function(data_set)
{
  within(data_set, {
    party_riding <- str_trim(party_riding)

    full_name <- totitle(str_trim(full_name))
    full_name[full_name == ""] <- NA

    city <- totitle(str_trim(city))
    city[city == ""] <- NA

    province <- totitle(str_trim(province))
    province[province == ""] <- NA

    postal_code <- gsub("(-|\\s)","", postal_code) # remove hyphens and whitespace
    postal_code <- toupper(postal_code)
    postal_code[postal_code == ""] <- NA

    contribution_date <- str_trim(contribution_date)
  })
}

disambiguate_party_riding <- function(data_set, party_shorthand)
{
  within(data_set, {
    # generate 'party' column
    party <- official_party_names[party_shorthand]

    # generate 'target_riding' column
    target_riding <- party_riding
    target_riding[target_riding == official_party_names[party_shorthand]] <- NA

    # generate 'federal_contribution' column
    federal_contribution <- is.na(target_riding)
  })
}

flag_errant_contribtuion_amounts <- function(data_set)
{
  within(data_set, {
    # blank contribtuions
    flag.blank_contrib <- (contribution_amount == 0) | is.na(contribution_amount)

    # negative contributions
    flag.negative_contrib <- contribution_amount < 0

    # contributions that exceed limits
    flag.excessive_contrib <- contribution_amount > 120000
  })
}

adjust_errant_dates <- function(data_set, current_year)
{
  # var setup
  current_year_numeric <- as.numeric(current_year)
  next_year <- as.character(current_year_numeric + 1)
  last_year <- as.character(current_year_numeric - 1)
  max_date <- as.Date(GetoptLong::qq("@{next_year}-04-01"))
  min_date <- as.Date(GetoptLong::qq("@{last_year}-10-01"))

  within(data_set, {
    contribution_date <- as.Date(contribution_date, format="%b %d, %Y")
    adjusted.contribution_date <- contribution_date

    invalid_years <- (
      !is.na(adjusted.contribution_date) &
      (adjusted.contribution_date > max_date |
       adjusted.contribution_date < min_date)
    )

    adjusted.contribution_date[invalid_years] <-
      strftime(adjusted.contribution_date[invalid_years],
      GetoptLong::qq("@{current_year}-%m-%d"))
  })
}