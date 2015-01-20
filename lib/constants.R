raw_data_column_names <-
  c("party_riding", "id", "full_name", "contribution_date.ec",
    "contribution_amount", "city", "province", "postal_code")
pcode_riding_geo_col_names <-
  c("postal_code", "contributor.riding_id", "contributor.riding_name",
    "pcode.latitude", "pcode.longitude", "city", "province")
riding_name_concordance_col_names <-
  c("party_riding", "target.riding_name", "target.riding_id")
official_party_names <-
  c(Bloc="Bloc Québécois",
    Conservative="Conservative Party of Canada",
    Green="Green Party of Canada",
    Liberal="Liberal Party of Canada",
    NDP="New Democratic Party")
province_levels <-
  c("AB", "BC", "MB", "NB", "NL", "NS", "NT", "NU", "ON", "PE", "QC", "SK", "YT", NA)
all_years <-
  c("2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013")
all_data_csv_file_name <- "all_contributions_2004_to_2013.csv"
postal_code_regex <- "^[ABCEGHJKLMNPRSTVXY]{1}[[:digit:]]{1}[ABCEGHJKLMNPRSTVWXYZ]{1}[[:digit:]]{1}[ABCEGHJKLMNPRSTVWXYZ]{1}[[:digit:]]{1}$"