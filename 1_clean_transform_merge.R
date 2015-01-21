# concatenate all raw data, performing basic munging tasks, and merge in concordances

source("lib/formatting.R", chdir=TRUE)
library("GetoptLong")
library("plyr")
library("dplyr")

target_dir_name <- "1_clean_transform_merge"
dir.create(target_dir_name)

# source_dir_name <- "0_test_raw_data" # DO NOT COMMIT THIS LINE IF IT IS UNCOMMENTED
source_dir_name <- "raw_data_as_submitted"
subfolders <- names(official_party_names)

data_set <- data.frame()

for(subfolder in subfolders) {
  source_subfolder_path <- GetoptLong::qq("@{source_dir_name}/@{subfolder}")
  files <- list.files(source_subfolder_path)

  print(GetoptLong::qq("Formatting, flagging and concatenating all csvs in @{source_subfolder_path}..."))
  for(file in files) {
    print(file)
    current_year <- strsplit(file, ".", fixed=TRUE)[[1]][2]

    csv <- read.csv(
      GetoptLong::qq("@{source_subfolder_path}/@{file}"), header=FALSE, as.is=TRUE, encoding="UTF-8"
    )

    # generate a 'party_name' column and a 'federal_contribution' boolean column
    colnames(csv) <- raw_data_column_names
    csv <- generate_federal_cols(csv, subfolder)
    csv <- adjust_errant_dates(csv, current_year)

    data_set <- rbind(data_set, csv)
  }
}

print("Munging character columns...")
data_set <- clean_char_columns(data_set)

print("Merge in target riding columns...")
riding_name_concordance <-
  read.csv(
    "lib/data/patry_to_official_riding_name_concordance.csv",
    as.is=TRUE, encoding="UTF-8"
  )
colnames(riding_name_concordance) <- riding_name_concordance_col_names
data_set <- merge(data_set, riding_name_concordance, all.x=TRUE)

print("Remove superfluous columns...")
data_set <- data_set[ ,c(
  "party_name",
  "federal_contribution",
  "contribution_date",
  "contribution_amount",
  "full_name",
  "postal_code",
  "target.riding_name",
  "target.riding_id"
)]

print("Read contributor riding and geo data...")
postal_code_riding_geo_concordance <-
  read.csv(
    "lib/data/postal_code_riding_geo_concordance.csv",
    as.is=TRUE, encoding="UTF-8"
  )
colnames(postal_code_riding_geo_concordance) <- pcode_riding_geo_col_names

print("Seperate records with valid and invalid postal codes...")
valid_postal_codes <- postal_code_riding_geo_concordance$postal_code
valid_pcodes_bool <- data_set$postal_code %in% valid_postal_codes
data_set_with_invalid_pcodes <- data_set[!valid_pcodes_bool, ]
data_set <- data_set[valid_pcodes_bool, ]

print("Wrtie data with invalid pcodes as separate CSV...")
write.csv(
  data_set_with_invalid_pcodes,
  file=GetoptLong::qq("@{target_dir_name}/contributions_with_invalid_postal_codes.CSV"),
  row.names=FALSE
)

print("Merge in contributor riding data for riding contribs with ambiguous pcodes...")
concord_pcodes <- postal_code_riding_geo_concordance$postal_code
dup_concord_pcodes_bool <- duplicated(concord_pcodes)
pcodes_with_multiple_ridings <- levels(
  as.factor(concord_pcodes[dup_concord_pcodes_bool])
)

contribs_w_ambgus_pcodes <-
  filter(data_set, !federal_contribution, postal_code %in% pcodes_with_multiple_ridings)

ambgus_pcode_riding_geo_concord <-
  filter(postal_code_riding_geo_concordance, postal_code %in% pcodes_with_multiple_ridings)

contribs_w_ambgus_pcodes_merged <-
  adply(contribs_w_ambgus_pcodes, 1,
    find_contributors_riding_for_ambiguous_postal_code,
    concordance=ambgus_pcode_riding_geo_concord
  )

print("Merge in contributor riding data for the rest of the data set...")
postal_code_unique_riding_geo_concord <-
  postal_code_riding_geo_concordance[!dup_concord_pcodes_bool, ]

other_contribs <-
  filter(data_set,
    (!federal_contribution & !(postal_code %in% pcodes_with_multiple_ridings)) |
      federal_contribution
  )

contribs_w_unique_pcodes_merged <-
  merge(other_contribs, postal_code_unique_riding_geo_concord, all.x=TRUE)

data_set <- rbind(contribs_w_unique_pcodes_merged, contribs_w_ambgus_pcodes_merged)

print("Wrtie munged data set as CSV...")
write.csv(
  data_set,
  file=GetoptLong::qq("@{target_dir_name}/@{all_data_csv_file_name}"),
  row.names=FALSE
)