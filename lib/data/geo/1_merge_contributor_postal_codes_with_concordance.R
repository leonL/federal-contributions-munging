source("../../constants.R")

library(GetoptLong)

print("Reading postal_code_riding_geo_concordance.csv ...")
pcode_riding_geo_cncrdnc <-
  read.csv(
    "open_concordances/munge_and_merge_geo_data_ouput/postal_code_riding_geo_concordance.csv",
    encoding="UTF-8", as.is=TRUE
  )

print("Reading contribution data...")
data_set <- read.csv(
  GetoptLong::qq("../../../1_format_flag_concat_output/@{all_data_csv_file_name}"),
  encoding="UTF-8",
  as.is=TRUE
)

print("Filtering unique postal codes...")
valid_postal_codes <-
  data_set$postal_code[grepl(postal_code_regex, data_set$postal_code)]
valid_postal_codes <- as.factor(valid_postal_codes)
unique_postal_codes <- data.frame(postal_code = levels(valid_postal_codes))

print("Merging data...")
contrib_pcode_riding_geo_cncrdnc <-
  merge(unique_postal_codes, pcode_riding_geo_cncrdnc, all.x=TRUE)

complete_cases_bool <- complete.cases(contrib_pcode_riding_geo_cncrdnc)
incomplete_cases <-
  contrib_pcode_riding_geo_cncrdnc[!complete_cases_bool, ]
complete_cases <-
  contrib_pcode_riding_geo_cncrdnc[complete_cases_bool, ]

print("Writing complete cases...")
write.csv(
  complete_cases,
  file="1_merge_output/complete_cases.csv",
  row.names=FALSE,
  fileEncoding = "UTF-8",
)

print("Writing incomplete cases...")
write.csv(
  incomplete_cases,
  file="1_merge_output/incomplete_cases.csv",
  row.names=FALSE,
  fileEncoding = "UTF-8",
)
print("Burpp!")