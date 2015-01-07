source("../../constants.R")

library(GetoptLong)

print("Reading postal_code_riding_id_name_concord.csv ...")
postal_code_riding_concordance <-
  read.csv("downloads/postal_code_riding_id_name_concord.csv", encoding="UTF-8")

print("Reading Canada.csv ...")
postal_code_geo_concordance <- read.csv("downloads/Canada.csv", encoding="UTF-8")
colnames(postal_code_geo_concordance) <-
  c("postal_code", "latitude", "longitude", "city", "province")

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
postal_code_geo_concordance <-
  merge(unique_postal_codes, postal_code_geo_concordance, all.x=TRUE)
postal_code_geo_riding_concordance <-
  merge(postal_code_geo_concordance, postal_code_riding_concordance, all.x=TRUE)

print("Writing concordance...")
write.csv(
  postal_code_geo_riding_concordance,
  file="postal_code_geo_riding_concordance.csv",
  row.names=FALSE
)
print("Burpp!")