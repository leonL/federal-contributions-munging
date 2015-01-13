library("metrumrg")
library("stringr")

print("Reading Canada.csv ...")
postal_code_geo_concordance <- read.csv("Canada.csv", header=FALSE, as.is=TRUE, encoding="UTF-8")
colnames(postal_code_geo_concordance) <-
  c("postal_code", "latitude", "longitude", "city", "province")

print("Replaceing html encoded apostrophes in city names...")
apostrophe <- "'"
html_apostrophe <- "&#39;"
postal_code_geo_concordance$city <- gsub(html_apostrophe, apostrophe, postal_code_geo_concordance$city)

print("WTF is 117 Baie Du Vin; replacing as Bay Du Vin...")
postal_code_geo_concordance$city <- gsub("117 Baie Du Vin", "Bay Du Vin", postal_code_geo_concordance$city)

print("WTF is Ehatis 11; replacing as Kyuquot...")
postal_code_geo_concordance$city <- gsub("Ehatis 11", "Kyuquot", postal_code_geo_concordance$city)

print("WTF is Six Nations (Part) 40; replacing as Ohsweken...")
postal_code_geo_concordance$city <- gsub("Six Nations (Part) 40", "Ohsweken", postal_code_geo_concordance$city)

print("Basic string formatting...")
postal_code_geo_concordance$city <- totitle(tolower(str_trim(postal_code_geo_concordance$city)))
postal_code_geo_concordance$province <- toupper(postal_code_geo_concordance$province)

print("Reading postal_code_riding_id_name_concord.csv ...")
postal_code_riding_concordance <-
  read.csv("postal_code_riding_id_name_concordance.csv", encoding="UTF-8")

print("Merging data...")
postal_code_geo_riding_concordance <-
  merge(postal_code_riding_concordance, postal_code_geo_concordance, all.x=TRUE, all.y=TRUE)

print("Writing concordance...")
write.csv(
  postal_code_geo_riding_concordance,
  file="munge_and_merge_geo_data_ouput/postal_code_riding_geo_concordance.csv",
  row.names=FALSE,
  fileEncoding = "UTF-8"
)
print("Burpp!")