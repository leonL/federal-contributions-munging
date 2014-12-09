# derive the contributors home riding based on their postal code

source("lib/postal_codes.R")
source("lib/constants.R")

library(GetoptLong)
library(stringr)

target_dir_name <- "3_contributors_riding_output"
dir.create(target_dir_name)

# load data_set
source_dir_name <- "2_target_riding_output"
data_set <- read.csv(GetoptLong::qq("@{source_dir_name}/@{all_data_csv_file_name}"), encoding="UTF-8")

# load postcal code to riding id concordance
postal_code_riding_concordance <- read.csv("lib/data/postal_code_riding_id_name_concord.csv", encoding="UTF-8")

# retain valid postal codes, set the invalid to NA
data_set$postal_code[!grepl(postal_code_regex, data_set$postal_code)] <- NA

# for postal codes that span multiple ridings, nominate a single one for use
code_concord_wo_dups <- postal_code_riding_concordance[!duplicated(postal_code_riding_concordance$postal_code), ]

# merge official riding codes into data_set
colnames(code_concord_wo_dups) <- c("postal_code", "contributors_riding_id", "contributors_riding_name")
data_set <- merge(data_set, code_concord_wo_dups, all.x=TRUE)

write.csv(data_set, file=GetoptLong::qq("@{target_dir_name}/@{all_data_csv_file_name}"), row.names=FALSE)