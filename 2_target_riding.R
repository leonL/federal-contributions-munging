# normalize riding names used by different parties

source("lib/federal_ridings.R")
source("lib/constants.R")

library(stringr)
library(stringdist)
library(GetoptLong)

target_dir_name <- "2_target_riding_output"
dir.create(target_dir_name)

source_dir_name <- "1_format_flag_concat_output"
data_set <- read.csv(GetoptLong::qq("@{source_dir_name}/@{all_data_csv_file_name}"), encoding="UTF-8")

# define set of official riding names (adding the missing ones)
riding_id_name_concord <- read.csv("lib/data/riding_id_name_concord.csv", encoding="UTF-8", stringsAsFactors=FALSE)
all_official_ridings <- c(riding_id_name_concord[[2]], missingRidings)

# define set of riding names used in contrib data, and scrub boilerplate
unique_target_ridings <- levels(data_set[,"party_riding"])
names(unique_target_ridings) <- NULL
scrubbed_target_ridings <- stripPatterns(unique_target_ridings, partyPats)
scrubbed_target_ridings <- stripPatterns(scrubbed_target_ridings, noisePats)
scrubbed_target_ridings <- str_trim(scrubbed_target_ridings)
scrubbed_target_ridings <- stripPatterns(scrubbed_target_ridings, extraPats)
scrubbed_target_ridings <- str_trim(scrubbed_target_ridings)

# munge the two sets of riding names to allow easier similarity search
scrubbed_target_ridings <- tolower(str_replace_all(iconv(scrubbed_target_ridings,to="ASCII//TRANSLIT", from="UTF-8"), "[ .,-]",""))
scrubbed_target_ridings <- sapply(scrubbed_target_ridings, fixRidings)
scrubbed_official_ridings <- tolower(str_replace_all(iconv(all_official_ridings,to="ASCII//TRANSLIT", from="UTF-8"), "[ .,-]",""))

# find the best matching official name for every riding
mx <- amatch(scrubbed_target_ridings, scrubbed_official_ridings, maxDist=2)
# add a column to the data_set with the best matching official name for every record
target_riding <- all_official_ridings[mx[as.numeric(data_set[,"party_riding"])]]
newdf <- cbind(data_set, target_riding)
write.csv(newdf, file=GetoptLong::qq("@{target_dir_name}/@{all_data_csv_file_name}"), row.names=FALSE)

# this is test code to make sure the data makes sense.
# these are failed entries, only one in the full data 904095, has an empty party_riding
failed <- is.na(mx[data_set[,"party_riding"]]) & scrubbed_target_ridings[data_set[,"party_riding"]]!=""

# various dimensions of failures
failU <- is.na(mx) & (scrubbed_target_ridings!="" | unique_target_ridings=="")
failName <- scrubbed_target_ridings[failU]
failRiding <- unique_target_ridings[failU]
failIdx <- which(failU)

#generate a random sample x long from the frame to allow spot checking of the target_riding and federal flag
genTest <- function(x) {
  s <- sample(nrow(data_set),x)
  matrix(c(as.character(data_set[s,"party_riding"]),target_riding[s],federal[s]),ncol=3)
}

## these are the ones that need fixing up.
failedData <- data_set[failed,"party_riding"]

failedList <- unique(failedData)
failedList