source("lib/constants.R")
source("lib/record_linking/normalize_names.R")
source("lib/record_linking/linking.R")

library(GetoptLong)
library(plyr); library(dplyr)

target_dir_name <- "2_link_records_by_name_output"
dir.create(target_dir_name)

source_dir_name <- "1_clean_transform_merge_output"
data_set <- read.csv(GetoptLong::qq("@{source_dir_name}/@{all_data_csv_file_name}"), encoding="UTF-8")

print("Subsetting unique names by postal code...")
name_and_postal_data <- data_set[,c("full_name", "postal_code")]
unique_name_and_postal <- name_and_postal_data[!duplicated(name_and_postal_data),]

print("Normalizing unique contribtutor names...")
normed_names <- normalize_names(unique_name_and_postal$full_name)
unique_normed_names_and_postal <- cbind(unique_name_and_postal, normed_names)

print("Match similiar names...")
probable_links <- find_probable_name_matches(unique_normed_names_and_postal)

# sets ids for linked names
print("assigning unique ids to linked names...")
unique_name_and_postal$contributor_id <- NA
side_effect <- dlply(probable_links, .(id1), link_contributors_by_id); rm(side_effect)

# set ids for all the unique names that were not matched
print("assigning unique ids to remaning unique names...")
number_without_id <- nrow(unique_name_and_postal[is.na(unique_name_and_postal$contributor_id),])
next_unique_contrib_id <- max(unique_name_and_postal$contributor_id, na.rm=TRUE)
last_unique_contrib_id <- next_unique_contrib_id + number_without_id - 1
unique_name_and_postal$contributor_id[is.na(unique_name_and_postal$contributor_id)] <-
  c(next_unique_contrib_id:last_unique_contrib_id)

# merge the newly defined contrib_ids back into the original data_set
print("Merging contributor_ids into data set...")
data_set <- merge(data_set, unique_name_and_postal)

print("Write CSV file...")
write.csv(data_set, file=GetoptLong::qq("@{target_dir_name}/@{all_data_csv_file_name}"), row.names=FALSE)