source("lib/constants.R")
source("lib/record_linking.R")

library("GetoptLong")
library("RecordLinkage")

target_dir_name <- "4_link_records_by_name_output"
dir.create(target_dir_name)
munged_data_dir_name <- "munged_data"
dir.create(munged_data_dir_name)

source_dir_name <- "1_contributors_riding_output"
data_set <- read.csv(GetoptLong::qq("@{source_dir_name}/@{all_data_csv_file_name}"), encoding="UTF-8")

print("Subsetting unique names by postal code...")
name_and_postal_data <- data_set[,c("full_name", "postal_code")]
unique_name_and_postal <- name_and_postal_data[!duplicated(name_and_postal_data),]

print("Normalizing unique contribtutor names...")
normed_names <- normalize_names(unique_name_and_postal$full_name)
unique_normed_names_and_postal <- cbind(unique_name_and_postal, normed_names)

print("Calculating contributor name string distances...")
link_probabilities <- compare.linkage(
  unique_normed_names_and_postal, unique_normed_names_and_postal,
  blockfld="postal_code",
  exclude=c("full_name", "postal_code", "last_name", "first_name", "remaining"),
  strcmp="clean_full_name"
)
print("Calculating link probabilities using epiLink algo...")
link_probabilities <- epiWeights(link_probabilities)

print("Classifying link probabilities...")
link_probabilities <- epiClassify(link_probabilities, 0.8)

# define a subset of record pairs that are probable links
probable_links <- with(link_probabilities, {
  pairs[prediction == "L", 1:2]
})

print("Remove self-referential links...")
probable_links <- probable_links[probable_links$id1 != probable_links$id2, ]
# remove inverted pairs
links_row_index <- 1
total_inversions <- nrow(probable_links) / 2
repeat
{
  if (links_row_index > nrow(probable_links)) { break }
  print(GetoptLong::qq("Removing inversion @{links_row_index} of about @{total_inversions}"))
  duplicate_row <- which(probable_links[links_row_index, 1] == probable_links[, 2] & probable_links[links_row_index, 2] == probable_links[, 1])
  probable_links <- probable_links[-duplicate_row,]
  links_row_index <<- links_row_index + 1
}

# record contributor links by setting ids
print("assigning unique ids to linked names...")
total_probable_links <- nrow(probable_links)
probable_links <- as.matrix(probable_links)
unique_name_and_postal$contributor_id <- NA
next_unique_contrib_id <- 1
apply(probable_links, 1, link_contributor_pair)

# set ids for all the unique names that were not matched
print("assigning unique ids to remaning unique names...")
number_without_id <- nrow(unique_name_and_postal[is.na(unique_name_and_postal$contributor_id),])
last_unique_contrib_id <- next_unique_contrib_id + number_without_id - 1
unique_name_and_postal$contributor_id[is.na(unique_name_and_postal$contributor_id)] <-
  c(next_unique_contrib_id:last_unique_contrib_id)

# merge the newly defined contrib_ids back into the original data_set
print("Merging contributor_ids into data set...")
data_set <- merge(data_set, unique_name_and_postal)

print("Writing csv files...")
write.csv(data_set, file=GetoptLong::qq("@{target_dir_name}/@{all_data_csv_file_name}"), row.names=FALSE)

# separate data by party name and year, and save each subset (for reasonably sized files)
all_party_names <- levels(data_set$party_name)
for(pname in all_party_names)
{
  data_party_subset <- subset(data_set, party_name==pname)
  party_nickname <- names(official_party_names[official_party_names==pname])
  for(year in all_years)
  {
    data_year_subset <- subset(data_party_subset, grepl(year, data_party_subset$contribution_date))
    dir.create(GetoptLong::qq("@{munged_data_dir_name}/@{party_nickname}"))
    print(GetoptLong::qq("@{munged_data_dir_name}/@{party_nickname}_@{year}_contributions"))
    write.csv(data_year_subset,
      file=GetoptLong::qq("@{munged_data_dir_name}/@{party_nickname}/@{party_nickname}_@{year}_contributions.csv"),
      row.names=FALSE
    )
  }
}