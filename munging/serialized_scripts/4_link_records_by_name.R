source("lib/record_linking.R")

library("GetoptLong")
library("RecordLinkage")

target_dir_name <- "4_link_records_by_name_output"
dir.create(target_dir_name)

source_dir_name <- "2_target_riding_output"
date_set_file_name <- "submitted_contributions_2004_to_2013.csv"
data_set <- read.csv(GetoptLong::qq("@{source_dir_name}/@{date_set_file_name}"), encoding="UTF-8")

print("Normalizing contribtutor names...")
normed_names <- normalize_names(data_set$full_name)
data_set_with_normed_names <- cbind(data_set, normed_names)

print("Calculating contributor name string distances...")
link_probabilities <- compare.linkage(
  data_set_with_normed_names, data_set_with_normed_names,
  blockfld=9, exclude=c(1:17, 21), strcmp=18:20
)
print("Calculating link probabilities using epiLink algo...")
link_probabilities <- epiWeights(link_probabilities)

print("Classifying link probabilities...")
link_probabilities <- epiClassify(link_probabilities, 0.8)

# define a subset of record pairs that are probable links
probable_links <- with(link_probabilities, {
  pairs[prediction == "L", 1:2]
})

# remove pairs where a record is mathced to itself
probable_links <- probable_links[probable_links$id1 != probable_links$id2, ]
probable_links <- as.matrix(probable_links)

data_set$contributor_id <- NA

apply(probable_links, 1, link_contributor_pair)

write.csv(data_set, file=GetoptLong::qq("@{target_dir_name}/@{date_set_file_name}"))
