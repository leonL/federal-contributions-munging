find_probable_name_matches <- function(names_by_pcode) {

  library(RecordLinkage)
  library(plyr)

  print("Calculating contributor name string distances...")
  link_probabilities <- compare.linkage(
    names_by_pcode, names_by_pcode,
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

  print("removing inverted pairs...")
  probable_links <- adply(probable_links, 1, smallest_first)
  unique_probable_links <- probable_links[!duplicated(probable_links), ]

  return(unique_probable_links)
}

smallest_first <- function(row) {
  if (row$id1 > row$id2) {
    data.frame(id1=row$id2, id2=row$id1)
  } else {
    row
  }
}

link_contributors_by_id <- function(ids) {
  ids <- unique(as.vector(as.matrix(ids)))
  current_ids <- unique_name_and_postal$contributor_id[ids]
  if (!any(is.na(current_ids))) { return() }
  current_ids <- current_ids[!is.na(current_ids)]
  contrib_id <-
    if (length(current_ids) > 0) {
      current_ids[1]
    } else {
      ids[1]
    }
  unique_name_and_postal$contributor_id[ids] <<- contrib_id
  return()
}