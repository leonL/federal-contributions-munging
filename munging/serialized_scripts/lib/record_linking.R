library("GetoptLong")

normalize_names <- function(full_names){

  # Clean a vector of names and output the clean string, first name, last name, and remaining characters.
  # Cleaning consists of the following steps:
  # 1. convert to all lower case
  # 2. convert accented characters (é) to their alphabetical counterparts (e)
  # 3. drop punctuation marks ("-")
  # 4. replace empty names by 'anon'

  library(dplyr)
  library(magrittr)

  ## iterate gsub over a vector of patterns (i.e. set of special characters)
  gsub2 <- function(pattern, replacement, x, ...) {
    for(i in 1:length(pattern))
      x <- gsub(pattern[i], replacement[i], x, ...)
    x
  }

  #coerce a character vector to only contain alphabetical characters
  coerce_to_alpha <- function(names){

    #convert accented characters to their alphabetical counterparts
    from <- c('Š', 'š', 'Ž', 'ž', 'À', 'Á', 'Â', 'Ã', 'Ä', 'Å', 'Æ', 'Ç', 'È', 'É',
              'Ê', 'Ë', 'Ì', 'Í', 'Î', 'Ï', 'Ñ', 'Ò', 'Ó', 'Ô', 'Õ', 'Ö', 'Ø', 'Ù',
              'Ú', 'Û', 'Ü', 'Ý', 'Þ', 'ß', 'à', 'á', 'â', 'ã', 'ä', 'å', 'æ', 'ç',
              'è', 'é', 'ê', 'ë', 'ì', 'í', 'î', 'ï', 'ð', 'ñ', 'ò', 'ó', 'ô', 'õ',
              'ö', 'ø', 'ù', 'ú', 'û', 'ý', 'ý', 'þ', 'ÿ')

    to <- c('S', 's', 'Z', 'z', 'A', 'A', 'A', 'A', 'A', 'A', 'A', 'C', 'E', 'E',
            'E', 'E', 'I', 'I', 'I', 'I', 'N', 'O', 'O', 'O', 'O', 'O', 'O', 'U',
            'U', 'U', 'U', 'Y', 'B', 'Ss','a', 'a', 'a', 'a', 'a', 'a', 'a', 'c',
            'e', 'e', 'e', 'e', 'i', 'i', 'i', 'i', 'o', 'n', 'o', 'o', 'o', 'o',
            'o', 'o', 'u', 'u', 'u', 'y', 'y', 'b', 'y')

    normalized <- gsub2(from, to, names)

    # Replace non alphabetical characters by an empty string
    normalized <- gsub("[^a-z| ]", "", normalized, ignore.case=TRUE)

    return(normalized)
  }

  split_names <- function(x){
    #split full name character vector into last name, first name and remaining characters
    if (length(x)==0) x <- c('anon', 'anon') #if name field is empty, set first_name='anon', last_name='anon'

    n <- length(x)
    first_name <- x[1]
    last_name <- x[n]
    remaining <- paste0(x[-c(1,n)], collapse=' ')

    if (length(remaining)==0) remaining <- ''

    data.frame(clean_full_name=paste0(x, collapse=" "), last_name=last_name, first_name=first_name, remaining=remaining, stringsAsFactors=F)
  }

  full_names %>%
    tolower %>%
    coerce_to_alpha %>%
    strsplit(split=' ') %>%
    lapply(FUN=split_names) %>%
    rbind_all
}

link_contributor_pair_counter <- 1
link_contributor_pair <- function(pair) {
  r1_row_num <- pair[1]
  r2_row_num <- pair[2]
  contrib_ids <- unique_name_and_postal$contributor_id

  matched_names <- unique_name_and_postal$full_name[c(r1_row_num, r2_row_num)]
  print(GetoptLong::qq("Linking @{matched_names[1]} with @{matched_names[2]}"))

  if (is.na(contrib_ids[r1_row_num])) {
    if (is.na(contrib_ids[r2_row_num])) {
      contrib_ids[c(r1_row_num, r2_row_num)] <- next_unique_contrib_id
      print(GetoptLong::qq("Defining new contributor_id: @{next_unique_contrib_id}"))
      next_unique_contrib_id <<- next_unique_contrib_id + 1
    } else {
      contrib_ids[r1_row_num] <- contrib_ids[r2_row_num]
    }
  } else {
    if (is.na(contrib_ids[r2_row_num])) {
      contrib_ids[r2_row_num] <- contrib_ids[r1_row_num]
    } else {
      contrib_ids[contrib_ids == contrib_ids[r2_row_num]] <- contrib_ids[r1_row_num]
    }
  }
  print(GetoptLong::qq("@{link_contributor_pair_counter} of @{total_probable_links} links defined..."))
  link_contributor_pair_counter <<- link_contributor_pair_counter + 1
  unique_name_and_postal$contributor_id <<- contrib_ids
  NULL
}