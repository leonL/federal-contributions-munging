postal_code_regex <- "^[ABCEGHJKLMNPRSTVXY]{1}[[:digit:]]{1}[ABCEGHJKLMNPRSTVWXYZ]{1}[[:digit:]]{1}[ABCEGHJKLMNPRSTVWXYZ]{1}[[:digit:]]{1}$"

# the logic of this function needs to be reconsidered; after which it
# should be used to correct obvious typos in the raw data before
# filtering out the invalid postal codes
# correct_postal_code_typos <- function(codes)
# {
#   # convert letter-o-letter to number-0-number
#   codes <- str_replace_all(codes, ignore.case("([a-z])o([a-z]|$)"), "\\10\\2")

#   # convert letter-l-letter to number-1-number
#   codes <- str_replace_all(codes, ignore.case("([a-z])l([a-z]|$)"), "\\1\\2")

#   # convert number-0-number to letter-O-letter
#   codes <- str_replace_all(codes,ignore.case("(^|[[:digit:]])0([[:digit:]])"),"\\1O\\2")

#   # convert number-1-number to letter-L
#   codes <- str_replace_all(codes,ignore.case("(^|[[:digit:]])1([[:digit:]])"),"\\1L\\2")
# }
