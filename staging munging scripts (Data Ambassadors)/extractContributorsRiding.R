library(stringr)

## loading data
# The name of the file with the contributions.
allPartyDataFile <- "all_contributions.csv"

# The name of the file with the concordance of postal code to riding code
postalCodeConcordanceFile <- "postal_code_riding_id_conconcord.csv"

# load the full concordance
postalCodeConcordance <- read.csv(postalCodeConcordanceFile, encoding="UTF-8", header=FALSE)

# load the full data
allPartyData <- read.csv(allPartyDataFile, encoding="UTF-8")

## definitions
postal_code_regex <- "^[ABCEGHJKLMNPRSTVXY]{1}[[:digit:]]{1}[ABCEGHJKLMNPRSTVWXYZ]{1}[[:digit:]]{1}[ABCEGHJKLMNPRSTVWXYZ]{1}[[:digit:]]{1}$"
rem_dash_space <- "^([ABCEGHJKLMNPRSTVXY]{1}[[:digit:]]{1}[ABCEGHJKLMNPRSTVWXYZ]{1})[- ]([[:digit:]]{1}[ABCEGHJKLMNPRSTVWXYZ]{1}[[:digit:]]{1})$"


## extract the unique list of postal codes
pc_strings <- levels(allPartyData$postal_code)

## various fixes on postal codes
# Take out a dash or space in the middle of an otherwise valid postal code
fixed_pc <- str_replace_all(pc_strings,ignore.case(rem_dash_space),"\\1\\2")

# convert letter-o-letter to letter-0-letter
fixed_pc <- str_replace_all(fixed_pc,ignore.case("([a-z])o([a-z]|$)"),"\\10\\2")

# convert letter-l-letter to letter-1-letter
fixed_pc <- str_replace_all(fixed_pc,ignore.case("([a-z])l([a-z]|$)"),"\\1\\2")

# convert number-0-number to number-O-number
fixed_pc <- str_replace_all(fixed_pc,ignore.case("(^|[[:digit:]])0([[:digit:]])"),"\\1O\\2")

# convert number-l-number to number-O-number
fixed_pc <- str_replace_all(fixed_pc,ignore.case("(^|[[:digit:]])1([[:digit:]])"),"\\1L\\2")

## Now we find the match in the cocordance and create a column with them in it.
# find the posn in the corcordance of the match
riding_posn <- match(fixed_pc, postalCodeConcordance[,1])
# get the matching riding codes
riding_code <- postalCodeConcordance[riding_posn,2]

# creat the final data column by extracting the codes from for all the contribution rows.
allPartyData$contributors_riding <- riding_code[as.numeric(allPartyData$postal_code)]
