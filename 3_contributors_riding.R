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
postal_code_riding_concordance <- read.csv("lib/data/postal_code_riding_id_conconcord.csv", encoding="UTF-8", header=FALSE)

## extract the unique list of postal codes
pc_strings <- levels(data_set$postal_code)

## various fixes on postal codes
# Take out a dash or space in the middle of an otherwise valid postal code
fixed_pc <- str_replace_all(pc_strings, ignore.case(rem_dash_space), "\\1\\2")

# convert letter-o-letter to letter-0-letter
fixed_pc <- str_replace_all(fixed_pc, ignore.case("([a-z])o([a-z]|$)"), "\\10\\2")

# convert letter-l-letter to letter-1-letter
fixed_pc <- str_replace_all(fixed_pc, ignore.case("([a-z])l([a-z]|$)"), "\\1\\2")

# convert number-0-number to number-O-number
fixed_pc <- str_replace_all(fixed_pc,ignore.case("(^|[[:digit:]])0([[:digit:]])"),"\\1O\\2")

# convert number-l-number to number-O-number
fixed_pc <- str_replace_all(fixed_pc,ignore.case("(^|[[:digit:]])1([[:digit:]])"),"\\1L\\2")

## Now we find the match in the cocordance and create a column with them in it.
# find the posn in the corcordance of the match
riding_posn <- match(fixed_pc, postal_code_riding_concordance[,1])
# get the matching riding codes
riding_code <- postal_code_riding_concordance[riding_posn,2]

# creat the final data column by extracting the codes from for all the contribution rows.
data_set$contributors_riding <- riding_code[as.numeric(data_set$postal_code)]

write.csv(data_set, file=GetoptLong::qq("@{target_dir_name}/@{all_data_csv_file_name}"), row.names=FALSE)