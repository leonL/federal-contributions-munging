# concatenate all CSVs, perform basic transforms, formatting and flagging

source("lib/formatting.R")
library("GetoptLong")

target_dir_name <- "1_format_flag_concat_output"
dir.create(target_dir_name)

source_dir_name <- "0_as_submitted_raw_data"
subfolders <- c("Bloc", "Conservative", "Green", "Liberal", "NDP")

data_set <- data.frame()

for(subfolder in subfolders) {
  source_subfolder_path <- GetoptLong::qq("@{source_dir_name}/@{subfolder}")
  files <- list.files(source_subfolder_path)

  print(GetoptLong::qq("Formatting, flagging and concatenating all csvs in @{source_subfolder_path}..."))
  for(file in files) {
    print(file)
    current_year <- strsplit(file, ".", fixed=TRUE)[[1]][2]

    csv <- read.csv(
      GetoptLong::qq("@{source_subfolder_path}/@{file}"), header=FALSE, as.is=TRUE, encoding="UTF-8"
    )

    # generate a 'party_name' column and a 'federal_contribution' boolean column
    csv <- set_column_names(csv)
    csv <- generate_federal_cols(csv, subfolder)
    csv <- adjust_errant_dates(csv, current_year)

    data_set <- rbind(data_set, csv)
  }
}

print("Munging character columns...")
data_set <- clean_char_columns(data_set)
print("Flagging errant contribution amounts")
data_set <- flag_errant_contribtuion_amounts(data_set)

# write munged and concatenated data set to CSV
write.csv(
  data_set, file=GetoptLong::qq("@{target_dir_name}/submitted_contributions_2004_to_2013.csv"), row.names=FALSE
)