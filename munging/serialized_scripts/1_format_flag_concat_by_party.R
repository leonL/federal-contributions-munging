source("lib/formatting.R")
library("GetoptLong")

target_dir_name <- "1_format_flag_concat_by_party"
dir.create(target_dir_name)

source_dir_name <- list.files(pattern="0")
subfolders <- c("Bloc", "Conservative", "Green", "Liberal", "NDP")

for(subfolder in subfolders) {
  target_subfolder_path <- GetoptLong::qq("@{target_dir_name}/@{subfolder}")
  dir.create(target_subfolder_path)

  source_subfolder_path <- GetoptLong::qq("@{source_dir_name}/@{subfolder}")
  files <- list.files(source_subfolder_path)

  print(GetoptLong::qq("Formatting, flagging and concatenating all csvs in @{source_subfolder_path}..."))
  for(file in files) {
    print(file)
    current_year <- strsplit(file, ".", fixed=TRUE)[[1]][2]

    data_set <- read.csv(GetoptLong::qq("@{source_subfolder_path}/@{file}"),
                          header=FALSE, as.is=TRUE)

    data_set <- set_column_names(data_set)
    data_set <- clean_char_columns(data_set)
    data_set <- disambiguate_party_riding(data_set, subfolder)
    data_set <- flag_errant_contribtuion_amounts(data_set)
    data_set <- adjust_errant_dates(data_set, current_year)

    # write transformed data to CSV
    write.table(
      data_set, file=GetoptLong::qq("@{target_subfolder_path}/@{file}"), sep=","
    )
  }
}