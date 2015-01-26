# separate data by party name and year, and save each subset (for reasonably sized files)
library(GetoptLong)
source("lib/constants.R")

munged_data_dir_name <- "munged_data"
dir.create(munged_data_dir_name)

source_dir_name <- "2_link_records_by_name_output"
data_set <- read.csv(GetoptLong::qq("@{source_dir_name}/@{all_data_csv_file_name}"), encoding="UTF-8")

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