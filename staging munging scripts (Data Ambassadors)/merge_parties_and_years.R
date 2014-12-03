#merge data across parties and years, and do some basic processing: 
#name columns, convert contribution to dollars, format dates and add party/year columns

#root_folder <- '~/Projects/datathon/federal-contributions/as reviewed/' # raw (complete data)
root_folder <- '~/Projects/datathon/federal-contributions/as submitted/' # clean (incomplete) 
folders<-list.files(root_folder) 

#remove a.out if it exists
if (length(folders[-(grep('a.out', folders))])>0)
  folders <- folders[-(grep('a.out', folders))]

d <- data.frame()
for (folder in folders){
  files<-list.files(paste0(root_folder, folder), full.names=TRUE) 
  for (file in files){
    print(file)
    dyear <- read.csv(file, header=F, stringsAsFactors=F)
    dyear$party <- folder
    dyear$year <- gsub('.*\\.(\\d{4})\\..*',"\\1", file)
    d <- rbind(d,dyear)
    print(paste(nrow(dyear), 'rows'))
  }
}

names(d) <- c('party_riding','ec_id', 'full_name', 'contribution_date', 'contribution', 'city', 'province', 'postal_code', 'party', 'year')
d$contribution <- d$contribution/100 #convert to dollars
d$contribution_date <- as.Date(d$contribution_date, format ='%b %d, %Y')
d$year <- format(d$contribution_date, '%Y')
write.csv(d, file='~/Projects/datathon/federal-contributions/munging/all_contributions.csv')
#80 blanks