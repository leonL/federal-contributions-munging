library(stringr)
library(stringdist)

# these riding association name strings (after processing) are to be substituted, to handle strange variations
fixup_ridings <- matrix(
          c( "kamloopsthompson", "kamloopsthompsoncariboo",
             "saultstemarie&area", "saultstemarie",
             "sdg(canada)", "stormontdundassouthglengarry",
             "kelowna","kelownalakecountry",
             "hastingsfrontenaclennoxandaddington","lanarkfrontenaclennoxandaddington"),
          ncol=2, byrow=TRUE)



#Ridings not in the ridings file
missingRidings <- c("South Okanagan-west Kootenay",
                    "Niagara Centre",
                    "Avalon")
fixRidings <- function(oldName){
  res <- which(fixup_ridings[,1]==oldName)
  ifelse(length(res),fixup_ridings[res,2],oldName)
}



# various regular expressions used to strip out non riding text
partyPats <- c(
  "(liberal)|(lib.rale)",
  "(conservative)|(cpc)|(Conservateur)",
  "(ndp)|(npd)|(new democratic)",
  "bloc Qu..?b.cois",
  "(greens?)|(gpc)|(Parti Vert)"
)
noisePats <- c(
  "federal",
  "f.d.rale",
  "party",
  "(du )?parti",
  "riding",
  "electoral district",
  " eda",
  "Association of the",
  "(l')?association( du)?",
  "(de )?(la )?Circonscription( du)?",
  "Pour La",
  "of canada",
  ",",
  "du canada",
  "constituency",
  "Comt. ",
  "pour "
)
extraPats <- c(
  "^ *(d|l)'",
  "^ *the +",
  "^ *du +",
  " +du *$",
  "^ *de +",
  "^ *(d|l)'",
  "^ *of +"
)

# apply a set of regular expressions to some strings to strip out patterns.
stripPatterns <- function(strings, patterns) {
  sapply(strings, function(r) {
      for (pat in patterns) {
        r <- str_replace_all(r,ignore.case(pat),"")
      }
      r
  })
}

# The name of the file with the contributions.
allPartyDataFile <- "all_contributions.csv"

## load the full data
allPartyData <- read.csv(allPartyDataFile, encoding="UTF-8")

## load the list of formal EDA names and add on the missing ones.
ridings <- read.csv("riding_id_name_concord.csv",encoding="UTF-8", stringsAsFactors=FALSE)[[2]]
ridings <- c(ridings, missingRidings)

# get the unique names from the eda data
uallEda <- levels(allPartyData[,"party_riding"])

# strip out anything related to the party name and extra spaces using the above regular expressions
genRidings <- stripPatterns(uallEda,partyPats)
genRidings <- stripPatterns(genRidings,noisePats)
genRidings <- str_trim(genRidings)
genRidings <- stripPatterns(genRidings,extraPats)
genRidings <- str_trim(genRidings)

# get rid of the names, which by now mean nothing.
names(genRidings) <- NULL

# munge the real riding names and the extracted ones to allow easier similarity search
genRidings <- tolower(str_replace_all(iconv(genRidings,to="ASCII//TRANSLIT", from="UTF-8"), "[ .,-]",""))
genRidings <- sapply(genRidings, fixRidings)
realRidings <- tolower(str_replace_all(iconv(ridings,to="ASCII//TRANSLIT", from="UTF-8"), "[ .,-]",""))

# find the real riding with a match at most 2 away from the generated riding name.
mx <- amatch(genRidings,realRidings,maxDist=2)


###
These two columns can be added to allPartyData as the riding name and the federal flag
# this is the column with real riding names
target_riding <- ridings[mx[as.numeric(allPartyData[,"party_riding"])]]

# This is a boolean that says that the donation is to the federal party rather than to a riding.
# the third clause is to make sure an empty party_riding value does not show as federal
federal <- is.na(mx[allPartyData[,"party_riding"]]) & (genRidings[allPartyData[,"party_riding"]]=="" & allPartyData[,"party_riding"]!="")


newdf <- cbind(allPartyData,target_riding,federal)
write.csv(newdf, file='all_contributions_with_ridings.csv')


#####
# this is test code to make sure the data makes sense.


#these are failed entries, only one in the full data 904095, has an empty party_riding
failed <- is.na(mx[allPartyData[,"party_riding"]]) & genRidings[allPartyData[,"party_riding"]]!=""


# various dimensions of failures
failU <- is.na(mx) & (genRidings!="" | uallEda=="")
failName <- genRidings[failU]
failRiding <- uallEda[failU]
failIdx <- which(failU)


#generate a random sample x long from the frame to allow spot checking of the target_riding and federal flag
genTest <- function(x) {
  s <- sample(nrow(allPartyData),x)
  matrix(c(as.character(allPartyData[s,"party_riding"]),target_riding[s],federal[s]),ncol=3)
}

## these are the ones that need fixing up.
failedData <- allPartyData[failed,"party_riding"]


failedList <- unique(failedData)
failedList
