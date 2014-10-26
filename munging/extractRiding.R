library(stringr)
library(stringdist)

# these riding association name strings (after processing) are to be substituted
manualFixup <- c( "kamloopsthompson", "kamloopsthompsoncariboo",
             "saultstemarie&area", "saultstemarie",
             "sdg(canada)", "stormontdundassouthglengarry",
             "kelowna","kelownalakecountry")

#Ridings not in the file
missingRidings <- c("South Okanagan-west Kootenay",
                    "Niagara Centre",
                    "Avalon")

manual <- tolower(str_replace_all(iconv(manual,to="ASCII//TRANSLIT"), "[ .,-]",""))
fixup_ridings <- matrix(manual,ncol=2, byrow=TRUE)

manaulFixup <- function(oldName){
  fixup_ridings[which(fixup_ridings)]
  qq <- amatch(unique(failedData),realRidings,maxDist=2)
}

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

stripPatterns <- function(strings, patterns) {
  sapply(strings, function(r) {
      for (pat in patterns) {
        r <- str_replace_all(r,ignore.case(pat),"")
      }
      r
  })
}

allPartyDataFile <- "all_contributions.csv"

## load the full data
allPartyData <- read.csv(allPartyDataFile, encoding="UTF-8")

## load the list of formal EDA names
#ridings <- read.csv("../../Finance Data/2003 Riding.csv",encoding="UTF-8", stringsAsFactors=FALSE)
ridings <- read.csv("riding_id_name_concord.csv",encoding="UTF-8", stringsAsFactors=FALSE)[[2]]

## load the party names
#parties <- read.csv("../../Finance Data/2003 RidingNational.csv",encoding="UTF-8", stringsAsFactors=FALSE,header = FALSE)

# get the unique names from the eda file
uallEda <- levels(allPartyData[,"party_riding"])

# strip out anything related to the party name and extra spaces
genRidings <- stripPatterns(uallEda,partyPats)
genRidings <- stripPatterns(genRidings,noisePats)
genRidings <- str_trim(genRidings)
genRidings <- stripPatterns(genRidings,extraPats)
genRidings <- str_trim(genRidings)

names(genRidings) <- NULL

# munge the real riding names and the extracted ones to allow easier similarity search
genRidings <- tolower(str_replace_all(iconv(genRidings,to="ASCII//TRANSLIT", from="UTF-8"), "[ .,-]",""))
#realRidings <- tolower(str_replace_all(iconv(ridings[[1]],to="ASCII//TRANSLIT"), "[ .,-]",""))
realRidings <- tolower(str_replace_all(iconv(ridings[[2]],to="ASCII//TRANSLIT", from="UTF-8"), "[ .,-]",""))

# find the real riding with a match at most 2 away.
mx <- amatch(genRidings,realRidings,maxDist=2)
ridingsColumn <- ridings[mx[as.numeric(allPartyData[,"party_riding"])]]
# federal is a boolean that says that the donation is to the federal party rether than to a riding.
federal <- is.na(mx[allPartyData[,"party_riding"]]) & genRidings[allPartyData[,"party_riding"]]==""
#failed <- is.na(mx[allPartyData[,"party_riding"]]) & genRidings[allPartyData[,"party_riding"]]!=""



failU <- is.na(mx) & genRidings!=""
failName <- genRidings[failU]
failRiding <- uallEda[failU]
failIdx <- which(failU)

## these are the ones that need fixing up.  The first one is ok, it is the fedral NDP, no riding.
failedData <- allPartyData[failed,"party_riding"]

failedList <- unique(failedData)
failedList
