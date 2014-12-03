# these riding association name strings (after processing) are to be substituted, to handle strange variations
fixup_ridings <- matrix(
          c( "kamloopsthompson", "kamloopsthompsoncariboo",
             "saultstemarie&area", "saultstemarie",
             "sdg(canada)", "stormontdundassouthglengarry",
             "kelowna","kelownalakecountry",
             "hastingsfrontenaclennoxandaddington","lanarkfrontenaclennoxandaddington"),
          ncol=2, byrow=TRUE)

#Ridings not in the ridings file
missingRidings <- c("South Okanagan-West Kootenay",
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