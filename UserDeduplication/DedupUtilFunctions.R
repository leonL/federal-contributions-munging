#=========================================================================#
# Data4Good - Datathon, December 2014
# Project: User Deduplication on Election Canada raw data
# 
# Utility Functions (used by DedupUsersMain.R and normalize_names.R)
#
# 2014-12-05 - Tri Nguyen <tritanix@gmail.com>
#=========================================================================#


#--------------------------------------------------------------------------
# A naive way to assign a Federal party name based on a Riding Name
# use simple Keyword detection in the name
# if a name matches multiple KWs, the last KW will win
# ex: "Bloc NDP Liberal T-Shirt Store" will be assigned "Liberal"
# TEST
# testRidingNames <- c("Bloc Québécois", "Bloc Québécois de Bas-Richelieu--Nicolet--Bécancour",
# 	"Conservative Party of Canada", "Yukon Conservative Association",
# 	"Green Party of Canada", "Saanich--Gulf Islands Green Party EDA",
# 	"Liberal Party of Canada", "Westmount--Ville-Marie Federal Liberal Association",
# 	"New Democratic Party", "Quebec Federal NDP Riding Association")
#
# > FederalPartyDetection(testRidingNames)
#                                             RidingName    PartyName
# 1                                       Bloc Québécois         Bloc
# 2  Bloc Québécois de Bas-Richelieu--Nicolet--Bécancour         Bloc
# 3                         Conservative Party of Canada Conservative
# 4                       Yukon Conservative Association Conservative
# 5                                Green Party of Canada        Green
# 6                Saanich--Gulf Islands Green Party EDA        Green
# 7                              Liberal Party of Canada      Liberal
# 8   Westmount--Ville-Marie Federal Liberal Association      Liberal
# 9                                 New Democratic Party          NDP
# 10               Quebec Federal NDP Riding Association          NDP
#--------------------------------------------------------------------------
FederalPartyDetection <- function(ridingName) {
	
	fParty <- gl(6, 1, labels = c("Bloc", "Conservative", "Green", "Liberal", "NDP", "UNKNOWN"))
	
	dfRiding <- data.frame(RidingName=ridingName, 
								  PartyName=factor("UNKNOWN", levels=levels(fParty)),
								  stringsAsFactors = FALSE)
	
	# detect simple KW and infer the party
	dfRiding[ which(grepl("Bloc .+", dfRiding$RidingName, ignore.case=TRUE)), "PartyName"] <- "Bloc"
	dfRiding[ which(grepl("Conservative .+", dfRiding$RidingName, ignore.case=TRUE)), "PartyName"] <- "Conservative"
	dfRiding[ which(grepl("Green .+", dfRiding$RidingName, ignore.case=TRUE)), "PartyName"] <- "Green"
	dfRiding[ which(grepl("Liberal .+", dfRiding$RidingName, ignore.case=TRUE)), "PartyName"] <- "Liberal"
	dfRiding[ which(grepl("Democratic|NDP .+", dfRiding$RidingName, ignore.case=TRUE)), "PartyName"] <- "NDP"
	
	return (dfRiding)
}


#---------------------------------------------------------------
# 1. Read a definition file with variations of popular firstnames
# 2. Build a Lookup dictionary for Firstname Substitution
# 
# Usage:
#   firstNameDic <- SimilarFirstnameDictionary("./PopularFirstnames.csv")
#
# Print entire firstname Dic datatable
#   firstNameDic$getDic()
#
# Find the reference 1st name
#   firstNameDic$findRefFirstname(c("DaN", "Alex", "ZZbogusYY", "mike"))
#
#   OrigFirstname ReferenceFirstname
# 1           DaN             daniel  <-- when found, reference Firstname is always lowercase
# 2          Alex          alexander
# 3     ZZbogusYY          ZZbogusYY  <-- when not found, orginal firstname returned unchanged
# 4          mike            michael
#---------------------------------------------------------------
SimilarFirstnameDictionary <- function(definitionFilename) {
	if (!file.exists(definitionFilename))
	{
		stop(sprintf("File %s not found", definitionFilename))
	}
	
	rawFNSubsDefinition <- readLines(con = definitionFilename, skipNul = TRUE)
	
	# simple string clean up on definition lines
	rawFNSubsDefinition <-
		rawFNSubsDefinition %>%
			gsub("\\s", "", .) %>% # delete all spaces
			gsub("^(.+)#.*$", "\\1", .) %>% # delete inline comment
			gsub("^[^a-z].*$", "", ., ignore.case=TRUE) %>% # ignore lines begining with non-alpha char
			tolower  
	
	# remove empty lines
	rawFNSubsDefinition <- rawFNSubsDefinition[which(!grepl("^$", rawFNSubsDefinition))] 
	
	# keep lines which have at least a comma between two words
	rawFNSubsDefinition <- rawFNSubsDefinition[which(grepl("^.+,.+$", rawFNSubsDefinition))] 
	
	# build the "Firstname Lookup" datatable
	dt1stNameDic <-
		rawFNSubsDefinition %>%
			lapply(function(definitionLine) {
				# message(sprintf("DEBUG: Processing [%s] ...", definitionLine))
				x = unlist(strsplit(definitionLine, split=",")) # strsplit() return list, unlist() to get vector
				data.frame(ReferenceFirstname=x[1], Firstname=x[-1], stringsAsFactors=FALSE)
			}) %>%
			rbind_all %>%
			# drop all duplicated Firstname
		   distinct(Firstname)
	
	# get the entire First name lookup datatable
	getAllDic <- function() {
		dt1stNameDic
	}
	
	# lookup a firstname and return a "Reference Firstname" as substitution
	# If none is found, the same input firstname is returned
	findReferenceFirstname <- function(origFirstname = character())
	{
		origFirstname %>%
			data.frame(Firstname=tolower(origFirstname), OrigFirstname=origFirstname) %>%
			dplyr::left_join(dt1stNameDic, by="Firstname") %>%
			dplyr::select(OrigFirstname, ReferenceFirstname) %>%
			dplyr::transmute(OrigFirstname,
								  ReferenceFirstname = ifelse(is.na(ReferenceFirstname), as.character(OrigFirstname), ReferenceFirstname))		
	}
	
	return(list(getDic = getAllDic, findRefFirstname = findReferenceFirstname))
}


#--------------------------------------------------------------------------
# Compute a sound signature of a word
# Algorithm used is the Soundex Enhanced Method (with parameters CensusOption=0) described in
# http://www.creativyst.com/Doc/Articles/SoundEx1/SoundEx1.htm
# NOTE: this code improves the original by adding accented chars conversion 
#
# TEST
# 
# testStr = c("Alexander", "Marius", "Tristan", "Victor", "Schmit", "ashcroft", "Pfister", "Frédêrîç", "Frederic", "Eùgèné-Frànçôïs", "Eugene-Francois")
# > sapply(testStr, SoundexEnhanced)
#       Alexander          Marius         Tristan          Victor          Schmit        ashcroft         Pfister        Frédêrîç        Frederic Eùgèné-Frànçôïs Eugene-Francois 
#          "A425"          "M620"          "T623"          "V236"          "S530"          "A226"          "F236"          "F636"          "F636"          "E251"          "E251"
#
# > sapply(testStr, SoundexEnhanced, pCodeLength=5)
#       Alexander          Marius         Tristan          Victor          Schmit        ashcroft         Pfister        Frédêrîç        Frederic Eùgèné-Frànçôïs Eugene-Francois 
#         "A4253"         "M6200"         "T6235"         "V2360"         "S5300"         "A2261"         "F2360"         "F6362"         "F6362"         "E2516"         "E2516" 
#
# All results (excepted accented chars) match "Enhanced SoundEx" of:
# http://www.creativyst.com/Doc/Articles/SoundEx1/SoundEx1.htm#SoundExConverter
# 
# 2014-11-26 - Tri Nguyen
#--------------------------------------------------------------------------
SoundexEnhanced <- function (origWord = character(), # the atomic string to compute SoundEx (use sapply on vector of strings)
									  pCodeLength = 4) # the length of the returned SoundEx code 
{
	library(magrittr) # for %>% operator
	
	# Using chaining operator %>% to avoid a repetitive sequence of origWord <- gsub(...)
	# the . means placeholder to indicate the position taken by the object in the pipeline
	origWord %>%
		# STEP1: String CleanUp
		# Remove all spaces, remove puntuations, uppercase all
		gsub("[^a-zàáâãäåæçèéêëìíîïòóôõöøùúûýÿ\\s]", "", ., ignore.case=TRUE) %>%
		toupper %>%		
		# STEP2: Soundex Enhancement
		gsub("^GH", "G", .) %>% # leading GH = G
		gsub("DG" , "G", .) %>% # Change DG to G
		gsub("GH" , "H", .) %>% # Change GH to H
		gsub("GN" , "N", .) %>% # Change GN to N
		gsub("KN" , "N", .) %>% # Change KN to N
		gsub("PH" , "F", .) %>% # Change PH to F
		gsub("MP([STZ])", "M\\1", .) %>% # MP if followed by ST|Z
		gsub("^PS", "S", .) %>% # Change leadng PS to S
		gsub("^PF", "F", .) %>% # Change leadng PF to F
		gsub("MB" , "M", .) %>% # Change MB to M
		gsub("TCH", "CH", .) -> origWord 
	
	# Remember the first letter of the word.
	firstLetter <- stringr::str_sub(origWord, start = 1L, end = 1L)
	
	# in case 1st letter is H or W, replace with - (something that is NOT a letter)
	if (firstLetter == 'H' | firstLetter == 'W') {
		origWord <- paste0('-', stringr::str_sub(origWord, start = 2L))
	}
	
	# STEP3: Classic SoundEx processing:
	# Change all occurrence of the following letters to '0' (zero):
	# 'A', E', 'I', 'O', 'U', 'H', 'W', 'Y'
	# Change letters from the following sets into the digit given:
	#   1 = 'B', 'F', 'P', 'V'
	#   2 = 'C', 'G', 'J', 'K', 'Q', 'S', 'X', 'Z'
	#   3 = 'D','T'
	#   4 = 'L'
	#   5 = 'M','N'
	#   6 = 'R' 
	origWord %>%
		gsub("[AEIOUYHWÀÁÂÃÄÅÆÈÉÊËÌÍÎÏÒÓÔÕÖØÙÚÛÝŸ]", "0", .) %>%
		gsub("[BPFV]", "1", .) %>%
		gsub("[CÇSŠGJKQXZŽ]", "2", .) %>%
		gsub("[DT]", "3", .) %>%
		gsub("[L]", "4", .) %>%
		gsub("[MNÑ]", "5", .) %>%
		gsub("[R]", "6", .) %>%
		# Remove extra repetitions of the same digit. Example: 122378889 -> 123789 (remove 2 and 88)
		# gsub("0{2,}", "0", .) %>% # no need all 0 will be removed
		gsub("1{2,}", "1", .) %>%
		gsub("2{2,}", "2", .) %>%
		gsub("3{2,}", "3", .) %>%
		gsub("4{2,}", "4", .) %>%
		gsub("5{2,}", "5", .) %>%
		gsub("6{2,}", "6", .) %>%
		gsub("7{2,}", "7", .) %>%
		gsub("8{2,}", "8", .) %>%
		gsub("9{2,}", "9", .) %>%
		# drop 1st char (not used in soundex result)
		stringr::str_sub(. , start = 2L) %>%
		# remove all spaces and zeroes
		gsub("[ 0]", "", .) %>%
		# rightpad with 0 (nothing happen if length(origWord) >= pCodeLength)
		stringr::str_pad(. , pCodeLength, side = "right", pad = "0") -> origWord 
	
	# STEP4: Result SoundEx code = Original 1st Letter + remaining SoundEx computation from Step3
	return (stringr::str_sub(paste0(firstLetter, origWord), start = 1L, end=pCodeLength))
}


#--------------------------------------------------------------------------
# Side by Side bar graph to show the Counts of Unique & Duplicates of Key distribution
# 
# PARAMS
# dfKeySmry: the dataframe containing data to plot
# xLabelVar: variable in dataframe which is the factor having the value to plot
# valueVar : variable in dataframe which contains the numerical values for xLabelVar (height of the bar)
# grpByVar : variable in dataframe which is the factor used as grouping var for fill
#--------------------------------------------------------------------------
KeyCountBarGraph <- function(dfKeySmry, xLabelVar, valueVar, grpByVar)
{
	ggplot(dfKeySmry, aes_string(x=xLabelVar, y=valueVar, fill=grpByVar)) +
	geom_bar(stat="identity", position="dodge", alpha=0.6) +
	geom_text(aes_string(label=valueVar, vjust=-0.5), position = position_dodge(width=0.9)) +
	guides(fill=FALSE) + # remove legend corresponding to fill
	theme(axis.text.x=element_text(face="bold", color="black", size=10))
	#labs(title = "Case 'KEY1 + KEY2' (SoundEx)\nDuplicates Counts", x="", y="Count") +
	#scale_fill_manual(values=c("grey10", "seagreen4", "dodgerblue4", "deeppink4"))
}


#--------------------------------------------------------------------------
# Pie Chart based on BarGraph + Polar Coordinates
# - Values & discrete factor displayed beside the pie slice
# - Remove legend, Remove X,Y axis and ticks
# ThankYou: http://mathematicalcoffee.blogspot.ca/2014/06/ggpie-pie-graphs-in-ggplot2.html
# PARAMS
# dfPie   : the dataframe containing data to plot
# valueVar: variable in dfPie which contains the numerical values (size of slice in pie)
# grpByVar: variable in dfPie which is the factor used as grouping var for fill
#--------------------------------------------------------------------------
DistributionPie <- function(dfPie, valueVar, grpByVar)
{
	ggplot(dfPie, aes_string(x=factor(1), y=valueVar, fill=grpByVar)) +
	geom_bar(stat="identity", color="black", size=0.3, alpha=0.5) +
	#labs(title = "Stage2A - Duplicates\nKEY1 = FullName | PC | Party\nKEY2 = SoundEx1stname | PC | Party\nUniqueKEY1 = UniqueKEY2 + DuplicKEY2") +
	#theme(plot.title = element_text(face="bold", color="blue", hjust=0)) +
	coord_polar(theta='y') +
	#scale_fill_manual(values=c("seagreen4", "dodgerblue4", "dodgerblue3")) +
	guides(fill=FALSE) + # remove legend corresponding to fill
	#guides(fill=guide_legend(override.aes=list(colour=NA))) + # removes black borders from legend
	coord_polar(theta='y') +
	# suppress x,y axis and ticks 
	theme(axis.ticks=element_blank(), axis.title=element_blank(), axis.text.y=element_blank(),
			axis.text.x=element_text(face="bold", color="black", size=10)) +
	# display pie slice values
	scale_y_continuous(breaks=cumsum(dfPie[[valueVar]]) - dfPie[[valueVar]]/2, labels=paste(dfPie[[grpByVar]], dfPie[[valueVar]], sep=" = "))
}

#---(end)---#

