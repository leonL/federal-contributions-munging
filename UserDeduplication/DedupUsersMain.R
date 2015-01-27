#=========================================================================#
# Data4Good - Datathon, December 2014
# Project: User Deduplication on Election Canada raw data
#
# This script reads a small test sample of data of 80 rows. Representing contributors from all 5 parties
# The test data has been ALTERED to introduce all kinds of duplicates (true name dup, typos, duplic with abbreviated first names)
# The script demonstrates 3 strategies of deduplication:
# - Stage1: KEY1 = Fullname, PostalCode, PartyName
# - Stage2: on duplicates which survive Stage1, KEY2 = Soundex(Firstname), PostalCode, PartyName
# - Stage3: on duplicates which survive Stage2, KEY3 = Firstname Substitution by Reference Firstname, PostalCode, PartyName
#
# Then the script saves a CSV file for human review. The original data is untouched.
#
# Resources:
# - ./data/*TEST.csv      : a small sample of data excerpted from https://github.com/leonL/federal-contributions/tree/master/as%20submitted
# - SimilarFirstnames.csv : a hand made dictionary, used in Stage3 Dedup to replace a firstname by a "reference firstname"
#
# 2014-12-05 - Tri Nguyen <tritanix@gmail.com>
#=========================================================================#
library(dplyr)
library(ggplot2)

# setwd("~/Documents/RProjects/DatathonElectionCanada/UserDeduplication")

source("./normalize_names.R")
source("./DedupUtilFunctions.R")

# ATTENTION: the data has been ALTERED to add intentional duplicates to test all scenarios of deduplication
origCSVFiles <- list.files(path = "./data", pattern = "*.csv", recursive=TRUE, full.names=TRUE)


message("01. Reading Source Data")
dfOrigData <- data.frame()
#for (oneFile in origCSVFiles) {
for (kk in seq_along(origCSVFiles)) {
	message (sprintf("    File %02d: %s", kk, origCSVFiles[kk]))
	
	dfTemp <- read.csv(origCSVFiles[kk], header=FALSE, sep = ",", stringsAsFactors=FALSE,
		col.names=c("OrigPartyOrRiding", "OrigRecordID", "OrigFullname", "OrigDateStr", "AmountCent", "City", "ProvinceCode", "PostalCode"))
	
	fileNameNoPath <- gsub(".+/(.+)", "\\1", origCSVFiles[kk], ignore.case=TRUE)
	dfOrigData <- rbind(dfOrigData, cbind(SourceFile=fileNameNoPath, dfTemp))
}

rm(dfTemp)

#---Test Decode of PartyName from RidingName
# table(FederalPartyDetection(dfOrigData$OrigPartyOrRiding)$PartyName)
# FederalPartyDetection(dfOrigData$OrigPartyOrRiding)

#-----------------------------------------------------------------------
# Normalize Contributor Full Name:
# - Remove symbols, Salutation
# - Split firstname, lastname
# - Compute Soundex
#-----------------------------------------------------------------------
message("02. Normalize Original Full Name")
cleanNames <- normalize_names(dfOrigData$OrigFullname)


#-----------------------------------------------------------------------
# CleanUp data:
# - Convert Date String to true date, Amount cents becomes dollars
# - Get Federal Party Name from riding name
# - Add extra columns in to help deduplication
# Use transmute() to add column at specific position & to allow calculated expression
#-----------------------------------------------------------------------
message("03. Generate dfClean + KEY1")
dfClean <- dplyr::transmute(dfOrigData,
	SourceFile,
	OrigRecordID,
	OrigPartyOrRiding,
	Party = FederalPartyDetection(OrigPartyOrRiding)[, "PartyName"], # factor of Federal party names 
	ContribDate = as.Date(OrigDateStr, format="%b %d, %Y"), # Orig format "Nov 29, 2013"
	AmountDollar = AmountCent / 100,
	City, ProvinceCode, PostalCode,
	OrigFullname, CleanFullname=cleanNames$clean_full_name,
	CleanFirstname = cleanNames$first_name, FirstnameSoundex=cleanNames$first_soundex,
	#CleanLastname = cleanNames$last_name, LastnameSoundex=cleanNames$last_soundex,
	
	# User Unique ID. For now, everybody has a UniqueID because we don't know yet about duplicated users
	UserID = 1:nrow(dfOrigData),
	# KEY1 = Normalized Full Name + PostalCode + PartyName
	KEY1 = paste(cleanNames$clean_full_name, dfOrigData$PostalCode, Party, sep="|")
)

rm(dfOrigData)



#=======================================================================
#
# Stage1 -  Dedup using KEY1 (Fullname, PostalCode, Party)
#
#=======================================================================

message("04. Stage1: Dedup using KEY1 (Fullname, PostalCode, Party)")

dicFileName <- "./SimilarFirstnames.csv"
message (sprintf("    Init 'Similar Firstname Dictionary' from %s", dicFileName))
firstNameDic <- SimilarFirstnameDictionary(dicFileName)
# TEST
# firstNameDic$getDic()
# firstNameDic$findRefFirstname(c("DaN", "AleX", "ZZbogusYY", "Mike", "Mikael", "Michel", "MikaelA"))
# firstNameDic$findRefFirstname(dfStage1$CleanFirstname)


dedup1 <-
dfClean %>%
	group_by(KEY1) %>%
	summarize(DuplicCountKEY1 = n()) %>%         
	# ATTN: distinct() must be applied on dfClean before group_by()
	inner_join(distinct(dfClean, KEY1), by="KEY1") %>%
	select(UserID, KEY1, DuplicCountKEY1)


# dfStage1 = dfClean + Columns DuplicCountKEY1, DupK1WithUID, KEY2
dfStage1 <-
dfClean %>%
	inner_join(dedup1, by="KEY1") %>%
	transmute(
		DuplicCountKEY1,		
		DupK1WithUID = ifelse(UserID.x == UserID.y, "-", UserID.y), # the UserID having the same KEY1
		# compute KEY2, KEY3 for each unique KEY1
		# group of rows having duplicated KEY1: only the first row of the duplic group is considered
		KEY2 = ifelse(UserID.x == UserID.y, paste(FirstnameSoundex, PostalCode, Party, sep="|"), ""),
		KEY3 = ifelse(UserID.x == UserID.y, paste(firstNameDic$findRefFirstname(CleanFirstname)$ReferenceFirstname, PostalCode, Party, sep="|"), "")
		) %>%
	cbind(dfClean, .)

# KEY2, KEY3 have been calculated, drop these intermediate vars (save some place and make CSV easier to review)
dfStage1$CleanFirstname <- NULL
dfStage1$FirstnameSoundex <- NULL


outFilename <- "./outputReview/Stage1_K1.csv"
message(sprintf("    Writing %s", outFilename))

dfStage1 %>%
	select(UserID, OrigFullname, KEY1, DuplicCountKEY1, DupK1WithUID,
			 SourceFile, OrigRecordID, Party, OrigPartyOrRiding, ContribDate, AmountDollar,
			 City, ProvinceCode, PostalCode, CleanFullname) %>%
	write.table(file = outFilename, sep = ",", row.names=FALSE, col.names=TRUE)

# REVIEW Duplicate on KEY1:
# dfStage1[1:45, c("UserID", "KEY1", "KEY2", "DuplicCountKEY1", "DupK1WithUID")]

# REVIEW ONLY Duplicate on KEY1:
# dfStage1[dfStage1$DuplicCountKEY1 >1 & complete.cases(dfStage1), c("UserID", "KEY1", "KEY2", "DuplicCountKEY1", "DupK1WithUID")]



#=======================================================================
#
# Stage2A - KEY1 -> KEY2
#           Dedup Stage1 using KEY2 (Firstname Soundex, PostalCode, Party)
#
#=======================================================================
message("05. Stage2A: KEY1 + KEY2 (SoundEx)")

# dfUniqK1 = subset of dfStage1 having unique KEY1 
# (these rows have a nonEmpty KEY2, see Stage1 how KEY2 was constructed)
dfUniqK1 <-
dfStage1 %>%
	filter(DupK1WithUID == "-") %>%
	select(UserID, KEY2)

dedup2 <-
dfUniqK1 %>%
	group_by(KEY2) %>%
	summarize(DuplicCountKEY2 = n()) %>% # count for EACH KEY2
	inner_join(distinct(dfUniqK1, KEY2), by="KEY2") %>%
	select(UserID, KEY2, DuplicCountKEY2)


# dfStage2A = dfStage1 + Columns DuplicCountKEY2, DupK2WithUID
dfStage2A <-
dfStage1 %>%
	left_join(dedup2, by="KEY2") %>%
	transmute(
		DuplicCountKEY2,
		DupK2WithUID = ifelse(UserID.x == UserID.y, "-", UserID.y) # the "reference" UserID with which KEY2 is duplicated
		) %>%
	cbind(dfStage1, .)

# REVIEW Duplicate on KEY2:
# NOTE: when these DuplicCountKEY2, DupK2WithUID contain NA, this means these rows were found as duplicates of KEY1 at Stage1
# dfStage2A[1:45, c("UserID", "KEY1", "KEY2", "KEY3", "DuplicCountKEY2", "DupK2WithUID")]


# REVIEW ONLY Duplicate on KEY2:
# dfStage2A[dfStage2$DuplicCountKEY2 >1 & complete.cases(dfStage2), c("UserID", "KEY1", "KEY2", "KEY3", "DuplicCountKEY2", "DupK2WithUID")]


#-----[Output for Review: CSV & Plots]--------------------------------------

outFilename <- "./outputReview/Stage2A_K1K2_Soundex.csv"
message(sprintf("    Writing %s", outFilename))

dfStage2A %>%
	filter(DupK1WithUID == "-") %>% # remove duplicated KEY1
	select(UserID, OrigFullname, KEY1, KEY2, DuplicCountKEY2, DupK2WithUID,
			 SourceFile, OrigRecordID, Party, OrigPartyOrRiding, ContribDate, AmountDollar,
			 City, ProvinceCode, PostalCode, CleanFullname) %>%
	write.table(file = outFilename, sep = ",", row.names=FALSE, col.names=TRUE)


# Verif1: TotalRowCnt = DuplicKEY1 + UniqKEY2 + DuplicKEY2
# Verif2:    UniqKEY1 = UniqKEY2 + DuplicKEY2
SmryK1K2 <- dplyr::summarize(dfStage2A,
						TotalRowCnt= n(),
						UniqKEY1   = n_distinct(KEY1),
						DuplicKEY1 = TotalRowCnt - UniqKEY1,
						UniqKEY2   = n_distinct(KEY2) -1, # -1 to exclude the KEY2 == ""
						DuplicKEY2 = UniqKEY1 - UniqKEY2)

# pivot dataframe to make bargraph
meltSmry2A <- reshape::melt.data.frame(SmryK1K2, variable_name="CountLabel")

# Add Grouping factor to distinguish the category of the Key Counts
# NOTE the n_ prefix is to force the ggplot fill to respect the order of the color palette
meltSmry2A <- cbind(meltSmry2A, CountGrp=c("0_Total", "1_KEY1", "1_KEY1", "2_KEY2", "2_KEY2"))

#      CountLabel value CountGrp
# 1 TotalRowCnt      46  0_Total
# 2    UniqKEY1      41   1_KEY1
# 3    DuplicKEY1     5   1_KEY1
# 4    UniqKEY2      26   2_KEY2
# 5    DuplicKEY2    15   2_KEY2


message("    Generating Plot PNG file")

png(file = "./outputReview/Stage2A_K1K2_Soundex_BG.png", width=500, height=500, units="px")
# Side by side BarGraph showing Keys distribution
KeyCountBarGraph(meltSmry2A, xLabelVar='CountLabel', valueVar='value', grpByVar='CountGrp') +
	labs(title = "Dedup KEY1 -> KEY2", x="", y="Count") +
	scale_fill_manual(values=c("grey10", "seagreen4", "dodgerblue4")) +
	annotate("text", x=2, y=Inf, label="K1:Fullname", hjust=0, vjust=2, size=5, fontface="bold", col="seagreen4") +
	annotate("text", x=4, y=Inf, label="K2:SoundEx", hjust=0, vjust=2, size=5, fontface="bold", col="dodgerblue4")
	
dev.off()	

# for drawing Pie Chart: Count BreakDowns of TotalRowCnt
# subset(meltSmry2A, CountLabel %in% c("DuplicKEY1", "UniqKEY2", "DuplicKEY2"), select=c(CountLabel, value))
pie2A <- meltSmry2A[meltSmry2A$CountLabel %in% c("DuplicKEY1", "UniqKEY2", "DuplicKEY2"), c("CountLabel", "value")]

# Pie Chart, simple version
# pie(pie2A$value, labels=pie2A$CountLabel, col=RColorBrewer::brewer.pal(8, "Dark2"))

png(file = "./outputReview/Stage2A_K1K2_Soundex_Pie.png", width=500, height=500, units="px")
# Pie Chart, Geek version
DistributionPie(pie2A, valueVar='value', grpByVar='CountLabel') +
	labs(title = "Duplicates KEY1 & KEY2 (SoundEx)") +
	scale_fill_manual(values=c("seagreen4", "dodgerblue4", "dodgerblue3"))

dev.off()



#=======================================================================
#
# Stage2B - KEY1 -> KEY3
#           Dedup Stage1 using KEY3 (Firstname Substitution, PostalCode, Party)
#
#=======================================================================
message("06. Stage2B: KEY1 -> KEY3 (Firstname Substitution)")

# dfUniqK1 = subset of dfStage1 having unique KEY1 
# (these rows have a nonEmpty KEY3, see Stage1 how KEY3 was constructed)
dfUniqK1 <-
dfStage1 %>%
	filter(DupK1WithUID == "-") %>%
	select(UserID, KEY3)

dedup2 <-
dfUniqK1 %>%
	group_by(KEY3) %>%
	summarize(DuplicCountKEY3 = n()) %>% # count for EACH KEY3
	inner_join(distinct(dfUniqK1, KEY3), by="KEY3") %>%
	select(UserID, KEY3, DuplicCountKEY3)


# dfStage2B = dfStage1 + Columns DuplicCountKEY3, DupK3WithUID
dfStage2B <-
dfStage1 %>%
	left_join(dedup2, by="KEY3") %>%
	transmute(
		DuplicCountKEY3,
		DupK3WithUID = ifelse(UserID.x == UserID.y, "-", UserID.y) # the "reference" UserID with which KEY3 is duplicated
		) %>%
	cbind(dfStage1, .)


#-----[Output for Review: CSV & Plots]--------------------------------------

outFilename <- "./outputReview/Stage2B_K1K3_FNSubs.csv"
message(sprintf("    Writing %s", outFilename))

dfStage2B %>%
	filter(DupK1WithUID == "-") %>% # remove duplicated KEY1
	select(UserID, OrigFullname, KEY1, KEY3, DuplicCountKEY3, DupK3WithUID,
			 SourceFile, OrigRecordID, Party, OrigPartyOrRiding, ContribDate, AmountDollar,
			 City, ProvinceCode, PostalCode, CleanFullname) %>%
	write.table(file = outFilename, sep = ",", row.names=FALSE, col.names=TRUE)


# Verif1: TotalRowCnt = DuplicKEY1 + UniqKEY3 + DuplicKEY3
# Verif2:    UniqKEY1 = UniqKEY3 + DuplicKEY3
SmryK1K3 <- dplyr::summarize(dfStage2B,
						TotalRowCnt= n(),
						UniqKEY1   = n_distinct(KEY1),
						DuplicKEY1 = TotalRowCnt - UniqKEY1,
						UniqKEY3   = n_distinct(KEY3) -1, # -1 to exclude the KEY3 == ""
						DuplicKEY3 = UniqKEY1 - UniqKEY3)

# pivot dataframe to make bargraph
meltSmry2B <- reshape::melt.data.frame(SmryK1K3, variable_name="CountLabel")

# Add Grouping factor to distinguish the category of the Key Counts
# NOTE the n_ prefix is to force the ggplot fill to respect the order of the color palette
meltSmry2B <- cbind(meltSmry2B, CountGrp=c("0_Total", "1_KEY1", "1_KEY1", "3_KEY3", "3_KEY3"))


message("    Generating Plot PNG file")

png(file = "./outputReview/Stage2B_K1K3_FNSubs_BG.png", width=500, height=500, units="px")
# Side by side BarGraph showing Keys distribution
KeyCountBarGraph(meltSmry2B, xLabelVar='CountLabel', valueVar='value', grpByVar='CountGrp') +
	labs(title = "Dedup KEY1 -> KEY3", x="", y="Count") +
	scale_fill_manual(values=c("grey10", "seagreen4", "deeppink4")) +
	annotate("text", x=2, y=Inf, label="K1:Fullname", hjust=0, vjust=2, size=5, fontface="bold", col="seagreen4") +
	annotate("text", x=4, y=Inf, label="K3:Firstname Subs", hjust=0, vjust=2, size=5, fontface="bold", col="deeppink4")
	
dev.off()

png(file = "./outputReview/Stage2B_K1K3_FNSubs_Pie.png", width=500, height=500, units="px")
# Pie Chart, Geek version
DistributionPie(meltSmry2B[meltSmry2B$CountLabel %in% c("DuplicKEY1", "UniqKEY3", "DuplicKEY3"), c("CountLabel", "value")],
					 valueVar='value', grpByVar='CountLabel') +
	labs(title = "Duplicates KEY1 & KEY3 (Firstname Substitution)") +
	scale_fill_manual(values=c("seagreen4", "deeppink4", "deeppink3"))

dev.off()




#=======================================================================
#
# Stage3A - Dedup KEY1 -> KEY2 -> KEY3
#
#=======================================================================
message("07. Stage3A: KEY1 -> KEY2 -> KEY3")

# subset of dfStage2 having unique KEY2
dfUniqK2 <-
dfStage2A %>%
	filter(DupK2WithUID == "-") %>%
	select(UserID, KEY3)

dedup3 <-
dfUniqK2 %>%
	group_by(KEY3) %>%
	summarize(DuplicCountKEY3 = n()) %>% # count for EACH KEY3
	inner_join(distinct(dfUniqK2, KEY3), by="KEY3") %>%
	select(UserID, KEY3, DuplicCountKEY3)

# dfStage2A + Columns DuplicCountKEY3, DupK3WithUID
dfStage3A <-
dfStage2A %>%
	left_join(dedup3, by="KEY3") %>%
	transmute(
		DuplicCountKEY3,
		DupK3WithUID = ifelse(UserID.x == UserID.y, "-", UserID.y) # the UserID having the same KEY3
		) %>%
	cbind(dfStage2A, .) 


#-----[Output for Review: CSV & Plots]--------------------------------------

outFilename <- "./outputReview/Stage3A_K1K2K3.csv"
message(sprintf("    Writing %s", outFilename))

dfStage3A %>%
	filter(DupK1WithUID == "-") %>% # remove duplicated KEY1
	select(UserID, OrigFullname, KEY1, KEY2, DuplicCountKEY2, DupK2WithUID, KEY3, DuplicCountKEY3, DupK3WithUID,
			 SourceFile, OrigRecordID, Party, OrigPartyOrRiding, ContribDate, AmountDollar,
			 City, ProvinceCode, PostalCode, CleanFullname) %>%
	write.table(file = outFilename, sep = ",", row.names=FALSE, col.names=TRUE)

Smry3A <- dplyr::summarize(dfStage3A,
						TotalRowCnt= n(),
						UniqKEY1   = n_distinct(KEY1),
						DuplicKEY1 = TotalRowCnt - UniqKEY1,
						UniqKEY2   = n_distinct(KEY2) -1, # -1 to exclude the KEY2 == ""
						DuplicKEY2 = UniqKEY1 - UniqKEY2)

# KEY3 is applied on the subset of unique KEY2, therefore its counts must be computed on that subset
# the most convenient is the dedup3 subset
Smry3A <- cbind(Smry3A,
					 UniqKEY3   = sum(dedup3$DuplicCountKEY3 == 1),
					 DuplicKEY3 = sum(dedup3[which(dedup3$DuplicCountKEY3 > 1), "DuplicCountKEY3"]))

# VERIF
# TotalRowCnt = UniqKEY1 + DuplicKEY1
# UniqKEY1    = UniqKEY2 + DuplicKEY2
# UniqKEY2    = UniqKEY3 + DuplicKEY3
#   TotalRowCnt UniqKEY1 DuplicKEY1 UniqKEY2 DuplicKEY2 UniqKEY3 DuplicKEY3
# 1            46         41          5         26         15         13         13

# pivot dataframe to make bargraph
meltSmry3A <- reshape::melt.data.frame(Smry3A, variable_name="CountLabel")

# Add Grouping factor to distinguish the category of the Key Counts
# NOTE the n_ prefix is to force the ggplot fill to respect the order of the color palette
meltSmry3A <- cbind(meltSmry3A, CountGrp=c("0_Total", "1_KEY1", "1_KEY1", "2_KEY2", "2_KEY2", "3_KEY3", "3_KEY3"))


message("    Generating Plot PNG file")

png(file = "./outputReview/Stage3A_K1K2K3_BG.png", width=500, height=500, units="px")
# Side by side BarGraph showing Keys distribution
KeyCountBarGraph(meltSmry3A, xLabelVar='CountLabel', valueVar='value', grpByVar='CountGrp') +
	labs(title = "Dedup KEY1 -> KEY2 -> KEY3", x="", y="Count") +
	scale_fill_manual(values=c("grey10", "seagreen4", "dodgerblue4", "deeppink4")) +
	annotate("text", x=2, y=Inf, label="K1:Fullname", hjust=0, vjust=2, size=5, fontface="bold", col="seagreen4") +
	annotate("text", x=4, y=Inf, label="K2:SoundEx", hjust=0, vjust=2, size=5, fontface="bold", col="dodgerblue4") +
	annotate("text", x=6, y=Inf, label="K3:FN Subs", hjust=0, vjust=2, size=5, fontface="bold", col="deeppink4")

dev.off()

# INCORRECT & Confusing: Pie Chart, Geek version
# DistributionPie(meltSmry3A[-which(meltSmry3A$CountLabel %in% c("TotalRowCnt", "UniqKEY1")), c("CountLabel", "value")],
# 					 valueVar='value', grpByVar='CountLabel') +
# 	#labs(title = "Case 'KEY1 + KEY3' \nKEY1 = FullName | PC | Party\nKEY3 = SoundEx1stname | PC | Party\nUniqKEY1 = UniqKEY3 + DuplicKEY3") +
# 	labs(title = "Case 'KEY1 + KEY3' (Firstname Substitution)\nDuplicates Shares") +
# 	scale_fill_manual(values=c("seagreen4", "dodgerblue4", "dodgerblue3", "deeppink4", "deeppink3"))


					  
#=======================================================================
#
# Stage3B - Dedup KEY1 -> KEY3 -> KEY2
#
#=======================================================================
message("08. Stage3B: KEY1 -> KEY3 -> KEY2")

# subset of dfStage2 having unique KEY2
dfUniqK3 <-
dfStage2B %>%
	filter(DupK3WithUID == "-") %>%
	select(UserID, KEY2)

dedup3 <-
dfUniqK3 %>%
	group_by(KEY2) %>%
	summarize(DuplicCountKEY2 = n()) %>% # count for EACH KEY2
	inner_join(distinct(dfUniqK3, KEY2), by="KEY2") %>%
	select(UserID, KEY2, DuplicCountKEY2)

# dfStage2B + Columns DuplicCountKEY2, DupK2WithUID
dfStage3B <-
dfStage2B %>%
	left_join(dedup3, by="KEY2") %>%
	transmute(
		DuplicCountKEY2,
		DupK2WithUID = ifelse(UserID.x == UserID.y, "-", UserID.y) # the UserID having the same KEY2
		) %>%
	cbind(dfStage2B, .) 


#-----[Output for Review: CSV & Plots]--------------------------------------

outFilename <- "./outputReview/Stage3B_K1K3K2.csv"
message(sprintf("    Writing %s", outFilename))

dfStage3B %>%
	filter(DupK1WithUID == "-") %>% # remove duplicated KEY1
	select(UserID, OrigFullname, KEY1, KEY3, DuplicCountKEY3, DupK3WithUID, KEY2, DuplicCountKEY2, DupK2WithUID,
			 SourceFile, OrigRecordID, Party, OrigPartyOrRiding, ContribDate, AmountDollar,
			 City, ProvinceCode, PostalCode, CleanFullname) %>%
	write.table(file = outFilename, sep = ",", row.names=FALSE, col.names=TRUE)


Smry3B <- dplyr::summarize(dfStage3B,
						TotalRowCnt= n(),
						UniqKEY1   = n_distinct(KEY1),
						DuplicKEY1 = TotalRowCnt - UniqKEY1,
						UniqKEY3   = n_distinct(KEY3) -1, # -1 to exclude the KEY3 == ""
						DuplicKEY3 = UniqKEY1 - UniqKEY3)

# KEY2 is applied on the subset of unique KEY3, therefore its counts must be computed on that subset
# the most convenient is the dedup3 subset
Smry3B <- cbind(Smry3B,
					 UniqKEY2   = sum(dedup3$DuplicCountKEY2 == 1),
					 DuplicKEY2 = sum(dedup3[which(dedup3$DuplicCountKEY2 > 1), "DuplicCountKEY2"]))

# VERIF
# TotalRowCnt = UniqKEY1 + DuplicKEY1
# UniqKEY1    = UniqKEY3 + DuplicKEY3
# UniqKEY2    = UniqKEY2 + DuplicKEY2
#   TotalRowCnt UniqKEY1 DuplicKEY1 UniqKEY3 DuplicKEY3 UniqKEY2 DuplicKEY2
# 1            46         41          5         28         13         14         14

# pivot dataframe to make bargraph
meltSmry3B <- reshape::melt.data.frame(Smry3B, variable_name="CountLabel")

# Add Grouping factor to distinguish the category of the Key Counts
# NOTE the n_ prefix is to force the ggplot fill to respect the order of the color palette
meltSmry3B <- cbind(meltSmry3B, CountGrp=c("0_Total", "1_KEY1", "1_KEY1", "2_KEY3", "2_KEY3", "3_KEY2", "3_KEY2"))


message("    Generating Plot PNG file")

png(file = "./outputReview/Stage3B_K1K3K2_BG.png", width=500, height=500, units="px")
# Side by side BarGraph showing Keys distribution
KeyCountBarGraph(meltSmry3B, xLabelVar='CountLabel', valueVar='value', grpByVar='CountGrp') +
	labs(title = "Dedup KEY1 -> KEY3 -> KEY2", x="", y="Count") +
	scale_fill_manual(values=c("grey10", "seagreen4", "deeppink4", "dodgerblue4")) +
	annotate("text", x=2, y=Inf, label="K1:Fullname", hjust=0, vjust=2, size=5, fontface="bold", col="seagreen4") +
	annotate("text", x=4, y=Inf, label="K3:FN Subs", hjust=0, vjust=2, size=5, fontface="bold", col="deeppink4") +
	annotate("text", x=6, y=Inf, label="K2:SoundEx", hjust=0, vjust=2, size=5, fontface="bold", col="dodgerblue4")

dev.off()

#---(end)---#

