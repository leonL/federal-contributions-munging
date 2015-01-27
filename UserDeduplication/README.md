Data4Good - Datathon Dec2014
===============

## Overview
Original source data from [Elections Canada](http://www.elections.ca/WPAPPS/WPF/EN/EDA). NOTE: this is just for reference the raw data from this site is NOT used.  
[Official GitHub Repo](https://github.com/leonL/federal-contributions) (Data4Good Datathon Code & pre-processed original data)

The goal is to cleanup Elections Canada database. This database contains the records of donations for 5 federal parties from donors across Canada from 2004 to 2013. The original data suffer from some data quality issues: Postal code may not be always correct, the federal party name is not always supplied, discrepancy between donor's postal code and that of the target riding. In particular, there is an issue of uniquely identifying a donor. The same donor may be identified as multiple different persons. This is due to data input errors made throughout the years. The errors could be misspelling in the name or typos, incorrect Postal code, intentional variation of the first name, etc.

## Project Files content
This repo contains the code to solve the issue of "Uniquely Identify a Donor". The project code is composed of:

- [data (dir)](./data): A Sample of test data, used as read-only. This is a small sample of around 150 records from the [pre-processed original data](https://github.com/leonL/federal-contributions/tree/master/munged_data). The original data has been **altered** intentionally to obfuscate donor real names. Also intentional errors, tricky name variations and duplicates are intentionally introduced for testing purposes.
- [outputReview/Demo (dir)](./outputReview/Demo): contains CSV & Plots for verification of the results.
- [DedupUsersMain.R](./DedupUsersMain.R) is the main script which should be executed to produce the analysis results.
- [DedupUtilFunctions.R](./DedupUtilFunctions.R) contains various utility functions used by the main script.
- [normalize_names.R](./normalize_names.R) is an utility to clean up user full name.
- [SimilarFirstnames.csv](./SimilarFirstnames.csv) is a "home made" dictionary of similar firstnames. It is used by the "Firstname Substitution" strategy. Descibed in Stage3 below.

## Deduplication Strategies
The goal is two fold:
- Duplicate detection based on a Key
- Determine similarities between two different names
- Combine several Dedup strategies in order to reach the maximum rate of duplicate detection efficiency.

The data is processed through different stages:

- **Stage1**: Duplicate detection using **KEY1** = Fullname, PostalCode, PartyName
- **Stage2**: on duplicates which survive Stage1, **KEY2** = Soundex(Firstname), PostalCode, PartyName
- **Stage3**: on duplicates which survive Stage2, **KEY3** = Firstname Substitution by Reference Firstname, PostalCode, PartyName

At each stage, the [DedupUsersMain.R](./DedupUsersMain.R) script generates a [CSV](./outputReview/Demo) and [Plots](./outputReview/Demo) for human verification. This script will **NOT** delete any data. As of this writing (Dec 2014) we are not sure of a strategy to determine with certainty how 2 seemingly similar users could be declared as true duplicates.

## How to Finalize the UserID?
Once the "User Dedup" team determines the best strategy to dedup the users. The DedupUsersMain.R script will be improved to assign the final unique UserID to each donor.

