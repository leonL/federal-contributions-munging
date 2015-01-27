normalize_names <- function(full_names){
  
  # Clean a vector of names and output the clean string, first name, last name, and remaining characters.
  # The original fullname is suppose to be in the format "Firstname Lastname", "Firstname M. Lastname"
  # Cleaning consists of the following steps:
  # 1. CleanUp: Remove leading & trailing spaces, Remove salutation (Mr, Ms, Dr, Pr, M)
  # 2. convert to all lower case
  # 3. convert accented characters (é) to their alphabetical counterparts (e)
  # 4. drop punctuation marks ("-")
  # 5. replace empty names by 'anon'
  # 6. add SoundEx calculation for Firstname and Lastname
  # TEST:
  # someTestNames <- c("Eùgènê-Rémî Çhâtëæùnêúf", "Fränçôïs J. Béràrd", "Frédérîc d'Acôté", "  J.-Rodrigue Bonbon  ", "  Kenneth J J Cox  ", "Dr. Katherine F Forest", "Ms. Elaine V. Parry", "Mr Paul Dirac", "M Johan M Gibson", "A. Einstein Jr")	
  # data.frame(OrigFullName=someTestNames, normalize_names(someTestNames))
  #
  # 2014-10-26 - Alex Yakubovich
  # 2014-11-27 - Tri Nguyen, remove lead/trail spaces, remove Salutation, add SoundEx, add test code
  
  library(dplyr)
  library(magrittr)
    
  ## iterate gsub over a vector of patterns (i.e. set of special characters)
  gsub2 <- function(pattern, replacement, x, ...) {      
    for(i in 1:length(pattern))
      x <- gsub(pattern[i], replacement[i], x, ...)
    x
  }
  
  #coerce a character vector to only contain alphabetical characters
  coerce_to_alpha <- function(names){  
    
    #convert accented characters to their alphabetical counterparts
    from <- c('Š', 'š', 'Ž', 'ž', 'À', 'Á', 'Â', 'Ã', 'Ä', 'Å', 'Æ', 'Ç', 'È', 'É',
              'Ê', 'Ë', 'Ì', 'Í', 'Î', 'Ï', 'Ñ', 'Ò', 'Ó', 'Ô', 'Õ', 'Ö', 'Ø', 'Ù',
              'Ú', 'Û', 'Ü', 'Ý', 'Þ', 'ß', 'à', 'á', 'â', 'ã', 'ä', 'å', 'æ', 'ç',
              'è', 'é', 'ê', 'ë', 'ì', 'í', 'î', 'ï', 'ð', 'ñ', 'ò', 'ó', 'ô', 'õ',
              'ö', 'ø', 'ù', 'ú', 'û', 'ý', 'ý', 'þ', 'ÿ')
    
    to <- c('S', 's', 'Z', 'z', 'A', 'A', 'A', 'A', 'A', 'A', 'A', 'C', 'E', 'E',
            'E', 'E', 'I', 'I', 'I', 'I', 'N', 'O', 'O', 'O', 'O', 'O', 'O', 'U',
            'U', 'U', 'U', 'Y', 'B', 'Ss','a', 'a', 'a', 'a', 'a', 'a', 'a', 'c',
            'e', 'e', 'e', 'e', 'i', 'i', 'i', 'i', 'o', 'n', 'o', 'o', 'o', 'o',
            'o', 'o', 'u', 'u', 'u', 'y', 'y', 'b', 'y')
    
    normalized <- gsub2(from, to, names)
    
    # Replace non alphabetical characters by an empty string
    normalized <- gsub("[^a-z ]", "", normalized, ignore.case=TRUE)  
    
    return(normalized)
  }      
  
  split_names <- function(x){
    #split full name character vector into last name, first name and remaining characters   
    if (length(x)==0) x <- c('anon', 'anon') #if name field is empty, set first_name='anon', last_name='anon'
    
    n <- length(x)  
    first_name <- x[1]
    last_name <- x[n]  
    remaining <- paste0(x[-c(1,n)], collapse=' ')
    first_soundex <- SoundexEnhanced(first_name, 5)
    last_soundex  <- SoundexEnhanced(last_name, 5)
    
    if (length(remaining)==0) remaining <- ''    
    
    data.frame(clean_full_name=paste0(x, collapse=" "), last_name=last_name, last_soundex=last_soundex, first_name=first_name, first_soundex=first_soundex, remaining=remaining, stringsAsFactors=F)
  }
    
  full_names %>%
  	 # Remove leading & trailing spaces
  	 stringr::str_trim(side = "both") %>%
    # Remove the Salutation (M. Ms. Mr. Dr. Pr.) from Fullname
    # "Dr. Paul Dirac", "Mr. Paul Dirac", "Mr Paul Dirac", "M Paul Dirac" -> "Paul Dirac"
    gsub("^(Ms|Mr|M|Dr|Pr)([\\.| ]*\\s+)(.+$)", "\\3", ., ignore.case=TRUE) %>%
    tolower %>% 
    coerce_to_alpha %>%
    strsplit(split=' ') %>%     
    lapply(FUN=split_names) %>%
    rbind_all    
}
