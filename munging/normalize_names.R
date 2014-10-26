normalize_names <- function(full_name){
  
  # Clean a vector of names and output the clean string, first name, last name, and remaining characters. 
  # Cleaning consists of the following steps:
  # 1. convert to all lower case
  # 2. convert accented characters (é) to their alphabetical counterparts (e)
  # 3. drop punctuation marks ("-")
  # 4. replace empty names by 'anon'
  
  library(dplyr)
    
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
    normalized <- gsub("[^a-z| ]", "", normalized, ignore.case=TRUE)  
    
    return(normalized)
  }      
    
  full_name_lowered <- tolower(full_name)
  
  nm <- coerce_to_alpha(full_name_lowered)
  
  nm_split <- strsplit(nm, split=' ')      
  
  #split full name into last name, first name and remaining characters 
  nm_split_result <- lapply(nm_split, FUN=function(x){
    
    if (length(x)==0) x <- c('anon', 'anon') #if name field is empty, set first_name='anon', last_name='anon'
    
    n <- length(x)  
    first_name <- x[1]
    last_name <- x[n]  
    remaining <- paste0(x[-c(1,n)], collapse=' ')
    
    if (length(remaining)==0) remaining <- ''    
    
    data.frame(last_name=last_name, first_name=first_name, remaining=remaining, stringsAsFactors=F)   
  }
  )
  
  nm_df <- rbind_all(nm_split_result)
  
  return(cbind(clean_full_name=nm, nm_df)) 
}