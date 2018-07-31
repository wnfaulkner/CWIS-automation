#Capitalize the first letter of each word in a substring
  
  FirstLetterCap_OneElement <- function(x){
    if(class(x) != "character"){print(paste("Warning: Input is of class ",toupper(class(x)),". Coercing to character vector.",sep = ""))}
    #if(length(x) > 1){print("Warning: Input has length greater than 1. All elements will be collapsed into single element.")}
    if(!is.null(dim(x))){print("Error: Input is not an atomic vector.")}
    
    s <- strsplit(as.character(x), " ")  %>% unlist
    paste(toupper(substring(s, 1, 1)), tolower(substring(s, 2)), sep = "", collapse = " ")
  }
  
  FirstLetterCap_MultElements <- function(x) {
	  s <- strsplit(as.character(x), " ")
	  sapply(s, FirstLetterCap_OneElement)
  }
