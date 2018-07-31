#Capitalize the first letter of each word in a substring
  FirstLetterCap <- function(x) {
	s <- strsplit(x, " ")[[1]]
	paste(toupper(substring(s, 1, 1)), tolower(substring(s, 2)),
		  sep = "", collapse = " ")
  }
