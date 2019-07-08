#00000000000000000000000000000000000000000000000000000000000#
#0000       STANDARD UTIL FUNCTIONS FOR R SCRIPTS       0000#
#00000000000000000000000000000000000000000000000000000000000#

#Function to convert character string to object
  as.object <- function(x){return(eval(parse(text = x)))}

#FUNCTIONS FOR CODE CLOCKING
  #TODO: MAKE UTILS FUNCTIONS THAT 
    #(A) DESIGNATE SYS.TIME() 'BOOKMARKS' OR 'WAYPOINTS'
    #(B) PRINT A TABLE OF WAYPOINTS IN ROWS AND COLUMNS REPRESENTING
      #(1) TIME SINCE MOST RECENT WAYPOINT
      #(2) CUMULATIVE TIME SINCE FIRST WAYPOINT
      #(3) TIME SINCE MOST RECENT WAYPOINT/TIME SINCE FIRST WAYPOINT (PERCENTAGE)
  
 #TODO: MAKE UTILS MEMORY FUNCTIONS
    #MEASURE MEMORY USAGE OF OBJECTS
    #FUNCTION TO LIST MEMORY USAGE OF LARGEST OBJECTS IN CURRENT ENVIRONMENT
    #WARN WHEN APPROACHING DEFINED MEMORY USAGE LIMITS AND, IF INSIDE OF A LOOP, BREAK THE LOOP

#FUNCTIONS FOR ERROR HANDLING --------------------
  #Return TRUE/FALSE if expression throws an error
    IsError <- function(.expr){
      result <-
        ifelse(
          tryCatch(
            .expr, 
            error = function(x) {return(TRUE)}
          ) == TRUE,
          TRUE,
          FALSE
        )
      return(result)
    }
  
  #StopQuietly (no error message)
  	StopQuietly <- function() {
		  opt <- options(show.error.messages = FALSE)
		  on.exit(options(opt))
		  stop()
		}
      
  #Print standard loop messages
    PrintLoopMessages <- function(loop.index){ #loop.index = object which defines loop number (e.g. 'i' in most common cases)
      print(
        paste0(
          "LOOP '",loop.index, "' -- Loop num: ", eval(parse(text = loop.index))
        )
      )
    }
    
#FUNCTIONS FOR INITIAL CODE SETUP & LOADING DATA --------------------
  
  #Install commonly used packages
    InstallCommonPackages <- function(){
      install.packages('devtools')
      install.packages("readr")
      install.packages("data.table")
      install.packages("dplyr")
      install.packages('tidyr')
      install.packages("googlesheets")
      install.packages("stringr")
      install.packages('officer')
      install.packages("magrittr")
      install.packages('reshape2')
      install.packages('ggplot2')
      install.packages('xlsx')
      #install.packages('styler')
    }
  
  #Install commonly used packages
    LoadCommonPackages <- function(){
      #library(devtools)
      library(readr)
      #library(data.table)
      #library(plyr)
      library(dplyr)
      library(tidyr)
      library(googlesheets)
      library(stringr)
      #library(officer)
      library(magrittr)
      library(reshape2)
      library(ggplot2)
      #library(xlsx)
      #library(styler)
    }

  #Select right 'n' characters of string
    substrRight <- function(x, n){
      substr(x, nchar(x)-n+1, nchar(x))
    }
    
  #Find most recently modified file in a directory
    #Test Inputs
      title.string.match = "MYDev_Master"
      file.type = ".xlsx"
      dir = "C:\\Users\\WNF\\Documents\\GIT PROJECTS\\2019-03-EDC-SNA\\3_source_tables\\"
    
    MostRecentlyModifiedFilename <- function(
      title.string.match, 
      file.type, 
      dir
    ){
      setwd(dir)
      print(paste("File Directory: ", dir, sep = ""))
      match.files.v <-
        list.files()[
          grepl(tolower(title.string.match), tolower(list.files())) &  #match title string
          grepl(  #match file type
            tolower(file.type), 
            sapply(tolower(list.files()), 
            function(x){substrRight(x, nchar(file.type))})
          ) &           
          !grepl("\\~\\$", list.files())       #restrict to non-temporary files
        ]
      
      if(length(match.files.v)==0){stop(paste0("No matching files. Files in directory: ",paste0(list.files(), collapse = ", ")))}
      
      most.recent.match.file <- match.files.v[file.info(match.files.v)$mtime == sapply(match.files.v, function(x){file.info(x)$mtime}) %>% max]
      print(paste("File Name: ", most.recent.match.file, sep = ""))
      return(most.recent.match.file)
    }
    

    
#FUNCTIONS FOR MANIPULATING VECTORS & COLUMNS --------------------
  
  #Capitalize first letter of each word in string
    proper <- function(s, strict = FALSE) {
      cap <- function(s) 
        paste(
          toupper(substring(s, 1, 1)),
          {s <- substring(s, 2); if(strict) tolower(s) else s},
          sep = "", collapse = " " 
        )
      sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
    }
    
  #Filter Vector based on condition
    FilterVector <- function(condition,vector.input){
      vector.input[condition]
    }
  
  #Remove NA from vector
    RemoveNA <- function(x){
      if(!is.null(dim(x))){stop("Input must be a vector.")}
      result <- x[!is.na(x)]
      return(result)
    }
    
 	#Remove user-defined value from vector
    RemoveValFromVector <- function(x, val){
      if(!is.null(dim(x))){stop("Input must be a vector.")}
      result <- x[!x == val]
      return(result)
    }
  
  #Replace NAs in a vector with a replacement value
    SubNA <- function(vector,na.replacement){
      vector[is.na(vector)] <- na.replacement
      return(vector)
    }
    
  #Table with NAs
    TableWithNA <- function(x){
      result <- 
        table(x, useNA = "always")
      return(result)
    }
    
  #Number of times specified substring occurs within vector of character strings
    NumSubstringMatches <- function(pattern, vector){
      sapply(
        gregexpr( pattern, as.character(vector)),
        function(x) if( x[1]==-1 ){ 0 }else{ length(x) } 
      )
    }
    
  #'Multiple gsub' to find/replace multiple patterns in a vector
    #Test Inputs
      #pattern <- questions.sem.df$row.1[!is.na(questions.sem.df$q.changename)]
      #replacement <- questions.sem.df$q.changename[!is.na(questions.sem.df$q.changename)]
      #x <- names(resp1.df)
    
    mgsub <- function(
      pattern, 
      replacement, 
      x, 
      print.replacements = c(TRUE,FALSE)
    ){
      n = length(pattern)
      if (n != length(replacement)) {
        print(pattern)
        print(replacement)
        stop("Pattern and replacement do not have the same length.")
      }
      
      result = x
      num.replacements <- vector()
      
      for (i in 1:n) {
        result[grep(pattern[i], x)] <- gsub(pattern[i], replacement[i], result[grep(pattern[i], x)])
        num.replacements[i] <- length(grep(pattern[i], x))
      }
      
      if(!missing(print.replacements)){
        #print.replacements <- match.arg(print.replacements)
        if(print.replacements){print(cbind(pattern,replacement,num.replacements))}
      }
      
      return(result)
    }
  
	#Set base reference level of a factor to level with most rows in table
    RelevelFactorWithMaxRowsAsBase <-
    	function(x){
    		if(!is.factor(x)){x <- ConvertToFactor(x)}
    		
    		base.levelname <- #name of level with most rows
    			x %>% table %>%
    			as.data.frame() %>%
      		filter(Freq == max(Freq)) %>% 
      		.[1,] %>% #just in case there are two levels with same number of rows
      		dplyr::select(".") %>% 
      		unlist %>% as.vector
    		
    		result <- relevel(x, ref = base.levelname)
    		
				return(result)
    	}
    
  #Recode only one value in a vector
    RecodeOneVal <- 
    	function(vector, current.val, recode.val){
    		
    		if(is.na(current.val)){
    			vector[is.na(vector)] <- recode.val
    		}else{
	    		vector[vector == current.val] <- recode.val
    		}  		
    		return(vector)
    	
  		}
    
  #Unique values from multiple columns of a data frame (returns list)
    #TEST INPUTS
    #df <- resp.long.df.b
    #varnames <- loop.varnames.c
    
    UniqueValsFromColnames <- 
      function(
        df, 
        varnames
      ){
      
      if(!any(varnames %in% names(df))){
        stop(
          paste0(
            c(
              "Varnames: '",
              varnames,
              "' do not exist in table names. Table names: '", 
              paste0(names(df), collapse = ", "),
              "'."
            ),
            collapse = ""
          )
        )
      }
      
      result <- 
        df[,names(df) %in% varnames] %>%
        as.data.frame %>%
        lapply(., unique) %>%
        lapply(., RemoveNA) %>%
        lapply(., as.character) %>%
        lapply(., function(x) {strsplit(x, ",")}) %>%
        lapply(., unlist) %>%
        lapply(., unique)
      return(result)
    }
    
  #All combinations of unique values of variables in a data frame
    UniqueCombnFromColnames <- 
      function(
        df, 
        varnames
      ){
      if(!(varnames %in% names(df))){
        stop(
          paste0(
            c(
              "Varnames: '",
              varnames,
              "' do not exist in table names. Table names: '", 
              paste0(names(df), collapse = ", "),
              "'."
            ),
            collapse = ""
          )
        )
      }
      
      result <- 
        UniqueValsFromColnames(df, varnames) %>%
        expand.grid(., stringsAsFactors = FALSE) %>%
        ReplaceNames(., current.names = names(.), new.names = varnames)
      return(result)
    }
    
  #Check which elements in a vector are different from the one before and return position of spots where values change
    VectorValueChangePositions <- function(x){
      check.mtx <-
        cbind(
          x[1:length(x)-1],
          x[2:length(x)]
        )
      check.mtxl <- matrix(nrow = nrow(check.mtx),ncol=ncol(check.mtx))
      for(i in 1:dim(check.mtx)[1]){
        
        check.mtx.i <- check.mtx[i,]
        
        if(any(is.na(check.mtx.i))){
          check.mtxl[i,] <- is.na(check.mtx.i)
        }else{
          check.mtxl[i,] <- check.mtx.i[1] == check.mtx.i[2]
        }
      }
      value.change.positions <- c(1,which(apply(check.mtxl,1,function(x){unlist(x[1]!=x[2])})),length(x))
      value.change.positions
      value.change.positions + 1
      
      result.ls <- list()
      for(j in 1:(length(value.change.positions)-1)){
        result.ls[[j]] <- 
          data.frame(
            start.position = ifelse(j == 1, 1, value.change.positions[j]+1), 
            end.position = value.change.positions[j+1]
          )
      }
      
      result <- 
        cbind(
          section.id = c(1:length(result.ls)),
          do.call(rbind, result.ls)
        )
      
      return(result)
    }
  
  #Collate a vector (e.g. "1, 1, 2, 2, 3, 3," -> "1, 2, 3, 1, 2, 3, 1, 2, 3")
    #vector <- output.df$slide.section.2
    
    CollateVector <- function(vector){
      collated.values <- 
        vector %>% 
        unique %>%
        .[order(.)] %>%
        RemoveNA %>% 
        rep(., length(vector %>% RemoveNA)/length(.))
      
      for(i in 1:length(vector)){
        
        if(i == 1){
          select.collated.value.num <- 1
        }
        
        if(!is.na(vector[i])){
          vector[i] <- collated.values[select.collated.value.num]
          select.collated.value.num <- select.collated.value.num + 1
        }
      }
      
      return(vector)  
    }  
 
  #Capitalize the first letter of each word in a substring
    FirstLetterCap_OneElement <- function(x){
      if(class(x) != "character"){print(paste("Warning: Input is of class ",toupper(class(x)),". Coercing to character vector.",sep = ""))}
      if(!is.null(dim(x))){print("Error: Input is not an atomic vector.")}
      
      s <- strsplit(as.character(x), " ")  %>% unlist
      paste(toupper(substring(s, 1, 1)), tolower(substring(s, 2)), sep = "", collapse = " ")
    }
    
    FirstLetterCap_MultElements <- function(x) {
      s <- strsplit(as.character(x), " ")
      sapply(s, FirstLetterCap_OneElement)
    }


#FUNCTIONS FOR CONVERTING VECTORS/VARIABLES TO DIFFERENT CLASSES -------------------- 
      
    #TESTING OBJECTS
      #x2 <- data.frame(
      #  factor.name = c("a","xa","222","xa"),
      #  character.name = c("char","a","b","cd"),
      #  logical.name = c(TRUE, FALSE,TRUE,FALSE),
      #  integer.name = c(1,2,3,4) %>% as.integer,
      #  numeric.name = c(1.1,2.2,3.3,4.4)
      #)
      #x2$character.name <- x2$character.name %>% as.character
      
      #x1 <- data.frame(numeric.name = c(1.1,2.2,3.3))
      
      #x <- x2
      
      #Convert to Factor  
        ConvertToFactor <- function(x){
          if(!is.null(dim(x))){stop("Input is not an atomic vector.")}
          result <- as.factor(x)
          return(result)
        }
      
      #Convert to Character
        ConvertToCharacter <- function(x){
          if(!is.null(dim(x))){stop("Input is not an atomic vector.")}
          result <- as.character(x)
          return(result)
        }
      
      #Convert to Logical
        ConvertToLogical <- function(x){
        if(!is.null(dim(x))){stop("Input is not an atomic vector.")}
        
        if(class(x) == "logical"){print("Input is already logical. No conversion performed.")}
        
        if(class(x) %in% c("factor","character","numeric","integer")){
          y <- as.character(x) %>% trimws(., which="both")
          unique.yvals <- unique(y)[order(unique(y))]
          
          if(length(unique.yvals) > 2){
            result <- x
            print("Input must have exactly two values. Input has more than two values. No changes made.")
          }
          
          if(length(unique.yvals) < 2){
            result <- x
            print("Input must have exactly two values. Input has less than two values.  No changes made.")
          }
          
          if(all(unique.yvals == c(0,1))){ #Factor is composed of 1 and 0 only
            result <- as.logical(y)
          }
          
          if(all(unique.yvals == c("FALSE","TRUE"))){ #Factor is composed of TRUE and FALSE only
            result <- as.logical(y)
          }
          
          if(length(unique(y)) == 2 & !all(unique.yvals == c(0,1)) & !all(unique.yvals == c("FALSE","TRUE"))){ #Factor is composed of two character strings not automatically convertible into logical
            print(unique.yvals)
            zero.val <- unique.yvals[readline(prompt = "Which value should be set as TRUE? (Enter 1 if first value, 2 if second value)") %>% as.numeric]
            y[which(y!=zero.val)] <- FALSE
            y[which(y==zero.val)] <- TRUE
            result <- as.logical(y)
          }
        }
        return(result)
      }
      
      #Convert to Numeric 
        ConvertToNumeric <- function(x){
        if(!is.null(dim(x))){stop("Input is not an atomic vector.")}
        
        if(class(x) %in% c("numeric","integer")){
          result <- as.numeric(x)
        }
        
        if(class(x) %in% c("factor")){
          if(as.numeric(as.character(x)) %>% unique %>% is.na %>% all){print("Warning: Converting character to number resulted in all NA. No numbers to convert")}
          result <- as.numeric(as.character(x))
        }
        
        if(class(x) %in% c("logical")){
          convert.LogicalToNumeric <- readline(prompt = "Input is logical. Would you like to convert to binary - FALSE = 0, TRUE = 1? (Y/N)")
          if(convert.LogicalToNumeric == "Y"){
            result <- as.numeric(x)
          }else{
            result <- x
            print("Cannot convert logical to numeric. No output.")
          }
        }
        
        if(class(x) %in% c("character")){
          if(as.numeric(x) %>% unique %>% is.na %>% all){print("Warning: Converting character to number resulted in all NA.")}
          result <- as.numeric(x)
        }
        return(result)
      }
      
      #Convert to Integer
        ConvertToInteger <- function(x){
        if(!is.null(dim(x))){stop("Input is not an atomic vector.")}
        
        if(class(x) %in% c("numeric","integer")){ result <- as.integer(x)}
        
        if(class(x) %in% c("factor")){
          y <- as.integer(as.character(x))
          if(y %>% unique %>% is.na){print("Warning: Converting character to integer resulted in all NA. No numbers to convert")}
          result <- y
        }
        
        if(class(x) %in% c("logical")){
          convert.LogicalToNumeric <- readline(prompt = "Input is logical. Would you like to convert to binary - FALSE = 0, TRUE = 1? (Y/N)")
          if(convert.LogicalToNumeric == "Y"){
            result <- as.numeric(x)
          }else{
            print("Cannot convert logical to numeric. No output.")
            result <- x
          }
        }
        
        if(class(x) %in% c("character")){
          if(as.integer(x) %>% unique %>% is.na){print("Warning: Converting character to integer resulted in all NA.")}
          result <- as.integer(x)
        }
        return(result)
      }
      
      #Convert to Date
        ConvertToDate <- function(x){
        if(!is.null(dim(x))){stop("Input is not an atomic vector.")}
        y <- as.character(x)
        result <- tryCatch({y %>% as.Date()},
                           error=function(e){
                             return(y)
                             message("Error: Unable to convert character to date format.")
                           }
        )
        return(result)
        }
      
      #Set Class of a named column
        SetColClass <- function(
          tb, 
          colname, 
          to.class = c("factor", "character", "logical", "numeric", "integer", "date")
        ){
          
          if(!(colname %in% names(tb))){
            stop(paste0("Colname '", colname, "' is not present in tb names."))
          }
          match.arg(to.class)
          
          #print(class(tb[[which(names(tb) == colname)]]) )
          
          if(to.class == "factor"){tb[[which(names(tb) == colname)]] <- ConvertToFactor(tb[[which(names(tb) == colname)]])}
          if(to.class == "character"){tb[[which(names(tb) == colname)]] <- ConvertToCharacter(tb[[which(names(tb) == colname)]])}
          if(to.class == "logical"){tb[[which(names(tb) == colname)]] <- ConvertToLogical(tb[[which(names(tb) == colname)]])}
          if(to.class == "numeric"){tb[[which(names(tb) == colname)]] <- ConvertToNumeric(tb[[which(names(tb) == colname)]])}
          if(to.class == "integer"){tb[[which(names(tb) == colname)]] <- ConvertToInteger(tb[[which(names(tb) == colname)]])}
          if(to.class == "date"){tb[[which(names(tb) == colname)]] <- ConvertToDate(tb[[which(names(tb) == colname)]])}
        
          return(tb)
        }

#FUNCTIONS FOR MANIPULATING DATA FRAMES & TABLES --------------------
  
  #Replace value in table (table form of gsub)
    TableGsub <- 
      function(
        tb, 
        value, 
        replacement.value
      ){
        result <- apply(tb, c(1:2), function(x){ifelse(x == value, replacement.value, x)})
        return(result)
      }
        
  #Transpose table with correct column & row names
    TransposeTable <- function(
      tb,
      keep.first.colname = c(TRUE,FALSE)
    ){
      
      result.rownames <- names(tb)
      
      result1.tb <- tb %>% t %>% as_tibble %>% mutate(x = result.rownames)
      
      result.colnames <- result1.tb[1,] %>% unlist %>% as.vector
      
      result2.tb <- 
        result1.tb[-1,] %>% 
        ReplaceNames(., names(.), result.colnames) %>%
        MoveColsLeft(., colnames = names(.)[ncol(.)])
        
      if(missing(keep.first.colname)){keep.first.colname <- FALSE}
      
      if(!keep.first.colname){
        names(result2.tb)[1] <- ""
      }
      
      return(result2.tb)
    }
      
  #Output variable names in data frame which can be converted to numeric       
    NumericVarnames <- function(df) {
        result <- 
          df %>%
          apply(., 2, unique) %>%
          sapply(., 
                 function(x){
                   ifelse(
                     length(x) == 1,
                     as.numeric(x) %>% is.na(.) %>% sum(.) < 1,
                     as.numeric(x) %>% is.na(.) %>% sum(.) <= 1
                   )
                 }
          ) %>%
          names(df)[.]
        return(result)
      }
  
  #Output variable names in data frame which are of user-defined class
     VarnamesOfClass <- function(
       dat,
       colclass = c("numeric","character","logical","factor","date","integer") 
     ){
        colclass <- match.arg(colclass)
        
        name.classes <- 
          lapply(dat, class) %>% 
          unlist %>% 
          as.vector(.)
        
        result <- 
          names(dat)[which(name.classes %in% colclass)]
        
        return(result)
      }
        
  #Order Data Frame by specific column
    OrderDfByVar <- function(df, order.by.varname, rev) {
      if(!exists("rev")){rev <- FALSE}
      
      df <- as.data.frame(df)
      
      if(rev == FALSE){
        result <- df[order(df[,names(df) == order.by.varname]),]
      }else{
        result <- df[rev(order(df[,names(df) == order.by.varname])),]
      }
        
      return(result)
    }
  
  #Manually order a table's rows using a character vector which contains all values in a names variable in the table
    #Test Inputs
      #tb = result 
      #tb.order.varnames = names(result)[names(result) == names(y.headers)] 
      #ordering.vectors.list = list(config.input$y.varname.order %>% strsplit(., ",") %>% unlist)
    
    ManualOrderTableByVectorsWithValuesCorrespondingToVariableInTable <- function(
      tb, #table to order
      tb.order.varnames, #names of variable in table with values corresponding to vector 
      ordering.vectors.list, #vectors with ordered values
      ...
    ){
      if(c(tb.order.varnames) %in% names(tb) %>% all %>% not){
        stop(paste0("No variable named '", tb.order.varname, "' in data table."))
      }
      
      if(length(tb.order.varnames) != length(ordering.vectors.list)){
        stop(
          paste0(
            "Number of tb.order.varnames (", 
            length(tb.order.varnames),
            ") does not match number of ordering vectors in the list (",
            length(ordering.vectors.list),")."
          )
        )
      }
      
      tb.ls <- list()
      
      for(i in 1:length(tb.order.varnames)){  
        
        if(i != 1){tb <- tb.ls[[i-1]]} #cumulatively build final table
        tb.order.varname.i <- tb.order.varnames[i]
        ordering.vector.i <- ordering.vectors.list[[i]]
        
        order.tb.i <- #one-column data frame with correctly ordered values
          ordering.vector.i %>% 
          as.data.frame(., stringsAsFactors = FALSE) %>% 
          mutate(num.var = paste(1:length(ordering.vector.i), ordering.vector.i,sep=".")) %>%
          ReplaceNames(
            df = ., 
            current.names = names(.), 
            new.names = c(tb.order.varname.i,paste0(tb.order.varname.i,".num"))
          )
        
        tb.ls[[i]] <- #same adding a column with those values and numbers in front
          left_join(
            tb,
            order.tb.i,
            by = tb.order.varname.i
          ) 
      }
      
      
      order.formula <- paste0("result.tb$", tb.order.varnames, ".num") %>% 
        paste(., collapse = ",") %>% 
        paste0("order(",.,")")
      
      result.tb <- tb.ls[[i]]
      result <- 
        result.tb[eval(expr = parse(text = order.formula)),] %>%
        dplyr::select(SelectNamesIn(tb = ., condition = "NOT.IN", paste0(tb.order.varnames,".num")))
        
      return(result)
    }  
    
  #Replace names in a data frame
    ReplaceNames <- function(df,current.names, new.names) {
      
      #Data Checks
      if(!is.data.frame(df)){
        stop("Input not a data frame. Input must be of class 'data.frame'.")
      }
      
      #New Names Checks
      if(!exists("new.names")){
        new.names <- readline(prompt = "No new names defined. Enter a vector of new names to replace current names: ")
      }
      
      if(!is.character(new.names)){
        new.names <- as.character(new.names)
        warning("'new.names' input not of class 'character.' Coercing to character vector.")
      }
      
      #Current Names Checks
      if(!exists("current.names")){
        
        if(length(names(df)) == length(new.names)){
          print("No current names to replace specified. All current names will be replaced.")
          current.names <- names(df)
        }
        
        if(length(names(df)) != length(new.names)){
          stop(
            paste(
              "No current names to replace specified. Current df has ",
              length(names(df)),
              " columns. New names is of length ",
              length(new.names),
              ".",
              sep = ""
            )
          )
        }
        
      } #End of if statement for when current.names not defined by user
      
      if(any(!current.names %in% names(df))){
        warning(
          paste(
            "One or more current.names were not found in input data frame: '",
            current.names[!current.names %in% names(df)],
            "'. ",
            sep = ""
          )
        )
      }
      
      #Actual Function: name replacement
      names(df)[names(df) %in% current.names] <- new.names
      return(df)
    }
  
  #Select names based on in/not in string vector
    SelectNamesIn <- function(tb, condition = c("IN","NOT.IN"), char.vector){
      condition <- match.arg(condition)
      if(condition == "IN"){return(names(tb)[names(tb) %in% char.vector])}
      if(condition == "NOT.IN"){return(names(tb)[!(names(tb) %in% char.vector)])}
    }
  
  #Select columns based on in/not in string vector
    SelectColsIn <- function(tb, condition = c("IN","NOT.IN"), char.vector){
      condition <- match.arg(condition)
      if(condition == "IN"){return(tb[,names(tb) %in% char.vector])}
      if(condition == "NOT.IN"){return(tb[,!(names(tb) %in% char.vector)])}
    }
  
  #Move named columns to the front/left side of table
    MoveColsLeft <- function(
      dat,
      colnames
    ){
      
      if(any(!(colnames %in% names(dat)))){
        stop(paste0("colnames '", colnames[!(colnames %in% names(dat))], "' missing from table names."))
      }
      
      result <- 
        cbind(
          dat %>% dplyr::select(colnames),
          dat[,!(names(dat) %in% colnames)]
        ) %>% as_tibble()
      
      return(result)
    }
    
  #Move named columns to the back/right side of table
    MoveColsRight <- function(
      dat,
      colnames
    ){
      
      if(any(!(colnames %in% names(dat)))){
        stop(paste0("colnames '", colnames[!(colnames %in% names(dat))], "' missing from table names."))
      }
      
      result <- 
        cbind(
          dat[,!(names(dat) %in% colnames)],
          dat %>% dplyr::select(colnames)
        ) %>% as_tibble()
      
      return(result)
    }
      
  #Left Join & Replace NAs
    left.join.NA <- 
    	function(
    		x, 
    		y, 
    		by, 
    		na.replacement 
    		#keep.na.from.right.side = c(TRUE,FALSE)
  		) {
	      result <- 
	      	left_join(x = x, y = y, by = by, stringsAsFactors = FALSE) %>% 
	        mutate_all(funs(replace(., which(is.na(.)), na.replacement)))
	      
	      return(result)
    }

  #Unique Values of Named Variables in Data Frame
    #TODO: Generalize to graph code as well? So treat like a pivot table with arbitrary number of x.vars and y.vars, a summary var and a summary function.
    #TODO: Maybe would make it so could use a single config table?
    #TODO: Should generalize so that can handle arbitrary number of nested variables on both axes like pivot
    #Test Inputs
      #varnames <- graph.varnames.d
      #tb <- resp.long.tb %>% as_tibble()
    
    UniqueVariableValues <- function( tb, varnames){
      
      varnames <- as.character(varnames)
      tb <- as_tibble(tb)
      #all.cats.ls <- list()
      
      result <- apply(tb %>% dplyr::select(varnames), 2, function(x) RemoveNA(unique(x)))
      
      return(result)
    }
    
  #Unique Values of Measures of Key Column (for long data)
    #Test Inputs
      #.data <- resp.long.df
      #.keyvarname <- 
    
    UniqueKeyVals <- function(.data, .keyvarname, .keyvals, .measurevarname) {
      keyvals <- as.character(.keyvals)
      keyvarname <- as.character(.keyvarname)
      measurevarname <- as.character(.measurevarname)
      tb <- as_tibble(.data)
      
      result <- 
        tb %>%
        filter(keyvarname %in% keyvals) %>%
        dplyr::select(measurevarname) %>%
        unique(.)
        
        
      return(result)
    }
  
  #Reshaping data into long format based on splitting a column on a character
    #Test inputs
      #df = ans.set.tb[,1:3]
      #id.varname = "ans.set.id"
      #split.varname = "ans.nums"
      #split.char = ";"
  
    SplitColReshape.ToLong <- function(df, id.varname, split.varname, split.char){ 
      
      if(!is.data.frame(df)){stop("Input not a data frame.")}
    
      if(!exists("split.char")){
        split.varname <- readline(prompt = "Enter the variable name that will be split and used to reshape data: ")
      }
      
      if(!exists("split.varname")){
        split.varname <- readline(prompt = "Enter the variable name that will be split and used to reshape data: ")
      }
      
      if(!exists("split.char")){
        split.char <- readline(prompt = "Enter the character(s) you would like to split your variable on: ")
      }
      
      if(dim(df)[1] == 0){
        result <- df
      }else{
        id.var <- df[,names(df)==id.varname]
        split.var <- df[,names(df)==split.varname]
        
        result <- 
          df %>% 
          mutate(new.split.var =
            #df[,names(df)==split.varname] = strsplit(df[,names(df)==split.varname],",")
              sapply(
                split.var, function(x){strsplit(x,split.char)}
              )
          ) %>% 
          unnest(new.split.var, .drop = FALSE) %>% 
          .[,names(.)[names(.) != split.varname]] %>%
          OrderDfByVar(
            df = ., 
            order.by.varname = id.varname,
            rev = FALSE
          ) %>%
          ReplaceNames(
            df = .,
            current.names = "new.split.var",
            new.names = split.varname
          )
      }  
      
      return(result)
    }

  #Display Unique Vector Values (with trimming to 25 characters, useful for ColClassConvert function)
    DisplayUniqueColVals <- function(x){
      if(paste(unique(x), collapse = ", ") %>% nchar <= 25){
        result <- paste(unique(x)[order(unique(x))], collapse = ", ")   
      }
      if(paste(unique(x), collapse = ", ") %>% nchar > 25){
        result <- paste(unique(x)[order(unique(x))], collapse = ", ") %>% substr(., 1, 30) %>% paste(.,"...",sep="")   
      }
      return(result)
    }
    

  #Convert data frame column classess according to user input
    #Test Inputs
      #tb.to.be.reclassed.x = resp.tb
      #tb.config.y = var.norm.tb
      #tb.config.linking.varname.y = "var.id"
      #tb.config.class.varname = "var.class"
      
    ColClassConvertFromRefTable <- 
      function(
        tb.to.be.reclassed.x,
        tb.config.y,
        tb.config.linking.varname.y,
        tb.config.class.varname
      ){
        
        #Loop to define column classes
          #a <- 1 #LOOP TESTER
          for(a in 1:length(names(tb.to.be.reclassed.x))){
            
            #result <- tb.to.be.reclassed.x
            
            tb.linking.v <- 
              tb.config.y %>% 
              dplyr::select(tb.config.linking.varname.y) %>%
              unlist %>% as.vector
            
            if(!names(tb.to.be.reclassed.x[,a]) %in% tb.linking.v){
              print(
                paste(
                  "Column name does not exist in reference table. Loop #: ", a, 
                  " -- Var name: ", names(tb.to.be.reclassed.x[,a]),
                  sep = ""))
              next()
            }
            
            class.x <- tb.to.be.reclassed.x[,i] %>% unlist %>% class
            
            class.y <- 
              tb.config.y %>% 
              filter(tb.linking.v == names(tb.to.be.reclassed.x[,i])) %>% 
              dplyr::select(tb.config.class.varname) %>% 
              unlist %>% as.vector
            
            if(!class.y %in% c("factor","character","logical","integer","numeric","date","")){
              print("Warning: input does not match valid column class. Enter one of the following: factor, character, logical, integer, numeric.") 
              corrected.class.a <- readline(prompt = paste("Enter corrected column class for '",display.df$colname[a],"':",sep=""))
            }
            
            if(class.x == class.y){ #No conversion necessary, skip to next loop iteration
              paste("Class already correct. Loop #: ",a, sep = "")
              next()
            } 
            
            #if(corrected.class.a == ""){
            #  display.df$corrected.class[a] <- display.df$class[a] %>% as.character
            #}else{
            #  display.df$corrected.class[a] <- corrected.class.a
            #}
            #print(display.df)
            
            tb.to.be.reclassed.x[,a] <- 
              SetColClass(
                tb = tb.to.be.reclassed.x[,a],
                colname = names(tb.to.be.reclassed.x[,a]),
                to.class = class.y
              )
            
          } #END OF LOOP BY COLUMN
        
        return(tb.to.be.reclassed.x)
      }
    

 	#Create Threshold Dummy Variable
    #Test Inputs
      #threshold.cutoff.input = configs.j$threshold.cutoff
      #input.tbname = "summarized.result.tb"
      #input.tb.key.varname = summarize.key.varname
      #input.tb.dummy.colname = new.measure.varname
      #at.or.above.threshold.equals.1 = configs.j$at.or.above.threshold.equals.1
  		
  		#threshold.cutoff.input = configs.p$threshold.cutoff
      #input.tbname = "input.tb"
      #input.tb.key.varname = base.key.varnames #TODO: will not generalize if more than one keys in base table
      #input.tb.dummy.colname = configs.p$varname
      #at.or.above.threshold.equals.1 = configs.p$at.or.above.threshold.equals.1
				           
  
    CreateThresholdDummy <- 
      function(
        input.tbname,
        input.tb.key.varname,
        input.tb.dummy.colname,
        threshold.cutoff.input,
        at.or.above.threshold.equals.1
     ){
        input.tb <- as.object(input.tbname)  
        
        if(!input.tb.dummy.colname %in% names(input.tb)){
          input.tb.dummy.colname2 <- names(input.tb)[2]
        }else{
          input.tb.dummy.colname2 <- input.tb.dummy.colname
        }
        
        #Define Threshold Value
          if(input.tb.dummy.colname == input.tb.dummy.colname2){
            if( #TODO: right now function assumes that they will both be of some class that when converted to character will turn out to be the same
              as.character(threshold.cutoff.input) %in% 
              (input.tb %>% dplyr::select(as.character(input.tb.dummy.colname)) %>% unique %>% 
              unlist %>% as.vector %>% as.character)
            ){
              threshold <- threshold.cutoff.input
            }else{
              threshold.fun <- paste(threshold.cutoff.input,"(",input.tbname,"$",input.tb.dummy.colname,")",sep = "")
              threshold <- as.object(threshold.fun)
            }
          }
        	
        	#threshold[!is.na(as.numeric(threshold))] <- suppressWarnings(as.numeric(threshold))# %>% suppressWarnings(.) #just in case
    
          if(input.tb.dummy.colname != input.tb.dummy.colname2){ #for dummies
            threshold <- 1
          }
    
        #Calculate Threshold Dummy  
          if(class(threshold) == "character"){
          	input.tb$threshold.dummy <- #add threshold dummy to input.tb
            input.tb %>% 
            dplyr::select(input.tb.dummy.colname2) %>% unlist %>% as.vector() %>%
            equals(threshold) %>% ifelse(.,1,0)
          }
        
          if(class(threshold) == "numeric"){
          	input.tb$threshold.dummy <- #add threshold dummy to input.tb
            input.tb %>% 
            dplyr::select(input.tb.dummy.colname2) %>% unlist %>% as.vector() %>%
            is_weakly_greater_than(threshold) %>% ifelse(.,1,0)
          }
        	
          if(!at.or.above.threshold.equals.1){ #recode (reverse values) if at.or.above.threshold.equals.1 config is set to 'FALSE'
            input.tb$threshold.dummy %>% dplyr::recode(., `1` = 0, `0` = 1)
          }
        
        #Format & Store Result
          if(class(threshold) == "character"){
          	threshold.dummy.varname <- 
	            paste(
	              input.tb.dummy.colname,
	              ifelse(at.or.above.threshold.equals.1, "is","not"),
	              threshold.cutoff.input,
	              sep = "."
	            )
          }
        
	        if(class(threshold) == "numeric"){
	          	threshold.dummy.varname <- 
		            paste(
		              input.tb.dummy.colname,
		              ifelse(at.or.above.threshold.equals.1, "above","below"),
		              threshold.cutoff.input,
		              sep = "."
		            )
          }
        
          
          result <- 
            input.tb %>%
            dplyr::select(input.tb.key.varname, threshold.dummy) %>%
            ReplaceNames(., "threshold.dummy",threshold.dummy.varname)
          
        return(result)
      }
     
	#SummarizeManyToOneVar - for regressions, aggregate variable in an table ('summarize.tb') with many-to-one 
	  #relationship with a 'base table' ('base.tb') into unit of the base table using one from a list of 
	  #aggregation functions.
	  
	  #Test Inputs
    	#base.key.varname = base.key.varnames
      #summarize.tb = summarize.tb
      #summarize.key.varname = summarize.key.varname
      #summarize.varname = summarize.varname
      #summarize.function = summarize.function
      #new.measure.varname = new.measure.varname
    	#is.threshold.dummy = configs.j$is.threshold.dummy
    	#threshold.cutoff = configs.j$threshold.cutoff
    	#at.or.above.threshold.equals.1 = configs.j$at.or.above.threshold.equals.1
    	#join.with.base.keyvar = TRUE
    	#sub.na.val = configs.j$sub.na.val
    
    	#base.tbname = "resp.tb"
			#base.key.varname = "resp.id"
			#summarize.tb = overlap.resp.alter.org.tb
			#summarize.key.varname = "resp.id"
			#summarize.varname = "alter.id"
			#summarize.function = "count.unique"
			#new.measure.varname = "overlap.any"
			#is.threshold.dummy = TRUE
			#threshold.cutoff = 1
			#at.or.above.threshold.equals.1 = TRUE
			#join.with.base.keyvar = TRUE
			#sub.na.val = 0
	
	  SummarizeManyToOneVar <-
	    function(
	      base.tbname,
	      base.key.varname,
	      summarize.tb,
	      summarize.key.varname,
	      summarize.varname,
	      summarize.function,
	    	new.measure.varname,
	    	is.threshold.dummy = c(TRUE,FALSE),
	      threshold.cutoff,
	      at.or.above.threshold.equals.1 = c(TRUE, FALSE),
	    	join.with.base.keyvar = c(TRUE,FALSE),
	    	sub.na.val
	    ){
	      #Check function is in allowed functions
	        
	        simple.functions <- c("count","count.unique","mean","sum","max","min","median", "threshold.dummy")
	        need.numeric.input.functions <- c("mean","sum","max","min","median")
	        complex.functions <- c("unique.vals.dummies")
	        allowed.functions <- c(simple.functions, complex.functions)
	        
	        if(!summarize.function %in% allowed.functions){
	          stop(
	            paste0(
	              "Summary function must be one of the following: '",
	              paste0(allowed.functions, collapse = ",' '"),
	              "'"
	            )
	          )
	        }
	      
	      #Check summarize.fun is one of allowed values
	        #TODO
	        
	      #Check varname exists in table
	        if(!summarize.varname %in% names(summarize.tb)){
	          stop(paste0("Variable '",summarize.varname,"' does not exist in source.table."))
	        }
	        
	      #Check variable is numeric if doing any sort of calculated function besides count or count.unique
	        if(summarize.function %in% need.numeric.input.functions){
	          var.class <- 
	            summarize.tb %>% dplyr::select(summarize.varname) %>% 
	            as.data.frame %>% unlist %>% class
	          
	          if(var.class != "numeric"){
	            stop(
	              paste0(
	                "Summary function '",
	                summarize.function,
	                "' requires numeric input, but variable '",
	                summarize.varname,
	                "' is of class ",
	                var.class
	              )
	            )
	          }
	        }
	        
	      #Summarizing when you want a count, count.unique, mean, sum, min, max, or median by groups
	        if(summarize.function %in% simple.functions){
	          
	          #Create character string for dplyr:summarize 'measure' variable
	            if(summarize.function == "count"){
	              dplyr.summarize.fun <- paste("length(",summarize.varname,")",sep = "")
	            }
	            if(summarize.function == "count.unique"){
	              dplyr.summarize.fun <- paste("length(unique(",summarize.varname,"))",sep = "")
	            }
	            if(summarize.function %in% c("mean","sum","max","min","median")){
	              dplyr.summarize.fun <- paste(summarize.function,"(",summarize.varname,", na.rm = TRUE)",sep = "")
	            }
	          
	          summarized.output <-   #create regression data table 
	            summarize.tb %>% 
	            dplyr::select(summarize.key.varname, summarize.varname) %>% #only relevant columns
	            group_by(eval(parse(text = summarize.key.varname))) %>% #group by id variable (resp.id)
	            summarize( #summarize using function provided as input
	              .data = ., 
	              measure = eval(parse(text = dplyr.summarize.fun))
	            ) %>%
	            ReplaceNames(
	              ., 
	              current.names = names(.),
	              new.names = c(summarize.key.varname, paste(summarize.function, new.measure.varname, sep = "."))
	            ) 
	        }
	        
	      #Summarizing when you want to get a bunch of dummies by unique values in a column  
	        if(summarize.function == "unique.vals.dummies"){
	          
	          if(is.na(new.measure.varname)){
	            base.new.varnames <- summarize.varname
	          }else{
	            base.new.varnames <- new.measure.varname
	          }
	          
	          dcast.formula <- paste(summarize.key.varname, " ~ ", summarize.varname)
	          
	          summarized.output <-
	            dcast(
	                data = summarize.tb %>% dplyr::select(summarize.key.varname, summarize.varname),
	                formula = dcast.formula,
	                #id.var = summarize.key.varname,
	                value.var = summarize.varname,
	                fun.aggregate = function(x){length(unique(x))}
	            ) %>%
	            ReplaceNames(
	              ., 
	              current.names = names(.)[names(.) != summarize.key.varname],
	              new.names = 
	                paste(base.new.varnames, "_", names(.)[names(.) != summarize.key.varname], sep = "")
	            ) %>% as_tibble()
	        }
	      
	      #Summarizing when you want to create a dummy based on whether values are above/below a threshold  
	       	#if(!hasArg(is.threshold.dummy)){is.threshold.dummy <- FALSE}
	        if(is.threshold.dummy){
	        	
	          #if(!hasArg(at.or.above.threshold.equals.1)){at.or.above.threshold.equals.1 <- TRUE} #set default to TRUE
	        	
	        	#If have a threshold dummy coming from a categorical variable in non-base table,
	              #have to use unique.vals.dummy then (with code below) pair down only to relevant column
	          	
	        		if(summarize.function == "unique.vals.dummies"){    
	              colname.with.cutoff.val <- 
	              	strsplit(names(summarized.output), "_") %>% 
	              	lapply(., `[`, -1) %>%
	              	lapply(., function(x){threshold.cutoff %in% x}) %>% 
	              	unlist %>% as.vector %>%
	              	names(summarized.output)[.]

	              summarized.output %<>% 
	              	dplyr::select(c(summarize.key.varname, as.character(colname.with.cutoff.val)))
	        		}else{
	        			summarized.output[[names(summarized.output)[2]]] <- 
      						summarized.output[,2] %>%
      						is_weakly_greater_than(threshold.cutoff) %>%
      						as.numeric
	        		}

	       	}
        
        #Join summarized output with base.tb if specified
	        #if(!hasArg("join.with.base.keyvar")){join.with.base.keyvar <- FALSE} #set default to FALSE
	        if(join.with.base.keyvar){
	        	
	        	if(!hasArg(sub.na.val)){sub.na.val <- 0}
	        	
	        	x <- 
	        		as.object(base.tbname) %>% 
	        		dplyr::select(base.key.varname)
	        	
	        	result <-
	        		left.join.NA(
	        			x = x,
	        			y = summarized.output,
	        			by = base.key.varname,
	        			na.replacement = sub.na.val
	        		)
	        	
	        }else{
	        	
	        	result <- summarized.output
	        	
	        }
	        
	      return(result)
	    }
	  
