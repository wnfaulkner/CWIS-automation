#RESHAPE DATA INTO LONG FORMAT BASED ON SPLITTING COLUMN ON A CHARACTER
  
  SplitColReshape <- function(x){
    if(!is.data.frame(x)){stop("Input not a data frame.")}
    split.colname <- "slide.graph.type" #! SHOULD LEARN HOW TO INCLUDE THESE AS ARGUMENTS OF THE FUNCTION!
    split.char <- "," #!
    
    if(!exists("split.char")){
      split.col <- readline(prompt = "Enter the variable name that will be split and used to reshape data: ")
    }
    
    if(!exists("split.char")){
      split.char <- readline(prompt = "Enter the character(s) you would like to split your variable on: ")
    }
    
    x[,names(x)==split.col] %>% strsplit(., split.char)
    
    
  }
  
  




#CONVERT DATA FRAME COLUMNS ACCORDING TO USER INPUT
  
  #2. Prompt for classes of each column; have option to just say 'as-is' (already in correct format)
  #3. As each column is entered, bind and display with column names
  #4. Once all entered, convert to 

#TESTING OBJECTS
  x2 <- data.frame(
	  factor.name = c("a","xa","222","xa"),
	  character.name = c("char","a","b","cd"),
	  logical.name = c(TRUE, FALSE,TRUE,FALSE),
	  integer.name = c(1,2,3,4) %>% as.integer,
	  numeric.name = c(1.1,2.2,3.3,4.4)
	)
  x2$character.name <- x2$character.name %>% as.character
  
  x1 <- data.frame(numeric.name = c(1.1,2.2,3.3))
  
  x <- x2
  
  ConvertToFactor <- function(x){
    if(!is.null(dim(x))){stop("Input is not an atomic vector.")}
    result <- as.factor(x)
    return(result)
  }
  
  ConvertToCharacter <- function(x){
    if(!is.null(dim(x))){stop("Input is not an atomic vector.")}
    result <- as.character(x)
    return(result)
  }
  
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
      if(as.numeric(x) %>% unique %>% is.na){print("Warning: Converting character to number resulted in all NA.")}
      result <- as.numeric(x)
    }
    return(result)
  }
  
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
  
  DisplayUniqueColVals <- function(x){
    if(paste(unique(x), collapse = ", ") %>% nchar <= 25){
      result <- paste(unique(x)[order(unique(x))], collapse = ", ")   
    }
    if(paste(unique(x), collapse = ", ") %>% nchar > 25){
      result <- paste(unique(x)[order(unique(x))], collapse = ", ") %>% substr(., 1, 30) %>% paste(.,"...",sep="")   
    }
  return(result)
  }
  
  ColClassConvert <- function(x){
    
    if(!is.data.frame(x)){stop("Input not a data frame.")}
    
    #Display names of data.frame with class they are currently
      display.df <- lapply(x, class) %>% #! Makes a data frame storing lists
        as.matrix %>% 
        cbind(
          1:dim(x)[2],
          names(x),
          apply(x, 2, DisplayUniqueColVals) %>% unlist,
          .
        ) %>% 
        as.data.frame() %>%
        lapply(., unlist) %>%
        as.data.frame
      
      
      names(display.df) <- c("colnum","colname","unique.values","class")
      #display.df <- apply(display.df, 2, function(z){as.character(z) %>% trimws(., which = "both")})
      display.df$corrected.class <- ""
      
      print(display.df)
    
    #Loop to collect user input for what classes columns should be converted to
      #a <- 1 #LOOP TESTER
      for(a in 1:dim(display.df)[1]){
        corrected.class.a <- readline(prompt = paste("Enter corrected column class for '",display.df$colname[a],"' (Press enter to leave the same):",sep="")) %>% tolower
        
        if(!corrected.class.a %in% c("factor","character","logical","integer","numeric","date","")){
          print("Warning: input does not match valid column class. Enter one of the following: factor, character, logical, integer, numeric.") 
          corrected.class.a <- readline(prompt = paste("Enter corrected column class for '",display.df$colname[a],"':",sep=""))
        }
        
        if(corrected.class.a == ""){
          display.df$corrected.class[a] <- display.df$class[a] %>% as.character
        }else{
          display.df$corrected.class[a] <- corrected.class.a
        }
        print(display.df)
      }
    
    
      #Conversion loop by column of data frame
      progress.bar.b <- txtProgressBar(min = 0, max = 100, style = 3)
      progress.bar.b.max <- ncol(x)					
    
      for(b in 1:ncol(x)){ #START OF LOOP BY COLUMN
        if(display.df$class[b] == display.df$corrected.class[b]){next()}
        convert.to.b <- display.df$corrected.class[b]
        if(convert.to.b == "factor"){x[,b] <- ConvertToFactor(x[,b])}
        if(convert.to.b == "character"){x[,b] <- ConvertToCharacter(x[,b])}
        if(convert.to.b == "logical"){x[,b] <- ConvertToLogical(x[,b])}
        if(convert.to.b == "integer"){x[,b] <- ConvertToInteger(x[,b])}
        if(convert.to.b == "numeric"){x[,b] <- ConvertToNumeric(x[,b])}
        if(convert.to.b == "date"){x[,b] <- ConvertToDate(x[,b])}
          
        setTxtProgressBar(progress.bar.b, 100*b/progress.bar.b.max)	
        
      } #END OF LOOP BY COLUMN
      
      close(progress.bar.b)
    
    #Display column names with new variable to confirm new classes
      display.df$confirm.converted.colclass <- lapply(x, class) %>% unlist
    
    #print(display.df)
    return(x)
  }
  
  #ColClassConvert(x)
    
   

      