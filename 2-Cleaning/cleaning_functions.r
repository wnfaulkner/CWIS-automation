#########################################################
##### 	2-CLEANING FUNCTIONS                        #####
#########################################################

  #Add slide.type columns via inner-join
    AddSlideTypeCols <- function(config.table){
     return( 
        inner_join(config.slide.types.tb, config.table, by = "slide.type.id", all.x = FALSE)
     )
    }
  
  #Lower-case names of a tibble
    LowerCaseNames <- function(x){
      names(x) <- tolower(names(x))
      return(x)
    }
  
  #Lower-case all character values in a tibble
    LowerCaseCharVars <- function(tb){
      for(i in 1:dim(tb)[2]){
        if(class(tb[[i]]) == "character"){
          tb[[i]] <- tolower(tb[[i]])
        } 
      }
      return(tb)
    }
  
  #Detect Special Character and return TRUE/FALSE
    IsSpecialChar <- function(char){
      if(class(char) != "character"){stop("Input must be of class 'character'.")}
      if(nchar(char) != 1){stop("Input must be a character of length 1.")}
      return(grepl("\\\\|\\^|\\$|\\.|\\?|\\*|\\||\\+|\\(|\\)|\\[|\\{",char))
    }
    
  #Substitute any number of repeated occurences of a character with a single one of that character
    SubRepeatedCharWithSingleChar <- function(string.vector, char){
      
      if(class(char) != "character"){stop("'char' input must be of class 'character'.")}
      if(nchar(char) != 1){
        stop(paste("'char' input must be a character of length 1. Current length: ", length(char), sep = ""))
      }
      
      if(IsSpecialChar(char)){
        char <- paste("\\", char, sep = "")
      }
      
      while(any(grepl(paste(char, char, sep = ""), string.vector))){
        string.vector <- 
          gsub(
            paste(char, char, sep = ""),
            char,
            string.vector
          )
      }
      return(string.vector)
    }
    
  #QUALTRICS: Remove extra header rows
    RemoveExtraHeaderRowsBasedOnStartChar <- function(tb, header.rownum, search.colname, search.char){
      search.colnum <- which(names(tb) %in% search.colname)
      if(tb[[search.colnum]] %>% as.character %>% substr(., 1,1) %>% equals(., search.char) %>% any){
        remove.endrow <- which(substr(resp1.tb[,1],1,1) == symbol)
        remove.rownums <- c(1:remove.endrow)[-header.rownum]
        print(
          paste("Rows removed: ", remove.rownums, sep = "")
        )
        result <- tb %>% subset(subset = !(1:dim(tb)[1] %in% remove.rownums))
      }else{
        print(
          paste(
            "Specified character '", 
            search.char,
            "' not found in first character of column '",
            search.colname,
            "'. No rows removed.",
            sep = ""
          )
        )
        result <- tb
      }
      return(result)
    }

  #Index-match equivalent for replacing values of a vector with values from a tibble given column names
    IndexMatchToVectorFromTibble <- function(vector, lookup.tb, match.colname, replacement.vals.colname){
      match.col <- lookup.tb %>% select(match.colname) %>% unlist
      replacement.col <- lookup.tb %>% select(replacement.vals.colname) %>% unlist
      
      for(i in 1:length(vector)){
        if(!any(match.col == vector[i])){
          warning(
            paste("No match for '", vector[i], "' found in column '", match.colname, "'.", sep = "")
          )
        }else{
          vector[i] <- replacement.col %>% unlist %>% .[match.col == vector[i]]
        }
      }
      return(vector)
    }
    
  #Create ID Column by concatenating two columns
    #TEST INPUTS
      #tb = resp1.tb
      #id.unit = report.unit
      #additional.colnames = c("district")
      #remove.blanks = "ANY.MISSING"
      #paste.char = "_"
      
    CreateUnitIDCol <- 
      function(
        tb, 
        id.unit, 
        additional.colnames = NULL, 
        remove.blanks = c("NONE", "ALL.MISSING",  "ANY.MISSING"),
        paste.char
      ){
        
        #Check that additional.colnames of length one or greater
          if(missing(additional.colnames)){
            print("No additional.colnames specified. Returning tb with additional column 'id' equal to column specified in id.unit.")
            return(tb %>% mutate(id = tb %>% select(id.unit) %>% unlist))
          }
        
        #Check additional.colnames in tb names
          if(!all(additional.colnames %in% names(tb))){ 
            stop(
              paste0(
                "additional.colnames '",
                paste(additional.colnames[!additional.colnames %in% names(tb)], collapse = "', '"),
                "' do(es) not appear in tb names."
              )
            )
          }
          
        #Check that the id.unit in tb names
          if(!(id.unit %in% names(tb))){ 
            stop(
              paste0(
                "id.unit '",
                id.unit,
                "' does not appear in tb names."
              )
            )
          }
        
        #Remove blanks according to user specification
          remove.blanks <- match.arg(remove.blanks)
          colnames <- c(id.unit, additional.colnames)
          if(remove.blanks == "NONE"){}
          if(remove.blanks == "ALL.MISSING"){
            tb <- tb %>% filter(tb %>% select(colnames) %>% apply(., 1, function(x){!all(x == "")}))
          }
          if(remove.blanks == "ANY.MISSING"){
            tb <- tb %>% filter(tb %>% select(colnames) %>% apply(., 1, function(x){!any(x == "")}))
          }
          
        #TODO: Check that the report unit is the most disaggregated unit of the values in the columns to be concatenated
          #disaggregate.check.data.tb <- tb %>% select(additional.colnames) %>% filter(!duplicated(.)) %>% melt
          #disaggregate.check.formula <- 
          #  paste0(
          #    id.unit, "+", 
          #    ifelse(
          #      additional.colnames[additional.colnames != id.unit] %>% length %>% ispaste0(additional.colnames[additional.colnames != id.unit])
          #  )
          #dcast(
          #  data = disaggregate.check.tb, 
          #  formula =  
          #    "building + district ~ .", #additional.colnames[additional.colnames == id.unit]
          #  function(x) length(x)
          #)
        
        #Substitute slash for " " in both id.unit column and additional.colnames so file storage for report works properly
          for(i in 1:length(colnames)){
            tb[names(tb) == colnames[i]] <- gsub("\\/", " ", unlist(tb[names(tb) == colnames[i]]))
          }
          
        #Create ID column itself
          tb <- 
            tb %>% 
              mutate(
                id = tb %>% 
                  select(additional.colnames) %>% 
                  apply(., 1, function(x){paste(x, collapse = paste.char)}) %>%
                  cbind(., tb %>% select(id.unit)) %>%
                  apply(., 1, function(x){paste(x, collapse = paste.char)})
              )
          return(tb)
      }
   
  #Restrict data to a sample given a unit and an aggregate unit (e.g. 'building' and 'district'
    #Notes for CWIS: code selects whole districts at random so that it will generate all reports for those 
    #districts. This is important so that you don't get a bunch of scattered districts and the 
    #district averages aren't realistic. The code samples district combinations until it finds one
    #where the number of report ids is equal to the user-defined sample size.
    
    #TEST INPUTS
      #tb = resp1.tb
      #report.unit = report.unit
      #sample.group.unit = sample.group.unit
      #sample.size = sample.size
      
    RestrictDataToSample <- function(tb, report.unit, sample.group.unit, sample.size){
      
      sample.size <- as.numeric(sample.size)
      
      if(nrow(tb) < sample.size){
        stop(
          paste0("Dataset has fewer rows (", nrow(tb), ") than requested for sample (", sample.size, ").")
        )
      }
      
      num.reports <- 0
      
      report.unit.counts.tb <-
        tb %>% 
        group_by(!!sym(sample.group.unit)) %>%
        dplyr::summarize(count = n_distinct(!!sym(report.unit))) %>%
        set_names(c("group","count"))
      
      sample.groups.tb <- tibble()
      
      #While loop to define random sample of group unit that will give correct number of report units equal to sample size
        while(num.reports != sample.size){
         
          sample.groups.tb <- 
            rbind(
              sample.groups.tb,
              sample_n(report.unit.counts.tb, size = 1, replace = FALSE)
            )
          
          num.reports <- sum(sample.groups.tb$count)
          report.unit.counts.tb <- report.unit.counts.tb %>% filter(!(group %in% sample.groups.tb$count))
          
          #print(sample.groups.tb)
          #print(num.reports)
          
          if(num.reports > sample.size){
            num.reports <- 0
            sample.groups.tb  <- tibble()
            report.unit.counts.tb <-
              tb %>% 
              group_by(!!sym(sample.group.unit)) %>%
              dplyr::summarize(count = n_distinct(!!sym(report.unit))) %>%
              set_names(c("group","count"))
          }
        }
      
      #Restricting data to group units as defined in while loop
        result <-
          tb %>% filter(!!sym(sample.group.unit) %in% sample.groups.tb$group)
      
      return(result)
    }
    
    