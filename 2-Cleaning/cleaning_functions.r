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
    #Test Inputs
      #vector = resp9.tb$pd_coaching.instruction
      #lookup.tb = config.ans.opt.tb
      #match.colname = "ans.text.agreement"
      #replacement.vals.colname = "ans.text.agreement.num"
      #mult.replacements.per.cell = TRUE
      #mult.replacements.separator.char = ","
      #print.matches = TRUE
    
    IndexMatchToVectorFromTibble <- 
      function(
        vector, 
        lookup.tb, 
        match.colname, 
        replacement.vals.colname,
        mult.replacements.per.cell = c(FALSE,TRUE),
        mult.replacements.separator.char = NULL,
        print.matches = c(TRUE,FALSE)
      ){
       
        if(mult.replacements.per.cell){
          lookup.tb <- 
            SplitColReshape.ToLong(
              df = lookup.tb,
              id.varname = replacement.vals.colname,
              split.varname = match.colname, 
              split.char = ","
            ) #strsplit(match.col, mult.replacements.separator.char) %>% unlist %>% as.vector
        }
        
        match.col <- lookup.tb %>% select(match.colname) %>% unlist %>% as.vector
        replacement.col <- lookup.tb %>% select(replacement.vals.colname) %>% unlist %>% as.vector
        matched.vals.ls <- list()
        unmatched.vals.ls <- list()
        
        for(i in 1:length(vector)){
          if(is.na(vector[i])){next()} #Skips NAs
          if(!any(match.col == vector[i])){
            unmatched.vals.ls[[i]] <- vector[i]
            warning(
              paste("No match for '", vector[i], "' found in column '", match.colname, "'.", sep = "")
            )
          }else{
            matched.vals.ls <- vector[i]
            vector[i] <- replacement.col %>% unlist %>% .[match.col == vector[i]]
          }
        }
        
        if(!missing(print.matches) && print.matches){
          matched.vals.ls %>% unlist %>% as.vector %>% RemoveNA %>% paste(., collapse = ", ") %>%
            paste0("Values replaced: ",.) %>% print
          unmatched.vals.ls %>% unlist %>% as.vector %>% RemoveNA %>% paste(., collapse = ", ") %>%
            paste0("Values not replaced: ",.) %>% print
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
        tb, #data frame
        id.unit, #district or building for now, taken directly from configs
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
                unit.id = tb %>% 
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
    
  #TODO: Function to rearrange columns according to names that meet a TRUE/FALSE condition
    
  #Unbranching Variables based on a "Var Guide" lookup table
    #Structure of Var Guide Table:
      #var.guide.example <-
      #  tibble(
      #    current.names.colname = c("v1.1","v1.2","v2.a","v2.b","v2.c"),
      #    unbranched.names.colname = c("v1","v1","v2","v2","v2")
      #  )
    
    #Test Inputs
      #data.tb <- resp8.tb
      #data.id.varname <- "responseid"
      #var.guide.tb <- questions.tb
      #current.names.colname <- "var.id"
      #unbranched.names.colname <- "branch.master.var.id"
      
    Unbranch <- function(
      data.tb, 
      data.id.varname, 
      var.guide.tb, 
      current.names.colname, 
      unbranched.names.colname){
      
      #Unbranching Data
        branch0.colnames <- #names of the current columns that don't need unbranching
          var.guide.tb %>% 
          filter(UQ(as.name(unbranched.names.colname)) %>% is.na) %>%
          select(UQ(as.name(current.names.colname))) %>%
          unlist %>% as.vector %>% 
          c("unit.id",.)
        
        branch1.colnames <- #names of current columns that need unbranching
          var.guide.tb %>% 
          filter(UQ(as.name(unbranched.names.colname)) %>% is.na %>% not)  %>%
          select(UQ(as.name(current.names.colname))) %>%
          unlist %>% as.vector
        
        unbranched.colnames <- #names of final columns after unbranching
          var.guide.tb %>% 
          filter(UQ(as.name(unbranched.names.colname)) %>% is.na %>% not)  %>%
          select(UQ(as.name(unbranched.names.colname))) %>%
          unlist %>% unique
        
        branch0.tb <- data.tb[, names(data.tb) %in% c(data.id.varname,branch0.colnames)] #Columns that don't need unbranching

        unbranched.data.ls <- list(branch0.tb)
        unbranched.var.guide.ls <- 
          list(var.guide.tb %>% filter(UQ(as.name(unbranched.names.colname)) %>% is.na))
        
        for(i in 1:length(unbranched.colnames)){
          unbranched.colname.i <- unbranched.colnames[i]
          branched.colnames.i <- #current columns that will get unbranched into this final column
            var.guide.tb %>% 
            filter(UQ(as.name(unbranched.names.colname)) == unbranched.colname.i) %>%
            select(UQ(as.name(current.names.colname))) %>%
            unlist %>% as.vector
          
          branched.tb.i <- data.tb[, names(data.tb) %in% c(data.id.varname,branched.colnames.i)]
          
          #Check if any columns have values/responses in more than one column
            #mult.values <- 
            #  apply(branched.tb.i, 1, function(x){x %>% equals("") %>% not %>% sum %>% is_greater_than(1)}) %>% any
            
            #if(mult.values){
            #  mult.entries.tb <-
            #    branched.tb.i %>%
            #    filter(
            #      apply(
            #        branched.tb.i, 1, 
            #        function(x){x %>% equals("") %>% not %>% sum %>% is_greater_than(1)}
            ##      )
            #    )
            #  stop("Rows have data in "
            #}
          
          #Data
            unbranched.data.ls[[i + 1]] <- 
              melt(branched.tb.i, id = data.id.varname) %>% 
              filter(value != "") %>% 
              select(c(1,3)) %>%
              set_names(data.id.varname, unbranched.colname.i)
          
          #Var Guide
            branched.var.guide.i <-
              var.guide.tb %>% 
              filter(UQ(as.name(unbranched.names.colname)) == unbranched.colname.i)
            
            #TODO: could be converted to SplitColReshape function (exact opposite operation)
            unbranched.var.guide.ls[[i + 1]] <-
              branched.var.guide.i[1, names(branched.var.guide.i) != current.names.colname] %>%
              mutate(
                original.branched.varnames = 
                  branched.var.guide.i %>% 
                  select(UQ(as.name(current.names.colname))) %>% 
                  unlist %>% 
                  paste(., collapse = ", ")
              )
            names(unbranched.var.guide.ls[[i +1]])[names(unbranched.var.guide.ls[[i + 1]]) == unbranched.names.colname] <-
              current.names.colname
        }
        
        unbranched.data.tb <- 
          suppressMessages(
            Reduce(function(x, y) full_join(x, y, all = TRUE), unbranched.data.ls)
          )
        unbranched.var.guide.tb <- 
          suppressMessages(
            Reduce(function(x, y) full_join(x, y, all = TRUE), unbranched.var.guide.ls)
          )
        
      #Return Results
        return(
          list(
            unbranched.data.tb = unbranched.data.tb,
            unbranched.var.guide.tb = unbranched.var.guide.tb
          )
        )
    }
  
    #Loop Expander for creating full config tables
      #Test Inputs
        #configs = config.slidetypes.tb
        #loop.varnames = c("slide.loop.var.1","slide.loop.var.2","slide.loop.var.3")
        #collate.varname = "slide.section.1"
        #source.data = resp.long.df.b  
      
      loop.expander.fun <- function(
        configs, 
        loop.varnames, 
        collate.varname, 
        source.data){
        output.ls <- list()
        
        #c = 4 #LOOP TESTER: NO LOOPS
        #c = 3 #LOOP TESTER: ONE LOOP VAR
        #c = 4 #LOOP TESTER: TWO LOOP VARS
        #for(c in 2:3){
        for(c in 1:dim(configs)[1]){
          
          c.list.c <- list(c=c)
          configs.c <- configs[c,]
          
          #Make data frame with configurations repeated out across all unique combinations of loop.varname(s) in source.data      
          
          loop.varnames.c <- configs[c,names(configs) %in% loop.varnames] %>% 
            as.matrix %>% 
            as.vector %>% 
            RemoveNA()
          
          if(length(loop.varnames.c) == 0){
            output.ls[[c]] <- configs[c,]
            next()
          }
          
          output.ls[[c]] <- 
            UniqueCombnFromColnames(resp.long.df.b,loop.varnames.c) %>%
            cbind(configs.c,.)
          
        } ### END OF LOOP "C" BY ROW OF CONFIG INPUT ###
        
        output.df <- rbind.fill(output.ls)
        
        #Collate Report Sub-Sections
        
        manual.order.1 <- 
          output.df$slide.order.1 %>% 
          unique %>% 
          RemoveNA() %>% 
          strsplit(.,",") %>% 
          unlist %>%
          as.data.frame(.) %>%
          ReplaceNames(
            ., 
            current.names = names(.), 
            new.names = "module" #TODO: Not abstracted
          )
        
        
        #full_join(manual.order.1, output.df)
        output.df <- full_join(manual.order.1, output.df)
        output.df <- 
          output.df[
            order(output.df$slide.section.1),
            c(2:length(names(output.df)),1)
            ]
        
        #x<-output.df[
        #  order(
        #    output.df$slide.section.1,        # output.df$module),] %>% full_join(manual.order.1, ., by = "module")#.[match(manual.order.1,output.df$module),]
        #    output.df$module, 
        #    output.df$slide.section.2,
        #    output.df$slide.section.3
        #  )
        #  ,]
        
        
        #if(!missing(collate.varname)){
        
        #Form inputs for collating loop: list with sections (collated and non-colated, in order) 
        #collate.section.configs.df <- VectorValueChangePositions(output.df[,collate.varname])
        #collate.ls <- list()
        #for(e in 1:nrow(collate.section.configs.df)){
        #  collate.ls[[e]] <- output.df[collate.section.configs.df$start.position[e]:collate.section.configs.df$end.position[e],]
        #}
        
        ###                                          ###
        # Start of loop 'd' by collated report section #
        ###                                          ###
        
        #The following loop takes as input the list of report slides which have just been broken up into an ordered list of
        #collated and non-collated sections. For sections requiring collation, it will replace the list element with the
        #collated version of the slide configurations.
        
        #d = 2 #LOOP TESTER
        #for(d in 1:length(collate.ls)){  
        #for each unique report section:
        #if it doesn't require collation, do nothing
        #select lines of output.df with only that unique slide.loop.collate.section
        #order by looping variable that has same name as slide.loop.var (e.g. 'school') AND by slide.type.position
        #store in list (to be re-attached) to non-collated sections and other collated sections
        
        #Section to collate for this iteration
        #    collate.input.df.d <- collate.ls[[d]]
        
        #Skip iteration of no collation/re-ordering necessary
        #    if(unique(is.na(collate.input.df.d$slide.loop.collate.section))){
        #      next()
        #    }
        
        #Name of loop variable (will be used to order data-frame in b-loop)
        #    loop.var.d <- collate.input.df.d %>%
        #      select(slide.loop.var) %>%
        #      unlist %>%
        #      unique 
        
        #Create collated data frame to replace un-collated one in slide list
        #    collate.ls[[d]] <- 
        #      collate.input.df.d[
        #        order(
        #          collate.input.df.d %>% select(matches(loop.var.d)), 
        #          collate.input.df.d$slide.type.position
        #        )
        #        ,]
        
        #} #END OF LOOP "d" BY COLLATED SECTION
        
        #output.df <- do.call(rbind, collate.ls)
        
        #} #END OF 'IF' STATEMENT FOR WHEN SOME REPORT SECTIONS REQUIRE COLLATING
        
        return(output.df)
        
      } #END OF LOOP EXPANDER FUNCTION
      
    
  #Data Type Conversions
    
    #Doing IndexMatch type function to take values and concatenate them with an extra set of labels/strings
      #CWIS example: take CWIS vars and convert "Always" to "1. Always" and "Never" to "5. Never" according
      #to a lookup table (in this case config.ans.opt.tb)
      
      #Test Inputs
        #tb =  RecodeIndexMatch(
        #  tb = recode.addnums.tb,
        ##  lookup.tb = lookup.tb,
        #  match.colname = "ans.text.freq.num",
        #  replacement.vals.colname = "ans.num"
        #)
        #lookup.tb = config.ans.opt.tb
        #match.colname = "ans.text.agreement.num"
        #replacement.vals.colname = "ans.num"
        #na.replacement = NULL
      
      RecodeIndexMatch <- function(tb, lookup.tb, match.colname, replacement.vals.colname, na.replacement = NULL){
        for(i in 1:ncol(tb)){
          
          replacement.vals <- lookup.tb %>% select(UQ(as.name(match.colname))) %>% unlist %>% as.vector %>% unique
          if((tb[[i]] %>% unique) %in% replacement.vals %>% any %>% not){next()}
          
          tb[,i] <- 
            IndexMatchToVectorFromTibble(
              vector = tb[[i]],
              lookup.tb = lookup.tb,
              match.colname = match.colname,
              replacement.vals.colname = replacement.vals.colname,
              mult.replacements.per.cell = TRUE,
              mult.replacements.separator.char = ",",
              print.matches = FALSE
            ) 
          
          if(!is.null(na.replacement)){
            tb[[i]] <- SubNA(tb[[i]], na.replacement = na.replacement)
          }
          
          tb <- 
            SetColClass(
              tb = tb, 
              colname = names(tb)[i], 
              to.class = class(lookup.tb[[which(names(lookup.tb)==replacement.vals.colname)]])
            )
        }
        return(tb)
      }
    
   
      
    #TODO: Apply function to set of named variables in a data frame
      #tb = resp9.tb
      #varnames = cwis.varnames.unbranched
      #function.to.apply = "numeric.recode.fun(x)"
    
    #ApplyToVarnamesReturnTable <- function(tb, varnames, function.to.apply){
      #  for(i in 1:length(varnames)){
      #    original.class.i <- tb[[i]] %>% class
      #    tb[, names(tb) == varnames[i]] <-
      #      apply(
      #        tb[, names(tb) == varnames[i]], 
      #        2, 
      #        FUN = eval(parse(text = 
      #             paste('f <- function(', args, ') { return(' , body , ')}', sep='')function.to.apply)))
      #  }
      #}
    
