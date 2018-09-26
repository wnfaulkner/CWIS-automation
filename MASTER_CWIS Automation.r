#########################################################
##### 	CWIS Automation for MMD                   	#####
#########################################################


########################################################################################################################################################      
### INITIAL SETUP ###
 
{ #SECTION COLLAPSE BRACKET
  
  rm(list=ls()) #Remove lists
  options(java.parameters = "- Xmx30g") #helps r not to fail when importing large xlsx files with xlsx package
  
  
  #Record code start time for processing time calculations
    start_time <- Sys.time()
  
  #Load libraries
    
    #In case working on new R install that does not have packages installed
      #install.packages('devtools')
      #install.packages("httr")
      #install.packages("readr")
      #install.packages("data.table")
      #install.packages("dplyr")
      #install.packages("googlesheets")
      #install.packages("stringr")
      #install.packages("ReporteRs")
      #install.packages("jsonline")
    
    #library(devtools)
    library(extrafont)
    extrafont::loadfonts(device="win")
    library(magrittr)
    library(googlesheets)
    library(plyr) 
    library(tidyr)
    library(dplyr)
    library(ReporteRs)
    library(ggplot2)
    library(stringr)
    library(reshape2)
    library(xlsx)
    library(jsonlite)
    library(rlang)
  
} #END SECTION COLLAPSE BRACKET   

report.startnum <- 1

########################################################################################################################################################      
### USER INPUTS ###

{ #SECTION COLLAPSE BRACKET
  
  year <- "2018"
  #year <- readline("What year is this data from? (enter number in formay YYYY): ") %>% as.character
  
  semester <- "fall"
  #semester <- readline("What semester is this data from? (enter 'Fall' or 'Spring'): ") %>% tolower
  
} #END SECTION COLLAPSE BRACKET
    
########################################################################################################################################################      
### ESTABLISH DIRECTORIES ###

{ #SECTION COLLAPSE BRACKET
  
# Main data
  #Directories
    
    #M900
      rproj.dir <- "C:/Users/WNF/Documents/Git Projects/CWIS-automation"
      wd <- "C:/Users/WNF/Google Drive/1. FLUX CONTRACTS - CURRENT/2016-09 EXT Missouri Education/3. Missouri Education - GDRIVE/8. CWIS/2018-09 Green Reports Phase 2/"
    
    #Thinkpad T470
      #rproj.dir <- "C:/Users/WNF/Documents/Git Projects/CWIS-automation"  
      #wd <- "G:/My Drive/1. FLUX CONTRACTS - CURRENT/2016-09 EXT Missouri Education/3. Missouri Education - GDRIVE/8. CWIS/2018-09 Green Reports Phase 2/" #%>%

    #Function Directories
      setwd(rproj.dir)
      source("FUN_FirstletterCap.r")
      source("FUN_ColClassConvert.r")
      
    #Data & Output Directories
      setwd(wd)
      source.dir <- paste(wd,"data_source/", sep = "")
      target.dir <- paste("C:/Users/WNF/Desktop/","r_output/",    #! File paths for some reports too long. Can reorganize folder in G-Drive?
                            "Output_",
                            gsub(":",".",Sys.time()), sep = "")
      dir.create(
        target.dir,
        recursive = TRUE
      )
   
} #END SECTION COLLAPSE BRACKET

#OUTPUTS
  #rproj.dir: directory for R project; also contains source data, additional function scripts, and config tables.
  #wd: working directory - Google Drive folder "2018-08 Green Reports"
  #source.dir
    
########################################################################################################################################################      
### LOAD DATA ###

{ #SECTION COLLAPSE BRACKET

  setwd(source.dir)

  #Read data files

#FUN #Function: Select right 'n' characters of string
    substrRight <- function(x, n){
      substr(x, nchar(x)-n+1, nchar(x))
    }
    
#FUN #Find most recently modified file in a directory    
    most.recently.modified.filename.fun <- function(title.string.match, file.type, dir){
      match.files.v <-
        list.files()[
          grepl(tolower(title.string.match), tolower(list.files())) &  #match title string
          grepl(file.type, sapply(list.files(), function(x){substrRight(x, nchar(file.type))})) &           #match file type
          !grepl("\\~\\$", list.files())       #restrict to non-temporary files
        ]
      
      most.recent.match.file <- match.files.v[file.info(match.files.v)$mtime == sapply(match.files.v, function(x){file.info(x)$mtime}) %>% max]
      return(most.recent.match.file)
    }
    
    #Questions Table (imported as list)
      configs.ss <- gs_key("1clXjraTC8w3_RnFkNetxUCSDWEtkWebZHCKOp_SLML8",verbose = TRUE) 
      questions.ls <- 	gs_read(configs.ss, ws = "questions", range = NULL, literal = TRUE) %>% as.list() %>% lapply(., tolower)
      questions.df <- do.call(cbind, questions.ls) %>% as.data.frame(., stringsAsFactors = FALSE)
      #! Update to read all configs from same google sheet?
      
    #Responses table (main data, imported as data frame)
      resp1.df <- read.csv(
        file =  
          most.recently.modified.filename.fun(
            title.string.match = "CWIS",
            file.type = "csv",
            dir = source.dir
          ),
        stringsAsFactors = FALSE,
        header = TRUE
      )
}#END SECTION COLLAPSE BRACKET

#OUTPUTS
  #questions.df
  #resp1.df (initial responses dataset which will need extensive cleaning and organization in next sections)

########################################################################################################################################################      
### INITIAL INFORMATICS & 'UNBRANCHING' (STACKING) OF BRANCHED VARIABLES ###

{ #SECTION COLLAPSE BRACKET
  
  #Initial informatics
    
    #Global Answer Options
      ans.opt.always.df <-  cbind(
        c(5:1),
        c("Always","Most of the time","About half the time","Sometimes","Never"),
        c("Strongly Agree","Agree","Neutral","Disagree","Strongly Disagree")
      ) %>% as.data.frame
      names(ans.opt.always.df) <- c("ans.num","ans.text.freq","ans.text.agreement")
      ans.opt.always.df[,1] <- ans.opt.always.df[,1] %>% as.character %>% as.numeric
      ans.opt.always.df[,2] <- ans.opt.always.df[,2] %>% as.character
      ans.opt.always.df[,3] <- ans.opt.always.df[,3] %>% as.character
    
    #Restrict questions.df to only rows for this year/semester
      questions.sem.df <- 
        questions.df[
          questions.df$year == year & questions.df$semester == semester,
        ]
  
    #Remove extra header rows
      #dat.startrow <- 
      resp1.df <- resp1.df[(which(substr(resp1.df[,1],1,1) == "{") + 1):length(resp1.df[,1]),]
      
    #Edit variable names
      names(resp1.df) <- resp1.df %>% names %>% tolower #Lower-case all variable names
      #names(resp1.df)[names(resp1.df) == "id"] <- "responseid"
      
      #Variable renaming of important variables
        names(resp1.df) <-
          mgsub(
            questions.sem.df$row.1[!is.na(questions.sem.df$q.changename)], 
            questions.sem.df$q.changename[!is.na(questions.sem.df$q.changename)], 
            names(resp1.df)
          )

    #Add "x" to questions.sem.df$row.1 so they match exactly with Qualtrics export as imported by R
      
#FUN #Function: output number of times specified substring occurs within vector of character strings
      num.substring.matches <- 
        function(pattern, vector){
          sapply( gregexpr( pattern, as.character(vector)),
          function(x) if( x[1]==-1 ){ 0 }else{ length(x) } )
        }
      
      questions.df$row.1[num.substring.matches("_",questions.df$row.1) == 2] <- 
        paste("x",questions.df$row.1[num.substring.matches("_",questions.df$row.1) == 2],sep="")
      
  #Stacking Columns Split by Survey Branching
    #"branch" refers to branching questions, so branch0 are columns without branches, and branch1 are columns that are part of branching questions
    #"ans" refers to having answer options, so ans0 are columns without answer options, and ans1 are columns that are part of questions with multiple answer options
      
    branch0.ans0.colnames.v <- names(resp1.df)[num.substring.matches("_",names(resp1.df))==0]
    branch0.ans1.colnames.v <- names(resp1.df)[num.substring.matches("_",names(resp1.df))==1 & substr(names(resp1.df),1,1) == "q"]
    branch1.ans0.colnames.v <- names(resp1.df)[num.substring.matches("_",names(resp1.df))==1 & substr(names(resp1.df),1,1) == "x"]
    branch1.ans1.colnames.v <- names(resp1.df)[num.substring.matches("_",names(resp1.df))==2]
    
    #branch.q.colnums.v <- which(resp1.df %>% names %>% substr(.,1,1) == "x")
    
    #Make data frame of base variables (that require no stacking)  
      branch0.df <- 
        resp1.df[ ,                                           
          names(resp1.df) %in% c("responseid",branch0.ans0.colnames.v,branch0.ans1.colnames.v)      
        ]              
    
    #Make data fram of variables to be stacked
      branch1.df <- 
        resp1.df[ ,
          names(resp1.df) %in% c("responseid",branch1.ans0.colnames.v,branch1.ans1.colnames.v) # ResponseId plus all columns whose names begin with "X"
        ]
    
    #Re-stack & collapse columns that are split up because of survey branching 
      
      #Base names of questions that have multiple branches
        branch1.q.v <- 
          strsplit(c(branch1.ans1.colnames.v,branch1.ans0.colnames.v), "_") %>% 
          unlist %>% 
          .[grep("q",.)] %>% 
          unique 
      
      ###                                                    ###
      # Start of loop 'a' by base question of branched columns #
      ###                                                    ###
      
      #Loop output storage
      q.ls <- list()
      varname.match.ls <- list()
      
      #a <- 1 #for testing loop
      for(a in 1:length(branch1.q.v)){     ### START OF LOOP BY QUESTION; only for questions with branched variables
        
        q.name.a <- branch1.q.v[a]                                 # base question name
        varnames.a <-                                              # all columns in branch0.df that belong to base question number
          names(branch1.df)[names(branch1.df) != "responseid"][
            which(names(branch1.df)[names(branch1.df) != "responseid"] %>% strsplit(.,"_") %>% lapply(., `[[`, 2) %>% unlist == q.name.a)
          ]

        if(num.substring.matches("_",varnames.a) %>% unique() == 1){ # final column names once branching is collapsed
          q.ans.options.a <- q.name.a
        }else{}
        
        if(num.substring.matches("_",varnames.a) %>% unique() == 2){
          q.ans.options.a <-
            paste(q.name.a, 
                  varnames.a %>% strsplit(.,"_") %>% lapply(., `[[`, 3) %>% unique %>% unlist,
                  sep = "_")
          }        
        varname.match.ls[[a]] <- q.ans.options.a

        unbranch.a.ls <- list()
        
        ###                                                    ###
        # Start of loop 'b' by base question of branched columns #
        ###                                                    ###
        
        #b = 12
        for(b in 1:length(q.ans.options.a)){    ### START OF LOOP BY ANSWER OPTION
          
          check.varnames.a <- str_sub(names(branch1.df), start = -nchar(q.ans.options.a[b]))
          
          branch.df.b <- #data frame of only columns to be stacked (for this question, for this answer option)
            branch1.df[,
              c(
                grep("responseid",names(branch1.df)),
                grep(q.ans.options.a[b], check.varnames.a)
              )
            ]
          
          unbranch.df.b <- #reshaped data frame of stacked columns
            reshape(
              data = branch.df.b, 
              idvar = "responseid",
              timevar = NULL,
              varying = names(branch.df.b)[names(branch.df.b) != "responseid"],
              v.names = q.ans.options.a[b],
              direction = "long"
            ) %>% 
            filter(.[,2] != "") #remove rows with blank answers
            
          unbranch.a.ls[[b]] <- unbranch.df.b
          
        } ### END OF LOOP BY ANSWER OPTION
        
        q.dat.df <- unbranch.a.ls %>% Reduce(function(x, y) full_join(x,y, all = TRUE), .)
        q.ls[[a + 1]] <- q.dat.df
        
      } ### END OF LOOP BY QUESTION
      
      #Re-merge with non-branched variables
      
      q.ls[[1]] <- branch0.df
      #q.ls <- lapply(q.ls, tolower)
      resp2.df <- q.ls %>% Reduce(function(x, y) full_join(x,y, all = TRUE), .) 
      
   
    #Re-do question table so no extraneous rows for roles that are now unbranched  
      q.unbranched.df <- 
        questions.sem.df[
          questions.sem.df$row.1 %in% 
          c(
            branch0.ans0.colnames.v,
            branch0.ans1.colnames.v,
            varname.match.ls %>% unlist %>% paste("1_",.,sep="")
          )
          ,
        ] #!looks like still uneven numbers - some columns must be missing from questions table, but seems to be columns we don't care about.
      q.unbranched.df$row.1[grep("q",q.unbranched.df$row.1)] <- 
        str_extract(
          q.unbranched.df$row.1[grep("q",q.unbranched.df$row.1)], 
          "q[0-9].+"
        )
      q.unbranched.df <-
        SplitColReshape.ToLong(
          df = q.unbranched.df,
          id.varname = "row.1",
          split.varname = "module",
          split.char = ","
        )
    
}#END SECTION COLLAPSE BRACKET    
    
#OUTPUTS
  #resp2.df - now with all branch variables 'unbranched' (stacked), and having removed two extra header rows
  #questions.sem.df - now only with rows pertaining to this year and semester
    
########################################################################################################################################################      
### FURTHER CLEANING & ADDING USEFUL VARIABLES ###

{ #SECTION COLLAPSE BRACKET
  
    #Lower-Case All Data
      resp3.df <- apply(resp2.df,c(1:2),tolower) %>% as.data.frame(., stringsAsFactors = FALSE)
    
    #Recode role variable
      resp3.df$role <- mgsub("Teacher", "Classroom Teacher", resp3.df$role)
      
    #Recode school & district names
      #school.name.patterns <- c("elem\\.","sch\\.","co\\.","jr\\.","sr\\.","meramec valley early childhood")
      #school.name.replacements <- c("elementary","school","county","junior","senior","early childhood center")
      #school.name.patterns <- c("\\.")
      #school.name.replacements <- c("")
      #resp3.df$building <- mgsub(school.name.patterns,school.name.replacements,resp3.df$building)
      resp3.df$building <- mgsub("bucahanan","buchanan",resp3.df$building)
      resp3.df$district <- mgsub("bucahanan","buchanan",resp3.df$district)
      
    #Create school.id variable which is concatenation of school and district
      resp3.df$building.id <- paste(resp3.df$district, resp3.df$building,sep = "_") %>% tolower
      resp3.df$building.id <- gsub("\\/"," ",resp3.df$building.id) #in case there is a slash in the school name itself, this replaces it so file storage for ppt works properly
      
    #Capitalize first letter of Building and District columns
      #resp3.df$building <- FirstLetterCap_MultElements(resp3.df$building)
      #resp3.df$district <- FirstLetterCap_MultElements(resp3.df$district)
      
    #School Level Variable
      #school.level.df <- 
      #  read.xlsx(
      #    "MMD List with Grade Spans.xlsx",
      #    sheetName = "MMD Cohort 1&2",
      #    header = TRUE,
      #    as.data.frame = TRUE,
      #    stringsAsFactors = FALSE) %>%
      #  mutate(school.id = paste(tolower(district.name),tolower(trimws(school.name, which = "both")),sep = "_"))
      
      #resp3.df <- left_join(resp3.df,school.level.df %>% select(school.id, school.level), by = "school.id")
      #resp3.df$building.level[is.na(resp3.df$building.level)] <- "Other"
      #resp3.df$building.level[resp3.df$building.id == "belton 124_bosco"] <- "Other"
      #resp3.df$building.level[resp3.df$building.id == "cameron r-i_cameron high school"] <- "High"
      #resp3.df$building.level[resp3.df$building.id == "poplar bluff r-i_poplar bluff early childhood center"] <- "Elem."
      #resp3.df$building.level[resp3.df$building.id == "poplar bluff r-i_poplar bluff technical career center"] <- "Other"
      #resp3.df$building.level[resp3.df$building.id == "sheldon r-viii_sheldon k-12"] <- "Other"
      #resp3.df$building.level <- FirstLetterCap_MultElements(resp3.df$building.level)
      
    #Capitalize First Letter of character variables
      resp3.df[,names(resp3.df) %in% c("year","role","district","school","school.level")] <- 
        apply(resp3.df[,names(resp3.df) %in% c("year","role","district","school","school.level")], 2, FirstLetterCap_MultElements)
      
    
    #Rearrange data columns
      resp3.df <- resp3.df[,   # CWIS response variables last, others first
          c(which(!grepl("_", names(resp3.df))),
            grep("_", names(resp3.df)))
        ]
      
      resp3.df <-  resp3.df[, #Put "responseid" in first column
                    c(grep("responseid", names(resp3.df)),which(!grepl("responseid", names(resp3.df))))
                    ]
    
    #Remove rows with no district or building name
      resp3.df <- resp3.df %>% filter(building.id != "_")
      
    ##########################################################################################################################################
    #Column Class Conversions
      #resp3.df <- ColClassConvert(resp3.df)
    
  #Add useful variables for analysis 
    
    #Useful vectors for selecting cwis answer variables
      cwis.vars.v <- which(names(resp3.df) %in% q.unbranched.df$row.1[!is.na(q.unbranched.df$module)])
      cwis.varnames.v <- names(resp3.df)[names(resp3.df) %in% q.unbranched.df$row.1[!is.na(q.unbranched.df$module)]]
      cwis.modules.v <- 
        questions.sem.df$module[!is.na(questions.sem.df$module)] %>% 
        unique %>% 
        strsplit(.,"\\/") %>% 
        unlist %>% 
        unique
    
    #Recode answer option variables as numeric
      numeric.recode.fun <- 
        function(x){
          recode(
            x,
            `always` = 5,
            `most of the time` = 4,
            `about half the time` = 3,
            `sometimes` = 2,
            `never` = 1,
            `strongly agree` = 5,
            `agree` = 4,
            `neutral` = 3,
            `neither agree nor disagree` = 3,
            `neither agree or disagree` = 3,
            `disagree` = 2,
            `strongly disagree` = 1
          )
        }
        
      recode.ansopt.varnames.v <- 
        which(resp3.df %>% 
        apply(., 2, unique) %>%
        sapply(., function(x) {
          x %in% c("always","most of the time","about half the time","sometimes","never","strongly agree","agree","neutral","disagree","strongly disagree")
        }) %>%
        sapply(., any)) %>%
        names(resp3.df)[.]

      num.ansopt.vars.df <- 
        apply(resp3.df[,names(resp3.df) %in% recode.ansopt.varnames.v], 
          2,
          numeric.recode.fun
        ) %>% 
        as.data.frame
      
      names(num.ansopt.vars.df) <- paste(names(resp3.df[,names(resp3.df) %in% recode.ansopt.varnames.v]),"_num", sep = "")
    
    #Recode Original Answers to add numbers (e.g. "Always" becomes "1. Always")
      addnums.recode.fun <- 
        function(x){
          recode(
            x,
            `always` = '5. always',
            `most of the time` = '4. most of the time',
            `about half the time` = '3. about half the time',
            `sometimes` = "2. sometimes",
            `never` = "1. never",
            `strongly agree` = "5. strongly agree",
            `agree` = "4. agree",
            `neutral` = "3. neutral",
            `neither agree nor disagree` = "3. neutral",
            `neither agree or disagree` = "3. neutral",
            `disagree` = "2. disagree",
            `strongly disagree` = "1. strongly disagree"
          )
        }
      
      recode.addnums.df <- 
        apply(resp3.df[,names(resp3.df) %in% recode.ansopt.varnames.v], 
              2,
              addnums.recode.fun
        ) %>% 
        as.data.frame
      
    #Create 'implementation' binary variables
      binary.ansopt.vars.df <- apply(num.ansopt.vars.df,c(1:2),function(x){ifelse(x >= 3.5,1,0)}) %>% as.data.frame
      names(binary.ansopt.vars.df) <- paste(names(resp3.df[,names(resp3.df) %in% recode.ansopt.varnames.v]),"_binary",sep = "")
      

#FUN  #Function: apply to columns to be converted into binary implementation
      #binary.recode.fun <- function(vector, binary.cutoff){
      #  result <- ifelse(vector >= binary.cutoff, 1, 0)
      #  return(result)
      #}

#FUN  #Function: output variable names in data frame which can be converted to numeric       
      numeric.varnames.v <-
        function(df){
          result <- 
            df %>%
            apply(., 2, unique) %>%
            sapply(., 
              function(x){
                (as.numeric(x) %>% is.na(.) %>% sum) <= 1
              }
            ) %>%
            names(df)[.]
          return(result)
        }
      
      #Convert numeric variables in original data to numeric
        resp3.df[,names(resp3.df) %in% numeric.varnames.v(resp3.df)] <-
          apply(
            resp3.df[,names(resp3.df) %in% numeric.varnames.v(resp3.df)],
            c(1:2),
            as.numeric
          )
      
      #Converting Slider variables to numeric and binary (according to different max/min/thresholds)
        
        slider.vars.df <- 
          resp3.df[,
            names(resp3.df) %in% q.unbranched.df$row.1[!is.na(q.unbranched.df$var.min)]
          ]
        
        slider.binary.vars.ls <- list()
        slider.num.vars.ls <- list()
        
        #!Should straighten out letters for loops
        for(c in 1:ncol(slider.vars.df)){
          
          colname.c <- names(slider.vars.df)[c]
          var.min.c <- 
            q.unbranched.df$var.min[q.unbranched.df$row.1 == colname.c][!is.na(q.unbranched.df$var.min[q.unbranched.df$row.1 == colname.c])] %>% 
            .[1] %>%                 #Once did SplitColReshape on the questions table, have two rows for some questions so have to select first one only.
            as.character %>% 
            as.numeric
          var.max.c <- 
            q.unbranched.df$var.max[q.unbranched.df$row.1 == colname.c][!is.na(q.unbranched.df$var.max[q.unbranched.df$row.1 == colname.c])] %>% 
            .[1] %>%
            as.character %>% 
            as.numeric
          
          if(var.min.c == 1 & var.max.c == 5){
            cuts <- c(1.5,2.5,3.5,4.5)
            slider.binary.threshold.c <- 3.5
          }
          
          if(var.min.c == 1 & var.max.c == 10){
            cuts <- seq(
              from = var.min.c,
              to = var.max.c, 
              #by = ((to - from)/(length.out - 1)), 
              length.out = 5)[1:4]
            slider.binary.threshold.c <- 7
          }
          
          if(length(var.min.c == 1 & var.max.c == 5) > 1){
            print(c)
          }
          slider.num.vars.ls[[c]] <- findInterval(slider.vars.df[,c], cuts)+1
          slider.binary.vars.ls[[c]] <- ifelse(slider.vars.df[,c] >= slider.binary.threshold.c, 1, 0)
        }
        
        slider.num.vars.df <- 
          do.call(cbind, slider.num.vars.ls) %>% 
          as.data.frame %>%
          replace.names.fun(
            df = .,
            current.names = names(.),
            new.names = names(slider.vars.df)
          ) 
        
        slider.agreement.varnames.v <- 
          q.unbranched.df$row.1[q.unbranched.df$var.type == "continuous" & q.unbranched.df$scale.type == "agreement"] %>%
          remove.na.from.vector()
        
        slider.freq.varnames.v <-
          q.unbranched.df$row.1[q.unbranched.df$var.type == "continuous" & q.unbranched.df$scale.type == "frequency"] %>%
          remove.na.from.vector()
        
        slider.agreement.df <-
          slider.num.vars.df[,names(slider.num.vars.df) %in% slider.agreement.varnames.v] %>%
          apply(., 2, function(x){
            mgsub(
              pattern = c(5:1),
              replacement = paste(ans.opt.always.df$ans.num,". ",ans.opt.always.df$ans.text.agreement, sep = "") %>% tolower,
              x = x
            )
          }) %>%
          as.data.frame()
        
        slider.freq.df <-
          slider.num.vars.df[,names(slider.num.vars.df) %in% slider.freq.varnames.v] %>%
          apply(., 2, function(x){
            mgsub(
              pattern = c(5:1),
              replacement = paste(ans.opt.always.df$ans.num,". ",ans.opt.always.df$ans.text.freq, sep = "") %>% tolower,
              x = x
            )
          }) %>%
          as.data.frame()
        
        slider.text.df <- 
          cbind(slider.agreement.df, slider.freq.df) %>%
          replace.names.fun(
            df = .,
            current.names = names(.),
            new.names = paste(names(.),"_text",sep="")
          )
        
        slider.binary.vars.df <- 
          do.call(cbind, slider.binary.vars.ls) %>% 
          as.data.frame %>%
          replace.names.fun(
            df = .,
            current.names = names(.),
            new.names = paste(names(slider.vars.df),"_binary",sep="")
          )
        
    #Create final data frames: 1. Wide; 2. Long for original CWIS data; 3. Long for impbinary data (both long include all original id variables)
      resp.wide.df <- 
        cbind(
          resp3.df[,setdiff(names(resp3.df),names(recode.addnums.df))], 
          recode.addnums.df, 
          num.ansopt.vars.df, 
          binary.ansopt.vars.df,
          slider.num.vars.df %>% replace.names.fun(df = ., current.names = names(.), new.names = paste(names(.),"_num",sep = "")),
          slider.binary.vars.df,
          slider.text.df
        )
      resp.long.df <- 
        melt(
          data = resp.wide.df,
          id.vars = names(resp.wide.df)[!grepl("q[0-9]",names(resp.wide.df))],
          variable.name = "question",
          value.name = "answer",
          stringsAsFactors = FALSE
        )
      resp.long.df$question <- as.character(resp.long.df$question)
      
      #filter.varnames.v <- resp.long.df$question %>% unique %>% vector.filter.fun(grepl("num|binary",.),.) %>% str_extract(., "q[0-9]*_[0-9]*") %>% unique
        #c(branch1.ans0.colnames.v,branch1.ans1.colnames.v) %>% gsub("x[0-9]*\\_","",.) %>% unique      
      #resp.long.df <- 
      #  resp.long.df[!resp.long.df$question %in% filter.varnames.v,]

    #Creating additional useful variables for long data frames
      
      #Variable for module
        resp.long.df$q.original <- 
          paste(
            str_extract(resp.long.df$question, "q[0-9]*"),
            ifelse(is.na(str_extract(resp.long.df$question,"_[0-9]")),"",str_extract(resp.long.df$question, "_[0-9]*")),
            sep = ""
          )
        resp.long.df <- 
          left_join(
            resp.long.df, 
            q.unbranched.df[,names(q.unbranched.df) %in% c("row.1","module","practice")], 
            by = c("q.original" = "row.1")
          )  
        
        #Later will matter that we have "cfa,etlp" value in module variable for forming graph data.
        resp.long.df <-
          SplitColReshape.ToLong(
            df = resp.long.df,#[grep(",",resp.long.df$module),],
            id.varname = "responseid",
            split.varname = "module",
            split.char = ","
          ) 
        
      #Variable designating questions to be used in implementation calculations (binary)
        resp.long.df$impbinary <- ifelse(grepl("binary",resp.long.df$question),1,0)
      
      #Variable designating questions to be used in average performance calculations
        #avg.perf.q.varnames.v <-
        #  c(grepl("_num",  )
        #    q.unbranched.df$row.1[!is.na(q.unbranched.df$var.type) & q.unbranched.df$var.type == "integer"],
        #    paste(names(slider.num.vars.df),"_num",sep="")
        #  )
          
        resp.long.df$avg.perf.q <- ifelse(grepl("_num", resp.long.df$question), 1, 0)
        
      #Variable designating questions to be used in tables (bucketing)
        table.q.varnames.v <- 
          c(
            q.unbranched.df$row.1[!is.na(q.unbranched.df$var.type) & q.unbranched.df$var.type == "integer"],
            names(slider.text.df)
          )
            
        resp.long.df$table.q <- ifelse(resp.long.df$question %in% table.q.varnames.v, 1, 0)
      
    #Loop: state average implementation rates [a], results in imp.state.df
      #imp.state.df <- data.frame(cwis.module = cwis.modules.v)
      #a <- 1 # LOOP TESTER
      #for(a in 1:length(cwis.modules.v)){
      #  imp.state.df$imp.rate[imp.state.df[,1]==cwis.modules.v[a]] <- 
      #    impbinary.df[,grep(cwis.modules.v[a], names(impbinary.df))] %>%
      #    apply(., 2, function(x){mean(x, na.rm = TRUE)}) %>% 
      #    mean() 
      #}
        
    #Write Unbranched Data to Excel File
      unbranched.file.name <- 
        paste( 
          "widedata_",
          gsub(":",".",Sys.time()),
          ".csv", 
          sep=""
        ) 
      
      setwd(target.dir)
      
      write.csv(
        resp.wide.df,
        file = unbranched.file.name,
        #sheetName = "responses",
        row.names = FALSE
        #showNA = FALSE,
        #append = FALSE
      )
      
      #write.xlsx(
      #  questions.sem.df,
      #  file = "unbranched_data.xlsx",
      #  sheetName = "questions",
      #  row.names = FALSE,
      #  showNA = FALSE,
      #  append = TRUE
      #)
        
}#END SECTION COLLAPSE BRACKET

#OUTPUTS
  #resp.wide.df: wide data with all variables including numeric and binary
  #resp.long.df: long format data frame with cwis responses
  #ans.opt.always.df: data frame with columns corresponding to answer numbers and answer text,
    #including both frequency scale (e.g. 'always', 'most of the time') and agreement scale (e.g.
    #'strongly agree', 'agree').

########################################################################################################################################################      
### PRODUCING SLIDE, GRAPH, AND TABLE CONFIGURATION TABLES ###

{ #SECTION COLLAPSE BRACKET
  
  #Report Unit Selection
    report.unit <- "building" #can be either "building" or "district"
    report.ids <- "all"
    
    if(!report.unit %in% c("building","district")){
      stop("Report unit must be either 'building' or 'district.'")
    }
    
    if(report.unit == "building"){
      report.id.colname <- "building.id"
    }else{
      report.id.colname <- "district"
    }
    
    report.id.col <- resp.wide.df[,names(resp.wide.df) == report.id.colname]
    
    if(tolower(report.ids) %in% "all" %>% any){
      report.ids <- 
        report.id.col[order(report.id.col)] %>%
        unique %>% 
        vector.filter.fun(
          condition = !grepl("district office",.),
          vector.input = .
        )
    }else{}   #If user has designated district names as "all", code will create reports for all district names present in the data
    
  #Load Graph & Slide Type Config Tables
    setwd(source.dir)
    config.slidetypes.df <- read.xlsx("graph_configs.xlsx", sheetName = "slide.types",header = TRUE, stringsAsFactors = FALSE) #gs_read(configs.ss, ws = "slide.types", range = NULL, literal = TRUE) #
    load.config.graphtypes.df <- read.xlsx("graph_configs.xlsx", sheetName = "graph.types",header = TRUE, stringsAsFactors = FALSE) #gs_read(configs.ss, ws = "graph.types", range = NULL, literal = TRUE) #
    load.config.tabletypes.df <- read.xlsx("graph_configs.xlsx", sheetName = "table.types",header = TRUE, stringsAsFactors = FALSE) #gs_read(configs.ss, ws = "table.types", range = NULL, literal = TRUE) #
    
    config.graphtypes.df <- 
      SplitColReshape.ToLong(
        config.slidetypes.df, 
        id.varname = "slide.type.id",
        split.varname = "slide.graph.type",
        split.char = ","
      ) %>%
      left_join(., load.config.graphtypes.df, by = c("slide.graph.type" = "graph.type.id")) %>% 
      filter(!is.na(slide.graph.type))
    
    config.tabletypes.df <- 
      SplitColReshape.ToLong(
        df = config.slidetypes.df, 
        id.var = "slide.type.id",
        split.varname = "slide.table.type",
        split.char = ","
      ) %>%
      left_join(., load.config.tabletypes.df, by = c("slide.table.type" = "table.type.id")) %>% 
      filter(!is.na(slide.table.type))
    
    #config.graphtypes.df <- config.graphtypes.df[,grep("slide.type.id|loop|data|graph|height|width|offx|offy",names(config.graphtypes.df))]
  
  #Expand Graph & Slide Config Tables for each district according to looping variables
    
    ###                          ###    
#   ### LOOP "b" BY REPORT.UNIT  ###
    ###                          ###
    
  #Loop Outputs 
    config.graphs.ls.b <- list()
    config.tables.ls.b <- list()
    config.slides.ls.b <- list()
  
  #Progress bar for loop
    progress.bar.b <- txtProgressBar(min = 0, max = 100, style = 3)
    maxrow.b <- length(report.ids)
    
  #b <- 1 #LOOP TESTER (19 = "Raytown C-2")
  #for(b in c(1,2)){   #LOOP TESTER
  for(b in report.startnum:length(report.ids)){   #START OF LOOP BY DISTRICT
    
    #print(b)
    loop.start.time.b <- Sys.time()
    
    if(b == 1){print("FORMING SLIDE, GRAPH, AND TABLE CONFIG TABLES...")}
    #print(c(b,100*b/length(report.ids)))
    
    #Create report.id.b (for this iteration) and skip if report for district office
      report.id.b <- report.ids[b]
      
      if(report.unit != "district" & grepl("district office", report.id.b)){
        next()
      }
      
    #Create data frames for this loop - restrict to district id i  
      resp.long.df.b <- 
        resp.long.df %>% 
        select(names(resp.long.df)[names(resp.long.df) == report.id.colname]) %>% 
        equals(report.id.b) %>% 
        resp.long.df[.,]
      #print(head(resp.long.df.b))
      
#FUN#Function: Loop Expander for creating full config tables
      #Function input testers
        #configs = config.slidetypes.df
        #loop.varname = "slide.loop.var"
        #collate.varname = "slide.type.position"
        #source.data = resp.long.df.b  
      
      loop.expander.fun <- function(configs, loop.varname, collate.varname, source.data){
        output.ls <- list()
        
        #c = 2 #LOOP TESTER: NO LOOPS
        #c = 3 #LOOP TESTER: ONE LOOP VAR
        #c = 8 #LOOP TESTER: TWO LOOP VARS
        #for(c in 1:3){
        for(c in 1:dim(configs)[1]){
        
          c.list.c <- list(c=c)
          
          #Make data frame with configurations repeated out across all unique combinations of loop.varname(s) in source.data      
            slide.loop.vars.c <- 
              ifelse(
                is.na(configs[c,names(configs)==loop.varname]),
                NA,
                configs[c,names(configs)==loop.varname] %>% 
                  as.character %>% 
                  strsplit(., ",") %>% 
                  unlist %>% 
                  trimws(., which = "both")
              )
            
            if(any(is.na(slide.loop.vars.c))){
              configs.c <- configs[c,]
            }
            
            if(any(!is.na(slide.loop.vars.c))){
      
              loop.unique.df <- 
                source.data[names(source.data) %in% slide.loop.vars.c] %>% 
                lapply(., unique) %>%
                lapply(., remove.na.from.vector) %>%
                lapply(., function(x) {strsplit(x, ",")}) %>%
                lapply(., unlist) %>%
                lapply(., unique) %>%
                expand.grid(., stringsAsFactors = FALSE) %>%
                as.data.frame #unique items for each loop that will specify graph (e.g. school name)
              loop.unique.df <- loop.unique.df[order(loop.unique.df[,names(loop.unique.df)==slide.loop.vars.c[1]]),] %>% as.data.frame
              names(loop.unique.df) <- slide.loop.vars.c
              
              configs.c <- 
                configs[  
                  rep(
                    c.list.c[["c"]],
                    dim(loop.unique.df)[1]
                  )
                ,] %>%
                cbind(.,loop.unique.df)
            }
            output.ls[[c]] <- configs.c
            #print(configs.c)
            
        } ### END OF LOOP "C" BY ROW OF CONFIG INPUT ###
        
        output.df <- rbind.fill(output.ls) %>% mutate(row.id = row.names(.))
        
      #Collate Report Sub-Sections 
        if(!missing(collate.varname)){
#FUN    #Function: check which elements in a vector are different from the one before and return position of spots where values change
          vector.value.change.positions.fun <- function(x){
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
          
        #Form inputs for collating loop: list with sections (collated and non-colated, in order) 
          collate.section.configs.df <- vector.value.change.positions.fun(output.df$slide.loop.collate.section)
          collate.ls <- list()
          for(e in 1:nrow(collate.section.configs.df)){
            collate.ls[[e]] <- output.df[collate.section.configs.df$start.position[e]:collate.section.configs.df$end.position[e],]
          }
          
        ###                                          ###
        # Start of loop 'd' by collated report section #
        ###                                          ###
          
        #The following loop takes as input the list of report slides which have just been broken up into an ordered list of
          #collated and non-collated sections. For sections requiring collation, it will replace the list element with the
          #collated version of the slide configurations.
          
          #d = 2 #LOOP TESTER
          for(d in 1:length(collate.ls)){  
            #for each unique report section:
              #if it doesn't require collation, do nothing
              #select lines of output.df with only that unique slide.loop.collate.section
              #order by looping variable that has same name as slide.loop.var (e.g. 'school') AND by slide.type.position
              #store in list (to be re-attached) to non-collated sections and other collated sections
            
            #Section id to collate for this iteration
              collate.input.df.d <- collate.ls[[d]]
              
            #Skip iteration of no collation/re-ordering necessary
              if(unique(is.na(collate.input.df.d$slide.loop.collate.section))){
                next()
              }
            
            #Name of loop variable (will be used to order data-frame in b-loop)
              loop.var.d <- collate.input.df.d %>%
                select(slide.loop.var) %>%
                unlist %>%
                unique 
              
            #Create collated data frame to replace un-collated one in slide list
              collate.ls[[d]] <- 
                collate.input.df.d[
                  order(
                    collate.input.df.d %>% select(matches(loop.var.d)), 
                    collate.input.df.d$slide.type.position
                  )
                ,]
       
            } #END OF LOOP "d" BY COLLATED SECTION
          
          output.df <- do.call(rbind, collate.ls)
      
          } #END OF 'IF' STATEMENT FOR WHEN SOME REPORT SECTIONS REQUIRE COLLATING
        
        return(output.df)
        
      } #END OF LOOP EXPANDER FUNCTION

#FUN#Function: Replace NAs in a vector with a replacement value
    na.sub <- function(vector,na.replacement){
      vector[is.na(vector)] <- na.replacement
      return(vector)
    }       

#FUN#Function: School-level slides should not include an iteration for the District Office 
    remove.district.office.fun <- function(x){
      if(report.unit != "district" & !grepl("district office", report.id.b)){
        #print("Report unit is 'building' and the report.id for this loop does not contain 'district office.' Returning input with no changes.")
        return(x)
      }
      
      if(report.unit != "district" & grepl("district office", report.id.b)){
        #print("Report unit is 'building' and the report.id for this loop contains 'district office.' Skipping to next loop")
        return(x)
      }
      
      if(report.unit == "district"){
        x[!(grepl("school", x$slide.loop.var) & (x$school %>% na.sub(.,"")) == "District Office"),] %>% 
          return(.)
      }
    }
    
    #Graphs config table for this report unit
      config.graphs.df <- 
        loop.expander.fun(
          configs = config.graphtypes.df, 
          loop.varname = "slide.loop.var", 
          source.data = resp.long.df.b
        )
    
      config.graphs.ls.b[[b]] <- remove.district.office.fun(config.graphs.df)
    
    #Tables config table for this report unit
      config.tables.df <-
        loop.expander.fun(
          configs = config.tabletypes.df, 
          loop.varname = "slide.loop.var", 
          source.data = resp.long.df.b
        )
      
      config.tables.ls.b[[b]] <- remove.district.office.fun(config.tables.df)
    
    #Slide config table for this report unit
      config.slides.df <- 
        loop.expander.fun(
          configs = config.slidetypes.df, 
          loop.varname = "slide.loop.var",
          collate.varname = "slide.type.position",
          source.data = resp.long.df.b
        )
      
      config.slides.ls.b[[b]] <- remove.district.office.fun(config.slides.df)
      
    
    setTxtProgressBar(progress.bar.b, 100*b/maxrow.b)

  } # END OF LOOP 'b' BY REPORT.UNIT
  close(progress.bar.b)
  
} # END SECTION COLLAPSE BRACKET
    
#OUTPUTS:
  #report.ids: vector with all report unit names in resp.long.df (length = 19 for baseline data)
  #config.slides.ls.b
    #[[report.unit]]
      #data frame where each line represents a slide
  #config.tables.ls.b
    #[[report.unit]]
      #data frame where each line represents a table
  #config.graphs.ls.b
    #[[report.unit]]
      #data frame where each line represents a graph
  

########################################################################################################################################################      
### PRODUCING GRAPH & TABLE DATA ###

{# SECTION COLLAPSE BRACKET
     
###                          ###    
### LOOP "c" BY REPORT UNIT  ###
###                          ###
      
  #Loop outputs
    graphdata.ls.c <- list()
    tabledata.ls.c <- list()
    #graphs.ls.c <- list()
  
  #Progress bar for loop
    progress.bar.c <- txtProgressBar(min = 0, max = 100, style = 3)
    maxrow.c <- config.graphs.ls.b %>% sapply(., dim) %>% sapply(`[[`,1) %>% unlist %>% sum
  
    slider.report.ids <- grep("waynesville middle|warrensburg high|perry co. middle|veterans elem.|hannibal middle|trojan intermediate|sunrise elem.|salem sr. high|eugene field elem.|potosi elem.|mark twain elem.|lonedell elem.",
                                         report.ids)
  #c <- 26 #LOOP TESTER (19 = "Raytown C-2", 244 = "waynesville middle")
  #for(c in slider.report.ids){   #LOOP TESTER
  for(c in report.startnum:length(report.ids)){   #START OF LOOP BY DISTRICT
    
    if(c == 1){print("Forming input data tables for graphs...")}
    
    #Loop Inputs (both graphs and tables)
      report.id.c <- report.ids[c]
      district.c <- resp.long.df %>% filter(building.id == report.id.c) %>% select(district) %>% unique %>% unlist %>% remove.na.from.vector()
      
      resp.long.df.c <- 
        resp.long.df %>% 
        select(names(resp.long.df)[names(resp.long.df) == report.id.colname]) %>% 
        equals(report.id.c) %>% 
        resp.long.df[.,]

    ###                    ###
#   ### LOOP "d" BY GRAPH  ###
    ###                    ###
  
    #Loop Inputs
      config.graphs.df.c <- config.graphs.ls.b[[c]]
      graphdata.ls.d <- list()
        
    #d <- 5
    #for(d in 1:2){ #LOOP TESTER
    for(d in 1:dim(config.graphs.df.c)[1]){
      
      config.graphs.df.d <- config.graphs.df.c[d,]
      
        group_by.d <- config.graphs.df.d$data.group.by.var %>% 
          strsplit(., ",") %>% 
          unlist
      
      #Create data frame "all.cats.df.e" of all possible answers for x-axis (role, module, year, answer)
        
        #!
        #1. EVENTUALLY WILL NEED TO GENERALIZE THIS FUNCTION SO CAN TAKE AN ARBITRARY NUMBER OF CATEGORIES AS INPUT
        #   RIGHT NOW CAN ONLY TAKE TWO AND ONE OF THEM MUST BE 'YEAR,' AND THAT NOT EVEN IN CURRENT VERSION (SEE FINAL COMMAND COMMENTED OUT).
        #2. ALSO, RIGHT NOW WHEN SELECTING 'practice' IT LOOKS FOR THE CHARACTER SUBSTRING OCCURENCE IN THE 'module' VARIABLE WITH GREPL
        #   EVENTUALLY WILL WANT TO DO A STRINGSPLIT AND EXACT MATCH IN CASE THERE ARE MODULES THAT CONTAIN THE CHARACTERSTRINGS OF 
        #   OTHER MODULES (E.G. IF THERE WAS A MODULE 'CFAM' AND 'CFA' THEN THE FUNCTION WOULD PICK UP BOTH WHEN LOOKING FOR JUST 'CFA').
        
        
        
        #! WILL NEED TO DO SAME THING FOR TABLES
        
        #If graph category is 'practice' as in 2018-08 Green Reports, have to make extra restriction to filter down to practices relevant to the specific module
        if(!is.na(config.graphs.df.d$data.group.by.var) && config.graphs.df.d$data.group.by.var == "practice"){
          all.cats.input1.d <- 
            resp.long.df %>% 
            filter(grepl(config.graphs.df.d$module,module))
        }else{
          all.cats.input1.d <- 
            resp.long.df #%>%
        }
        
        all.cats.input2.d <-
          all.cats.input1.d %>%
          filter(impbinary == 0) %>%
          .[,names(resp.long.df) == config.graphs.df.d$data.group.by.var] %>% 
          unique %>%
          strsplit(., ",") %>% 
          unlist %>%
          unique %>%
          remove.na.from.vector(.)
        
        all.cats.df.d <- 
          all.cats.input2.d[order(all.cats.input2.d)] %>%
          as.data.frame(., stringsAsFactors = FALSE)
        
        names(all.cats.df.d) <- group_by.d
        #print(all.cats.df.d)
           
#FUN  #Function: Data restriction - district vs. building.id
        
        #!NEED TO GENEARALIZE: IF REPORT.UNIT IS DISTRICT AND GRAPH DATA.LEVEL IS DISTRICT, THIS WORKS, BUT NOT IF REPORT.UNIT IS 
        #BUILDING.ID AND DATA.LEVEL IS DISTRICT.
        
        graph.data.restriction.fun <- function(x){
          
          if(config.graphs.df.d$data.level == "district"){
            y <- x
          }
          
          if(config.graphs.df.d$data.level == "building.id"){
            y <- 
              x %>% 
              filter(building.id == report.id.c) 
          }
          
          if(is.na(config.graphs.df.d$data.restriction)){
            result <- y
          }
          
          if(!is.na(config.graphs.df.d$data.restriction)){
            result <- 
              y %>% 
              filter(
                y[,names(y) == config.graphs.df.d$data.restriction] == 
                  config.graphs.df.d[,names(config.graphs.df.d) == config.graphs.df.d$data.restriction]
              )
          }
          
          return(result)
        }
        
#FUN  #Function: Data Summarize - participation vs. implementation vs. performance 
        #Test inputs
          #config.input <- config.graphs.df.d
          #data.input <-  resp.long.df.c %>% graph.data.restriction.fun %>% group_by(!!! syms(group_by.d))
        
        summarize.data.fun <- function(config.input, data.input){
          if(config.input$data.measure == "participation"){
            result <- 
              dplyr::summarize(data.input, measure.var =  length(unique(responseid)))
          }
          
          if(config.input$data.measure == "implementation"){
            result <- 
              data.input %>% 
              filter(impbinary == 1) %>%
              dplyr::summarize(measure.var = mean(as.numeric(answer), na.rm = TRUE)) %>%
              as.data.frame(., stringsAsFactors = FALSE)
          }
          
          if(config.input$data.measure == "performance"){
            result <- data.input %>%
              filter(impbinary == 0, !is.na(answer)) %>%
              dplyr::summarize(measure.var = as.character(length(unique(responseid))))
          }
          
          if(config.input$data.measure == "average performance"){
            result <- 
              data.input %>%
              filter(grepl("_num",question)) %>%
              dplyr::summarize(., measure.var =  mean(as.numeric(answer), na.rm = TRUE))
          }
          
          return(result)
        }
      
      #Form final data frame (no averages)
        graphdata.df.d <-  
          resp.long.df.c %>%
          graph.data.restriction.fun %>%
          group_by(!!! syms(group_by.d)) %>%
          summarize.data.fun(config.input = config.graphs.df.d, data.input = .) %>%
          left_join(all.cats.df.d, ., by = c(group_by.d))
        graphdata.df.d$measure.var[is.na(graphdata.df.d$measure.var)] <- 0
        
        #print(graphdata.df.d)
        
#FUN  #Function: Restriction function for graph average data
        
        #! THESE TWO FUNCTIONS ARE VERY SIMILAR TO THE ONES ABOVE WHICH HAVE BEEN CHANGED SO NOW NEED TO SPECIFY "config.input" BUT
        #   HAVE NOT MADE THOSE CHANGES HERE YET. PROBABLY COULD ROLL UP INTO ONE OR TWO FUNCTIONS.
        
        avg.data.restriction.fun <- function(x){
          
          if(config.graphs.df.d$data.level == "district"){
            y <- x
          }
          
          if(config.graphs.df.d$data.level == "building.id"){
            y <- 
              x %>% 
              filter(district == unique(resp.long.df$district[resp.long.df$building.id == report.id.c])) 
          }
          
          z <- y %>% filter(!is.na(y[,names(y)==group_by.d])) #!Might want to make flexible - i.e. add a parameter which allows user to inlcude NA
          
          if(!config.graphs.df.d$data.restriction=="module" | is.na(config.graphs.df.d$data.restriction)){ 
            #!Should look into a better way to deal with this restriction, think about input tables
            result <- z
          }
          
          if(config.graphs.df.d$data.restriction=="module" & !is.na(config.graphs.df.d$data.restriction)){
            result <- 
              z %>%
              filter(
                z[,names(z)==config.graphs.df.d$data.restriction] == 
                  config.graphs.df.d[,names(config.graphs.df.d)==config.graphs.df.d$data.restriction]
              )
          }
          
          return(result)
        }
      
#FUN  #Function: Summary Function for Graph Averages
        summarize.avg.fun <- function(x){
          
          if(config.graphs.df.d$data.measure == "participation"){
            result <- x %>%
              dplyr::summarize(avg = length(unique(responseid))/length(unique(school.id)))#participation
          }
          
          if(config.graphs.df.d$data.measure == "implementation"){
            result <- x %>% 
              filter(.,impbinary == 1) %>%
              dplyr::summarize(., avg = mean(as.numeric(answer), na.rm = TRUE))#implementation
            
          }
          
          if(config.graphs.df.d$data.measure == "performance"){
            result <- x %>%
              filter(impbinary == 0, !is.na(answer)) %>%
              dplyr::summarize(avg = length(unique(responseid))/length(unique(school.id)))
          }
          
          if(config.graphs.df.d$data.measure == "average performance"){
            result <- 
              x %>%
              filter(grepl("_num",question)) %>%
              dplyr::summarize(., measure.var =  mean(as.numeric(answer), na.rm = TRUE))
          }
          
          return(result)
        }
      
      #Add average variable to final data frame
        graph.avg.df.d <- 
          resp.long.df %>%
          avg.data.restriction.fun(.) %>%
          group_by(!!! syms(group_by.d)) %>%
          summarize.avg.fun(.)
        
#FUN  #Function: Left Join ?with NA?
        left.join.NA <- function(.x, .y, .by, na.replacement) {
          result <- left_join(x = .x, y = .y, by = .by, stringsAsFactors = FALSE) %>% 
            mutate_all(funs(replace(., which(is.na(.)), na.replacement)))
          return(result)
        }
          
        graphdata.df.d <- 
          left.join.NA(
            .x = graphdata.df.d, 
            .y = graph.avg.df.d, 
            .by = c(group_by.d),
            na.replacement = 0
          ) %>%
          replace.names.fun(
            df = .,
            current.names = c(names(.)),
            new.names = c(config.graphs.df.d$data.group.by.var,"measure.var","measure.var.avg")
          )
       
      #storage.ls.index <- length(graphdata.ls.d) + 1
      graphdata.ls.d[[d]] <- graphdata.df.d
      setTxtProgressBar(
        progress.bar.c, 
        100*(d + config.graphs.ls.b[1:(c-1)] %>% sapply(., dim) %>% sapply(`[[`,1) %>% unlist %>% sum)/maxrow.c
      )
      
      #print(c(d))
      #print(config.graphs.df.e[,names(config.graphs.df.e) == config.graphs.df.e$data.restriction] %>% as.character)
      #print(config.graphs.df.e[,names(config.graphs.df.e) == config.graphs.df.e$data.level] %>% as.character)
      #print(graphdata.df.d)
      #print(graphdata.ls.d[[graphdata.ls.index]])
    
    } ### END OF LOOP "d" BY GRAPH ###
    
  graphdata.ls.c[[c]] <- graphdata.ls.d
    
    ###                    ###
#   ### LOOP "d" BY TABLE  ###
    ###                    ###
      
    #Loop Inputs
      config.tables.df.c <- config.tables.ls.b[[c]]
      tabledata.ls.d <- list()
      
    #d <- 5
    #for(d in 1:2){ #LOOP TESTER
    for(d in 1:dim(config.tables.df.c)[1]){
      
      config.tables.df.d <- config.tables.df.c[d,]
      
      #Define all possible category values from table
        #!If table category is 'practice' as in 2018-08 Green Reports, have to make extra restriction to filter down to practices relevant to the specific module
        
        all.cats.varnames.v.d <- c(config.tables.df.d$x.var, config.tables.df.d$y.var)
        
        #!Generalize to graph code as well? So treat like a pivot table with arbitrary number of x.vars and y.vars, a summary var and a summary function.
          #! Maybe would make it so could use a single config table?
          #!Should generalize so that can handle arbitrary number of nested variables on both axes like pivot
        
        #Test Inputs
          varnames <- c(config.tables.df.d$x.var, config.tables.df.d$y.var) %>% remove.na.from.vector()
          tb <- resp.long.df %>% as_tibble()
          
        unique.variable.values.fun <- function(varnames, tb){
          
          varnames <- as.character(varnames)
          tb <- as_tibble(tb)
          all.cats.ls <- list()
          
          #LOOP 'i' BY VARNAME
            #i<-2 #LOOP TESTER
            for(i in 1:length(varnames)){
              
              varname.i <- varnames[i]
              
              if(is.na(varname.i)){
                all.cats.ls[[i]] <- ""
                next()
              }
              
              if(varname.i == "answer"){
                module.varnames <- 
                  q.unbranched.df %>% 
                  filter(module == config.tables.df.d$module) %>% 
                  select(row.1) %>% 
                  unlist %>% 
                  setdiff(., names(slider.vars.df))
                
                result <- 
                  tb$question %in% module.varnames %>% 
                  tb$answer[.] %>% 
                  unique %>%
                  .[.!=""] %>%
                  remove.na.from.vector() %>%
                  .[.!=""]
              }
              
              if(varname.i == "practice"){
                  result <- 
                    q.unbranched.df %>% 
                    filter(module == config.tables.df.d$module) %>% 
                    select(varname.i) %>% 
                    unique %>% 
                    unlist %>%
                    remove.na.from.vector() %>%
                    .[.!=""]
              }
              
              if(varname.i == "role"){
                result <- 
                  tb %>%
                  select(varname.i) %>%
                  unique %>% 
                  unlist %>%
                  remove.na.from.vector() %>%
                  .[.!=""]
              }
              
              all.cats.ls[[i]] <- result %>% as.data.frame %>% replace.names.fun(df = ., current.names = ".", new.names = "all.cats")
                
            } # END OF LOOP 'i' BY VARNAME
          names(all.cats.ls) <- c("x","y")
          return(all.cats.ls)
        }
        
        all.cats.ls.d <- 
          unique.variable.values.fun(
            varnames = c(config.tables.df.d$x.var, config.tables.df.d$y.var), 
            tb = resp.long.df %>% as_tibble()
          )
      
      #!NEED TO GENEARALIZE: IF REPORT.UNIT IS DISTRICT AND table DATA.LEVEL IS DISTRICT, THIS WORKS, BUT NOT IF REPORT.UNIT IS 
      #BUILDING.ID AND DATA.LEVEL IS DISTRICT.
      
      table.data.filter.fun <- function(x){
        
        if(is.na(config.tables.df.d$module)){
          y <- x
        }else{
          y <- x %>% filter(module == config.tables.df.d$module) %>% filter()
        }
        
        y <- y %>% filter(table.q == 1) 
        
        if(is.na(config.tables.df.d$filter)){ #
          result <- y
        }else{
          
          if(!config.tables.df.d$filter %in% c("building.id","district")){
            stop("Configuration 'filter' is neither 'building.id' nor 'district.' Check input.")
          }
          
          if(config.tables.df.d$filter == "building.id"){
            result <- y %>% filter(building.id == report.id.c)
          }
          
          if(config.tables.df.d$filter == "district"){
            result <- y %>% filter(district == district.c)
          }
          
        }
        return(result)
      }
      
  #FUN  #Function: Data Summarize - participation vs. implementation vs. performance 
        #Test inputs
          config.input <- config.tables.df.d
          data.input <-  resp.long.df.c %>% table.data.filter.fun %>% group_by(!!! syms(config.tables.df.d$summary.var))
      
      summarize.data.fun <- function(config.input, data.input){
        #na.replace <- function(x, na.replacement){x[is.na(x)] <- na.replacement} #!This didn't work, but may not need after generalizing.
          
        result.1 <- melt(data.input, id.vars = names(data.input)) 
        
        if(d == 1){ #!Needs to be generalized - right now just uses number of loop but should be based on configs
          result <-
            reshape2::dcast(
              data = result.1,
              formula = role ~ building.id,
              value.var = "responseid",
              fun.aggregate = function(x){length(unique(x))}
            ) %>%
            right_join(
              ., 
              all.cats.ls.d$y, 
              by = c("role" = "all.cats")
            ) %>%
            filter(role != "District Administrator") %>%
            .[c(2,3,5,6,7,4,8,1),] %>%
            #.[c(1,2,3,4,6,7,8,5),] %>%
            replace.names.fun(
              df = .,
              current.names = report.id.c,
              new.names = "num. responses"
            ) %>%
            replace.names.fun(
              df = .,
              current.names = names(.),
              new.names = FirstLetterCap_MultElements(names(.))
            ) %>%
            rbind(
              .,
              c("Total",sum(select(., "Num. Responses"), na.rm = TRUE))
            )
          result[is.na(result)] <- 0
            
          return(result)
        }else{
        
        #Draft table (have to merge with all.cats to make sure have every column and row represented)
          result.2 <- 
            reshape2::dcast(
              data = result.1, 
              formula = 
                unlist(data.input[names(data.input) == config.tables.df.d$y.varname]) ~ 
                unlist(data.input[names(data.input) == config.tables.df.d$x.varname]),#syms(paste(config.input$x.var,"~",config.input$y.var,sep="")), 
              value.var ="responseid",
              fun.aggregate = length
            ) %>% 
            replace.names.fun(
              df = .,
              current.names = "unlist(data.input[names(data.input) == config.tables.df.d$y.varname])",
              new.names = "all.cats"
            ) 
        
        #Add all.cats to rows (y axis) 
          result.3 <- 
            right_join(
              result.2, 
              all.cats.ls.d$y, 
              by = "all.cats"
            )
        
        ##Add all.cats to columns (x axis)
          missing.cats <- unlist(all.cats.ls.d$x)[!unlist(all.cats.ls.d$x) %in% names(result.3)] %>% as.character
          
          result.4 <- 
            matrix(
              ncol = length(missing.cats),
              nrow = dim(result.3)[1]
            ) %>%
            as_tibble() %>%
            replace.names.fun(
              df = .,
              current.names = names(.),
              new.names = missing.cats
            ) %>%
            cbind(result.3, .) %>%
            df.order.by.var.fun(
              df = .,
              order.by.varname = "all.cats",
              rev = TRUE
            ) %>%
            replace.names.fun(
              df = .,
              current.names = "all.cats",
              new.names = FirstLetterCap_OneElement(config.tables.df.d$y.varname)
            )
            
          return(result.4)
        }
      }
        
      #Form final data frame (no averages)
        tabledata.df.d <-  
          resp.long.df.c %>%
          table.data.filter.fun(.) %>%
          group_by(!!! syms(config.tables.df.d$summary.var)) %>%
          summarize.data.fun(config.input = config.tables.df.d, data.input = .) %>%
          mutate_all(funs(replace(., is.na(.), 0)))
        tabledata.df.d[,1] <- FirstLetterCap_MultElements(tabledata.df.d[,1])

      #storage.ls.index <- length(tabledata.ls.d) + 1
      tabledata.ls.d[[d]] <- tabledata.df.d
      
      #setTxtProgressBar(progress.bar.c, 100*(d + config.tables.ls.b[1:(c-1)] %>% sapply(., dim) %>% .[1,] %>% sum)/maxrow.c)
      
    } ### END OF LOOP "d" BY TABLE ###
    
  names(tabledata.ls.d) <- config.tables.df.c$module %>% remove.na.from.vector() %>% as.character %>% c("role",.)
  tabledata.ls.c[[c]] <- tabledata.ls.d   

} ### END OF LOOP "c" BY DISTRICT     
close(progress.bar.c)  

} #END OF SECTION COLLAPSE BRACKET

#OUTPUTS:
  #tabledata.ls.c
    #[[report.unit]]
      ##data frame where each line represents a table
  #graphdata.ls.c
    #[[report unit]]
      #data frame where each line represents a graph
    
########################################################################################################################################################      
### PRODUCING GRAPHS & TABLES THEMSELVES  ###

{#SECTION COLLAPSE BRACKET
  
  ###                       ###    
# ### LOOP "f" BY DISTRICT  ###
  ###                       ###
  
  graphs.ls.f <- list()
  tables.ls.f <- list()
  progress.bar.f <- txtProgressBar(min = 0, max = 100, style = 3)
  maxrow.f <- graphdata.ls.c %>% lengths %>% sum
  
  
  #f <- 1 #LOOP TESTER
  #for(f in 1:2){ #LOOP TESTER
  for(f in report.startnum:length(report.ids)){
    
    if(f == 1){print("FORMING GRAPHS & TABLES IN GGPLOT...")}
    school.id.f <- report.ids[f]
    config.graphs.df.f <- config.graphs.ls.b[[f]]
    
    ###                       ###    
#   ### LOOP "g" BY GRAPH     ###
    ###                       ###
    
    #Loop output object(s)
      graphs.ls.g <- list()
    
    #g <- 2 #LOOP TESTER
    #for(g in 1:2) #LOOP TESTER
    for(g in 1:length(graphdata.ls.c[[f]]))
      local({ #Necessary to avoid annoying and confusing ggplot lazy evaluation problem (see journal)
        g<-g #same as above
        
        ### GRAPH INPUTS FOR GGPLOT ###
        
        #GRAPH DATA & CONFIGS DATA FRAMES
          graphdata.df.g <- graphdata.ls.c[[f]][[g]] %>% as.data.frame()
          names(graphdata.df.g) <- gsub("graphdata.","",names(graphdata.df.g))
          
          if(names(graphdata.df.g)[!grepl("measure",names(graphdata.df.g))] %>% grepl("module",.)){
            graphdata.df.g[,!grepl("measure",names(graphdata.df.g))] <- graphdata.df.g[,!grepl("measure",names(graphdata.df.g))] %>% toupper()
          }else{
            graphdata.df.g[,!grepl("measure",names(graphdata.df.g))] <- graphdata.df.g[,!grepl("measure",names(graphdata.df.g))] %>% FirstLetterCap_MultElements()
          }
          
          config.graphs.df.g <- config.graphs.df.f[g,] %>% as.data.frame()
          names(config.graphs.df.g) <- gsub("configs.","",names(config.graphs.df.g))
          
          graph.cat.varname <- config.graphs.df.g$data.group.by.var  
        
          if(config.graphs.df.g$data.group.by.var == "answer"){
            graphdata.df.g <- left_join(graphdata.df.g,ans.opt.always.df, by = c("answer" = "ans.num"))#graphdata.df.g[order(graphdata.df.g[,2]),]
            
            if(config.graphs.df.g$module %in% c("LEAD","PD")){
              graphdata.df.g <- graphdata.df.g %>% select(year, ans.text.agreement, measure.var, avg)
              graph.cat.varname <- "ans.text.agreement"
            }else{
              graphdata.df.g <- graphdata.df.g %>% select(year, ans.text.freq, measure.var, avg)
              graph.cat.varname <- "ans.text.freq"
            }
          }else{}
          
          if(is.na(config.graphs.df.g$graph.group.by.var)){
            graph.group.by.varname <- NULL
            graph.group.by.var <- NULL
          }else{
            graph.group.by.varname <- config.graphs.df.g$graph.group.by.var
            graph.group.by.var <- graphdata.df.g[,names(graphdata.df.g) == graph.group.by.varname] 
          }
          
        ### BASE GRAPH FORMATION WITH GGPLOT2 ###
        
          graph.g <- 
            ggplot(
              data = graphdata.df.g 
            ) + 
            
            theme(
              panel.background = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.major.x = element_blank(),
              axis.text.x = element_text(size = 20, color = "#5a6b63"),
              axis.text.y = element_blank(),
              axis.ticks = element_blank(),
              axis.title = element_blank(),
              legend.position = "top",
              legend.title = element_blank(),
              legend.text = element_text(size = 12)
            )
            #windows()
            #graph.g
          
          #Adding Columns (Clustered or Non-Clustered)
            #Fill values
            #!Currently set manually - need to make it so fill happens within aes when have groups, within geom_bar() when setting manually
            #!Would be nice to be able to set fill manually from config file as well.
            
              if(config.graphs.df.g$slide.graph.type == "a"){
                graph.fill.g <- c(rep("#5F3356",4),"#91AC3E")
              }else{
                graph.fill.g <- rep("#91AC3E",nrow(graphdata.df.g))
              }
                
            if(is.null(graph.group.by.varname)){
              graph.g <-
                graph.g +
                
                geom_bar(
                  aes(x = graphdata.df.g[[graph.cat.varname]], 
                      y = measure.var %>% as.numeric
                      #fill = factor(graphdata.df.g[,1])
                  ),
                  fill = graph.fill.g,
                  alpha = 1,
                  position = "dodge", 
                  stat = "identity",
                  show.legend = FALSE
                )
            }else{
              graph.g <-
                graph.g +
                
                geom_bar(
                  aes(x = graphdata.df.g[[graph.cat.varname]], 
                      y = measure.var %>% as.numeric,
                      group = graph.group.by.var, 
                      fill = factor(graph.group.by.var)
                      #alpha = I(0.1)
                  ),
                  alpha = 1,
                  position = "dodge", 
                  stat = "identity"
                )
            }
            #windows()
            #graph.g
                   
        #GRAPH DATA LABELS 
        
#FUN      #Function: Graph Label Heights (defined based on ratio of tallest to shortest columns)
            #Test Inputs
              #df = graphdata.df.g
              #measure.var = "measure.var"
              #height.ratio.threshold = 8.2
              
            create.graph.labels.fun <- function(df, measure.var, height.ratio.threshold){
              
              if(!is.data.frame(as.data.frame(df))){stop("Input cannot be coerced into data frame.")}
              
              df <- as.data.frame(df)
              
              var <- df[,names(df) == measure.var] %>% as.matrix %>% as.vector(.,mode = "numeric")
              
              #Label Heights
                min <- min(var, na.rm = TRUE)
                max <- max(var, na.rm = TRUE)
                height.ratio.threshold <- height.ratio.threshold
                height.ratio <- ifelse(max == 0, 0, max/min)
                
                #print(paste("Max: ",max,"  Min: ",min,"  Ratio: ", height.ratio, "  Ratio threshold: ",height.ratio.threshold,sep = ""))
                
                if(height.ratio < height.ratio.threshold){ 
                  graph.labels.heights.v <- rep(min/2, length(var)) #if ratio between min and max height below threshold, all labels are minimum height divided by 2
                  above.label.vectorposition <- max/var > height.ratio.threshold
                }
                
                if((min == 0 && max !=0) | height.ratio >= height.ratio.threshold){
                  graph.labels.heights.v <- vector(length = length(var))
                  above.label.vectorposition <- max/var > height.ratio.threshold
                  graph.labels.heights.v[above.label.vectorposition] <-   #labels for columns above threshold, position is height of bar plus 1/10 of max bar height 
                    var[above.label.vectorposition] + max/10
                  graph.labels.heights.v[graph.labels.heights.v == 0] <-    #labels for columns above threshold, position is height of smallest bar divided by 2
                    min(var[!above.label.vectorposition])/2
                }
              
              #Label Text
                if(config.graphs.df.g$data.measure == "implementation"){
                  graph.labels.text.v <- as.character(100*var %>% round(., 2)) %>% paste(.,"%",sep="")
                }else{
                  graph.labels.text.v <- var %>% as.numeric %>% formatC( round( ., 1), format='f', digits=1 ) %>% trimws(., which = "both") 
                }
              
              #Label visibility
                graph.labels.show.v <- ifelse(var != 0, 1, 0)  
              
              #Label color for graph.type.e
                if(config.graphs.df.g$slide.graph.type == "e"){
                  graph.labels.color.v <- rep(c("#000000","#FFFFFF"),length(df[,1])/2) %>% rev
                }else{
                  graph.labels.color.v <- rep(c("#000000","#FFFFFF"),length(df[,1])/2)
                }
                graph.labels.color.v[var==0] <- "#000000"
                graph.labels.color.v[above.label.vectorposition] <- "#000000"
              
              #result <- cbind(graph.labels.heights.v,graph.labels.text.v,graph.labels.color.v,graph.labels.show.v) %>% as.data.frame(., stringsAsFactors = FALSE)
                result <- data.frame(
                  graph.labels.text = graph.labels.text.v,
                  graph.labels.heights = graph.labels.heights.v,
                  graph.labels.show.v = graph.labels.show.v,
                  graph.labels.color = graph.labels.color.v,
                  stringsAsFactors = FALSE
                )
              
              #print(paste("Graph Label Heights: ",paste(graph.labels.heights.v, collapse = ", "),sep=""))
              return(result)
            }
          
          #Grach label data frame
            graph.labels.df <- 
              create.graph.labels.fun(
                df = graphdata.df.g, 
                measure.var = "measure.var", 
                height.ratio.threshold = 8.2
              )
          
          #Add Data labels to graph
            graph.g <- 
              graph.g +
              geom_text( 
                aes(                                                          
                  y = graph.labels.df$graph.labels.heights, 
                  x = graphdata.df.g[[graph.cat.varname]],
                  label = graph.labels.df$graph.labels.text,
                  alpha = 1,#graph.labels.df$graph.labels.show,
                  group = graphdata.df.g[,1]
                  
                ),
                
                #lineheight = 10.0,
                
                color = "#FFFFFF", #graph.labels.df$graph.labels.color,
                size = 4,
                fontface = "bold",
                position = position_dodge(width = 1),
                show.legend = FALSE
                
              )
            #windows()
            #graph.g
          
        #GRAPH AVERAGES
          #! Need to make so can group on arbitrary variable with arbitrary number of groups and sub-groups. Right now can only two groups of 2 (e.g. year in Repeated Measures)
          graphdata.df.g$avg.alpha <- 
            ifelse(
              is.na(config.graphs.df.g$graph.group.by.vars),# != "Baseline" & graphdata.df.g$measure.var.avg != 0,
              1,
              rep(c(0.8,0.0),nrow(graphdata.df.g))
            )
        
        if(config.graphs.df.g$graph.average == "yes"){
          graph.g <- 
            
            graph.g +
            
            geom_errorbar( #error bar shadow
              aes(
                x = graphdata.df.g[[graph.cat.varname]],
                #group = graphdata.df.g[[graph.cat.varname]], #!removed group for Green Reports because didn't need it, but will have ot add back in and generalize
                ymin = graphdata.df.g$measure.var.avg-max(graphdata.df.g$measure.var.avg)/450, 
                ymax = graphdata.df.g$measure.var.avg-max(graphdata.df.g$measure.var.avg)/450,
                alpha = graphdata.df.g$avg.alpha
              ), 
              position = position_dodge(width = 1), # 1 is dead center, < 1 moves towards other series, >1 away from it
              color = "black", 
              width = 1,
              size = 2,
              show.legend = FALSE
            ) #+
            
            #geom_errorbar(
            #  aes(
            #    x = graphdata.df.g[[graph.cat.varname]],
            #    #group = graphdata.df.g[[graph.cat.varname]],
            #    ymin = graphdata.df.g$measure.var.avg, 
            #    ymax = graphdata.df.g$measure.var.avg,
            #    alpha = graphdata.df.g$avg.alpha
            #  ), 
            #  position = position_dodge(width = 1), # 1 is dead center, < 1 moves towards other series, >1 away from it
            #  color = "yellow", 
            #  width = 1,
            #  size = 2,
            #  alpha = 1,
            #  show.legend = FALSE
            #)
          
        }else{}
        
        #GRAPH CATEGORY NAMES, CORRECTING CATEGORY AXIS ORDERING
        #!potential function 
        #! if can make first character of all categories into numeric vector, then just order by that
        
          #year, school.level, module, answer
            graph.cat.order.ls <-
              list(
                year = c("Baseline","2017-18"),
                school.level = c("Elem.","Middle","High","Mult.","Other"),
                role = c("Special Educator","Classroom Teacher","Instructional Coach","School Counselor","School Social Worker","Building Administrator","Other"),
                module = c("ETLP", "CFA","DBDM","LEAD","PD"),
                ans.text.freq = c("Always","Most of the time","About half the time","Sometimes","Never"),
                ans.text.agreement = c("Strongly Agree","Agree","Neutral","Disagree","Strongly Disagree"),
                practice = if("practice" %in% names(graphdata.df.g)){graphdata.df.g$practice}else{""} #!When moving this out of loop, will need to generalize for all module practices
              )
          
          #When graphs are bar as opposed to columns, have to reverse order because the coord_flip() command does a mirror image
            if(config.graphs.df.g$graph.type.orientation == "bar"){
              graph.order.g <- graph.cat.order.ls[graph.cat.varname] %>% unlist %>% factor(., levels = graph.cat.order.ls[graph.cat.varname] %>% unlist %>% rev)
            }else{
              graph.order.g <- graph.cat.order.ls[graph.cat.varname]  %>% unlist %>% factor(., levels = graph.cat.order.ls[graph.cat.varname] %>% unlist)        
            }
          
          #Graph category axis ordering
            graph.g <- 
              graph.g + 
              scale_x_discrete(limits=levels(graph.order.g))
          
        #GRAPH ORIENTATION
          if(config.graphs.df.g$graph.type.orientation == "bar"){
            graph.g <- 
              graph.g +
              coord_flip() +
              theme(
                axis.text.x = element_blank(),
                axis.text.y = element_text(
                  size = 20, 
                  family = "Century Gothic",
                  color = "#5a6b63",
                  hjust = 1)
              )
          }

        graphs.ls.g[[g]] <<- graph.g
        setTxtProgressBar(progress.bar.f, 100*(g + graphdata.ls.c[1:(f-1)] %>% lengths %>% sum)/maxrow.f)
        
      })  ### END OF LOOP "g" BY GRAPH ###
    #close(progress.bar.g)
    
    graphs.ls.f[[f]] <- graphs.ls.g
    
    ###                       ###    
#   ### LOOP "g" BY TABLE     ###
    ###                       ###
    
    #Loop output object(s)
    tables.ls.g <- list()
    
    #g <- 1 #LOOP TESTER
    #for(g in 1:2) #LOOP TESTER
    for(g in 1:length(tabledata.ls.c[[f]])){
      
      ft.g <- FlexTable(
        data = tabledata.ls.c[[f]][[g]],
        header.columns = TRUE,
        add.rownames = FALSE,
        
        header.cell.props = cellProperties(background.color = "#5F3356", border.style = "none"), #!Should put into configs instead of specifying in code
        header.text.props = textProperties(
          color = "white", 
          font.size = 15,
          font.family = "Century Gothic",
          font.weight = "bold"),
        header.par.props = parProperties(text.align = "center"),
        body.cell.props = cellProperties(background.color = "white", border.style = "none"),
        body.text.props = textProperties(
          color = "#515151",
          font.size = 15,
          font.family = "Century Gothic"
        )
      )
      
      if(g == 1){
        ft.g[dim(tabledata.ls.c[[f]][[g]])[1],] <- 
          chprop(
            textProperties(
              font.weight = "bold",
              font.size = 18,
              font.family = "Century Gothic"
            )
          ) #Bold text on last line (totals)
        ft.g[,1] <- chprop(parProperties(text.align = "center"))
        #ft.g <- setFlexTableWidths(ft.g, widths = c(4, rep(6,dim(tabledata.ls.c[[f]][[g]])[2]-1)))      
        
      }
      
      if(g != 1){
        ft.g[,1] <- chprop(parProperties(text.align = "right"))
      }
      
      #ft.g[1,1] <-  chprop(parProperties(text.align = "left")) 
      ft.g[1:dim(tabledata.ls.c[[f]][[g]])[1],2:dim(tabledata.ls.c[[f]][[g]])[2]] <- #Center align numbers in all but first column
        chprop(parProperties(text.align = "center")) 
      ft.g <- setZebraStyle(ft.g, odd = "#D0ABD6", even = "white" ) 
      
      tables.ls.g[[g]] <- ft.g
      
    } ### END OF LOOP "g" BY TABLE ###
    
    names(tables.ls.g) <- c("role","cfa","dbdm","etlp","lead","pd")
    tables.ls.f[[f]] <- tables.ls.g
    
  } ### END OF LOOP "f" BY REPORT.UNIT
  close(progress.bar.f)
  
}#END SECTION COLLAPSE BRACKET
  
#OUTPUT:
  #graphs.ls.f
    #[[report.unit]]
      #ggplot object
  #tables.ls.f
    #[[report.unit]]
      #FlexTable object


########################################################################################################################################################      
### POWERPOINT GLOBAL CONFIGURATIONS ###
    
{ #SECTION COLLAPSE BRACKET
    
    #Useful colors
    titlegreen <- rgb(118,153,48, maxColorValue=255)
    notesgrey <- rgb(131,130,105, maxColorValue=255)
    graphlabelsgrey <- "#5a6b63"
    graphgridlinesgrey <- "#e6e6e6"
    purpleshade <- "#d0abd6"
    purpleheader <- "#3d2242"
    purplegraphshade <- "#402339"
    backgroundgreen <- "#94c132"
    subtextgreen <- "#929e78"
    
    bar_series_fill.cols <- c("#800080","#ff33ff")
    
    #notesgray <- rgb(131,130,105, maxColorValue=255)
    
    #Text formatting
    title.format <- textProperties(color = titlegreen, font.size = 48, font.weight = "bold", font.family = "Century Gothic")
    title.format.small <- textProperties(color = titlegreen, font.size = 40, font.weight = "bold", font.family = "Century Gothic")
    subtitle.format <- textProperties(color = notesgrey, font.size = 28, font.weight = "bold", font.family = "Century Gothic")
    section.title.format <- textProperties(color = "white", font.size = 48, font.weight = "bold", font.family = "Century Gothic")
    notes.format <- textProperties(color = notesgrey, font.size = 14, font.family = "Century Gothic")
    
    
} # END OF SECTION COLLAPSE BRACKET
    
########################################################################################################################################################      
### POWERPOINT SLIDE CREATION  ###        

{ #SECTION COLLAPSE BRACKET   
  #rm(resp.long.df, resp.wide.df)
  jgc <- function(){
    gc()
    .jcall("java/lang/System", method = "gc")
  }    
  
  setwd(source.dir)  
  config.pot.df <- read.xlsx("graph_configs.xlsx", sheetName = "slide.pot.objects",header = TRUE, stringsAsFactors = FALSE)
  
    ###                          ###    
#   ### LOOP "h" BY REPORT UNIT  ###
    ###                          ###
    
    #Progress Bar
      progress.bar.h <- txtProgressBar(min = 0, max = 100, style = 3)
      maxrow.h <- sapply(config.slides.ls.b, dim) %>% sapply(`[[`,1) %>% unlist %>% sum
      printed.reports.ls <- list()
    
    #h <- 69 #LOOP TESTER
    for(h in ceiling(runif(5,1,length(config.slides.ls.b)))){
    #for(h in report.startnum:length(config.slides.ls.b)){ #LOOP TESTER
    #for(h in 1:length(config.slides.ls.b)){
      
      jgc()
       
      #Reading 'Cadre' so it can be added to file name
        buildings.tb <- 	
          gs_read(configs.ss, ws = "buildings", range = NULL, literal = TRUE) %>% 
          as.list() %>% 
          lapply(., tolower) %>%
          do.call(cbind, .) %>%
          as_tibble()
        
        cadre.h <- 
          buildings.tb %>% 
          filter(building.id == report.ids[h]) %>% 
          select(cadre) %>% 
          unlist %>% 
          FirstLetterCap_OneElement()
        
      #Set up target file
        template.file <- paste(source.dir,
                             "template_green reports.pptx",
                             sep = "")
        target.path.h <- paste(target.dir,
                                  "/",
                                  cadre.h,
                                  "_",
                                  report.ids[h],
                                  "_",
                                  gsub(":",".",Sys.time()) %>% substr(., 1,10),
                                  "_",
                                  gsub(":",".",Sys.time()) %>% substr(., 15,19),
                                  ".pptx", sep="") 
      
        file.copy(template.file, target.path.h)
      
      #Set up powerpoint object 
        ppt.h <- pptx(template = target.path.h )
        options("ReporteRs-fontsize" = 20)
        options("ReporteRs-default-font" = "Century Gothic")
      
      #Set up report-level inputs
        config.graphs.df.h <- 
          config.graphs.ls.b[[h]] %>%
          mutate(row.i = config.graphs.ls.b[[h]] %>% .[,ncol(config.graphs.ls.b[[h]])] %>% seq_along(.))
        
        config.tables.df.h <- config.tables.ls.b[[h]]
        
        #!Will need to generalize below for different report units (i.e. Repeated Measures vs. Green Reports)
        report.id.h <- report.ids[h]
        district.h <- strsplit(report.id.h, "_") %>% unlist %>% .[1] %>% toupper()
        school.h <- strsplit(report.id.h, "_") %>% unlist %>% .[2] %>% toupper()
        config.slides.df.h <- config.slides.ls.b[[h]]
        
        graphs.ls.h <- graphs.ls.f[[h]]
        tables.ls.h <- tables.ls.f[[h]]
     
      ###                     ###    
#     ### LOOP "i" BY SLIDE   ###
      ###                     ###

        i <- 6 #LOOP TESTER
        #for(i in 1:4){ #LOOP TESTER
        #for(i in 1:dim(config.slides.ls.b[[h]])[1]){
         
          config.slide.df.i <- config.slides.ls.b[[h]] %>% .[i,]
          slide.type.id.i <- config.slide.df.i$slide.type.id
          layout.i <- config.slide.df.i$slide.layout
        
        #SLIDE FORMATION
          
          ppt.h <- addSlide(ppt.h, slide.layout = layout.i)
          ppt.h <- addPageNumber( ppt.h )
          
        #ADD GRAPHS
          
          #Graph Loop Inputs
            config.graphs.df.i <- config.graphs.df.h %>% 
              filter(slide.type.id == slide.type.id.i)
            
          if(dim(config.graphs.df.i)[1] !=0 && !is.na(config.graphs.df.i$slide.graph.type)){
            #!Removed for expediencey but should be generalized.
            #if(is.na(config.slide.df.i$school)){
            #  config.graphs.df.i <- config.graphs.df.i[is.na(config.graphs.df.i$school),]
            #}else{
            #  config.graphs.df.i <- config.graphs.df.i[config.graphs.df.i$school == config.slide.df.i$school,]
            #}
            
            if(is.na(config.slide.df.i$module)){
              config.graphs.df.i <- config.graphs.df.i[is.na(config.graphs.df.i$module),]
            }else{
              config.graphs.df.i <- config.graphs.df.i[config.graphs.df.i$module == config.slide.df.i$module,]
            }
         
            
            ###                   ###    
#           ### LOOP "k" BY GRAPH ###
            ###                   ###
            
            #k <- 1 #LOOP TESTER
            #for(k in 1:2){ #LOOP TESTER
            for(k in 1:dim(config.graphs.df.i)[1]){
              if(dim(config.graphs.df.i)[1] < 1){
                #print(paste("No graph objects for slide.id: ",config.slide.df.i$slide.type.id,sep = ""))
                next()
              }
              
              graph.k <- graphs.ls.h[config.graphs.df.i$row.i[k]]
              ppt.h <- 
                addPlot(
                  ppt.h,
                  fun = print,
                  x = graph.k,
                  height = config.graphs.df.i$height[k],
                  width = config.graphs.df.i$width[k],
                  offx = config.graphs.df.i$offx[k],
                  offy = config.graphs.df.i$offy[k]
                )
            
            } # END OF LOOP "k" BY GRAPH
          }
            
          #ADD TABLES
            
            #! Will want to generalize so can add more than one table to each slide if necessary
            config.tables.df.i <- config.tables.df.h %>% 
              filter(slide.type.id == slide.type.id.i)
            
            if(dim(config.tables.df.i)[1] != 0 && !is.na(config.tables.df.i$slide.table.type)){
              
              if(is.na(config.slide.df.i$module)){
                config.tables.df.i <- config.tables.df.i[is.na(config.tables.df.i$module),]
              }else{
                config.tables.df.i <- config.tables.df.i[config.tables.df.i$module == config.slide.df.i$module,]
              }
              
              if(i == 2){
                ft.i <- tables.ls.f[[h]][[1]]
              }else{
                ft.i <- tables.ls.f[[h]][[which(names(tables.ls.f[[h]])==config.tables.df.i$module)]]
              }

              ppt.h <- addFlexTable(ppt.h, 
                                    ft.i, 
                                    height = config.tables.df.i$height,
                                    width = config.tables.df.i$width,
                                    offx = config.tables.df.i$offx,
                                    offy = config.tables.df.i$offy
                                     #par.properties=parProperties(text.align="center", padding=0)
              )
            }
            
          #ADD POT OBJECTS
  
          ###                         ###    
#         ### LOOP "j" BY POT OBJECT  ###
          ###                         ###
            
          config.pot.i <- config.pot.df[config.pot.df$slide.type.id == slide.type.id.i,]
          
          if(any(!is.na(config.pot.i$module))){
            config.pot.i <- filter(config.pot.i, grepl(as.character(config.slide.df.i$module), config.pot.i$module))
          }  
          
          j <- 1 #LOOP TESTER
          #for(j in 1:2){ #LOOP TESTER
          #for(j in 1:dim(config.pot.i)[1]){
            if(dim(config.pot.i)[1] < 1){
              #print(paste("No text objects for slide.id: ",config.slide.df.i$slide.id,sep = ""))
              next()
            }
            
            #print(c(i,j))
            
            pot.content.j <- 
              paste(
                ifelse(
                  !is.na(config.pot.i$content.static[j]),
                  config.pot.i$content.static[j],
                  ""
                ),
                ifelse(
                  !is.na(config.pot.i$content.dynamic[j]),
                  eval(parse(text=config.pot.i$content.dynamic[j])),
                  ""
                ),
                sep = ""
              )
            
            pot.j <- 
              pot(
                pot.content.j,
                textProperties(
                  color = 
                    alpha(
                      ifelse(!is.na(config.pot.i$color[j]),
                        config.pot.i$color[j] %>% 
                        strsplit(.,",") %>% unlist %>% as.numeric %>% 
                        rgb(red = .[1],green = .[2],blue = .[3] ,maxColorValue = 255) %>% .[1],
                        "black"
                      )
                    ,1),
                  font.size = config.pot.i$font.size[j], 
                  font.weight = ifelse(is.na(config.pot.i$font.weight[j]),'normal',config.pot.i$font.weight[j]),
                  font.family = config.pot.i$font[j]
                )
              )
              
            ppt.h <- 
              addParagraph(
                ppt.h,
                pot.j,
                height = config.pot.i$height[j],
                width = config.pot.i$width[j],
                offx = config.pot.i$offx[j],
                offy = config.pot.i$offy[j],
                par.properties = parProperties(
                  text.align=config.pot.i$text.align[j], 
                  padding=0
                )
              )
            
          } #END OF LOOP "j" BY POT OBJECT (ROW OF POT CONFIG TABLE)
          
          writeDoc(ppt.h, file = target.path.h) #test Slide 1 build
          #rm(ppt.h)
        setTxtProgressBar(progress.bar.h, 100*h/length(report.ids))
          
        } #END OF LOOP "i" BY SLIDE
      print(h)
      printed.reports.ls[[h]] <- report.ids[h]
    } # END OF LOOP "h" BY REPORT.UNIT      
    close(progress.bar.h)      
          
          
} #END SECTION COLLAPSE BRACKET          

end_time <- Sys.time()
code_runtime <- end_time - start_time
print(code_runtime)

windows()        
    
 
      
      
      