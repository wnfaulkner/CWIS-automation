#########################################################
##### 	CWIS Automation for MMD                   	#####
#########################################################


########################################################################################################################################################      
### INITIAL SETUP ###
 
{ #SECTION COLLAPSE BRACKET
  
  rm(list=ls()) #Remove lists
  options(java.parameters = "- Xmx1024m") #helps r not to fail when importing large xlsx files with xlsx package
  
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
      wd <- "C:/Users/WNF/Google Drive/1. FLUX CONTRACTS - CURRENT/2016-09 EXT Missouri Education/3. Missouri Education - GDRIVE/8. CWIS/2018-08 Green Reports/"
    
    #Thinkpad T470
      #rproj.dir <- "C:/Users/WNF/Documents/Git Projects/CWIS-automation"  
      #wd <- "G:/My Drive/1. FLUX CONTRACTS - CURRENT/2016-09 Missouri Education/3. Missouri Education - GDRIVE/8. CWIS/2018-05 Repeated Measures/"

    
    #Function Directories
      setwd(rproj.dir)
      source("FUN_FirstletterCap.r")
      source("FUN_ColClassConvert.r")
      
    #Data & Output Directories
      setwd(wd)
      source.dir <- paste(wd,"/data_source/", sep = "")
      target.dir <- paste(wd,"r_script_outputs/",
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
    
########################################################################################################################################################      
### LOAD DATA ###

{ #SECTION COLLAPSE BRACKET

  setwd(source.dir)

  #Read data files

#FUN #Select right 'n' characters of string
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
      questions.ss <- gs_key("1WCS1IZkMpZDztHeEyTyzD_cMMs-aIfknxYtDKg0T-Rg",verbose = TRUE) 
      questions.ls <- 	gs_read(questions.ss, ws = 1, range = NULL, literal = TRUE) %>% as.list() %>% lapply(., tolower)
      questions.df <- do.call(cbind, questions.ls) %>% as.data.frame(., stringsAsFactors = FALSE)
    
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
#FUN  #Function 'multiple gsub' to find/replace multiple patterns in a vector
      mgsub <- function(pattern, replacement, x, ...) {
        n = length(pattern)
        print(cbind(pattern,replacement))
        if (n != length(replacement)) {
          stop("pattern and replacement do not have the same length.")
        }
        result = x
        for (i in 1:n) {
          result[grep(pattern[i], x, ...)] = replacement[i]
        }
        return(result)
      }
      
      names(resp1.df) <-
        mgsub(
          questions.sem.df$row.1[!is.na(questions.sem.df$q.changename)], 
          questions.sem.df$q.changename[!is.na(questions.sem.df$q.changename)], 
          names(resp1.df)
        )
      
    
    #Add "x" to questions.sem.df$row.1 so they match exactly with Qualtrics export as imported by R
      
#FUN #Function to output number of times specified substring occurs within vector of character strings
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
      questions.unbranched.df <- 
        questions.sem.df[
          questions.sem.df$row.1 %in% 
          c(
            branch0.ans0.colnames.v,
            branch0.ans1.colnames.v,
            varname.match.ls %>% unlist %>% paste("1_",.,sep="")
          )
          ,
        ] #!looks like still uneven numbers - some columns must be missing from questions table, but seems to be columns we don't care about.
      questions.unbranched.df$row.1[grep("q",questions.unbranched.df$row.1)] <- 
        str_extract(
          questions.unbranched.df$row.1[grep("q",questions.unbranched.df$row.1)], 
          "q[0-9].+"
        ) 
    
    #Write Unbranched Data to Excel File
      #unbranched.file.name <- 
      #  paste( 
      #    "unbranched_data_",
      #    gsub(":",".",Sys.time()),
      #    ".xlsx", 
      #    sep=""
      #  ) 
      
      #setwd(target.dir)
      
      #write.xlsx(
      #  resp2.df,
      #  file = unbranched.file.name,
      #  sheetName = "responses",
      #  row.names = FALSE,
      #  showNA = FALSE,
      #  append = FALSE
      #  )
      
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
  #resp2.df - now with all branch variables 'unbranched' (stacked), and having removed two extra header rows
  #questions.sem.df - now only with rows pertaining to this year and semester
    
########################################################################################################################################################      
### FURTHER CLEANING & ADDING USEFUL VARIABLES ###

{ #SECTION COLLAPSE BRACKET
  
    #Lower-Case All Data
      resp3.df <- apply(resp2.df,c(1:2),tolower) %>% as.data.frame(., stringsAsFactors = FALSE)
    
    #Recode role variable
      resp3.df$role <- mgsub("Teacher", "Classroom Teacher", resp3.df$role)
      
    #Recode school names
      school.name.patterns <- c("elem\\.","sch\\.","co\\.","jr\\.","sr\\.","meramec valley early childhood")
      school.name.replacements <- c("elementary","school","county","junior","senior","early childhood center")
      resp3.df$building <- mgsub(school.name.patterns,school.name.replacements,resp3.df$building)
      
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
      cwis.vars.v <- which(names(resp3.df) %in% questions.unbranched.df$row.1[!is.na(questions.unbranched.df$module)])
      cwis.varnames.v <- names(resp3.df)[names(resp3.df) %in% questions.unbranched.df$row.1[!is.na(questions.unbranched.df$module)]]
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
        sapply(., function(x) {x %in% c("always","most of the time","about half the time","sometimes","never","strongly agree","agree","neutral","disagree","strongly disagree")}) %>%
        sapply(., any)) %>%
        names(resp3.df)[.]

      num.ansopt.vars.df <- 
        apply(resp3.df[,names(resp3.df) %in% recode.ansopt.varnames.v], 
          2,
          numeric.recode.fun
        ) %>% as.data.frame
      
      names(num.ansopt.vars.df) <- paste(names(resp3.df[,names(resp3.df) %in% recode.ansopt.varnames.v]),"_num", sep = "")
    
    #Create 'implementation' binary variables
      binary.ansopt.vars.df <- apply(num.ansopt.vars.df,c(1:2),function(x){ifelse(x >= 3.5,1,0)}) %>% as.data.frame
      names(binary.ansopt.vars.df) <- paste(names(resp3.df[,names(resp3.df) %in% recode.ansopt.varnames.v]),"_binary",sep = "")
      

#FUN  #Function to apply to columns to be converted into binary implementation
      #binary.recode.fun <- function(vector, binary.cutoff){
      #  result <- ifelse(vector >= binary.cutoff, 1, 0)
      #  return(result)
      #}

#FUN  #Function to output variable names in data frame which can be converted to numeric       
      numeric.varnames.v <-
        function(data.frame){
          result <- 
            data.frame %>%
            apply(., 2, unique) %>%
            sapply(., 
              function(x){
                (as.numeric(x) %>% is.na(.) %>% sum) <= 1
              }
            ) %>%
            names(data.frame)[.]
          return(result)
        }
      
      #Convert numeric variables in original data to numeric
        resp3.df[,names(resp3.df) %in% numeric.varnames.v(resp3.df)] <-
          apply(
            resp3.df[,names(resp3.df) %in% numeric.varnames.v(resp3.df)],
            c(1:2),
            as.numeric
          )
      
      #Converting Slider variables to binary (according to different max/min/thresholds)
        
        slider.vars.df <- 
          resp3.df[,
            names(resp3.df) %in% questions.unbranched.df$row.1[!is.na(questions.unbranched.df$var.min)]
          ]
        
        slider.vars.ls <- list()
        
        for(c in 1:ncol(slider.vars.df)){
          
          colname.c <- names(slider.vars.df)[c]
          var.min.c <- questions.unbranched.df$var.min[questions.unbranched.df$row.1 == colname.c][!is.na(questions.unbranched.df$var.min[questions.unbranched.df$row.1 == colname.c])] %>% 
            as.character %>% as.numeric
          var.max.c <- questions.unbranched.df$var.max[questions.unbranched.df$row.1 == colname.c][!is.na(questions.unbranched.df$var.max[questions.unbranched.df$row.1 == colname.c])] %>% 
            as.character %>% as.numeric
          
          if(var.min.c == 1 & var.max.c == 5){slider.threshold.c <- 3.5}
          if(var.min.c == 1 & var.max.c == 10){slider.threshold.c <- 7}
          if(length(var.min.c == 1 & var.max.c == 5) > 1){print(c)}
          slider.vars.ls[[c]] <- ifelse(slider.vars.df[,c] >= slider.threshold.c, 1, 0)
        }
        
        binary.slider.vars.df <- do.call(cbind, slider.vars.ls) %>% as.data.frame
        names(binary.slider.vars.df) <- 
          paste(names(slider.vars.df),"_binary",sep="")
      
    #Create final data frames: 1. Wide; 2. Long for original CWIS data; 3. Long for impbinary data (both long include all original id variables)
      resp.wide.df <- cbind(resp3.df, num.ansopt.vars.df, binary.ansopt.vars.df, binary.slider.vars.df)
      resp.long.df <- 
        melt(
          data = resp.wide.df,
          id.vars = names(resp.wide.df)[!grepl("q[0-9]",names(resp.wide.df))],
          variable.name = "question",
          value.name = "answer",
          stringsAsFactors = FALSE
        )
      resp.long.df$question <- as.character(resp.long.df$question)

    #Creating additional useful variables for long data frames
      
      #Variable for module
        resp.long.df$q.original <- str_extract(resp.long.df$question, "q[0-9]*_[0-9]")
        resp.long.df <- 
          left_join(
            resp.long.df, 
            questions.unbranched.df[,names(questions.unbranched.df) %in% c("row.1","module")], 
            by = c("question" = "row.1")
          )  
     
        resp.long.df$impbinary <- ifelse(grepl("binary",resp.long.df$question),1,0)

    #Loop: state average implementation rates [a], results in imp.state.df
      #imp.state.df <- data.frame(cwis.module = cwis.modules.v)
      #a <- 1 # LOOP TESTER
      #for(a in 1:length(cwis.modules.v)){
      #  imp.state.df$imp.rate[imp.state.df[,1]==cwis.modules.v[a]] <- 
      #    impbinary.df[,grep(cwis.modules.v[a], names(impbinary.df))] %>%
      #    apply(., 2, function(x){mean(x, na.rm = TRUE)}) %>% 
      #    mean() 
      #}
        
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
  
}#END SECTION COLLAPSE BRACKET

#OUTPUTS
  #resp.wide.df: wide data with all variables including numeric and binary
  #resp.long.df: long format data frame with cwis responses
  #ans.opt.always.df: data frame with columns corresponding to answer numbers and answer text,
    #including both frequency scale (e.g. 'always', 'most of the time') and agreement scale (e.g.
    #'strongly agree', 'agree').

########################################################################################################################################################      
### PRODUCING GRAPH & SLIDE CONFIGURATION TABLES ###

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
      report.ids <- report.id.col[order(report.id.col)] %>% unique
    }else{}   #If user has designated district names as "all", code will create reports for all district names present in the data
    
  #Load Graph & Slide Type Config Tables
    setwd(source.dir)
    
    config.slidetypes.df <- read.xlsx("graph_configs.xlsx", sheetName = "slide.types",header = TRUE, stringsAsFactors = FALSE)
    load.config.graphtypes.df <- read.xlsx("graph_configs.xlsx", sheetName = "graph.types",header = TRUE, stringsAsFactors = FALSE)
    load.config.tabletypes.df <- read.xlsx("graph_configs.xlsx", sheetName = "table.types",header = TRUE, stringsAsFactors = FALSE)
    
    config.graphtypes.df <- SplitColReshape.ToLong(config.slidetypes.df, id.var = "slide.type.id",split.varname = "slide.graph.type",split.char = ",") %>%
      left_join(., load.config.graphtypes.df, by = c("slide.graph.type" = "graph.type.id")) %>% 
      filter(!is.na(slide.graph.type))
    
    config.tabletypes.df <- SplitColReshape.ToLong(config.slidetypes.df, id.var = "slide.type.id",split.varname = "slide.table.type",split.char = ",") %>%
      left_join(., load.config.tabletypes.df, by = c("slide.table.type" = "table.type.id")) %>% 
      filter(!is.na(slide.table.type))
    
    #config.graphtypes.df <- config.graphtypes.df[,grep("slide.type.id|loop|data|graph|height|width|offx|offy",names(config.graphtypes.df))]
  
  #Expand Graph & Slide Config Tables for each district according to looping variables
    
    ###                       ###    
#   ### LOOP "b" BY DISTRICT  ###
    ###                       ###
    
  #Loop Outputs 
    config.graphs.ls.b <- list()
    config.tables.ls.b <- list()
    config.slides.ls.b <- list()
  
  #Progress bar for loop
    progress.bar.b <- txtProgressBar(min = 0, max = 100, style = 3)
    maxrow.b <- length(report.ids)
    
  #b <- 1 #LOOP TESTER (19 = "Raytown C-2")
  #for(b in c(1,2)){   #LOOP TESTER
  for(b in 1:length(report.ids)){   #START OF LOOP BY DISTRICT
    #print(b)
    loop.start.time.b <- Sys.time()
    if(b == 1){print("FORMING GRAPH & SLIDE CONFIG TABLES...")}
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
      
#FUN#Loop Expander Function (for creating full config tables) 
      #Function input testers
        configs = config.slidetypes.df
        loop.varname = "slide.loop.var"
        collate.varname = "slide.type.position"
        source.data = resp.long.df.b  
      
      loop.expander.fun <- function(configs, loop.varname, collate.varname, source.data){
        output.ls <- list()
        
        #c = 2 #LOOP TESTER: NO LOOPS
        #c = 3 #LOOP TESTER: ONE LOOP VAR
        #c = 8 #LOOP TESTER: TWO LOOP VARS
        #for(c in 1:3){
        for(c in 1:dim(configs)[1]){
        
          c.list.c <- list(c=c)
          
          #Make data frame with configurations repeated out across all unique combinations of loop.varname(s) in source.data      
            slide.loop.vars.c <- configs[c,names(configs)==loop.varname] %>% strsplit(., ",") %>% unlist %>% trimws(., which = "both")
            
            if(any(is.na(slide.loop.vars.c))){
              configs.c <- configs[c,]
            }
            
            if(any(!is.na(slide.loop.vars.c))){
      
#FUN        #Function to remove "NA" from a vector
              remove.na.from.vector <- function(x){
                if(!is.null(dim(x))){stop("Input must be a vector.")}
                  result <- x[!is.na(x)]
                  return(result)
              }
              
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

#FUN  #Replace NAs in a vector with a replacement value
    na.sub <- function(vector,na.replacement){
      vector[is.na(vector)] <- na.replacement
      return(vector)
    }       

#FUN  #School-level slides should not include an iteration for the District Office 
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
    
      config.graphs.ls.b[[length(config.graphs.ls.b)+1]] <- remove.district.office.fun(config.graphs.df)
    
    #Tables config table for this report unit
      config.tables.df <-
        loop.expander.fun(
          configs = config.tabletypes.df, 
          loop.varname = "slide.loop.var", 
          source.data = resp.long.df.b
        )
      
      config.tables.ls.b[[length(config.tables.ls.b)+1]] <- remove.district.office.fun(config.tables.df)
    
    #Slide config table for this report unit
      config.slides.df <- 
        loop.expander.fun(
          configs = config.slidetypes.df, 
          loop.varname = "slide.loop.var",
          collate.varname = "slide.type.position",
          source.data = resp.long.df.b
        )
      
      config.slides.ls.b[[length(config.slides.ls.b)+1]] <- remove.district.office.fun(config.slides.df)
      
    
    setTxtProgressBar(progress.bar.b, 100*b/maxrow.b)

  } # END OF LOOP 'b' BY DISTRICT
  close(progress.bar.b)
  
} # END SECTION COLLAPSE BRACKET
    
#OUTPUTS:
  #report.ids: vector with all district names in resp.long.df (length = 19 for baseline data)
  #config.graphs.ls.b
    #[[district]]
      #data frame where each line represents a graph
  #config.slides.ls.b
    #[[district]]
      #data frame where each line represents a slide

########################################################################################################################################################      
### PRODUCING GRAPH DATA ###

#{# SECTION COLLAPSE BRACKET
     
###                       ###    
### LOOP "c" BY DISTRICT  ###
###                       ###
      
  # Loop outputs
    graphdata.ls.c <- list()
    #graphs.ls.c <- list()
  
  # Progress bar for loop
    progress.bar.c <- txtProgressBar(min = 0, max = 100, style = 3)
    maxrow.c <- config.graphs.ls.b %>% sapply(., dim) %>% .[1,] %>% sum
  
  c <- 1 #LOOP TESTER (19 = "Raytown C-2")
  #for(c in c(1,2)){   #LOOP TESTER
  #for(c in 1:length(report.ids)){   #START OF LOOP BY DISTRICT
    
    if(c == 1){print("Forming input data tables for graphs...")}
    
    report.id.c <- report.ids[c]                               
    
    resp.long.df.c <- 
      resp.long.df %>% 
      select(names(resp.long.df)[names(resp.long.df) == report.id.colname]) %>% 
      equals(report.id.b) %>% 
      resp.long.df[.,]
    
    config.graphs.df.c <- config.graphs.ls.b[[c]]
    
    graphdata.ls.d <- list()
    
    ###                    ###
#   ### LOOP "d" BY GRAPH  ###
    ###                    ###
    
    d <- 1
    #for(d in 1:2){ #LOOP TESTER
    #for(d in 1:dim(config.graphs.df.c)[1]){
      
      config.graphs.df.d <- config.graphs.df.c[d,]
      
        group_by.d <- config.graphs.df.d$data.group.by.var %>% 
          strsplit(., ",") %>% 
          unlist
      
      #Create data frame "all.cats.df.e" of all possible answers for x-axis (role, module, year, answer)
        
        all.cats.d <- resp.long.df %>%
          filter( resp.long.df$impbinary == 0 ) %>%
          .[,names(resp.long.df) == config.graphs.df.d$data.group.by.var] %>% 
          as.data.frame(., stringsAsFactors = FALSE) %>% 
          apply(., 2, function(x){x[!is.na(x)] %>% unique}) %>%
          as.data.frame(., stringsAsFactors = FALSE) %>%
          .[,1]
        
        if(config.graphs.df.d$graph.cat.varname != "year"){
          all.cats.df.d <- expand.grid(unique(dat.answer.long.df$year), all.cats.d) %>% as.data.frame(., stringsAsFactors = FALSE)
        }else{
          all.cats.df.d <- all.cats.d %>% as.data.frame(., stringsAsFactors = FALSE)
        }
        
        names(all.cats.df.d) <- group_by.d
        
#FUN  #Data restriction function: district vs. school
        graph.data.restriction.fun <- function(x){
          
          if(config.graphs.df.d$data.level == "district"){
            y <- x
          }
          
          if(config.graphs.df.d$data.level == "school"){
            y <- 
              x %>% 
              filter(school == config.graphs.df.d[,names(config.graphs.df.d) == config.graphs.df.d$data.level]) 
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
        
#FUN  #Data Summarize Function: participation vs. implementation vs. performance 
        summarize.data.fun <- function(x){
          if(config.graphs.df.d$data.measure == "participation"){
            result <- 
              dplyr::summarize(x, measure.var =  length(unique(responseid)))
          }
          if(config.graphs.df.d$data.measure == "implementation"){
            result <- x %>% 
              filter(impbinary == 1) %>%
              dplyr::summarize(measure.var = mean(answer, na.rm = TRUE)) %>%
              as.data.frame(., stringsAsFactors = FALSE)
          }
          if(config.graphs.df.d$data.measure == "performance"){
            result <- x %>%
              filter(impbinary == 0, !is.na(answer)) %>%
              dplyr::summarize(measure.var = as.character(length(unique(responseid))))
          }
          return(result)
        }
      
      #Form final data frame (no averages)
        graphdata.df.d <-  
          resp.long.df.c %>%
          graph.data.restriction.fun %>%
          group_by(!!! syms(group_by.d)) %>%
          summarize.data.fun %>%
          left_join(all.cats.df.d, ., by = c(group_by.d))
        graphdata.df.d$measure.var[is.na(graphdata.df.d$measure.var)] <- 0
        
  #FUN      #Average data restriction function
        avg.data.restriction.fun <- function(x){
          
          if(config.graphs.df.d$data.level == "district"){
            y <- x
          }
          
          if(config.graphs.df.d$data.level == "school"){
            y <- 
              x %>% 
              filter(district == report.id.c) 
          }
          
          if(!config.graphs.df.d$data.restriction=="module" | is.na(config.graphs.df.d$data.restriction)){ #!Should look into a better way to deal with this restriction
            result <- y
          }
          
          if(config.graphs.df.d$data.restriction=="module" & !is.na(config.graphs.df.d$data.restriction)){
            result <- 
              y %>%
              filter(
                y[,names(y)==config.graphs.df.d$data.restriction] == 
                  config.graphs.df.d[,names(config.graphs.df.d)==config.graphs.df.d$data.restriction]
              )
          }
          
          
          return(result)
        }
      
  #FUN      #Average Summary Function
        summarize.avg.fun <- function(x){
          if(config.graphs.df.d$data.measure == "participation"){
            result <- x %>%
              dplyr::summarize(avg = length(unique(responseid))/length(unique(school.id)))#participation
          }
          if(config.graphs.df.d$data.measure == "implementation"){
            result <- x %>% 
              filter(.,impbinary == 1) %>%
              dplyr::summarize(., avg = mean(answer, na.rm = TRUE))#implementation
            
          }
          if(config.graphs.df.d$data.measure == "performance"){
            result <- x %>%
              filter(impbinary == 0, !is.na(answer)) %>%
              dplyr::summarize(avg = length(unique(responseid))/length(unique(school.id)))
          }
          return(result)
        }
      
      #Add average variable to final data frame
        graph.avg.df.d <- 
          resp.long.df %>%
          avg.data.restriction.fun(.) %>%
          group_by(!!! syms(group_by.d)) %>%
          summarize.avg.fun(.)
  #FUN
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
          )
       
      storage.ls.index <- length(graphdata.ls.d) + 1
      graphdata.ls.d[[storage.ls.index]] <- graphdata.df.d
      setTxtProgressBar(progress.bar.c, 100*(d + config.graphs.ls.b[1:(c-1)] %>% sapply(., dim) %>% .[1,] %>% sum)/maxrow.c)
      
      #print(c(d))
      #print(config.graphs.df.e[,names(config.graphs.df.e) == config.graphs.df.e$data.restriction] %>% as.character)
      #print(config.graphs.df.e[,names(config.graphs.df.e) == config.graphs.df.e$data.level] %>% as.character)
      #print(graphdata.df.e)
      #print(graphdata.ls.d[[graphdata.ls.index]])
    
    } ### END OF LOOP "d" BY GRAPH ###

  graphdata.ls.c[[c]] <- graphdata.ls.d
  #graphdata.ls.c[[b]]['loop.duration'] <- Sys.time()-loop.start.time.b  #100*b/maxrow.b
  #est.time.remaining <- (lapply(graphdata.ls.c, function(x){x['loop.duration']}) %>% unlist %>% mean())*(maxrow.b-b)
  #print(paste("Estimated time remaining: ",est.time.remaining," sec",sep = ""))
  #setTxtProgressBar(progress.bar.c, 100*c/maxrow.c)
  
} ### END OF LOOP "c" BY DISTRICT     
close(progress.bar.c)  

} #END OF SECTION COLLAPSE BRACKET

#OUTPUTS:
    #graphdata.ls.c
      #[[district]]
        #data frame where each line represents a graph
    
########################################################################################################################################################      
### PRODUCING GRAPHS THEMSELVES  ###

{#SECTION COLLAPSE BRACKET
  
  ###                       ###    
# ### LOOP "f" BY DISTRICT  ###
  ###                       ###
  
  graphs.ls.f <- list()
  progress.bar.f <- txtProgressBar(min = 0, max = 100, style = 3)
  maxrow.f <- graphdata.ls.c %>% lengths %>% sum
  
  
  #f <- 1 #LOOP TESTER
  #for(f in 1:2){ #LOOP TESTER
  for(f in 1:length(report.ids)){
    
    if(f == 1){print("FORMING GRAPHS IN GGPLOT...")}
    school.id.f <- report.ids[f]
    config.graphs.df.f <- config.graphs.ls.b[[f]]
    
    ###                       ###    
#   ### LOOP "g" BY GRAPH     ###
    ###                       ###
    
    #Loop output object(s)
      graphs.ls.g <- list()
    
    #g <- 1 #LOOP TESTER
    #for(g in 1:2) #LOOP TESTER
    for(g in 1:length(graphdata.ls.c[[f]]))
      local({ #Necessary to avoid annoying and confusing ggplot lazy evaluation problem (see journal)
        g<-g #same as above
        
        ### GRAPH INPUTS FOR GGPLOT ###
        
        #GRAPH DATA & CONFIGS DATA FRAMES
          graphdata.df.g <- graphdata.ls.c[[f]][[g]] %>% as.data.frame()
          names(graphdata.df.g) <- gsub("graphdata.","",names(graphdata.df.g))
          
          config.graphs.df.g <- config.graphs.df.f[g,] %>% as.data.frame()
          names(config.graphs.df.g) <- gsub("configs.","",names(config.graphs.df.g))
          
          graph.cat.varname <- config.graphs.df.g$graph.cat.varname  
        
          if(config.graphs.df.g$graph.cat.varname == "answer"){
            graphdata.df.g <- left_join(graphdata.df.g,ans.opt.always.df, by = c("answer" = "ans.num"))#graphdata.df.g[order(graphdata.df.g[,2]),]
            
            if(config.graphs.df.g$module %in% c("LEAD","PD")){
              graphdata.df.g <- graphdata.df.g %>% select(year, ans.text.agreement, measure.var, avg)
              graph.cat.varname <- "ans.text.agreement"
            }else{
              graphdata.df.g <- graphdata.df.g %>% select(year, ans.text.freq, measure.var, avg)
              graph.cat.varname <- "ans.text.freq"
            }
          }else{}
          
        ### BASE GRAPH FORMATION WITH GGPLOT2 ###
        
          graph.g <- 
            ggplot(data = graphdata.df.g 
                   
            ) + 
            
            geom_bar(
              aes(x = graphdata.df.g[[graph.cat.varname]], 
                  y = measure.var %>% as.numeric,
                  group = year, 
                  fill = factor(year)
                  #alpha = I(0.1)
              ),
              alpha = 1,
              position = "dodge", 
              stat = "identity"
            ) +
            
            theme(panel.background = element_blank(),
                  panel.grid.major.y = element_blank(),
                  panel.grid.major.x = element_blank(),
                  axis.text.x = element_text(size = 12, color = "#5a6b63"),
                  axis.text.y = element_blank(),
                  axis.ticks = element_blank(),
                  axis.title = element_blank(),
                  legend.position = "top",
                  legend.title = element_blank(),
                  legend.text = element_text(size = 12)
            ) +     
            
            #guides(fill = FALSE) +
            
            scale_fill_manual(
              values = c("#914E83","#c7c7c7")
            ) 
          
        #GRAPH DATA LABELS 
        
#FUN      #Graph Label Heights (defined based on ratio of tallest to shortest columns)
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
                  graph.labels.text.v <- var %>% as.numeric %>% round(.,2) %>% trimws(., which = "both") 
                }
              
              #Label visibility
                graph.labels.show.v <- ifelse(var != 0, 1, 0)  
              
              #Label color
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
          graph.labels.df <- create.graph.labels.fun(df = graphdata.df.g, measure.var = "measure.var", height.ratio.threshold = 8.2)
          
          #Add Data labels to graph
          graph.g <- 
            graph.g +
            geom_text( 
              aes(                                                          
                y = graph.labels.df$graph.labels.heights, 
                x = graphdata.df.g[[graph.cat.varname]],
                label = graph.labels.df$graph.labels.text,
                alpha = graph.labels.df$graph.labels.show,
                group = year
                
              ),
              
              #lineheight = 10.0,
              
              color = "#000000", #graph.labels.df$graph.labels.color,
              size = 4,
              fontface = "bold",
              position = position_dodge(width = 1),
              show.legend = FALSE
              
            )
          
        #GRAPH AVERAGES
        graphdata.df.g$avg.alpha <- 
          ifelse(
            graphdata.df.g$year != "Baseline" & graphdata.df.g$avg != 0,
            0.8,
            0.0
          )
        
        if(config.graphs.df.g$graph.average == "yes"){
          graph.g <- 
            graph.g +
            geom_errorbar(
              aes(
                x = graphdata.df.g[[graph.cat.varname]],
                group = year,
                ymin =graphdata.df.g$avg, 
                ymax = graphdata.df.g$avg,
                alpha = graphdata.df.g$avg.alpha
              ), 
              position = position_dodge(width = 1), # 1 is dead center, < 1 moves towards other series, >1 away from it
              color = "black", 
              width = 1,
              size = 1,
              show.legend = FALSE
            )
        }else{}
        
        #GRAPH CATEGORY NAMES, CORRECTING CATEGORY AXIS ORDERING
        #!potential function 
        
        #year, school.level, module, answer
        graph.cat.order.ls <-
          list(
            year = c("Baseline","2017-18"),
            school.level = c("Elem.","Middle","High","Mult.","Other"),
            role = c("Special Educator","Classroom Teacher","Instructional Coach","School Counselor","School Social Worker","Building Administrator","Other"),
            module = c("CFA", "ETLP","DBDM","LEAD","PD"),
            ans.text.freq = c("Always","Most of the time","About half the time","Sometimes","Never"),
            ans.text.agreement = c("Strongly Agree","Agree","Neutral","Disagree","Strongly Disagree")
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
            #scale_y_discrete(limits = graph.cat.order.ls[graph.cat.varname] %>% unlist %>% factor(., )) +
            theme(
              axis.text.x = element_blank(),
              axis.text.y = element_text(size = 15, color = "#5a6b63")
            )
        }
        
        #Sys.sleep(0.1)
        #print(graph.g)
        
        graphs.ls.g[[g]] <<- graph.g
        setTxtProgressBar(progress.bar.f, 100*(g + graphdata.ls.c[1:(f-1)] %>% lengths %>% sum)/maxrow.f)
        
      })  ### END OF LOOP "g" BY GRAPH ###
    #close(progress.bar.g)
    
    graphs.ls.f[[f]] <- graphs.ls.g
    
  } ### END OF LOOP "f" BY DISTRICT
  close(progress.bar.f)
  
}#END SECTION COLLAPSE BRACKET
  
#OUTPUT:
  #graphs.ls.f
    #[[district]]
      #ggplot object

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
    title.format <- textProperties(color = titlegreen, font.size = 48, font.weight = "bold")
    title.format.small <- textProperties(color = titlegreen, font.size = 40, font.weight = "bold")
    subtitle.format <- textProperties(color = notesgrey, font.size = 28, font.weight = "bold")
    section.title.format <- textProperties(color = "white", font.size = 48, font.weight = "bold")
    notes.format <- textProperties(color = notesgrey, font.size = 14)
    
    
} # END OF SECTION COLLAPSE BRACKET
    
########################################################################################################################################################      
### POWERPOINT SLIDE CREATION  ###        

{ #SECTION COLLAPSE BRACKET   
    
  config.pot.df <- read.xlsx("graph_configs.xlsx", sheetName = "slide.pot.objects",header = TRUE, stringsAsFactors = FALSE)
    ###                       ###    
#   ### LOOP "h" BY DISTRICT  ###
    ###                       ###
    
    #Progress Bar
      progress.bar.h <- txtProgressBar(min = 0, max = 100, style = 3)
      maxrow.h <- sapply(config.slides.ls.b, dim)[1,] %>% sum
    
    #h <- 8 #LOOP TESTER
    #for(h in 1:2){ #LOOP TESTER
    for(h in 1:length(config.slides.ls.b)){
      
      #Set up target file
        template.file <- paste(wd,
                             "Report Template/CWIS Template.pptx",
                             sep = "")
        target.path.h <- paste(target.dir,
                                  "/",
                                  "CWIS Report_",
                                  report.ids[h],
                                  "_",
                                  gsub(":",".",Sys.time()),
                                  ".pptx", sep="") 
      
        file.copy(template.file, target.path.h)
      
      #Set up powerpoing object 
        ppt.h <- pptx( template = target.path.h )
        options("ReporteRs-fontsize" = 20)
        options("ReporteRs-default-font" = "Calibri")
      
      #Set up district-level inputs
        config.graphs.df.h <- 
          config.graphs.ls.b[[h]] %>%
          mutate(row.i = config.graphs.ls.b[[h]] %>% .[,ncol(config.graphs.ls.b[[1]])] %>% seq_along(.))
        
        config.slides.df.h <- config.slides.ls.b[[h]]
        
        graphs.ls.h <- graphs.ls.f[[h]]
     
      ###                     ###    
#     ### LOOP "i" BY SLIDE   ###
      ###                     ###

        #i <- 3 #LOOP TESTER
        #for(i in 1:4){ #LOOP TESTER
        for(i in 1:dim(config.slides.ls.b[[h]])[1]){
          
          config.slide.df.i <- config.slides.ls.b[[h]] %>% .[i,]
          slide.type.id.i <- config.slide.df.i$slide.type.id
          layout.i <- config.slide.df.i$slide.layout
        
        #SLIDE FORMATION
          
          ppt.h <- addSlide( ppt.h, slide.layout = layout.i)
          ppt.h <- addPageNumber( ppt.h )
          
        #ADD GRAPHS
          
          #Graph Loop Inputs
            config.graphs.df.i <- config.graphs.df.h %>% 
              filter(slide.type.id == slide.type.id.i)
            
            if(is.na(config.slide.df.i$school)){
              config.graphs.df.i <- config.graphs.df.i[is.na(config.graphs.df.i$school),]
            }else{
              config.graphs.df.i <- config.graphs.df.i[config.graphs.df.i$school == config.slide.df.i$school,]
            }
            
            if(is.na(config.slide.df.i$module)){
              config.graphs.df.i <- config.graphs.df.i[is.na(config.graphs.df.i$module),]
            }else{
              config.graphs.df.i <- config.graphs.df.i[config.graphs.df.i$module == config.slide.df.i$module,]
            }
         
            
          ###                   ###    
#         ### LOOP "k" BY GRAPH ###
          ###                   ###
          
          #k <- 1 #LOOP TESTER
          #for(k in 1:2){ #LOOP TESTER
          for(k in 1:dim(config.graphs.df.i)[1]){
            if(dim(config.graphs.df.i)[1] < 1){
              #print(paste("No graph objects for slide.id: ",config.slide.df.i$slide.type.id,sep = ""))
              next()
            }
            
            graph.k <- graphs.ls.h[config.graphs.df.i$row.i %>% .[k]]
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
          
  
          ###                         ###    
#         ### LOOP "j" BY POT OBJECT  ###
          ###                         ###
            
          config.pot.i <- config.pot.df[config.pot.df$slide.type.id == slide.type.id.i,]
            
          #j <- 5 #LOOP TESTER
          #for(j in 1:2){ #LOOP TESTER
          for(j in 1:dim(config.pot.i)[1]){
            if(dim(config.pot.i)[1] < 1){
              print(paste("No text objects for slide.id: ",config.slide.df.i$slide.id,sep = ""))
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
                  color = alpha(ifelse(
                    !is.na(config.pot.i$color[j]),
                    config.pot.i$color[j] %>% 
                    strsplit(.,",") %>% unlist %>% as.numeric %>% 
                    rgb(red = .[1],green = .[2],blue = .[3] ,maxColorValue = 255) %>% .[1],
                    "black"
                  ),1),
                  font.size = config.pot.i$font.size[j], 
                  font.weight = ifelse(is.na(config.pot.i$font.weight[j]),'normal',config.pot.i$font.weight[j])
                  #alpha = 1
                  #shading.color = 'black'
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
        
        setTxtProgressBar(progress.bar.h, 100*(i + config.slides.ls.b[1:(h-1)] %>% sapply(., dim) %>% .[1,] %>% sum)/maxrow.h)
          
        } #END OF LOOP "i" BY SLIDE
          
    } # END OF LOOP "h" BY DISTRICT      
    close(progress.bar.h)      
          
          
} #END SECTION COLLAPSE BRACKET          

end_time <- Sys.time()
code_runtime <- end_time - start_time
print(code_runtime)

        
    
 
      
      
      