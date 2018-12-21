#########################################################
##### 	CWIS Automation for MMD                   	#####
#########################################################



# INITIAL SETUP -----------------------------------------------------------
  
  rm(list=ls()) #Remove lists
  options(java.parameters = "- Xmx8g") #helps r not to fail when importing large xlsx files with xlsx package
  
  
  #Record code start time for processing time calculations
  start_time <- Sys.time()
  

# ESTABLISH DIRECTORIES ---------------------------------------------------

  #M900
    working.dir <- "C:/Users/willi/Google Drive/1. FLUX CONTRACTS - CURRENT/2016-09 EXT Missouri Education/3. Missouri Education - GDRIVE/8. CWIS/2018-12 Green Reports Phase 6/"
    rproj.dir <- "C:/Users/willi/Documents/GIT PROJECTS/CWIS-automation"
    
  #Thinkpad T470
    #working.dir <- "G:/My Drive/1. FLUX CONTRACTS - CURRENT/2016-09 EXT Missouri Education/3. Missouri Education - GDRIVE/8. CWIS/2018-12 Green Reports Phase 6/"
    #rproj.dir <- "C:/Users/WNF/Documents/Git Projects/CWIS-automation"
  
  #Source Code Directory
    source.code.dir <- rproj.dir #paste(rproj.dir,"2_source_code/",sep="") #Changed back to using 'Documents' folder after attempting to move project into Google Drive but running into problems
  
  #Source Resources Director (raw data)
    source.resources.dir <- paste(working.dir,"3_source_resources/", sep = "")
  
  #Source Inputs (configs)
    source.inputs.dir <- paste(working.dir,"4_source_inputs/",sep="")

    
##### OUTPUTS #####
  #working.dir: working directory - Google Drive folder "2018-08 Green Reports"
  #source.code.dir: directory for R project; also contains source data, additional function scripts, and config tables.
  #source.resources.dir: directory with raw data
  #source.inputs.dir: directory with config tables and powerpoint template


# LOAD SOURCE CODE --------------------------------------------------------

  setwd(source.code.dir)
  source("utils_wnf.r")
  source("CWIS_custom.r")


# LOAD LIBRARIES/PACKAGES  ------------------------------------------------
 
  #In case working on new R install that does not have packages installed
    #InstallCommonPackages()
    #install.packages("ReporteRs")
    #install.packages("jsonline")
    #install.packages('httpuv')
    #install.packages('xtable')
    #install.packages('sourcetools')
    #install.packages('shiny')
    #install.packages('miniUI')
  
  LoadCommonPackages()
  library(proftools)
  library(jsonlite)
  library(rlang)
  library(extrafont)
  extrafont::loadfonts(device="win")

  
# LOAD SOURCES, RESOURCES, INPUTS -----------------------------------------
  
  #Global Configs Table
    configs.ss <- gs_key("1ku_OC9W87ut6W1qrdpFeYBlWlPN5X4fGHJ3h1k0HrOA",verbose = TRUE) 
    global.configs.df <- gs_read(configs.ss, ws = "global.configs", range = NULL, literal = TRUE)
    
    report.unit <- 
      global.configs.df[
        global.configs.df$Config == "Report Unit",
        tolower(names(global.configs.df)) == "value"
      ] %>% unlist %>% tolower
    
    #report.ids <- 
    #  global.configs.df[
    #    global.configs.df$Config == "Report Ids",
    #    tolower(names(global.configs.df)) == "value"
    #  ] %>% unlist %>% tolower
    
    report.version <- 
      global.configs.df[
        global.configs.df$Config == "Report Version",
        tolower(names(global.configs.df)) == "value"
      ] %>% unlist %>% tolower
    
    year <- 
      global.configs.df[
        global.configs.df$Config == "Data Year",
        tolower(names(global.configs.df)) == "value"
      ] %>% unlist %>% tolower
    
    semester <- 
      global.configs.df[
        global.configs.df$Config == "Data Semester",
        tolower(names(global.configs.df)) == "value"
      ] %>% unlist %>% tolower
    
    sample.print <- 
      ifelse(
        global.configs.df[
          global.configs.df$Config == "Sample Print",
          names(global.configs.df) == "Value"
          ] %>% 
          unique %>% 
          tolower == "yes",
        TRUE,
        FALSE
      )
    
    sample.size <- 
      global.configs.df[
        global.configs.df$Config == "Sample Print Size",
        tolower(names(global.configs.df)) == "value"
      ] %>% unlist %>% as.numeric()
   
  #Slide Types Configs Table 
    config.slidetypes.tb <- gs_read(configs.ss, ws = "slide.types", range = NULL, literal = TRUE) #read.xlsx("graph_configs.xlsx", sheetName = "slide.types",header = TRUE, stringsAsFactors = FALSE) 
  
  #Graph Types Configs Table
    load.config.graphtypes.tb <- gs_read(configs.ss, ws = "graph.types", range = NULL, literal = TRUE) #read.xlsx("graph_configs.xlsx", sheetName = "graph.types",header = TRUE, stringsAsFactors = FALSE) 
    config.graphtypes.df <- 
      inner_join(config.slidetypes.tb, load.config.graphtypes.tb, by = "slide.type.id", all.x = FALSE) 
  
  #Table Types Configs Table
    load.config.tabletypes.tb <- gs_read(configs.ss, ws = "table.types", range = NULL, literal = TRUE) #read.xlsx("graph_configs.xlsx", sheetName = "table.types",header = TRUE, stringsAsFactors = FALSE) 
    config.tabletypes.df <- 
      inner_join(config.slidetypes.tb, load.config.tabletypes.tb, by = c("slide.type.id")) 
    
  #Piece-of-text (POT) Config Table
    config.pot.tb <- gs_read(configs.ss, ws = "pot.types", range = NULL, literal = TRUE) #read.xlsx("graph_configs_Jason Altman.xlsx", sheetName = "slide.pot.objects",header = TRUE, stringsAsFactors = FALSE) 
    config.pot.tb$color <- 
      config.pot.tb$color %>% 
      gsub("x","",.)
  
  #Questions Configs Table (imported as list)
    questions.ls <- 	gs_read(configs.ss, ws = "questions", range = NULL, literal = TRUE) %>% as.list() %>% lapply(., tolower)
    questions.df <- do.call(cbind, questions.ls) %>% as.data.frame(., stringsAsFactors = FALSE)
  
  #Buildings Config Table  
    buildings.tb <- 	
      gs_read(configs.ss, ws = "buildings", range = NULL, literal = TRUE) %>% 
      as.list() %>% 
      lapply(., tolower) %>%
      do.call(cbind, .) %>%
      as_tibble()
    
    buildings.tb$report.id <- mgsub("bucahanan","buchanan",buildings.tb$report.id)
    
  #Responses table (main data, imported as data frame)
    
    setwd(source.resources.dir)
    
    resp1.df <- read.csv(
      file =  
        MostRecentlyModifiedFilename(
          title.string.match = "CWIS",
          file.type = "csv",
          dir = source.resources.dir
        ),
      stringsAsFactors = FALSE,
      header = TRUE
    )

##### OUTPUTS #####
  #global.configs.df
  #config.slidetypes.tb
  #config.graphtypes.df
  #config.tabletypes.df
  #config.pot.tb
  #buildings.tb
  #questions.df
  #resp1.df (initial responses dataset which will need extensive cleaning and organization in next sections)


# INITIAL INFORMATICS & 'UNBRANCHING' (STACKING) OF BRANCHED VARIABLES --------
  
  #INITIAL INFORMATICS
  
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
      
    #Add "x" to questions.sem.df$row.1 so they match exactly with Qualtrics export as imported by R
      questions.sem.df$row.1[NumSubstringMatches("_",questions.sem.df$row.1) == 2] <- 
        paste("x",questions.sem.df$row.1[NumSubstringMatches("_",questions.sem.df$row.1) == 2],sep="")
    
    #Remove extra header rows
      dat.startrow <- 
        ifelse(
          any(substr(resp1.df[,1],1,1) == "{"),
          which(substr(resp1.df[,1],1,1) == "{") + 1,
          1
        )
      resp1.df <- resp1.df[dat.startrow:length(resp1.df[,1]),]
    
    #Lower-case all variable names
      names(resp1.df) <- resp1.df %>% names %>% tolower 
    
    #Variable renaming of important variables
      names(resp1.df) <-
        mgsub(
          questions.sem.df$row.1[!is.na(questions.sem.df$q.changename)], 
          questions.sem.df$q.changename[!is.na(questions.sem.df$q.changename)], 
          names(resp1.df)
        )
    
    #Define Report Unit and Report IDs
      if(!report.unit %in% c("building","district")){
        stop("Report unit must be either 'building' or 'district.'")
      }
      
      if(report.unit == "building"){
        report.id.colname <- "building"
      }else{
        report.id.colname <- "district"
      }
      
      report.id.col <- resp1.df[,names(resp1.df) == report.id.colname]
      
      if(report.id.colname %in% c("building","district")){
        
        if(report.id.colname == "district"){
          resp1.df <-
            resp1.df %>%
            mutate(
              report.id =
                resp1.df[,grep(report.id.colname,names(resp1.df))] %>% tolower
            )
        }
        
        if(report.id.colname == "building"){
          resp1.df <- 
            resp1.df %>%
            mutate(
              report.id =
                paste(
                  resp1.df[,names(resp1.df) %in% "district"],
                  resp1.df[,names(resp1.df) %in% report.id.colname],
                  sep = "_"
                ) %>%
                tolower %>%
                replace(., . == "_", "")
            )
        }
        
        resp1.df$report.id <- gsub("\\/"," ",resp1.df$report.id) #in case there is a slash in the school name itself, this replaces it so file storage for ppt works properly
        
        report.ids.all <- #report ids filtering out district office, ids without a district, or blanks
          resp1.df$report.id %>%
          unique %>% 
          FilterVector(
            condition = !grepl("district office",.),
            vector.input = .
          ) %>%
          .[!grepl("_",substr(.,1,1))] %>%
          .[. != ""]
        
      }else{}   #If user has designated district names as "all", code will create reports for all district names present in the data
      
      #Restrict Responses table to produce only a sample of reports unless this is final print
        #Notes: code selects whole districts at random so that it will generate all reports for those districts. This is important so that you don't get
        #a bunch of scattered districts and the district averages aren't realistic. The code samples district combinations until it finds one where the
        #number of report ids is equalt to the user-defined sample size.
      
        if(sample.print & report.unit == "building"){ #TODO: generalize so will work if report.unit is district
            
          building.counts.df <- 
            resp1.df %>% 
            group_by(district) %>% 
            dplyr::summarize(n_buildings = n_distinct(building)) %>% 
            as.data.frame()
          
          report.ids.sample <- ""
          report.districts.sample <- ""
          unique.report.ids <- resp1.df$report.id %>% unique()
          
          i <- 1
          
          while(length(report.ids.sample) != sample.size){            
            
            report.districts.sample <-
              c(
                report.districts.sample,
                sample(resp1.df$district %>% unique,1)
              ) %>% 
              .[. != ""] %>% 
              tolower
            
            report.ids.sample <- 
              unique.report.ids[grep(paste(report.districts.sample,collapse = "|"),unique.report.ids)] %>%
              .[. != ""] %>% .[!grepl("district office", .)]
            
            if(length(report.ids.sample) > sample.size){
              report.districts.sample <- ""
              report.ids.sample <- ""
            }
            #print(i)
            #print(length(report.ids.sample))
            i = i+1
          }
        }
        
        if(!sample.print & report.unit == "building"){
          report.ids.sample <- report.ids.all
        }
        
        #Restrict response dataset to rows for generated report ids
          resp2.df <- resp1.df[resp1.df$report.id %in% report.ids.sample,]
      
    #Remove extraneous variables
      remove.colnames <- 
        questions.df %>% 
        filter(tolower(necessary.in.final.data) == "no") %>%
        filter(year == year) %>%
        filter(semester == semester) %>%
        select(row.1) %>%
        unlist %>%
        RemoveNA
      
      resp2.df <-
        resp2.df[ , !(names(resp2.df) %in% remove.colnames)]
          
      
  #STACKING COLUMNS SPLIT BY SURVEY BRANCHING
    #"branch" refers to branching questions, so branch0 are columns without branches, and branch1 are columns that are part of branching questions
    #"ans" refers to having answer options, so ans0 are columns without answer options, and ans1 are columns that are part of questions with multiple answer options
    
    branch0.ans0.colnames.v <- names(resp2.df)[NumSubstringMatches("_",names(resp2.df))==0]
    branch0.ans1.colnames.v <- names(resp2.df)[NumSubstringMatches("_",names(resp2.df))==1 & substr(names(resp2.df),1,1) == "q"]
    branch1.ans0.colnames.v <- names(resp2.df)[NumSubstringMatches("_",names(resp2.df))==1 & substr(names(resp2.df),1,1) == "x"]
    branch1.ans1.colnames.v <- names(resp2.df)[NumSubstringMatches("_",names(resp2.df))==2]
    
    #branch.q.colnums.v <- which(resp2.df %>% names %>% substr(.,1,1) == "x")
    
  #Make data frame of base variables (that require no stacking)  
    branch0.df <- 
      resp2.df[ ,                                           
                names(resp2.df) %in% c("responseid",branch0.ans0.colnames.v,branch0.ans1.colnames.v)      
                ]              
  
  #Make data fram of variables to be stacked
    branch1.df <- 
      resp2.df[ ,
                names(resp2.df) %in% c("responseid",branch1.ans0.colnames.v,branch1.ans1.colnames.v) # ResponseId plus all columns whose names begin with "X"
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
      
      if(NumSubstringMatches("_",varnames.a) %>% unique() == 1){ # final column names once branching is collapsed
        q.ans.options.a <- q.name.a
      }else{}
      
      if(NumSubstringMatches("_",varnames.a) %>% unique() == 2){
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
        ] #TODO:looks like still uneven numbers - some columns must be missing from questions table, but seems to be columns we don't care about.
    
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

##### OUTPUTS #####
  #resp2.df - now with all branch variables 'unbranched' (stacked), and having removed two extra header rows
  #questions.sem.df - now only with rows pertaining to this year and semester
  #ans.opt.always.df - global answer options table with numerical scale, agreement scale, and frequency scale lined up


# FURTHER CLEANING & ADDING USEFUL VARIABLES ------------------------------
  
  #Lower-Case All Data
    resp3.df <- apply(resp2.df,c(1:2),tolower) %>% as.data.frame(., stringsAsFactors = FALSE)
  
  #Recode role variable
    resp3.df$role <- mgsub("Teacher", "Classroom Teacher", resp3.df$role)
  
  #Recode school & district names
    resp3.df$building <- mgsub("bucahanan","buchanan",resp3.df$building)
    resp3.df$district <- mgsub("bucahanan","buchanan",resp3.df$district)
  
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
    resp3.df <- resp3.df %>% filter(report.id != "_")
  
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
    
    #Convert numeric variables in original data to numeric
      resp3.df[,names(resp3.df) %in% NumericVarnames(resp3.df)] <-
        apply(
          resp3.df[,names(resp3.df) %in% NumericVarnames(resp3.df)],
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
        ReplaceNames(
          df = .,
          current.names = names(.),
          new.names = names(slider.vars.df)
        ) 
      
      slider.agreement.varnames.v <- 
        q.unbranched.df$row.1[q.unbranched.df$var.type == "continuous" & q.unbranched.df$scale.type == "agreement"] %>%
        RemoveNA()
      
      slider.freq.varnames.v <-
        q.unbranched.df$row.1[q.unbranched.df$var.type == "continuous" & q.unbranched.df$scale.type == "frequency"] %>%
        RemoveNA()
      
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
        ReplaceNames(
          df = .,
          current.names = names(.),
          new.names = paste(names(.),"_text",sep="")
        )
      
      slider.binary.vars.df <- 
        do.call(cbind, slider.binary.vars.ls) %>% 
        as.data.frame %>%
        ReplaceNames(
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
        slider.num.vars.df %>% ReplaceNames(df = ., current.names = names(.), new.names = paste(names(.),"_num",sep = "")),
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
    
    #filter.varnames.v <- resp.long.df$question %>% unique %>% FilterVector(grepl("num|binary",.),.) %>% str_extract(., "q[0-9]*_[0-9]*") %>% unique
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
  
  
  #Establish Outputs Directory
    if(sample.print){
      outputs.dir <- 
        paste(
          working.dir,
          "5_outputs/",
          gsub(":",".",Sys.time()), 
          sep = ""
        )
    }else{
      outputs.dir <- 
        paste(
          working.dir,
          "5_outputs/",
          gsub(":",".",Sys.time()),
          "_FULL PRINT",
          sep = ""
        )
    }
  
    dir.create(
      outputs.dir,
      recursive = TRUE
    )
    
    setwd(outputs.dir)
  
  #Write Unbranched Data to Excel File
      unbranched.file.name <- 
        paste( 
          "widedata_",
          gsub(":",".",Sys.time()),
          ".csv", 
          sep=""
        )
    
    write.csv(
      resp.wide.df,
      file = unbranched.file.name,
      row.names = FALSE
    )

##### OUTPUTS #####
  #outputs.dir: directory for all outputs. Will have "FULL PRINT" if full print, or just the system date & time if sample print
  #resp.wide.df: wide data with all variables including numeric and binary
  #resp.long.df: long format data frame with cwis responses
  #ans.opt.always.df: data frame with columns corresponding to answer numbers and answer text,
    #including both frequency scale (e.g. 'always', 'most of the time') and agreement scale (e.g.
    #'strongly agree', 'agree').


# PRODUCING SLIDE, GRAPH, AND TABLE CONFIGURATION TABLES ------------------
  
  #Load Graph & Slide Type Config Tables
  
    #setwd(source.inputs.dir)
   
  #Expand Config Tables for each district according to looping variables
  
  ###                          ###    
# ### LOOP "b" BY REPORT.UNIT  ###
  ###                          ###
  
  #Loop Outputs 
    config.graphs.ls.b <- list()
    config.tables.ls.b <- list()
    config.slides.ls.b <- list()
  
  #Progress bar for loop
    progress.bar.b <- txtProgressBar(min = 0, max = 100, style = 3)
    maxrow.b <- length(report.ids.sample)
  
  #b <- 1 #LOOP TESTER (19 = "Raytown C-2")
  #for(b in c(1,2)){   #LOOP TESTER
  for(b in 1:length(report.ids.sample)){   #START OF LOOP BY REPORT UNIT
  
    #print(b)
    loop.start.time.b <- Sys.time()
    
    if(b == 1){print("FORMING SLIDE, GRAPH, AND TABLE CONFIG TABLES...")}
    #print(c(b,100*b/length(report.ids.sample)))
    
    #Create report.id.b (for this iteration) and skip if report for district office
      report.id.b <- report.ids.sample[b]
    
      #if(report.unit != "district" & grepl("district office", report.id.b)){
      #  next()
      #}
    
    #Create data frames for this loop - restrict to district id i  
      resp.long.df.b <- 
        resp.long.df %>% filter(report.id == report.id.b)
    
    #Graphs config table for this report unit
      config.graphs.df <- 
        loop.expander.fun(
          configs = config.graphtypes.df, 
          loop.varname = c("slide.loop.var.1","slide.loop.var.2","slide.loop.var.3"), 
          collate.varname = "slide.section.1",
          source.data = resp.long.df.b
        )
      
      config.graphs.ls.b[[b]] <- remove.district.office.fun(config.graphs.df)
    
    #Tables config table for this report unit
      config.tables.df <-
        loop.expander.fun(
          configs = config.tabletypes.df, 
          loop.varname = c("slide.loop.var.1","slide.loop.var.2","slide.loop.var.3"), 
          collate.varname = "slide.section.1",
          source.data = resp.long.df.b
        )
      
      config.tables.ls.b[[b]] <- remove.district.office.fun(config.tables.df)
    
    #Slide config table for this report unit
      config.slides.df <- 
        loop.expander.fun(
          configs = config.slidetypes.tb,
          loop.varnames = c("slide.loop.var.1","slide.loop.var.2","slide.loop.var.3"),
          collate.varname = "slide.section.1",
          source.data = resp.long.df.b  
        )
    
      config.slides.ls.b[[b]] <- remove.district.office.fun(config.slides.df)
    
    
    setTxtProgressBar(progress.bar.b, 100*b/maxrow.b)
    
  } # END OF LOOP 'b' BY REPORT.UNIT
  close(progress.bar.b)

##### OUTPUTS #####
  #report.ids.sample: vector with all report unit names in resp.long.df (length = 19 for baseline data)
  #config.slides.ls.b
    #[[report.unit]]
      #data frame where each line represents a slide
  #config.tables.ls.b
    #[[report.unit]]
      #data frame where each line represents a table
  #config.graphs.ls.b
    #[[report.unit]]
      #data frame where each line represents a graph


# PRODUCING GRAPH & TABLE DATA --------------------------------------------

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
                              report.ids.sample)
  
  #c <- 1 #LOOP TESTER (19 = "Raytown C-2", 244 = "waynesville middle")
  #for(c in slider.report.ids.sample){   #LOOP TESTER
  for(c in 1:length(report.ids.sample)){   #START OF LOOP BY DISTRICT
    
    if(c == 1){print("Forming input data tables for graphs...")}
    
    #Loop Inputs (both graphs and tables)
    report.id.c <- report.ids.sample[c]
    district.c <- resp.long.df %>% filter(report.id == report.id.c) %>% select(district) %>% unique %>% unlist %>% RemoveNA()
    
    resp.long.df.c <- 
      resp.long.df %>%
      filter(report.id == report.id.c)
       
      #select(names(resp.long.df)[names(resp.long.df) == report.id.colname]) %>% 
      #equals(report.id.c) %>% 
      #resp.long.df[.,]
    
    ###                    ###
#   ### LOOP "d" BY GRAPH  ###
    ###                    ###
    
    #Loop Inputs
    config.graphs.df.c <- config.graphs.ls.b[[c]]
    graphdata.ls.d <- list()
    
    #d <- 1
    #for(d in 1:2){ #LOOP TESTER
    for(d in 1:dim(config.graphs.df.c)[1]){
      
      config.graphs.df.d <- config.graphs.df.c[d,]
      
      group_by.d <- config.graphs.df.d$x.varname.1 %>% 
        strsplit(., ",") %>% 
        unlist
      
      graph.varnames.d <- 
        GraphVarnamesInData(
          config.input = config.graphs.df.d,
          data.input = resp.long.df
        )
      
      #TODO: still needs work to make sure this forms the correct list with all possible x-axis categories
        #Then it gets used by the summarize.table.fun to form the final table data. Currently not working.
      all.cats.ls.d <- 
        unique.variable.values.fun(
          varnames = graph.varnames.d, 
          tb = resp.long.df
        )
      
      #Create data frame "all.cats.df.d" of all possible answers for x-axis (role, module, year, answer)
        #If graph category is 'practice' as in 2018-08 Green Reports, have to make extra restriction to filter down to practices relevant to the specific module
        if(!is.na(config.graphs.df.d$x.varname.1) && config.graphs.df.d$x.varname.1 == "practice"){
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
          .[,names(resp.long.df) == config.graphs.df.d$x.varname.1] %>% 
          unique %>%
          strsplit(., ",") %>% 
          unlist %>%
          unique %>%
          RemoveNA(.)
        
        all.cats.df.d <- 
          all.cats.input2.d[order(all.cats.input2.d)] %>%
          as.data.frame(., stringsAsFactors = FALSE)
        
        names(all.cats.df.d) <- group_by.d
        #print(all.cats.df.d)
        
      #Form final data frame (no averages)
        graphdata.df.d <-  
          resp.long.df.c %>%
          graph.data.restriction.fun %>%
          group_by(!!! syms(group_by.d)) %>%
          summarize.graph.fun(config.input = config.graphs.df.d, data.input = .) %>%
          left_join(all.cats.df.d, ., by = c(group_by.d))

      #print(graphdata.df.d)
      

      #Add average variable to final data frame
        graph.avg.df.d <- 
          resp.long.df %>%
          avg.data.restriction.fun(.) %>%
          group_by(!!! syms(group_by.d)) %>%
          summarize.avg.fun(.)

      
        graphdata.df.d <- 
          left.join.NA(
            .x = graphdata.df.d, 
            .y = graph.avg.df.d, 
            .by = c(group_by.d),
            na.replacement = 0
          ) %>%
          ReplaceNames(
            df = .,
            current.names = c(names(.)),
            new.names = c(config.graphs.df.d$x.varname.1,"measure.var","measure.var.avg")
          )
      
      graphdata.ls.d[[d]] <- graphdata.df.d
      setTxtProgressBar(
        progress.bar.c, 
        100*(d + config.graphs.ls.b[1:(c-1)] %>% sapply(., dim) %>% sapply(`[[`,1) %>% unlist %>% sum)/maxrow.c
      )
      
    } ### END OF LOOP "d" BY GRAPH ###
    
    graphdata.ls.c[[c]] <- graphdata.ls.d
    
    ###                    ###
#   ### LOOP "d" BY TABLE  ###
    ###                    ###
    
    #Loop Inputs
    config.tables.df.c <- config.tables.ls.b[[c]]
    tabledata.ls.d <- list()
    
    #d <- 1
    #for(d in 1:2){ #LOOP TESTER
    for(d in 1:dim(config.tables.df.c)[1]){
      
      config.tables.df.d <- config.tables.df.c[d,]
      
      #Define all possible category values from table

      #all.cats.varnames.v.d <- c(config.tables.df.d$x.varnamename, config.tables.df.d$y.varname)
      
      #all.cats.ls.d <- 
      #  unique.variable.values.fun(
      #    varnames = c(config.tables.df.d$x.varname, config.tables.df.d$y.varname), 
      #    tb = resp.wide.df %>% as_tibble()
      #  )
      
      
      #Form final data frame (no averages)
        tabledata.df.d <-  
          resp.long.df.c %>%
          table.data.filter.fun(data.input = ., config.input = config.tables.df.d) %>%
          group_by(!!! syms(config.tables.df.d$summary.var)) %>%
          summarize.table.fun(config.input = config.tables.df.d, data.input = .) %>%
          mutate_all(funs(replace(., is.na(.), 0)))
        tabledata.df.d[,1] <- FirstLetterCap_MultElements(tabledata.df.d[,1])
      
      #storage.ls.index <- length(tabledata.ls.d) + 1
      tabledata.ls.d[[d]] <- tabledata.df.d
      
      #setTxtProgressBar(progress.bar.c, 100*(d + config.tables.ls.b[1:(c-1)] %>% sapply(., dim) %>% .[1,] %>% sum)/maxrow.c)
      
    } ### END OF LOOP "d" BY TABLE ###
    
    names(tabledata.ls.d) <- config.tables.df.c$module %>% RemoveNA() %>% as.character %>% c("role",.)
    tabledata.ls.c[[c]] <- tabledata.ls.d   
    print(c)
  } ### END OF LOOP "c" BY REPORT UNIT     
  close(progress.bar.c)  

##### OUTPUTS #####
  #tabledata.ls.c
    #[[report.unit]]
    #data frame where each line represents a table
  #graphdata.ls.c
    #[[report unit]]
    #data frame where each line represents a graph



# PRODUCING GRAPHS & TABLES THEMSELVES ------------------------------------

  ###                       ###    
# ### LOOP "f" BY DISTRICT  ###
  ###                       ###
  
  graphs.ls.f <- list()
  tables.ls.f <- list()
  progress.bar.f <- txtProgressBar(min = 0, max = 100, style = 3)
  maxrow.f <- graphdata.ls.c %>% lengths %>% sum
  
  
  #f <- 1 #LOOP TESTER
  #for(f in 1:2){ #LOOP TESTER
  for(f in 1:length(report.ids.sample)){
    
    if(f == 1){print("FORMING GRAPHS & TABLES IN GGPLOT...")}
    school.id.f <- report.ids.sample[f]
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
        
        #Graph Label Heights (defined based on ratio of tallest to shortest columns)
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
            above.label.vectorposition[is.na(above.label.vectorposition)] <- TRUE
            var[is.na(var)] <- 0
            graph.labels.heights.v[above.label.vectorposition] <-   #labels for columns below threshold, position is height of bar plus 1/10 of max bar height 
              var[above.label.vectorposition] + max/10
            graph.labels.heights.v[graph.labels.heights.v == 0] <-    #labels for columns above threshold, position is height of smallest bar divided by 2
              min(var[!above.label.vectorposition])/2
          }
          
          #Label Text
          if(config.graphs.df.g$data.measure == "implementation"){
            graph.labels.text.v <- as.character(100*var %>% round(., 2)) %>% paste(.,"%",sep="")
          }else{
            graph.labels.text.v <- var %>% as.numeric %>% round( ., 1) %>% sprintf("%.1f",.) %>% trimws(., which = "both") 
          }
          graph.labels.text.v[df[,names(df) == measure.var] %>% as.matrix %>% as.vector(.,mode = "numeric") %>% is.na(.)] <- "No Responses"
          
          #Label visibility
          graph.labels.alpha.v <- 1 #ifelse(var != 0, 1, 0)  
          
          #Label color for graph.type.e
          if(config.graphs.df.g$graph.type.id == "e"){
            graph.labels.color.v <- rep(c("#000000","#FFFFFF"),length(df[,1])/2) %>% rev
          }else{
            graph.labels.color.v <- rep("#FFFFFF",100)[1:length(df[,1])]
          }
          graph.labels.color.v[var==0] <- "#000000"
          graph.labels.color.v[above.label.vectorposition] <- "#000000"
          graph.labels.color.v <- graph.labels.color.v %>% rev
          
          result <- data.frame(
            graph.labels.text = graph.labels.text.v,
            graph.labels.heights = graph.labels.heights.v,
            graph.labels.alpha.v = graph.labels.alpha.v,
            graph.labels.color = graph.labels.color.v,
            stringsAsFactors = FALSE
          )
          
          #print(paste("Graph Label Heights: ",paste(graph.labels.heights.v, collapse = ", "),sep=""))
          return(result)
        }
        
        
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
            data = graphdata.df.g,
            alpha = alpha
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
        #TODO:Currently set manually - need to make it so fill happens within aes when have groups, within geom_bar() when setting manually
        #TODO:Would be nice to be able to set fill manually from config file as well.
        
        if(config.graphs.df.g$graph.type.id == "a"){
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
        
        #Graph label data frame
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
              #alpha = graph.labels.df$graph.labels.alpha.v,
              group = graphdata.df.g[,1]
            ),
            alpha = graph.labels.df$graph.labels.alpha.v,
            color = graph.labels.df$graph.labels.color,
            size = 4,
            fontface = "bold",
            position = position_dodge(width = 1),
            show.legend = FALSE
          )
        #windows()
        #graph.g
        
        #GRAPH AVERAGES
          #TODO: Need to make so can group on arbitrary variable with arbitrary number of groups and sub-groups. Right now can only two groups of 2 (e.g. year in Repeated Measures)
          graphdata.df.g$avg.alpha <- 
            ifelse(
              is.na(config.graphs.df.g$graph.group.by.vars),# != "Baseline" & graphdata.df.g$measure.var.avg != 0,
              1,
              rep(c(0.8,0.0),nrow(graphdata.df.g))
            )
          
          if(config.graphs.df.g$graph.average == "yes"){
            graph.g <- 
              
              graph.g +
              
              #geom_errorbar( #error bar shadow
              #  aes(
              #    x = graphdata.df.g[[graph.cat.varname]],
              #    #group = graphdata.df.g[[graph.cat.varname]], #TODO:removed group for Green Reports because didn't need it, but will have ot add back in and generalize
              #    ymin = graphdata.df.g$measure.var.avg-max(graphdata.df.g$measure.var.avg)/450, 
              #    ymax = graphdata.df.g$measure.var.avg-max(graphdata.df.g$measure.var.avg)/450,
              #    alpha = graphdata.df.g$avg.alpha
              #  ), 
              #  position = position_dodge(width = 1), # 1 is dead center, < 1 moves towards other series, >1 away from it
              #  color = "black", 
              #  width = 1,
              #  size = 2,
              #  show.legend = FALSE
              #) #+
            
              geom_errorbar(
                aes(
                  x = graphdata.df.g[[graph.cat.varname]],
                  #group = graphdata.df.g[[graph.cat.varname]],
                  ymin = graphdata.df.g$measure.var.avg, 
                  ymax = graphdata.df.g$measure.var.avg,
                  alpha = graphdata.df.g$avg.alpha
                ), 
                position = position_dodge(width = 1), # 1 is dead center, < 1 moves towards other series, >1 away from it
                color = "yellow", 
                width = 1,
                size = 2,
                alpha = 1,
                show.legend = FALSE
              )
            
        }else{}
        
        #GRAPH CATEGORY NAMES, CORRECTING CATEGORY AXIS ORDERING
          #TODO:potential function 
          #TODO: if can make first character of all categories into numeric vector, then just order by that
        
        graph.cat.order.ls <-
          list(
            year = c("Baseline","2017-18"),
            school.level = c("Elem.","Middle","High","Mult.","Other"),
            role = c("Special Educator","Classroom Teacher","Instructional Coach","School Counselor","School Social Worker","Building Administrator","Other"),
            module = c("ETLP", "CFA","DBDM","LEAD","PD"),
            ans.text.freq = c("Always","Most of the time","About half the time","Sometimes","Never"),
            ans.text.agreement = c("Strongly Agree","Agree","Neutral","Disagree","Strongly Disagree"),
            practice = if("practice" %in% names(graphdata.df.g)){graphdata.df.g$practice}else{""} #TODO:When moving this out of loop, will need to generalize for all module practices
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

      graphs.ls.f[[f]] <- graphs.ls.g
      
      ###                       ###    
  #   ### LOOP "g" BY TABLE     ###
      ###                       ###
      
      #Loop output object(s)
        tables.ls.g <- list()
      
      #g <- 4 #LOOP TESTER
      #for(g in 1:2) #LOOP TESTER
      for(g in 1:length(tabledata.ls.c[[f]])){
        
        if(dim(tabledata.ls.c[[f]][[g]])[1] == 0){
          tabledata.ls.c[[f]][[g]][1,] <- rep(0, dim(tabledata.ls.c[[f]][[g]])[2]) 
        }
        
        ft.g <- FlexTable(
          data = tabledata.ls.c[[f]][[g]],
          header.columns = TRUE,
          add.rownames = FALSE,
          
          header.cell.props = cellProperties(background.color = "#5F3356", border.style = "none"), #TODO:Should put into configs instead of specifying in code
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
    
    names(tables.ls.g) <- c("role","etlp","cfa","dbdm","pd","lead") #TODO:WAS CAUSING PROBLEMS WITH ORDERING OF TABLES ON SLIDES BECAUSE HAD NOT BEEN UPDATED TO NEW ORDER OF MODULES
    tables.ls.f[[f]] <- tables.ls.g
    
  } ### END OF LOOP "f" BY REPORT.UNIT
  close(progress.bar.f)

##### OUTPUTS #####
  #graphs.ls.f
    #[[report.unit]]
      #ggplot object
  #tables.ls.f
    #[[report.unit]]
      #FlexTable object


# POWERPOINT GLOBAL CONFIGURATIONS ----------------------------------------

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


# POWERPOINT SLIDE CREATION -----------------------------------------------
  
  setwd(source.inputs.dir)
  
  ###                          ###    
# ### LOOP "h" BY REPORT UNIT  ###
  ###                          ###
  
  #Progress Bar
    progress.bar.h <- txtProgressBar(min = 0, max = 100, style = 3)
    maxrow.h <- sapply(config.slides.ls.b, dim) %>% sapply(`[[`,1) %>% unlist %>% sum
    printed.reports.ls <- list()
  
  #h <- 50 #LOOP TESTER
  #for(h in ceiling(runif(5,1,length(config.slides.ls.b)))){
  for(h in 1:length(config.slides.ls.b)){ #LOOP TESTER
    
    #Reading 'Cadre' so it can be added to file name
      cadre.h <- 
        buildings.tb %>% 
        filter(report.id == report.ids.sample[h]) %>% 
        select(cadre) %>% 
        unlist %>% 
        FirstLetterCap_OneElement()
    
    #Set up target file
      template.file <- paste(source.inputs.dir,
                             "template_green reports.pptx",
                             sep = "")
      if(sample.print){
        file.name.h <- 
          paste(
            cadre.h,
            "_",
            h,
            "_",
            report.ids.sample[h],
            "_",
            gsub(":",".",Sys.time()) %>% substr(., 15,19),
            ".pptx", 
            sep=""
          ) 
      }else{
        file.name.h <- 
          paste(
            cadre.h,
            "_",
            report.ids.sample[h],
            sep = ""
          )
      }
      
      target.path.h <- paste(outputs.dir,
                             "/",
                             file.name.h,
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
      
      #TODO:Will need to generalize below for different report units (i.e. Repeated Measures vs. Green Reports)
      report.id.h <- report.ids.sample[h]
      district.h <- strsplit(report.id.h, "_") %>% unlist %>% .[1] %>% toupper()
      school.h <- strsplit(report.id.h, "_") %>% unlist %>% .[2] %>% toupper()
      config.slides.df.h <- config.slides.ls.b[[h]]
      
      graphs.ls.h <- graphs.ls.f[[h]]
      tables.ls.h <- tables.ls.f[[h]]
    
    ###                     ###    
#   ### LOOP "i" BY SLIDE   ###
    ###                     ###
    
    #i <- 5 #LOOP TESTER
    #for(i in 1:4){ #LOOP TESTER
    for(i in 1:dim(config.slides.ls.b[[h]])[1]){
      
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
        
        if(dim(config.graphs.df.i)[1] !=0 && !is.na(config.graphs.df.i$graph.type.id)){
          
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
      
        #TODO: Will want to generalize so can add more than one table to each slide if necessary
        config.tables.df.i <- config.tables.df.h %>% 
          filter(slide.type.id == slide.type.id.i)
        
        if(dim(config.tables.df.i)[1] != 0 && !is.na(config.tables.df.i$table.type.id)){
          
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
#       ### LOOP "j" BY POT OBJECT  ###
        ###                         ###
        
        config.pot.i <- config.pot.tb[config.pot.tb$slide.type.id == slide.type.id.i,]
        
        if(any(!is.na(config.pot.i$module))){
          config.pot.i <- filter(config.pot.i, grepl(as.character(config.slide.df.i$module), config.pot.i$module))
        }  
        
        #j <- 1 #LOOP TESTER
        #for(j in 1:2){ #LOOP TESTER
        for(j in 1:dim(config.pot.i)[1]){
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
      
      #writeDoc(ppt.h, file = target.path.h) #test Slide 1 build
      #rm(ppt.h)
      setTxtProgressBar(progress.bar.h, 100*h/length(report.ids.sample))
      
    } #END OF LOOP "i" BY SLIDE
    
    writeDoc(ppt.h, file = target.path.h) #Write complete pptx object to file
    
    print(h)
    printed.reports.ls[[h]] <- report.ids.sample[h]
  } # END OF LOOP "h" BY REPORT.UNIT      
  close(progress.bar.h)      

end_time <- Sys.time()
code_runtime <- end_time - start_time
print(code_runtime)

windows()        




