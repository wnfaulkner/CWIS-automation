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
### ESTABLISH DIRECTORIES ###

{ #SECTION COLLAPSE BRACKET
  
# Main data
  #Directories
    
    #M900
      rproj.dir <- "C:/Users/WNF/Documents/Git Projects/CWIS-automation"
      wd <- "C:/Users/WNF/Google Drive/1. FLUX CONTRACTS - CURRENT/2016-09 EXT Missouri Education/3. Missouri Education - GDRIVE/8. CWIS/2018-05 Repeated Measures/"
    
    #Thinkpad T470
      #rproj.dir <- "C:/Users/WNF/Documents/Git Projects/CWIS-automation"  
      #wd <- "G:/My Drive/1. FLUX CONTRACTS - CURRENT/2016-09 Missouri Education/3. Missouri Education - GDRIVE/8. CWIS/2018-05 Repeated Measures/"

    
    #Function Directories
      setwd(rproj.dir)
      source("FUN_FirstletterCap.r")
      source("FUN_ColClassConvert.r")
      
    #Data & Output Directories
      source.dir <- paste(rproj.dir,"/data_source/", sep = "")
      target.dir <- paste(wd,"r_script_outputs/",
                            "Output_",
                            gsub(":",".",Sys.time()), sep = "")
      dir.create(target.dir)
   
  #Variable helper table
    cwis.embed.helper.ss <- gs_key("1FaBPQP8Gqwp5sI_0g793G6yjW5XNFbV8ji8N7i9oLjs",verbose = TRUE) 
    cwis.embed.helper.df <- 	gs_read(cwis.embed.helper.ss, ws = 1, range = NULL, literal = TRUE) %>% as.data.frame()
    cwis.embed.helper.df$q.id <- tolower(cwis.embed.helper.df$q.id)

} #END SECTION COLLAPSE BRACKET

#OUTPUTS
  #rproj.dir: directory for R project; also contains source data, additional function scripts, and config tables.
  #wd: working directory for output files
    
########################################################################################################################################################      
### DATA CLEANING & PREP ###

{ #SECTION COLLAPSE BRACKET

  setwd(source.dir)

  #Read data files
    cwis.df <- read.csv("All Baseline and Year 1 data_20180724.csv",
                        stringsAsFactors = FALSE,
                        header = TRUE)
    
  #Initial informatics
    
    #Edit variable names
      names(cwis.df) <- cwis.df %>% names %>% tolower #Lower-case all variable names
      names(cwis.df)[names(cwis.df) == "id"] <- "responseid"
      
    #Recode role variable
      cwis.df$role[cwis.df$role == "Teacher"] <- "Classroom Teacher"
      
    #Standardize school names
      cwis.df$school <- tolower(cwis.df$school) %>% gsub("elem\\.","elementary",.)
      cwis.df$school <- gsub("sch\\.","school", cwis.df$school)
      cwis.df$school <- gsub("co\\.","county", cwis.df$school)
      cwis.df$school <- gsub("jr\\.","junior", cwis.df$school)
      cwis.df$school <- gsub("sr\\.","senior", cwis.df$school)
      cwis.df$school[grep("meramec valley early childhood", cwis.df$school)] <- "early childhood center"
    
    #Rearrange data columns
      cwis.df <- cwis.df[,   # CWIS response variables last, others first
          c(which(!grepl("_", names(cwis.df))),
            grep("_", names(cwis.df)))
        ]
      
      cwis.df <-  cwis.df[, #Put "responseid" in first column
                    c(grep("responseid", names(cwis.df)),which(!grepl("responseid", names(cwis.df))))
                    ]
    ##########################################################################################################################################
    #Column Class Conversions
      #cwis.df <- ColClassConvert(cwis.df)
    
  #Add useful variables for analysis 
    
    #Useful vectors for selecting cwis answer variables
      cwis.vars.v <- grep("_", names(cwis.df))
      cwis.varnames.v <- names(cwis.df)[grep("_", names(cwis.df))]
      cwis.modules.v <- cwis.varnames.v %>% strsplit(.,"_") %>% sapply(., `[[`, 1) %>% unique
    
    #Create dummy variables for implementation (4 or above = 1)
      impbinary.df <- cwis.df[,cwis.vars.v] %>% apply(., c(1:2), function(x){ifelse(x>=4,1,0)}) %>% as.data.frame
      names(impbinary.df) <- paste(cwis.varnames.v,"_impbinary",sep="") 
    
    #Create school.id variable which is concatenation of school and district
      cwis.df$school.id <- paste(cwis.df$district, cwis.df$school,sep = "_") %>% tolower
      cwis.df$school.id <- gsub("\\/"," ",cwis.df$school.id) #in case there is a slash in the school name itself, this replaces it so file storage for ppt works properly

    #School Level Variable
      school.level.df <- 
        read.xlsx(
          "MMD List with Grade Spans.xlsx",
          sheetName = "MMD Cohort 1&2",
          header = TRUE,
          as.data.frame = TRUE,
          stringsAsFactors = FALSE) %>%
        mutate(school.id = paste(tolower(district.name),tolower(trimws(school.name, which = "both")),sep = "_"))
      
      #x<- unique(cwis.df$school.id) %in% unique(school.level.df$school.id) %>% cbind(unique(cwis.df$school.id),.) %>% as.data.frame(.,stringsAsFactors = FALSE) 
      #x[order(x$V1),] %>% filter(. == FALSE)# %>% .[23:57,]
      #x<- unique(cwis.df$district) %in% unique(school.level.df$district.name) %>% cbind(unique(cwis.df$district),.) %>% as.data.frame()
      #x[order(x$V1),]
        
      #####!FAKE CREATE SCHOOL LEVEL VARIABLE (HIGH, MIDDLE, ELEMENTARY)
      #school.level.df <- data.frame( 
      #  school.id = cwis.df$school.id %>% unique,
      #  school.level = sample(c("high","middle","elem."),length(unique(cwis.df$school.id)), replace = TRUE),
      #  stringsAsFactors = FALSE
      #)
      
      cwis.df <- left_join(cwis.df,school.level.df %>% select(school.id, school.level), by = "school.id")
      cwis.df$school.level[is.na(cwis.df$school.level)] <- "Other"
      cwis.df$school.level[cwis.df$school.id == "belton 124_bosco"] <- "Other"
      cwis.df$school.level[cwis.df$school.id == "cameron r-i_cameron high school"] <- "High"
      cwis.df$school.level[cwis.df$school.id == "poplar bluff r-i_poplar bluff early childhood center"] <- "Elem."
      cwis.df$school.level[cwis.df$school.id == "poplar bluff r-i_poplar bluff technical career center"] <- "Other"
      cwis.df$school.level[cwis.df$school.id == "sheldon r-viii_sheldon k-12"] <- "Other"
      
      cwis.df$school.level <- FirstLetterCap_MultElements(cwis.df$school.level)
      
    #Capitalize First Letter of character variables
      cwis.df[,names(cwis.df) %in% c("year","role","district","school","school.level")] <- 
        apply(cwis.df[,names(cwis.df) %in% c("year","role","district","school","school.level")], 2, FirstLetterCap_MultElements)
      
    #Create final data frames: 1. Wide; 2. Long for original CWIS data; 3. Long for impbinary data (both long include all original id variables)
      dat.wide.df <- cbind(cwis.df, impbinary.df)
      dat.idvars.df <- cwis.df[,!names(cwis.df) %in% cwis.varnames.v]
      dat.answer.long.df <- gather(cwis.df, key = "question", value = "answer", cwis.varnames.v, factor_key = FALSE)
      dat.impbinary.long.df <- gather(cbind(dat.idvars.df,impbinary.df), key = "question", value = "answer", names(impbinary.df), factor_key = FALSE) 
      dat.long.df <- rbind(dat.answer.long.df, dat.impbinary.long.df)
      
    #Creating additional useful variables for long data frames
      # Variable for module
        dat.long.df$module <- strsplit(dat.long.df$question, "_" ) %>% sapply(., `[[`, 1) %>% toupper()
        dat.long.df$impbinary <- ifelse(grepl("impbinary",dat.long.df$question),1,0)
        
        #dat.answer.long.df$module <- strsplit(dat.answer.long.df$question, "_" ) %>% sapply(., `[[`, 1)
        
    #Loop: state average implementation rates [a], results in imp.state.df
      imp.state.df <- data.frame(cwis.module = cwis.modules.v)
      #a <- 1 # LOOP TESTER
      for(a in 1:length(cwis.modules.v)){
        imp.state.df$imp.rate[imp.state.df[,1]==cwis.modules.v[a]] <- 
          impbinary.df[,grep(cwis.modules.v[a], names(impbinary.df))] %>%
          apply(., 2, function(x){mean(x, na.rm = TRUE)}) %>% 
          mean() 
      }

  ## BUILD VARIABLE/QUESTION LOOKUP TABLE
  
  { #BEGIN SECTION COLLAPSE BRACKET
    #Variable names & questions, adjusting for collapsed columns
      vars.df <- names(dat.wide.df) %>% as.data.frame(., stringsAsFactors = FALSE)
      names(vars.df) <- "q.id"
      vars.df <- left_join(vars.df, cwis.embed.helper.df, by = "q.id")
      
    #Remove repeated parts of questions
      question.full.remove.strings <- c(
        "Please ",
        "Please use the agreement scale to respond to each prompt representing your",
        "Please use the frequency scale to respond to each prompt representing your",
        " - Classroom Teacher - ",
        "\\[Field-2\\]",
        "\\[Field-3\\]",
        "\\\n"
      )
      vars.df$question.full <- gsub(paste(question.full.remove.strings, collapse = "|"),
                                    "",
                                    vars.df$question.full)
      
    #Answer Options
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
}#END SECTION COLLAPSE BRACKET

#OUTPUTS
  #dat.long.df: long format data frame with cwis responses
  #ans.opt.always.df: data frame with columns corresponding to answer numbers and answer text,
    #including both frequency scale (e.g. 'always', 'most of the time') and agreement scale (e.g.
    #'strongly agree', 'agree').

########################################################################################################################################################      
### PRODUCING GRAPH & SLIDE CONFIGURATION TABLES ###

{ #SECTION COLLAPSE BRACKET
  
  # District name selection
    district.ids <- "all"
    if(tolower(district.ids) %in% "all" %>% any){
      district.ids <- dat.wide.df$district[order(dat.wide.df$district)] %>% unique
    }else{}   #If user has designated district names as "all", code will create reports for all district names present in the data
    dat.wide.df %>% 
      group_by(district,school.level) %>% 
      dplyr::summarize(num.responding.schools = length(unique(school))) %>% as.data.frame()  #Check how many schools in each district
      
    # Load Graph & Slide Type Config Tables
      setwd(rproj.dir)
      
      config.slidetypes.df <- read.xlsx("graph_configs.xlsx", sheetName = "slide.types",header = TRUE, stringsAsFactors = FALSE)
      load.config.graphtypes.df <- read.xlsx("graph_configs.xlsx", sheetName = "graph.types",header = TRUE, stringsAsFactors = FALSE)
      
      config.graphtypes.df <- SplitColReshape.ToLong(config.slidetypes.df, id.var = "slide.type.id",split.varname = "slide.graph.type",split.char = ",") %>%
        left_join(., load.config.graphtypes.df, by = c("slide.graph.type" = "graph.type.id")) %>% 
        filter(!is.na(slide.graph.type))
      config.graphtypes.df <- config.graphtypes.df[,grep("slide.type.id|loop|data|graph|height|width|offx|offy",names(config.graphtypes.df))]
    
    # Expand Graph & Slide Config Tables for each district according to looping variables
      
      ###                       ###    
#     ### LOOP "b" BY DISTRICT  ###
      ###                       ###
      
    # Loop Outputs 
      config.graphs.ls.b <- list()
      config.slides.ls.b <- list()
    
    # Progress bar for loop
      progress.bar.b <- txtProgressBar(min = 0, max = 100, style = 3)
      maxrow.b <- length(district.ids)
      
    #b <- 2 #LOOP TESTER (19 = "Raytown C-2")
    #for(b in c(1,2)){   #LOOP TESTER
    for(b in 1:length(district.ids)){   #START OF LOOP BY DISTRICT
      
      loop.start.time.b <- Sys.time()
      if(b == 1){print("FORMING GRAPH & SLIDE CONFIG TABLES...")}
      #print(c(b,100*b/length(district.ids)))
      
      # Create data frames for this loop - restrict to district id i
        district.id.b <- district.ids[b]
        dat.long.df.b <- dat.long.df[dat.long.df$district == district.id.b,]
        #print(head(dat.long.df.b))
        
#FUN  #Loop Expander Function (for creating full config tables)  
      loop.expander.fun <- function(configs, loop.varname, intersperse.varname, source.data){
        output.ls <- list()
        
        #c = 1 #LOOP TESTER: NO LOOPS
        #c = 3 #LOOP TESTER: ONE LOOP VAR
        #c = 8 #LOOP TESTER: TWO LOOP VARS
        #for(c in 1:3){
        for(c in 1:dim(configs)[1]){
        
          c.list.c <- list(c=c)
          
          #Make data frame with configurations repeated out across all unique combinations of loop.varname in source.data      
            slide.loop.vars.c <- configs[c,names(configs)==loop.varname] %>% strsplit(., ",") %>% unlist %>% trimws(., which = "both")
            
            if(any(is.na(slide.loop.vars.c))){
              configs.c <- configs[c,]
            }
            
            if(any(!is.na(slide.loop.vars.c))){
          
              loop.unique.df <- 
                source.data[names(source.data) %in% slide.loop.vars.c] %>% 
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
        
        #Intersperse section titles: 
          if(!missing(intersperse.varname)){
            
            #for each unique slide.type.id meeting these conditions:
              #select lines with only that unique slide.type.id
              #slide.type.position == slide.type.position of previously selected lines + 1
            #order by variable that has same name as slide.loop.var (e.g. 'school')
            #re-attach to original data.frame
          
            #Name of loop variable (will be used to order data-frame in b-loop)
              loop.var.b <- output.df[output.df$slide.layout == "section" & !is.na(output.df$slide.loop.var),] %>%
                select(slide.loop.var) %>%
                unlist %>%
                unique 
            
            #Slide Type IDs that need to be interspersed
              loop.index.b <- output.df[output.df$slide.layout == "section" & !is.na(output.df$slide.loop.var),] %>% 
                select(slide.type.position) %>% 
                unique
            
            #Separate out lines of output.df where: slide.layout == "section" && !is.na(slide.loop.var)
              intersperse.df <- output.df[output.df$slide.type.position %in% c(loop.index.b,loop.index.b+1),]
              non.intersperse.df <- output.df[!(output.df$row.id %in% intersperse.df$row.id),]
            intersperse.ls <- list()
    
            #b=1 #LOOP TESTER
            for(b in 1:length(loop.index.b)){
              intersperse.df.b <-  output.df[output.df$slide.type.position %in% c(loop.index.b[b],loop.index.b[b]+1),]
              intersperse.df.b <- intersperse.df.b[order(intersperse.df.b[,names(intersperse.df.b) == loop.var.b]),] %>%
                mutate(row.id = paste(b,".",1:dim(.)[1],sep = ""))
              intersperse.ls[[b]] <- intersperse.df.b
            }
            
              #! This function still not ready to handle more than one set of interspersed titles because would need to figure out how to order 
              #! list elements and non-interspersed slides by slide.type.position. Right now will do fine as long as all interspersed lines are at
              #! the end of the expanded config table (but not if there are non-interspersed variables that come below any interspersed vars in the
              #! config table).
              output.df <- rbind(non.intersperse.df,rbind.fill(intersperse.ls))
              
          }
        return(output.df)
      }

#FUN  #Replace NAs in a vector with a replacement value
      na.sub <- function(vector,na.replacement){
        vector[is.na(vector)] <- na.replacement
        return(vector)
      }       

#FUN  #School-level slides should not include an iteration for the District Office 
      remove.district.office.fun <- function(x){
        x[!(grepl("school", x$slide.loop.var) & (x$school %>% na.sub(.,"")) == "District Office"),]
      }

      config.graphs.df <- 
        loop.expander.fun(
          configs = config.graphtypes.df, 
          loop.varname = "slide.loop.var", 
          source.data = dat.long.df.b
        )
      
      config.graphs.ls.b[[b]] <- config.graphs.df %>% remove.district.office.fun(.)
      
      config.slides.df <- 
        loop.expander.fun(
          configs <- config.slidetypes.df, 
          loop.varname <- "slide.loop.var",
          intersperse.varname = "slide.type.position",
          source.data = dat.long.df.b)
      config.slides.ls.b[[b]] <- config.slides.df %>% remove.district.office.fun(.)
      
      setTxtProgressBar(progress.bar.b, 100*b/maxrow.b)

    } # END OF LOOP 'b' BY DISTRICT
    close(progress.bar.b)
    
} # END SECTION COLLAPSE BRACKET
    
#OUTPUTS:
  #district.ids: vector with all district names in dat.long.df (length = 19 for baseline data)
  #config.graphs.ls.b
    #[[district]]
      #data frame where each line represents a graph
  #config.slides.ls.b
    #[[district]]
      #data frame where each line represents a slide

########################################################################################################################################################      
### PRODUCING GRAPH DATA ###

{# SECTION COLLAPSE BRACKET
     
###                       ###    
### LOOP "c" BY DISTRICT  ###
###                       ###
      
  # Loop outputs
    graphdata.ls.c <- list()
    #graphs.ls.c <- list()
  
  # Progress bar for loop
    progress.bar.c <- txtProgressBar(min = 0, max = 100, style = 3)
    maxrow.c <- config.graphs.ls.b %>% sapply(., dim) %>% .[1,] %>% sum
  
  #c <- 1 #LOOP TESTER (19 = "Raytown C-2")
  #for(c in c(1,2)){   #LOOP TESTER
  for(c in 1:length(district.ids)){   #START OF LOOP BY DISTRICT
    if(c == 1){print("Forming input data tables for graphs...")}
                                    
    dat.long.df.c <- dat.long.df[dat.long.df$district == district.ids[c],]
    district.id.c <- district.ids[c]
    config.graphs.df.c <- config.graphs.ls.b[[c]]
    graphdata.ls.d <- list()
    
    ###                    ###
#   ### LOOP "d" BY GRAPH  ###
    ###                    ###
    
    #d <- 13
    #for(d in 1:2){ #LOOP TESTER
    for(d in 1:dim(config.graphs.df.c)[1]){
      
      config.graphs.df.d <- config.graphs.df.c[d,]
      
        group_by.d <- config.graphs.df.d$data.group.by.var %>% 
          strsplit(., ",") %>% 
          unlist
      
      #Create data frame "all.cats.df.e" of all possible answers for x-axis (role, module, year, answer)
        
        all.cats.d <- dat.long.df %>%
          filter( dat.long.df$impbinary == 0 ) %>%
          .[,names(dat.long.df) == config.graphs.df.d$graph.cat.varname] %>% 
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
          dat.long.df.c %>%
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
              filter(district == district.id.c) 
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
          dat.long.df %>%
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
  for(f in 1:length(district.ids)){
    
    if(f == 1){print("FORMING GRAPHS IN GGPLOT...")}
    district.id.f <- district.ids[f]
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
        
        #When graphs are car as opposed to columns, have to reverse order because the coord_flip() command does a mirror image
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
                                  district.ids[h],
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

        
        
        
        
        
        
        
        
        
        
        
        
        
         

    
########################################################################################################################################################      
### WRITE POWERPOINTS TO FILE  ###
    
    dir.create(target.dir)  
    lapply(ppt.ls.h, function(x){writeDoc(x, file = x["target.filename"])})
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     
        
#create.slide.graph <- function(input){
#  result <- input["graphdata"]
#  return(result)
#input["graphdata"] %>% as.data.frame()%>% print

#}

#input <- graphdata.ls.c[[6]][[1]]
#lapply(input, create.slide.graph)

        
    
 
      
      
      