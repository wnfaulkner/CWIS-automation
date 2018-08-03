#########################################################
##### 	CWIS Automation for MMD                   	#####
#########################################################


### INITIAL SETUP ###
  
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
    library(tidyr)
    library(dplyr)
    library(ReporteRs)
    library(ggplot2)
    library(stringr)
    library(reshape2)
    library(xlsx)
    library(jsonlite)
  
      
### LOAD DATA ###

# Main data
  #Directories
    
    #M900
      #wd <- "C:/Users/WNF/Google Drive/1. FLUX CONTRACTS - CURRENT/2016-09 Missouri Education/3. Missouri Education - GDRIVE/8. CWIS/2018-04 CWIS Automation for CW/"
      #rproj.dir <- ""
    #Thinkpad T470
      rproj.dir <- "C:/Users/WNF/Documents/Git Projects/CWIS-automation"  
      wd <- "G:/My Drive/1. FLUX CONTRACTS - CURRENT/2016-09 Missouri Education/3. Missouri Education - GDRIVE/8. CWIS/2018-05 Repeated Measures/"

    
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
    
    
    
  #Google Sheets
    #cwis.ss <- 	gs_key("13--0r4jDrW8DgC4cBlrbwIOS7nLfHsjaLFqbk_9qjVs",verbose = TRUE)
    #cwis.df <- 	gs_read(cwis.ss, ws = 1, range = NULL, literal = TRUE) %>% as.data.frame()

  #Variable helper table
    cwis.embed.helper.ss <- gs_key("1FaBPQP8Gqwp5sI_0g793G6yjW5XNFbV8ji8N7i9oLjs",verbose = TRUE) 
    cwis.embed.helper.df <- 	gs_read(cwis.embed.helper.ss, ws = 1, range = NULL, literal = TRUE) %>% as.data.frame()
    cwis.embed.helper.df$q.id <- tolower(cwis.embed.helper.df$q.id)

########################################################################################################################################################      
### DATA CLEANING & PREP ###
#{ #SECTION COLLAPSE BRACKET

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
      cwis.df <- ColClassConvert(cwis.df)
    
    #Capitalize First Letter of character variables
      cwis.df[,names(cwis.df) %in% c("year","role","district","school")] <- 
                apply(cwis.df[,names(cwis.df) %in% c("year","role","district","school")], 2, FirstLetterCap_MultElements)
      
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
      
    #Create final data frames: 1. Wide; 2. Long for original CWIS data; 3. Long for impbinary data (both long include all original id variables)
      dat.wide.df <- cbind(cwis.df, impbinary.df)
      dat.idvars.df <- cwis.df[,!names(cwis.df) %in% cwis.varnames.v]
      dat.answer.long.df <- gather(cwis.df, key = "question", value = "answer", cwis.varnames.v, factor_key = FALSE)
      dat.impbinary.long.df <- gather(cbind(dat.idvars.df,impbinary.df), key = "question", value = "answer", names(impbinary.df), factor_key = FALSE) 
      dat.long.df <- rbind(dat.answer.long.df, dat.impbinary.long.df)
      
    #Creating additional useful variables for long data frames
      # Variable for module
        dat.long.df$module <- strsplit(dat.long.df$question, "_" ) %>% sapply(., `[[`, 1)
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
    
  #Variable names & questions, adjusting for collapsed columns
    vars.df <- names(dat.df.wide) %>% as.data.frame(., stringsAsFactors = FALSE)
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
      c("Strongly agree","Agree","Neither agree or disagree","Disagree","Strongly disagree")
    ) %>% as.data.frame
    names(ans.opt.always.df) <- c("ans.num","ans.text.freq","ans.text.agreement")
    ans.opt.always.df[,1] <- ans.opt.always.df[,1] %>% as.character %>% as.numeric
    ans.opt.always.df[,2] <- ans.opt.always.df[,2] %>% as.character
    ans.opt.always.df[,3] <- ans.opt.always.df[,3] %>% as.character
    
#}    

########################################################################################################################################################      
### Powerpoint Configurations ###
    
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
    

########################################################################################################################################################      
### PRODUCING DATA  ###

#{ #SECTION COLLAPSE BRACKET
  
  # District name selection
    #district.names <- readline(prompt = "Enter district names for repeated measures reports or 'all'.")
    district.ids <- "all"
    if(tolower(district.ids) %in% "all" %>% any){district.ids <- dat.wide.df$district %>% unique}else{}   #If user has designated district names as "all", code will create reports for all district names present in the data
    dat.wide.df %>% 
      group_by(district,year) %>% 
      summarize(num.responding.schools = length(unique(school)))  #Check how many schools in each district
      
    # Load Graph Config
      setwd(rproj.dir)
      
      config.slidetypes.df <- read.csv("config_slide types.csv", stringsAsFactors = FALSE)
      config.graphtypes.df <- read.csv("config_graph types.csv", stringsAsFactors = FALSE)
      
      config.graphs.df <- SplitColReshape.ToLong(config.slidetypes.df, id.var = "slide.type.id",split.varname = "slide.graph.type",split.char = ",") %>%
        left_join(., config.graphtypes.df, by = c("slide.graph.type" = "graphtype.id")) %>% 
        filter(!is.na(slide.graph.type))
  
    ###                       ###    
#   ### LOOP "b" BY DISTRICT  ###
    ###                       ###
      
    # Progress bar for loop
      progress.bar.b <- txtProgressBar(min = 0, max = 100, style = 3)
      maxrow.b <- length(district.ids)
  
    #b <- 2 #LOOP TESTER (19 = "Raytown C-2")
    #for(b in c(1,110:115)){   #LOOP TESTER
    graphdata.ls.b <- list()
    for(b in 1:length(district.ids)){   #START OF LOOP BY DISTRICT
    
      # Create data frames for this loop - restrict to district id i
        district.id.b <- district.ids[b]
        dat.wide.df.b <- dat.wide.df[dat.wide.df$district == district.id.b,] 
        dat.answer.long.df.b <- dat.answer.long.df[dat.answer.long.df$district == district.id.b,]
        dat.impbinary.long.df.b <- dat.impbinary.long.df[dat.impbinary.long.df$district == district.id.b,] 
        dat.long.df.b <- dat.long.df[dat.long.df$district == district.id.b,]
  
      ###                         ###
  #   ### LOOP "c" BY GRAPH TYPE  ###
      ###                         ###
        
      #c = 1 #LOOP TESTER: NO LOOPS
      #c = 3 #LOOP TESTER: ONE LOOP VAR
      #c = 8 #LOOP TESTER: TWO LOOP VARS
      config.graphs.ls <- list()
      for(c in 1:dim(config.graphs.df)[1]){
      
        c.list.c <- list(c=c)
        
        #Make data frame with configurations for graphs      
          slide.loop.vars.c <- config.graphs.df$slide.loop.var[c] %>% strsplit(., ",") %>% unlist %>% trimws(., which = "both")
          
          if(length(slide.loop.vars.c) == 0){
            config.graphs.df.c <- config.graphs.df[c,]
          }
          
          if(length(slide.loop.vars.c) != 0){
        
            loop.unique.df <- 
              dat.long.df.b[names(dat.long.df.b) %in% slide.loop.vars.c] %>% 
              lapply(., unique) %>%
              expand.grid(., stringsAsFactors = FALSE) %>%
              as.data.frame #unique items for each loop that will specify graph (e.g. school name)
            loop.unique.df <- loop.unique.df[order(loop.unique.df[,names(loop.unique.df)==slide.loop.vars.c[1]]),] %>% as.data.frame
            names(loop.unique.df) <- slide.loop.vars.c
            
            config.graphs.df.c <- 
              config.graphs.df[  
                rep(
                  c.list.c[["c"]],
                  dim(loop.unique.df)[1]
                )
              ,] %>%
              cbind(.,loop.unique.df)
          }
          config.graphs.ls[[c]] <- config.graphs.df.c
          #print(config.graphs.df.c)
      } ### END OF LOOP "C" BY GRAPH TYPE ###
          
        
      ###                   ###
#     ### LOOP "d" BY GRAPH ###
      ###                   ###
      
      graphdata.ls.d <- list()
      
      #d = 1 #LOOP TESTER 
      #for(d in 1:3){ #LOOP TESTER
      for(d in 1:length(config.graphs.ls)){
        
        config.graphs.df.d <- config.graphs.ls[[d]]
        
        for(e in 1:dim(config.graphs.df.d)[1]){
          
          config.graphs.df.e <- config.graphs.df.d[e,]
          
          #School-level slides should not include an iteration for the District Office 
          if(
              config.graphs.df.e$data.level == "school" && 
              config.graphs.df.e[,names(config.graphs.df.e) == config.graphs.df.e$data.restriction] %>% as.character == "District Office"
          ){next()}
            
          group_by.c <- config.graphs.df.e$data.group.by.var %>% #! will be moved into "c" loop
            strsplit(., ",") %>% 
            unlist
          
          #Create data frame of all possible answers (role, module, year, answer)
            
            if(config.graphs.df.e$graph.x == "school"){
              allx.df <- dat.long.df.b
            }else{
              allx.df <- dat.long.df
            }
            allx <- allx.df %>%
              filter( allx.df$impbinary == 0 ) %>%
              .[,names(allx.df) == config.graphs.df.e$graph.x] %>% 
              as.data.frame %>% 
              apply(., 2, function(x){x[!is.na(x)] %>% unique}) %>%
              as.data.frame %>%
              .[,1]
            
            if(config.graphs.df.e$graph.x != "year"){
              allx <- expand.grid(unique(dat.answer.long.df$year), allx)
            }else{
              allx <- allx %>% as.data.frame()
            }
            
            names(allx) <- group_by.c
            
            #Summarize Function: participation vs. performance 
            summarize.fun <- function(x){
              if(config.graphs.df.e$data.measure == "participation"){
                result <- summarize(x, measure.var =  length(unique(responseid)))
              }
              if(config.graphs.df.e$data.measure == "implementation"){
                result <- x %>% 
                  filter(impbinary == 1) %>%
                  summarize(measure.var = mean(answer, na.rm = TRUE))
              }
              if(config.graphs.df.e$data.measure == "performance"){
                result <- x %>%
                  filter(impbinary == 0, !is.na(answer)) %>%
                  summarize(measure.var = length(unique(responseid)))
              }
            return(result)
            }
          
          #Data restriction function: district vs. school
            graph.data.restriction.fun <- function(x){
              
              if(config.graphs.df.e$data.restriction == ""){
                result <- x
              }
              
              if(config.graphs.df.e$data.restriction != ""){
                result <- 
                  x %>% 
                  filter(
                    x[,names(x) == config.graphs.df.e$data.restriction] == 
                    config.graphs.df.e[,names(config.graphs.df.e) == config.graphs.df.e$data.restriction] %>% as.character
                  )
              }
              return(result)
            }
          
          graphdata.tib.d <-  
            dat.long.df.b %>%
            graph.data.restriction.fun %>%
            group_by(!!! syms(group_by.c)) %>%
            summarize.fun %>%
            left_join(allx, ., by = c(group_by.c))
          graphdata.tib.d$measure.var[is.na(graphdata.tib.d$measure.var)] <- 0
           
          graphdata.ls.index <- length(graphdata.ls.d) + 1
          graphdata.ls.d[[graphdata.ls.index]] <- graphdata.tib.d 

          #print(e)
          #print(config.graphs.df.e[,names(config.graphs.df.e) == config.graphs.df.e$data.restriction] %>% as.character)
          #print(config.graphs.df.e[,names(config.graphs.df.e) == config.graphs.df.e$data.level] %>% as.character)
          #print(graphdata.tib.d)
        
        } ### END OF LOOP "e" BY GRAPH ###
        
          
      } ### END OF LOOP "d" BY GRAPH ###
     
      graphdata.ls.b[[b]] <- graphdata.ls.d
      setTxtProgressBar(progress.bar.b, 100*b/maxrow.b)
    } ### END OF LOOP "b" BY DISTRICT     
    close(progress.bar.b)  

########################################################################################################################################################      
### CHARTS  ###

    ###                       ###    
#   ### LOOP "f" BY DISTRICT  ###
    ###                       ###
    
    for(f in 1:length(graphdata.ls.b)){
     
      #Graph Label Heights defined based on ratio of tallest to shortest columns
        
        create.graph.label.heights <- function(x){
          var <- x$measure.var
          min <- min(var, na.rm = TRUE)
          max <- max(var, na.rm = TRUE)
          height.ratio.threshold <- 10
          height.ratio <- max/min
          
          print(paste("Max: ",max,"  Min: ",min,"  Ratio: ",height.ratio, "  Ratio threshold: ",height.ratio.threshold,sep = ""))
          
          if(height.ratio < height.ratio.threshold){ 
            result <- rep(min/2, length(var)) #if ratio between min and max height below threshold, all labels are minimum height divided by 2
          }else{
            result <- vector(length = length(var))
            above.label.vectorposition <- var/max < 1/height.ratio.threshold
            result[above.label.vectorposition] <-   #labels for columns above threshold, position is height of bar plus 1/10 of max bar height 
              var[above.label.vectorposition] + max/10
            result[result == 0] <-    #labels for columns above threshold, position is height of smallest bar divided by 2
              min(var[!above.label.vectorposition])/2
          }
        print(paste("Graph Label Heights: ",paste(result, collapse = ", "),sep=""))
        return(result)
        }
        
        graph.label.heights.v <- create.graph.label.heights(graphdata.tib)
        graph.label.show.v <- ifelse(graphdata.tib$measure.var != 0,0.8,0)
        graph.x <- config.graphs.df$graph.x[c]
      
      ### GRAPH FORMATOIN WITH GGPLOT2 ###
        
        slide.graph <- 
          ggplot(data = graphdata.tib, 
            aes(x = (!!! syms(graph.x)), y = measure.var, group = year, fill = factor(year)) #graph 1
            #aes(x = role, y = measure.var, group = year, fill = factor(year)) #graph 2
          ) + 
          
          geom_bar(
            alpha = 0.7,
            position = "dodge", 
            stat = "identity"
          ) +
          
          scale_fill_manual(
            values = c(bar_series_fill.cols)
          ) +
          
          #geom_hline(
          #  yintercept = graph.avg.tib %>% as.numeric(),
          #  color = "darkgrey",
          #  linetype = "dashed",
          #  alpha = 0.8
          #) +
          
          #geom_errorbar(
          #  aes(ymin =graph.avg.tib$avg+3, ymax = graph.avg.tib$avg), 
          #  position = position_dodge(width = 1), # 1 is dead center, < 1 moves towards other series, >1 away from it
          #  color = "black", 
          #  width = 1,
          #  size = 1,
          #  alpha = 0.5) +
          
          geom_text(
            aes(                                                          #data labels inside base of columns
              y = graph.label.heights.v, 
              label = graphdata.tib$measure.var %>% format(., nsmall = 0),
              alpha = graph.label.show.v
            ), 
            position = position_dodge(width = 1),
            size = 3,
            color = "black") + 
          
          theme(panel.background = element_blank(),
                panel.grid.major.y = element_blank(),
                panel.grid.major.x = element_line(color = graphgridlinesgrey),
                axis.text.x = element_blank(),
                axis.text.y = element_text(size = 15, color = graphlabelsgrey),
                axis.ticks = element_blank()
          ) +     
          
          coord_flip() 
      
      slide.graph

        
        
    #}  
        
        
      
      
      
     
        
        
        
        
        
      
       
        
        geom_errorbar(
          mapping = aes(ymin = score_state_avg, ymax = score_state_avg), 
          color = "#fae029", 
          width = 0.9,
          size = 1.2) +
        ylim(0,5) +
        labs(x = "", y = "") +
        scale_x_discrete(labels = c("Effective Teaching and Learning",
                                    "Common Formative Assessment",
                                    "Data-based Decision-making",
                                    "Leadership",
                                    "Professional Development") %>% rev
        ) +
       
      
      slide.graph
      
      
    } 
      
      
      
      