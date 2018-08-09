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
    library(rlang)
  
      
### LOAD DATA ###

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
    
#####!FAKE CREATE SCHOOL LEVEL VARIABLE (HIGH, MIDDLE, ELEMENTARY)
      school.level.df <- data.frame( 
        school.id = cwis.df$school.id %>% unique,
        school.level = sample(c("high","middle","elem."),length(unique(cwis.df$school.id)), replace = TRUE),
        stringsAsFactors = FALSE
      )
      
      cwis.df <- left_join(cwis.df,school.level.df, by = "school.id")
      
    #Capitalize First Letter of character variables
      cwis.df[,names(cwis.df) %in% c("year","role","district","school")] <- 
        apply(cwis.df[,names(cwis.df) %in% c("year","role","district","school")], 2, FirstLetterCap_MultElements)
      
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
      c("Strongly agree","Agree","Neither agree or disagree","Disagree","Strongly disagree")
    ) %>% as.data.frame
    names(ans.opt.always.df) <- c("ans.num","ans.text.freq","ans.text.agreement")
    ans.opt.always.df[,1] <- ans.opt.always.df[,1] %>% as.character %>% as.numeric
    ans.opt.always.df[,2] <- ans.opt.always.df[,2] %>% as.character
    ans.opt.always.df[,3] <- ans.opt.always.df[,3] %>% as.character
    
}    

########################################################################################################################################################      
### PRODUCING GRAPH DATA  ###

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
        left_join(., config.graphtypes.df, by = c("slide.graph.type" = "graph.type.id")) %>% 
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
      
      loop.start.time.b <- Sys.time()
      if(b == 1){print("CALCULATING GRAPH SOURCE DATA FRAMES...")}
      
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
          
        
      ###                         ###
#     ### LOOP "d" BY GRAPH TYPE  ###
      ###                         ###
      
      graphdata.ls.d <- list()
      
      #d = 1 #LOOP TESTER 
      #for(d in 1:3){ #LOOP TESTER
      for(d in 1:length(config.graphs.ls)){
        
        ###                   ###
#       ### LOOP "d" BY GRAPH ###
        ###                   ###
        
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
          
          #Create data frame"all.cats.df" of all possible answers for x-axis (role, module, year, answer)
            
            #if(config.graphs.df.e$graph.cat.varname == "school"){
            #  print(c(d,e))
            #  config.graphs.df.e$graph.cat.varname <- "school.level" 
            #  all.cats.df <- dat.long.df
            #}else{
            #  all.cats.df <- dat.long.df
            #}
            
            all.cats <- dat.long.df %>%
              filter( dat.long.df$impbinary == 0 ) %>%
              .[,names(dat.long.df) == config.graphs.df.e$graph.cat.varname] %>% 
              as.data.frame %>% 
              apply(., 2, function(x){x[!is.na(x)] %>% unique}) %>%
              as.data.frame %>%
              .[,1]
            
            if(config.graphs.df.e$graph.cat.varname != "year"){
              all.cats.df <- expand.grid(unique(dat.answer.long.df$year), all.cats)
            }else{
              all.cats.df <- all.cats %>% as.data.frame()
            }
            
            names(all.cats.df) <- group_by.c
            
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
            left_join(all.cats.df, ., by = c(group_by.c))
          graphdata.tib.d$measure.var[is.na(graphdata.tib.d$measure.var)] <- 0
           
          graphdata.ls.index <- length(graphdata.ls.d) + 1
          graphdata.ls.d[[graphdata.ls.index]] <- list(configs = config.graphs.df.e, graphdata = graphdata.tib.d)

          #print(e)
          #print(config.graphs.df.e[,names(config.graphs.df.e) == config.graphs.df.e$data.restriction] %>% as.character)
          #print(config.graphs.df.e[,names(config.graphs.df.e) == config.graphs.df.e$data.level] %>% as.character)
          #print(graphdata.tib.d)
        
        } ### END OF LOOP "e" BY GRAPH ###
        
          
      } ### END OF LOOP "d" BY GRAPH ###
     
      graphdata.ls.b[[b]] <- graphdata.ls.d
      
      graphdata.ls.b[[b]]['loop.duration'] <- Sys.time()-loop.start.time.b  #100*b/maxrow.b
      est.time.remaining <- (lapply(graphdata.ls.b, function(x){x['loop.duration']}) %>% unlist %>% mean())*(maxrow.b-b)
      print(paste("Estimated time remaining: ",est.time.remaining," sec",sep = ""))
      setTxtProgressBar(progress.bar.b, 100*b/maxrow.b)
    } ### END OF LOOP "b" BY DISTRICT     
    close(progress.bar.b)  
#} #END OF SECTION COLLAPSE BRACKET

########################################################################################################################################################      
### GRAPH & POWERPOINT CONFIGURATIONS ###
    
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
### GRAPHS  ###

    ###                       ###    
#   ### LOOP "f" BY DISTRICT  ###
    ###                       ###
    
    slide.graphs.ls.f <- list()
    progress.bar.f <- txtProgressBar(min = 0, max = 100, style = 3)
    
    #f <- 1 #LOOP TESTER
    #for(f in 1:2){ #LOOP TESTER
    for(f in 1:length(graphdata.ls.b)){
      
      if(f == 1){print("FORMING GRAPHS IN GGPLOT...")}
      
      ###                       ###    
  #   ### LOOP "g" BY GRAPH     ###
      ###                       ###
    
      slide.graphs.ls.g <- list()
      #progress.bar.g <- txtProgressBar(min = 0, max = 100, style = 3)
      #g <- 1  #LOOP TESTER
      #for(g in 1:2) #LOOP TESTER
      for(g in 1:length(graphdata.ls.b[[f]]))
        local({ #Necessary to avoid annoying and confusing ggplot lazy evaluation problem (see journal)
        g<-g #same as above
        
        ### GRAPH INPUTS FOR GGPLOT ###
        
          graphdata.df.g <- graphdata.ls.b[[f]][[g]]["graphdata"] %>% as.data.frame()
          config.graphs.df.g <- graphdata.ls.b[[f]][[g]]["configs"] %>% as.data.frame()
          
          #Graph Data LabelS 
          
            #Graph Label Heights (defined based on ratio of tallest to shortest columns)
              create.graph.label.heights.fun <- function(df, measure.var, height.ratio.threshold){
              
              if(!is.data.frame(as.data.frame(df))){stop("Input cannot be coerced into data frame.")}
              
              df <- as.data.frame(df)
              
              var <- df[,names(df) == measure.var] %>% as.matrix %>% as.vector(.,mode = "numeric")
              min <- min(var, na.rm = TRUE)
              max <- max(var, na.rm = TRUE)
              height.ratio.threshold <- height.ratio.threshold
              height.ratio <- max/min
              
              #print(paste("Max: ",max,"  Min: ",min,"  Ratio: ", height.ratio, "  Ratio threshold: ",height.ratio.threshold,sep = ""))
              
              if(height.ratio < height.ratio.threshold){ 
                result <- rep(min/2, length(var)) #if ratio between min and max height below threshold, all labels are minimum height divided by 2
              }
              
              if(min == 0 | height.ratio >= height.ratio.threshold){
                result <- vector(length = length(var))
                above.label.vectorposition <- var/max < 1/height.ratio.threshold
                result[above.label.vectorposition] <-   #labels for columns above threshold, position is height of bar plus 1/10 of max bar height 
                  var[above.label.vectorposition] + max/10
                result[result == 0] <-    #labels for columns above threshold, position is height of smallest bar divided by 2
                  min(var[!above.label.vectorposition])/2
              }
              #print(paste("Graph Label Heights: ",paste(result, collapse = ", "),sep=""))
              return(result)
            }
            
            graph.label.text.v <- graphdata.df.g$graphdata.measure.var %>% format(., nsmall = 0) %>% trimws(., which = "both")
            graph.label.heights.v <- create.graph.label.heights.fun(df = graphdata.df.g, measure.var = "graphdata.measure.var", height.ratio.threshold = 10)
            graph.labels.show.v <- ifelse(graphdata.df.g$graphdata.measure.var != 0, 0.8, 0)
            
          #Graph Orientation
            graph.coord.flip.g <- ifelse(config.graphs.df.g$configs.graph.type.orientation == "bar", TRUE, FALSE)
            
          #Graph Category Names
            graph.cat.varname <- paste("graphdata.",config.graphs.df.g$configs.graph.cat.varname,sep="")
            graph.cat.v <- graphdata.df.g[[graph.cat.varname]]
            
      
        ### GRAPH FORMATION WITH GGPLOT2 ###
          
          slide.graph.g <- 
            ggplot(data = graphdata.df.g, 
              #aes_(x = graphdata.df.g$graphdata.school, y = graphdata.df.g$graphdata.measure.var, group = graphdata.df.g$graphdata.year, fill = factor(graphdata.df.g$graphdata.year)) #graph 1
              aes(x = graphdata.df.g[[graph.cat.varname]], 
                   y = graphdata.measure.var, 
                   group = graphdata.year, 
                   fill = factor(graphdata.year)
              ) #graph 1
            ) + 
            
            geom_bar(
              alpha = 0.7,
              position = "dodge", 
              stat = "identity"
            ) +
            
            theme(panel.background = element_blank(),
                  panel.grid.major.y = element_blank(),
                  panel.grid.major.x = element_blank(),
                  axis.text.x = element_text(size = 12, color = graphlabelsgrey),
                  axis.text.y = element_blank(),
                  axis.ticks = element_blank(),
                  axis.title = element_blank()
            ) +     
            
            guides(fill = FALSE) +
            
            scale_fill_manual(
              values = c(bar_series_fill.cols)
            ) +
            
            #Data labels inside base of columns
            geom_text( #!FORMATTING: NUMBER OF DECIMAL PLACES, PERCENTAGE SIGNS
              aes(                                                          
                y = graph.label.heights.v, 
                label = graph.label.text.v,
                alpha = graph.labels.show.v
              ), 
              position = position_dodge(width = 1),
              size = 3,
              color = "black",
              show.legend = FALSE)
          
          #Graph Orientation
            if(graph.coord.flip.g){
              slide.graph.g <- slide.graph.g +
                coord_flip() +
                theme(
                  axis.text.x = element_blank(),
                  axis.text.y = element_text(size = 15, color = graphlabelsgrey)
                )
            }
          
            
            
        #Sys.sleep(0.1)
        #print(slide.graph.g)
        
        #slide.graphs.ls.index <- length(slide.graphs.ls)+1
        slide.graphs.ls.g[[g]] <<- slide.graph.g
        
      #setTxtProgressBar(progress.bar.g, 100*g/length(graphdata.ls.b[[f]]))
    })  ### END OF LOOP "g" BY GRAPH ###


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
      
      #geom_errorbar(
      #  mapping = aes(ymin = score_state_avg, ymax = score_state_avg), 
      #  color = "#fae029", 
      #  width = 0.9,
      #  size = 1.2) +
      #  ylim(0,5) +
      #  labs(x = "", y = "") +
      #  scale_x_discrete(labels = c("Effective Teaching and Learning",
      #                              "Common Formative Assessment",
      #                              "Data-based Decision-making",
      #                              "Leadership",
      #                              "Professional Development") %>% rev
      #  ) +
        
 
    slide.graphs.ls.f[[f]] <- slide.graphs.ls.g
    setTxtProgressBar(progress.bar.f, 100*f/length(graphdata.ls.b))
    
  } ### END OF LOOP "f" BY DISTRICT

########################################################################################################################################################      
### POWERPOINT SLIDE CREATION  ###        
        
    ###                       ###    
#   ### LOOP "h" BY DISTRICT  ###
    ###                       ###
    
    ppt.ls.h <- list()
    progress.bar.h <- txtProgressBar(min = 0, max = 100, style = 3)
    
    h <- 1 #LOOP TESTER
    #for(f in 1:2){ #LOOP TESTER
    for(h in 1:length(slide.graphs.ls.f)){
      ppt.h <- pptx(template = target.path.h)
      
      
      
      target.filename.h <-  paste(target.dir,
                                  "/",
                                  "CWIS Report_",
                                  district.name.i,
                                  "_",
                                  school.name.i,
                                  gsub(":",".",Sys.time()),
                                  ".pptx", sep="") 
      file.copy(template.file, target.file.j)
      
      ppt.ls.h[[h]]["target.filename"] <- target.path.h
      ppt.ls.h[[h]]["ppt"] <- ppt.h
    }     

    
########################################################################################################################################################      
### WRITE POWERPOINTS TO FILE  ###
    
    dir.create(target.dir)  
    lapply(ppt.ls.h, function(x){writeDoc(x, file = x["target.filename"])})
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     
        
#create.slide.graph <- function(input){
#  result <- input["graphdata"]
#  return(result)
#input["graphdata"] %>% as.data.frame()%>% print

#}

#input <- graphdata.ls.b[[6]][[1]]
#lapply(input, create.slide.graph)

        
    
 
      
      
      