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
    
    #Rearrange data columns
      cwis.df <- cwis.df[,   # CWIS response variables last, others first
          c(which(!grepl("_", names(cwis.df))),
            grep("_", names(cwis.df)))
        ]
      
      cwis.df <-  cwis.df[, #Put "responseid" in first column
                    c(grep("responseid", names(cwis.df)),which(!grepl("responseid", names(cwis.df))))
                    ]
    
    #Column Class Conversions
      cwis.df <- ColClassConvert(cwis.df)
    
    #Capitalize First Letter of character variables
      cwis.df[,names(cwis.df) %in% c("year","role","district","school")] <- 
                apply(cwis.df[,names(cwis.df) %in% c("year","role","district","school")], 2, FirstLetterCap_MultElements)
      
  #Add useful variables for analysis 
    
    #Useful vectors for selecting cwis answer variables
      cwis.vars.v <- grep("_", names(cwis.df))
      cwis.varnames.v <- names(cwis.df)[grep("_", names(cwis.df))]
      cwis.areas.v <- cwis.varnames.v %>% strsplit(.,"_") %>% sapply(., `[[`, 1) %>% unique
    
    #Create dummy variables for implementation (4 or above = 1)
      impbinary.df <- cwis.df[,cwis.vars.v] %>% apply(., c(1:2), function(x){ifelse(x>=4,1,0)}) %>% as.data.frame
      names(impbinary.df) <- paste(cwis.varnames.v,"_impbinary",sep="") 
    
    #Create school.id variable which is concatenation of school and district
      cwis.df$school.id <- paste(cwis.df$district, cwis.df$school,sep = "_") %>% tolower
      cwis.df$school.id <- gsub("\\/"," ",cwis.df$school.id) #in case there is a slash in the school name itself, this replaces it so file storage for ppt works properly
      
    #Create final data frame: merge cwis.df and impbinary.df
      dat.df <- cbind(cwis.df, impbinary.df) 
        
    #Loop: state average implementation rates [a], results in imp.state.df
      imp.state.df <- data.frame(cwis.area = cwis.areas.v)
      #a <- 1 # LOOP TESTER
      for(a in 1:length(cwis.areas.v)){
        imp.state.df$imp.rate[imp.state.df[,1]==cwis.areas.v[a]] <- 
          impbinary.df[,grep(cwis.areas.v[a], names(impbinary.df))] %>%
          apply(., 2, function(x){mean(x, na.rm = TRUE)}) %>% 
          mean() 
      }

## BUILD VARIABLE/QUESTION LOOKUP TABLE
    
  #Variable names & questions, adjusting for collapsed columns
    vars.df <- names(dat.df) %>% as.data.frame(., stringsAsFactors = FALSE)
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
### PRODUCING DATA, TABLES, & CHARTS ###

#{ #SECTION COLLAPSE BRACKET
  
  # District name selection
    #district.names <- readline(prompt = "Enter district names for repeated measures reports or 'all'.")
    district.ids <- "all"
    if(tolower(district.ids) %in% "all" %>% any){district.ids <- dat.df$district %>% unique}else{}   #If user has designated district names as "all", code will create reports for all district names present in the data
    cwis.df %>% group_by(district) %>% summarize(count = length(unique(school))) #Check how many schools in each district
  
  # Load Graph Config
    setwd(rproj.dir)
    
    config.graphtypes.df <- read.csv("config_graph types.csv", stringsAsFactors = FALSE)
    config.graphs.df <- read.csv("config_graphs.csv", stringsAsFactors = FALSE) %>% 
      left_join(., config.graphtypes.df, by = c("graph.type" = "graphtype.id"))
    
  ### LOOP BY DISTRICT ###
    
  # Progress bar for loop
    #progress.bar.b <- txtProgressBar(min = 0, max = 100, style = 3)
    #maxrow.b <- length(district.ids)

  b <- 19 #LOOP TESTER
  #for(b in c(1,110:115)){   #LOOP TESTER
  #for(b in 1:length(district.names)){   #START OF LOOP BY DISTRICT
  
    # Create data frame for this loop - restrict to responses from district name i
      district.id.b <- district.ids[b]
  
      if(district.id.b %in% c("district office","other") %>% any){next()}else{}
      
      dat.df.b <- dat.df[dat.df$district == district.id.b & !dat.df$school %in% c("District Office","Other"),] 
      #district.name.b <- dat.df.b$district %>% unique
    
    ### LOOP BY GRAPH ###
      
      c = 1 #LOOP TESTER
      #for(c in 1:dim(config)){
        
        #Read necessary pieces of config file  
          group_by.c <- config.graphs.df$data.group.by.var[c] %>% 
            strsplit(., ",") %>% 
            unlist
          
          graph.avg.group.by.var.c <- 
            config.graphs.df$graph.avg.group.by.var[c] %>% 
            strsplit(., ",") %>% 
            unlist
        
        #Defining graph dataset
          
          #dat.df.b$role %>% unique %>% rep(., 2) %>% as.data.frame() %>% left_join(., graphdata.tib, by = c("." = "role"))
          graphdata.allcombinations.df <- dat.df.b[,names(dat.df.b) %in% group_by.c[2:length(group_by.c)]] %>% unique()
          
          graph.data.level.restriction.fun <- function(x){
            if(config.graphs.df$data.level[c] == "district"){
              result <- x
            }else{
              result <- x %>% filter(school == x$school %>% unique %>% .[c]) #! will need to change so not indexing on c
            }
            return(result)
          }
            
          graphdata.tib <-  
            dat.df.b %>% 
            group_by(!!! syms(group_by.c)) %>% 
            summarize(measure.var = length(unique(responseid))) %>%
            graph.data.level.restriction.fun %>%
            left_join(graphdata.allcombinations.df,., by = c("year","role"), all.x = TRUE)
        
        #Define graph average line(s)  
          graph.avg.attributes.fun <- function(x){
            if(!is.null(graph.avg.group.by.var.c)){
              result <- group_by(x, !!! syms(graph.avg.group.by.var.c)) 
            }else{
              result <- as.data.frame(x) %>% 
                select(count)
            }
            return(result)
          }
          
          graph.avg <- 
            dat.df %>% 
            filter(year != "Baseline") %>%
            group_by(!!! syms(group_by.c)) %>% 
            summarize(count = length(unique(responseid))) %>% 
            graph.avg.attributes.fun %>%
            summarize(avg = mean(count))  
       
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
          
          graph.label.heights <- create.graph.label.heights(graphdata.tib)
        
        #Graphs
          
          graph.x <- config.graphs.df$graph.x[c]
          
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
            #  yintercept = graph.avg %>% as.numeric(),
            #  color = "darkgrey",
            #  linetype = "dashed",
            #  alpha = 0.8
            #) +
            
            geom_errorbar(
              aes(y = graph.avg$avg %>% as.numeric)
            ) +
            
            geom_text(
              aes(                                                          #data labels inside base of columns
                y = graph.label.heights, 
                label = graphdata.tib$measure.var %>% format(., nsmall = 1)
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
        aes(ymin =graph.avg+3, ymax = graph.avg), 
        position = position_dodge(width = 1), # 1 is dead center, < 1 moves towards other series, >1 away from it
        color = "black", 
        width = 1,
        size = 1,
        alpha = 0.5) +
        
        
        
        
        
      
       
        
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
      
      
      
      
      
      
 ########################################################################################################################################     
      
      s2.mtx <- table(dat.df.i$role %>% as.character) %>% as.matrix
      s2.df <- cbind(row.names(s2.mtx),s2.mtx[,1]) %>% as.data.frame
      names(s2.df) <- c("Role","# Responses")
      rownames(s2.df) <- c()
      s2.df$Role <- s2.df$Role %>% as.character #convert factor to character
      s2.df$`# Responses` <- s2.df$`# Responses` %>% as.character %>% as.numeric #convert factor to numeric
      s2.df <- rbind(s2.df, c("Total", sum(s2.df[,2] %>% as.character %>% as.numeric))) #add "Total" row
      s2.roworder.df <- c("Special Educator",
                          "Classroom Teacher",
                          "Building Administrator",
                          "School Counselor",
                          "Instructional Coach",
                          "School Social Worker",
                          "Media Specialist",
                          "Other",
                          "Total") %>% as.data.frame(., stringsAsFactors = FALSE)
      names(s2.roworder.df) <- "Role"
      s2.outputs.df <- full_join(s2.roworder.df %>% as.data.frame,s2.df, by = "Role")
      s2.outputs.df[is.na(s2.outputs.df)] <- 0  #replace any NA with 0
    }      
    #S3 Table for slide 3 "Overall Scale Performance"
    {
      #Define input variables for each column
      
      fig.categories <- c("etl","cfa","dbdm","lead","pd")
      fig.outputs.ls <- list()
      
      #k <- 1 #for testing loop
      
      for(k in 1:length(fig.categories)){ #START OF BY CATEGORY LOOP, I.E. EACH COLUMN REPRESENTS A CATEGORY IN S3
        
        #Subset data
        fig.cat.k <- fig.categories[k]
        cat.calc.vars.k <- vars.df$q.id[vars.df$slide.category %>% grepl(fig.cat.k,.)]
        cat.df.k <- dat.df.i[, names(dat.df.i) %in% cat.calc.vars.k]  #Dataset containing all input values for school i
        cat.df.k.state <- dat.df[, names(dat.df.i) %in% cat.calc.vars.k]    #Dataset containing all input values for state
        
        #Re-coding variables as numeric
        ordinal.to.numeric.f <- function(x){
          recode(x, Never=1, Sometimes = 2, `About half the time` = 3 , `Most of the time` = 4, Always = 5,
                 `Strongly disagree` = 1, Disagree = 2, `Neither agree or disagree` = 3, Agree = 4, `Strongly agree` = 5,
                 `BLANK` = NA_real_)
        }
        
        #Calculate average of re-coded variables
        avg.cat.k <- apply(cat.df.k, 2, ordinal.to.numeric.f) %>% mean(., na.rm = TRUE) #for school
        avg.cat.k.state <- apply(cat.df.k.state, 2, ordinal.to.numeric.f) %>% mean(., na.rm = TRUE) #for school
        
        #Store result along with category name
        fig.outputs.ls[[k]] <- c(fig.cat.k, avg.cat.k, avg.cat.k.state) %>% as.matrix
        
        
      }  #END OF BY CATEGORY LOOP  
      
      s3.outputs.df <- do.call(cbind, fig.outputs.ls ) %>% t %>% as.data.frame
      names(s3.outputs.df) <- c("category","score_school_avg","score_state_avg")
      s3.outputs.df$category <- s3.outputs.df$category %>% as.character
      s3.outputs.df$score_school_avg <- s3.outputs.df$score_school_avg %>% as.character %>% as.numeric %>% round(., digits = 2)
      s3.outputs.df$score_state_avg <- s3.outputs.df$score_state_avg %>% as.character %>% as.numeric %>% round(., digits = 2)
      s3.outputs.df$score_school_avg[is.na(s3.outputs.df$score_school_avg)] <- 0
      
    }    
    #S6 Table for slide 6 "ETLP Scale Performance"
    {
      s6.headers.v <- paste(c(1:8),
                            c("learning targets",
                              "students assess",
                              "students identify",
                              "feedback to targets",
                              "student to student feedback",
                              "students state criteria",
                              "instruction state standards",
                              "student reviews cfa"),sep = ". "
      )
      
      s6.varnames.v <- vars.df$q.id[!is.na(vars.df$etlp)] 
      s6.var.df <- cbind(1:length(s6.headers.v),s6.varnames.v,s6.headers.v) %>% as.data.frame(., stringsAsFactors = FALSE)
      names(s6.var.df) <- c("num","s6.varname","s6.header")
      
      s6.ls <- list()
      
      for(j in 1:length(s6.varnames.v)){
        s6.varname.j <- s6.var.df$s6.varname[j]
        s6.df.j <- table(dat.df.i[,names(dat.df.i)==s6.varname.j]) %>% as.matrix %>% as.data.frame
        s6.df.j$ans.opt <- row.names(s6.df.j)
        names(s6.df.j) <- c(s6.varname.j, "ans.opt")
        s6.ls[[j]] <- s6.df.j
      }
      
      s6.outputs.df <- Reduce(function(df1,df2) full_join(df1, df2,by = "ans.opt"), s6.ls)
      s6.outputs.df <- s6.outputs.df[,c(which(names(s6.outputs.df)=="ans.opt"),1,3:length(names(s6.outputs.df)))] # re-order columns
      s6.outputs.df <- full_join(s6.outputs.df, ans.opt.always.df, by = c("ans.opt"="ans.text.freq"))
      s6.outputs.df[is.na(s6.outputs.df)] <- 0
      s6.outputs.df <- s6.outputs.df[order(s6.outputs.df$ans.num, decreasing = TRUE),
                                     c(which(grepl("ans.opt",names(s6.outputs.df))),which(!grepl("ans",names(s6.outputs.df))))]
      s6.outputs.df <- s6.outputs.df[s6.outputs.df$ans.opt != "",]
      names(s6.outputs.df) <- c("Answer option",
                                "1. learning targets",
                                "2. students assess",
                                "3. students identify",
                                "4. feedback to targets",
                                "5. student to student feedback",
                                "6. students state criteria",
                                "7. instruction state standards",
                                "8. student reviews cfa")
    }
    #S7 Bar Chart "ETLP Scale Rates of Implementation"
    {
      s7.headers.v <- s6.headers.v
      s7.varnames.v <- s6.varnames.v
      s7.ls <- list()
      
      for(m in 1:length(s7.varnames.v)){
        s7.m <- (100*(s6.outputs.df[1:2,m+1] %>% sum)/ (s6.outputs.df[1:5,m+1] %>% sum)) %>% round(., digits = 0)
        s7.ls[[m]] <- s7.m
      }
      
      s7.outputs.df <- do.call(rbind, s7.ls) %>% cbind(s7.headers.v, .) %>% as.data.frame
      names(s7.outputs.df) <- c("category","score_school_rate")
      s7.outputs.df$category <- s7.outputs.df$category %>% as.character
      s7.outputs.df$score_school_rate <- s7.outputs.df$score_school_rate %>% as.character %>% as.numeric
      s7.outputs.df$score_school_rate[is.na(s7.outputs.df$score_school_rate)] <- 0
    }
    #S9 Table "CFA Scale Performance"
    {
      s9.headers.v <- paste(c(1:4),
                            c("use cfa",
                              "all in cfa",
                              "student reviews cfa",
                              "cfa used to plan"),sep = ". "
      )
      
      s9.varnames.v <- vars.df$q.id[vars.df$slide.category %>% grepl("cfa",.)]
      
      dat.df.i.s9 <- dat.df.i[,names(dat.df.i) %in% s9.varnames.v]
      
      s9.ls <- list()
      
      for(j in 1:length(s9.varnames.v)){
        s9.varname.j <- s9.varnames.v[j]
        s9.df.j <- table(dat.df.i.s9[,names(dat.df.i.s9)==s9.varname.j]) %>% as.matrix %>% as.data.frame
        s9.df.j$ans.opt <- row.names(s9.df.j)
        names(s9.df.j) <- c(s9.varname.j, "ans.opt")
        s9.ls[[j]] <- s9.df.j
      }
      
      s9.outputs.df <- Reduce(function(df1,df2) full_join(df1, df2,by = "ans.opt"), s9.ls)
      s9.outputs.df <- s9.outputs.df[,c(which(names(s9.outputs.df)=="ans.opt"),1,3:length(names(s9.outputs.df)))] # re-order columns
      s9.outputs.df <- full_join(s9.outputs.df, ans.opt.always.df, by = c("ans.opt"="ans.text.freq"))
      s9.outputs.df <- s9.outputs.df[!is.na(s9.outputs.df$ans.text.agreement),]
      s9.outputs.df[is.na(s9.outputs.df)] <- 0
      s9.outputs.df <- s9.outputs.df[order(s9.outputs.df$ans.num, decreasing = TRUE),
                                     c(which(grepl("ans.opt",names(s9.outputs.df))),which(!grepl("ans",names(s9.outputs.df))))]
      names(s9.outputs.df) <- c("Answer option",s9.headers.v)
    }
    #s10 Bar Chart "CFA Scale Rates of Implementation"
    {
      s10.headers.v <- s9.headers.v
      s10.varnames.v <- s9.varnames.v
      s10.ls <- list()
      
      for(m in 1:length(s10.varnames.v)){
        s10.m <- (100*(s9.outputs.df[1:2,m+1] %>% sum)/(s9.outputs.df[1:nrow(s9.outputs.df),m+1] %>% sum)) %>% round(., digits = 0)
        s10.ls[[m]] <- s10.m
      }
      
      s10.outputs.df <- do.call(rbind, s10.ls) %>% cbind(s10.headers.v, .) %>% as.data.frame
      names(s10.outputs.df) <- c("category","score_school_rate")
      s10.outputs.df$category <- s10.outputs.df$category %>% as.character
      s10.outputs.df$score_school_rate <- s10.outputs.df$score_school_rate %>% as.character %>% as.numeric
      s10.outputs.df$score_school_rate[is.na(s10.outputs.df$score_school_rate)] <- 0
    }
    #S12 Table "DBDM Scale Performance"
    {
      s12.headers.v <- c("team reviews data",
                         "team positive",
                         "effective teaming practices",
                         "data determine practices",
                         "visual representations") %>%
        paste(c(1:length(.)),.,sep = ". ")
      
      
      s12.varnames.v <- vars.df$q.id[vars.df$slide.category %>% grepl("dbdm",.)]
      
      dat.df.i.s12 <- dat.df.i[,names(dat.df.i) %in% s12.varnames.v]
      
      s12.ls <- list()
      
      for(j in 1:length(s12.varnames.v)){
        s12.varname.j <- s12.varnames.v[j]
        s12.df.j <- table(dat.df.i.s12[,names(dat.df.i.s12)==s12.varname.j]) %>% as.matrix %>% as.data.frame
        s12.df.j$ans.opt <- row.names(s12.df.j)
        names(s12.df.j) <- c(s12.varname.j, "ans.opt")
        s12.ls[[j]] <- s12.df.j
      }
      
      s12.outputs.df <- Reduce(function(df1,df2) full_join(df1, df2,by = "ans.opt"), s12.ls)
      s12.outputs.df <- s12.outputs.df[,c(which(names(s12.outputs.df)=="ans.opt"),1,3:length(names(s12.outputs.df)))] # re-order columns
      s12.outputs.df <- full_join(s12.outputs.df, ans.opt.always.df, by = c("ans.opt"="ans.text.freq"))
      s12.outputs.df <- s12.outputs.df[!is.na(s12.outputs.df$ans.text.agreement),]
      s12.outputs.df[is.na(s12.outputs.df)] <- 0
      s12.outputs.df <- s12.outputs.df[s12.outputs.df$ans.opt != "",]
      s12.outputs.df <- s12.outputs.df[order(s12.outputs.df$ans.num, decreasing = TRUE),
                                       c(which(grepl("ans.opt",names(s12.outputs.df))),which(!grepl("ans",names(s12.outputs.df))))]
      names(s12.outputs.df) <- c("Answer option",s12.headers.v)
    } 
    #S13 Bar Chart "DBDM Scale Rates of Implementation"
    {
      s13.headers.v <- s12.headers.v
      s13.varnames.v <- s12.varnames.v
      s13.ls <- list()
      
      for(m in 1:length(s13.varnames.v)){
        s13.m <- (100*(s12.outputs.df[1:2,m+1] %>% sum)/(s12.outputs.df[1:nrow(s12.outputs.df),m+1] %>% sum)) %>% round(., digits = 0)
        s13.ls[[m]] <- s13.m
      }
      
      s13.outputs.df <- do.call(rbind, s13.ls) %>% cbind(s13.headers.v, .) %>% as.data.frame
      names(s13.outputs.df) <- c("category","score_school_rate")
      s13.outputs.df$category <- s13.outputs.df$category %>% as.character
      s13.outputs.df$score_school_rate <- s13.outputs.df$score_school_rate %>% as.character %>% as.numeric
      s13.outputs.df$score_school_rate[is.na(s13.outputs.df$score_school_rate)] <- 0
    }
    #S15 Table "Leadership Scale Performance"
    {
      s15.headers.v <- c("leaders manage",
                         "teacher to teacher feedback",
                         "leader committed",
                         "leader active") %>%
        paste(c(1:length(.)),.,sep = ". ")
      
      
      s15.varnames.v <- vars.df$q.id[vars.df$slide.category %>% grepl("lead",.)]
      
      dat.df.i.s15 <- dat.df.i[,names(dat.df.i) %in% s15.varnames.v]
      
      s15.ls <- list()
      
      for(j in 1:length(s15.varnames.v)){
        s15.varname.j <- s15.varnames.v[j]
        s15.df.j <- table(dat.df.i.s15[,names(dat.df.i.s15)==s15.varname.j]) %>% as.matrix %>% as.data.frame
        s15.df.j$ans.opt <- row.names(s15.df.j)
        names(s15.df.j) <- c(s15.varname.j, "ans.opt")
        s15.ls[[j]] <- s15.df.j
      }
      
      s15.outputs.df <- Reduce(function(df1,df2) full_join(df1, df2,by = "ans.opt"), s15.ls)
      s15.outputs.df <- s15.outputs.df[,c(which(names(s15.outputs.df)=="ans.opt"),1,3:length(names(s15.outputs.df)))] # re-order columns
      s15.outputs.df <- full_join(s15.outputs.df, ans.opt.always.df, by = c("ans.opt"="ans.text.agreement"))
      s15.outputs.df <- s15.outputs.df[!is.na(s15.outputs.df$ans.text.freq),]
      s15.outputs.df[is.na(s15.outputs.df)] <- 0
      s15.outputs.df <- s15.outputs.df[order(s15.outputs.df$ans.num, decreasing = TRUE),
                                       c(which(grepl("ans.opt",names(s15.outputs.df))),which(!grepl("ans",names(s15.outputs.df))))]
      names(s15.outputs.df) <- c("Answer option",s15.headers.v)
    } 
    #S16 Bar Chart "Leadership Scale Rates of Implementation"
    {
      s16.headers.v <- s15.headers.v
      s16.varnames.v <- s15.varnames.v
      s16.ls <- list()
      
      for(m in 1:length(s16.varnames.v)){
        s16.m <- (100*(s15.outputs.df[1:2,m+1] %>% sum)/(s15.outputs.df[1:nrow(s15.outputs.df),m+1] %>% sum)) %>% round(., digits = 0)
        s16.ls[[m]] <- s16.m
      }
      
      s16.outputs.df <- do.call(rbind, s16.ls) %>% cbind(s16.headers.v, .) %>% as.data.frame
      names(s16.outputs.df) <- c("category","score_school_rate")
      s16.outputs.df$category <- s16.outputs.df$category %>% as.character
      s16.outputs.df$score_school_rate <- s16.outputs.df$score_school_rate %>% as.character %>% as.numeric
      s16.outputs.df$score_school_rate[is.na(s16.outputs.df$score_school_rate)] <- 0
    }
    #S18 Table "PD Scale Performance"
    {
      s18.headers.v <- c("pd instruction",
                         "coaching instruction",
                         "pd monitor",
                         "teacher feedback instruction") %>%
        paste(c(1:length(.)),.,sep = ". ")
      
      
      s18.varnames.v <- vars.df$q.id[vars.df$slide.category %>% grepl("pd",.)]
      
      dat.df.i.s18 <- dat.df.i[,names(dat.df.i) %in% s18.varnames.v]
      
      s18.ls <- list()
      
      for(j in 1:length(s18.varnames.v)){
        s18.varname.j <- s18.varnames.v[j]
        s18.df.j <- table(dat.df.i.s18[,names(dat.df.i.s18)==s18.varname.j]) %>% as.matrix %>% as.data.frame
        s18.df.j$ans.opt <- row.names(s18.df.j)
        names(s18.df.j) <- c(s18.varname.j, "ans.opt")
        s18.ls[[j]] <- s18.df.j
      }
      
      s18.outputs.df <- Reduce(function(df1,df2) full_join(df1, df2,by = "ans.opt"), s18.ls)
      s18.outputs.df <- s18.outputs.df[,c(which(names(s18.outputs.df)=="ans.opt"),1,3:length(names(s18.outputs.df)))] # re-order columns
      s18.outputs.df <- full_join(s18.outputs.df, ans.opt.always.df, by = c("ans.opt"="ans.text.agreement"))
      s18.outputs.df <- s18.outputs.df[!is.na(s18.outputs.df$ans.text.freq),]
      s18.outputs.df[is.na(s18.outputs.df)] <- 0
      s18.outputs.df <- s18.outputs.df[order(s18.outputs.df$ans.num, decreasing = TRUE),
                                       c(which(grepl("ans.opt",names(s18.outputs.df))),which(!grepl("ans",names(s18.outputs.df))))]
      names(s18.outputs.df) <- c("Answer option",s18.headers.v)
    } 
    #S19 Bar Chart "PD Scale Rates of Implementation"
    {
      s19.headers.v <- s18.headers.v
      s19.varnames.v <- s18.varnames.v
      s19.ls <- list()
      
      for(m in 1:length(s19.varnames.v)){
        s19.m <- (100*(s18.outputs.df[1:2,m+1] %>% sum)/(s18.outputs.df[1:nrow(s18.outputs.df),m+1] %>% sum)) %>% round(., digits = 0)
        s19.ls[[m]] <- s19.m
      }
      
      s19.outputs.df <- do.call(rbind, s19.ls) %>% cbind(s19.headers.v, .) %>% as.data.frame
      names(s19.outputs.df) <- c("category","score_school_rate")
      s19.outputs.df$category <- s19.outputs.df$category %>% as.character
      s19.outputs.df$score_school_rate <- s19.outputs.df$score_school_rate %>% as.character %>% as.numeric
      s19.outputs.df$score_school_rate[is.na(s19.outputs.df$score_school_rate)] <- 0
    }
    #S20 Bar Chart "Recent Progress"
    {
      s20.headers.v <- c("Collaborative Data Teaming","Data-based Decision-making", "Common Formative Assessment","Effective Teaching and Learning")
      s20.varnames.v <- names(dat.df.i)[grepl("q25",names(dat.df.i))]
      dat.df.i.s20 <- dat.df.i[,names(dat.df.i) %in% s20.varnames.v] %>% as.data.frame
      s20.outputs.df <- apply(dat.df.i.s20, 2, function(x) mean(as.numeric(as.character(x)), na.rm = TRUE)) %>% as.data.frame
      s20.outputs.df <- s20.outputs.df %>% cbind(s20.headers.v,.) %>% as.data.frame
      names(s20.outputs.df) <- c("category","avg.progress")
      s20.outputs.df$avg.progress <- s20.outputs.df$avg.progress %>% as.character %>% as.numeric %>% round(., digits = 1)
      s20.outputs.df <- s20.outputs.df[c(3,4,1,2),]
    }
  } #END OF LOOP BY DISTRICT

} #SECTION COLLAPSE BRACKET

########################################################################################################################################################                  
### EXPORTING RESULTS TO POWERPOINT ###

#j <- 1 #LOOP TESTER
for(j in 1:length(district.names)){    #START LOOP J BY SCHOOL
  j <- i
  
  #Copy template file into target directory & rename with individual report name 
  #if(j == 1){
  #  dir.create(target.dir)
  #}
  
  target.file.j <- paste( target.dir,
                          "/",
                          "CWIS Report_",
                          district.name.i,
                          "_",
                          district.id.i,
                          ".pptx", sep="") 
  file.copy(template.file, target.file.j)
  
  #Powerpoint Formatting Setup
  pptx.j <- pptx(template = target.file.j)
  
  options("ReporteRs-fontsize" = 20)
  options("ReporteRs-default-font" = "Calibri")
  
  layouts = slide.layouts(pptx.j)
  layouts
  for(k in layouts ){
    slide.layouts(pptx.j, k )
    title(sub = k )
  }
  
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
  #notesgray <- rgb(131,130,105, maxColorValue=255)
  
  #Text formatting
  title.format <- textProperties(color = titlegreen, font.size = 48, font.weight = "bold")
  title.format.small <- textProperties(color = titlegreen, font.size = 40, font.weight = "bold")
  subtitle.format <- textProperties(color = notesgrey, font.size = 28, font.weight = "bold")
  section.title.format <- textProperties(color = "white", font.size = 48, font.weight = "bold")
  notes.format <- textProperties(color = notesgrey, font.size = 14)
  
  
  #Edit powerpoint template
  
  ## SLIDE 1 ## Cover Slide
  {
    pptx.j <- addSlide( pptx.j, slide.layout = 'Title Slide', bookmark = 1)
    
    #Text itself - as "piece of text" (pot) objects
    title.j <- pot(
      "Collaborative Work Implementation Survey",
      title.format
    )
    subtitle.j <- pot(
      paste("SCHOOL REPORT: ",district.id.i %>% toupper),
      subtitle.format
    )
    
    districttitle.j <- pot(
      paste("DISTRICT: ",district.name.i %>% toupper),
      subtitle.format
    )
    
    #Write Text into Slide
    pptx.j <- addParagraph(pptx.j, #Title
                           title.j, 
                           height = 1.92,
                           width = 7.76,
                           offx = 1.27,
                           offy = 2.78,
                           par.properties=parProperties(text.align="left", padding=0)
    )
    pptx.j <- addParagraph(pptx.j, #Subtitle (school name)
                           subtitle.j, 
                           height = 0.67,
                           width = 8.21,
                           offx = 1.27,
                           offy = 4.7,
                           par.properties=parProperties(text.align="left", padding=0)
    )
    
    pptx.j <- addParagraph(pptx.j, #District
                           districttitle.j, 
                           height = 0.67,
                           width = 7.76,
                           offx = 1.27,
                           offy = 5.85,
                           par.properties=parProperties(text.align="left", padding=0)
    )
    writeDoc(pptx.j, file = target.file.j) #test Slide 1 build
  }  
  ## SLIDE 2 ## Participation Details
  {
    pptx.j <- addSlide( pptx.j, slide.layout = 'S2')
    
    #Title
    s2.title <- pot("Participation Details",title.format)
    pptx.j <- addParagraph(pptx.j, 
                           s2.title, 
                           height = 0.89,
                           width = 8.47,
                           offx = 0.83,
                           offy = 0.85,
                           par.properties=parProperties(text.align="left", padding=0)
    )
    
    #Viz
    s2.ft <- FlexTable(
      data = s2.outputs.df,
      header.columns = TRUE,
      add.rownames = FALSE,
      
      header.cell.props = cellProperties(background.color = purpleheader, border.style = "none"),
      header.text.props = textProperties(color = "white", font.size = 22, font.weight = "bold"),
      header.par.props = parProperties(text.align = "center"),
      body.cell.props = cellProperties(background.color = "white", border.style = "none")
    )
    
    s2.ft[dim(s2.outputs.df)[1],] <- chprop(textProperties(font.weight = "bold")) #Bold text on last line (totals)
    s2.ft[,2] <- chprop(parProperties(text.align = "center")) #Center align numbers in second column
    s2.ft <- setFlexTableWidths(s2.ft, widths = c(4, 2.5))      
    s2.ft <- setZebraStyle(s2.ft, odd = purpleshade, even = "white" ) 
    
    pptx.j <- addFlexTable(pptx.j, 
                           s2.ft, 
                           height = 4.01,
                           width = 6.71,
                           offx = 1.65,
                           offy = 2.75
                           #par.properties=parProperties(text.align="center", padding=0)
    )
    #Page number
    pptx.j <- addPageNumber(pptx.j)
    
    writeDoc(pptx.j, file = target.file.j) #test Slide build up to this point
    
  }   
  ## SLIDE 3 ## Overall Scale Performance
  {  
    pptx.j <- addSlide( pptx.j, slide.layout = 'S2')
    
    #Title
    s3.title <- pot("Overall Scale Performance",title.format)
    pptx.j <- addParagraph(pptx.j, 
                           s3.title, 
                           height = 0.89,
                           width = 8.47,
                           offx = 0.83,
                           offy = 0.85,
                           par.properties=parProperties(text.align="left", padding=0)
    )
    
    #Viz
    s3.outputs.df$category <- factor(s3.outputs.df$category, levels = s3.outputs.df$category)
    
    s3.graph <- ggplot(data = s3.outputs.df, aes(x = rev(category))) + 
      geom_bar(
        aes(y = score_school_avg), 
        stat="identity", 
        fill = c(rep(purplegraphshade, length(s3.outputs.df$score_school_avg)-1), titlegreen),  
        #      cbind(s3.outputs.df$category %>% as.character(.),.) %>%
        #       as.data.frame(.),
        #s3.outputs.df[!is.na(s3.outputs.df$score_school_avg),],
        #by = c("V1","category")
        #)
        
        width = 0.8) + 
      geom_errorbar(
        mapping = aes(ymin = score_state_avg-.015, ymax = score_state_avg-.015), 
        color = "black", 
        width = 0.9,
        size = 1.2,
        alpha = 0.2) +
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
      geom_text(aes(                                                          #data labels inside base of columns
        y = 0.4, 
        label = s3.outputs.df$score_school_avg %>% round(.,1) %>% format(., nsmall = 1)
      ), 
      size = 4,
      color = "white") + 
      theme(panel.background = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_line(color = graphgridlinesgrey),
            axis.text.x = element_text(size = 0, color = "white"),
            axis.text.y = element_text(size = 15, color = graphlabelsgrey),
            axis.ticks = element_blank()
      ) +     
      coord_flip() 
    
    s3.graph
    
    pptx.j <- addPlot(pptx.j,
                      fun = print,
                      x = s3.graph,
                      height = 2.85,
                      width = 6.39,
                      offx = 1.8,
                      offy = 2.16)
    
    #Notes
    s3.notes <- pot("Notes: (a) A score of 4 represents a response of 'most of the time' for Effective Teaching and Learning Practices, Common Formative Assessment, and Data-based Decision-making scales, and 'agree' for the Leadership, and Professional Development scale. (b) The 2017-18 state average is marked with the yellow bar.",
                    notes.format)
    
    pptx.j <- addParagraph(pptx.j,
                           s3.notes,
                           height = 1.39,
                           width = 8.47,
                           offx = 0.83,
                           offy = 5.28,
                           par.properties = parProperties(text.align ="left", padding = 0)
    )
    
    s3.notes.2 <- pot("The 2017-18 state average is marked with the yellow bar.",
                      textProperties(color = "black", font.size = 14))
    
    pptx.j <- addParagraph(pptx.j,
                           s3.notes.2,
                           height = 0.71,
                           width = 8.47,
                           offx = 2.74,
                           offy = 6.85,
                           par.properties = parProperties(text.align ="left", padding = 0)
    )
    
    #Page number
    pptx.j <- addPageNumber(pptx.j)
    
    writeDoc(pptx.j, file = target.file.j) #test Slide build up to this point
  }  
  ## SLIDE 4 ## Individual Response Plot REMOVED
  {  
    #pptx.j <- addSlide( pptx.j, slide.layout = 'S2')
    
    #Title
    #  s4.title <- pot("Individual Response Plot",title.format)
    #  pptx.j <- addParagraph(pptx.j, 
    #                         s4.title, 
    #                         height = 0.89,
    #                         width = 8.47,
    #                         offx = 0.83,
    #                         offy = 0.85,
    #                         par.properties=parProperties(text.align="left", padding=0)
    #  )
    
    #Viz
    
    #Notes
    #  s4.notes <- pot("A score of 3 represents a response of 'about half the time' for Effective Teaching and Learning, Common Formative Assessment, and Data-based Decision-making scales, while a 2 represents 'sometimes'. 
    #                  A 3 corresponds to 'neither agree or disagree' for the Leadership, and Professional Development scale while a 2 represents 'disagree.'",
    #                  notes.format)
    #  pptx.j <- addParagraph(pptx.j,
    #                         s4.notes,
    #                         height = 1.39,
    #                         width = 8.47,
    #                         offx = 0.83,
    #                         offy = 5.28,
    #                         par.properties = parProperties(text.align ="left", padding = 0)
    #  )
    
    #Page number
    # pptx.j <- addPageNumber(pptx.j)
    
    #writeDoc(pptx.j, file = target.file.j) #test Slide build up to this point
  }  
  ## SLIDE 5 ## Section Title - ETLP
  {  
    pptx.j <- addSlide( pptx.j, slide.layout = 'Section Header')
    
    #S5 Title
    s5.title <- pot("Diving Deeper Into Scales: EFFECTIVE TEACHING AND LEARNING PRACTICES",
                    section.title.format)
    pptx.j <- addParagraph(pptx.j, 
                           s5.title, 
                           height = 2.63,
                           width = 7.76,
                           offx = 1.25,
                           offy = 2.48,
                           par.properties=parProperties(text.align="left", padding=0)
    )
    #Page number
    pptx.j <- addPageNumber(pptx.j)
    
    writeDoc(pptx.j, file = target.file.j) #test Slide build up to this point
  }
  ## Slide 6 ## ETLP Scale Performance
  {
    pptx.j <- addSlide( pptx.j, slide.layout = 'S2')
    
    #Title
    s6.title <- pot("ETLP Scale Performance",title.format)
    pptx.j <- addParagraph(pptx.j, 
                           s6.title, 
                           height = 0.89,
                           width = 8.47,
                           offx = 0.83,
                           offy = 0.18,
                           par.properties=parProperties(text.align="left", padding=0)
    )
    
    #Viz
    s6.ft <- FlexTable(
      data = s6.outputs.df,
      header.columns = TRUE,
      add.rownames = FALSE,
      
      header.cell.props = cellProperties(background.color = purpleheader, border.style = "none", vertical.align = "bottom"),
      header.par.props = parProperties(text.align = 'center'),
      header.text.props = textProperties(color = "white", font.size = 11, font.weight = "bold"),
      
      body.cell.props = cellProperties(background.color = "white", border.style = "none"), 
      body.text.props = textProperties(font.size = 12, font.weight = "bold", color = notesgrey)
    )
    
    s6.ft <- setFlexTableWidths(s6.ft, widths = c(1.4, rep(0.9,ncol(s6.outputs.df)-1)))      
    s6.ft <- setZebraStyle(s6.ft, odd = purpleshade, even = "white") 
    s6.ft[,1] <- textProperties(font.size = 11, color = notesgrey, font.weight = "bold") # Change row header text properties
    s6.ft[,1] <- parProperties(text.align = 'right') #Change row header alignment to right
    s6.ft[,2:(ncol(s6.outputs.df))] <-  parProperties(text.align='center') #Center align numbers in table
    
    #s6.ft  #FOR TESTING
    
    pptx.j <- addFlexTable(pptx.j, 
                           s6.ft, 
                           height = 1.72,
                           width = 8.77,
                           offx = 0.63,
                           offy = 1.2,
                           par.properties=parProperties(text.align="left", padding=0.2)
    )
    
    #Notes
    s6.notes <- pot("Prompt Text:
                    1. (learning targets) The students in my classroom, including students with disabilities, write/state learning targets using 'I can' or 'I know' statements.
                    2. (students assess) The students in my classroom, including students with disabilities, assess their progress by using evidence of student work (rubrics or portfolios).
                    3. (students identify) The students in my classroom, including students with disabilities, identify what they should do next in their learning based on self-assessment of their progress.
                    4. (feedback to targets) Students in my classroom, including students with disabilities, receive feedback on their progress toward their learning targets.
                    5. (student to student feedback) Student-to-student feedback, focused on improving learning, occurs during instruction.
                    6. (students state criteria) Students in my classroom state the success criteria for achieving their learning target.
                    7. (instruction state standards) The instruction of teachers in my building intentionally addresses the state standards for my grade/subject.
                    8. (student reviews cfa) Each student reviews his/her results of common formative assessments with a teacher.",
                    textProperties(color = notesgrey, font.size = 11))
    
    pptx.j <- addParagraph(pptx.j,
                           s6.notes,
                           height = 3.3,
                           width = 8.77,
                           offx = 0.63,
                           offy = 3.25,
                           par.properties = parProperties(text.align ="left", padding = 0)
    )
    
    #Page number
    pptx.j <- addPageNumber(pptx.j)
    
    writeDoc(pptx.j, file = target.file.j) #test Slide build up to this point
  }
  ## SLIDE 7 ## ETLP Scale Rates of Implementation
  {  
    pptx.j <- addSlide( pptx.j, slide.layout = 'S2')
    
    #Title
    s7.title <- pot("ETLP Scale Rates of Implementation",title.format.small)
    pptx.j <- addParagraph(pptx.j, 
                           s7.title, 
                           height = 0.89,
                           width = 8.47,
                           offx = 0.83,
                           offy = 0.85,
                           par.properties=parProperties(text.align="left", padding=0)
    )
    
    #Viz
    s7.graph <- ggplot(data = s7.outputs.df, aes(x = s7.outputs.df$category[order(s7.outputs.df$category, decreasing = TRUE)])) + 
      geom_bar(
        aes(y = score_school_rate), 
        stat="identity", 
        fill = titlegreen, 
        width = 0.8) + 
      ylim(0,100) +
      labs(x = "", y = "") +
      scale_x_discrete(labels = substring(s7.headers.v[order(s7.headers.v, decreasing = TRUE)], 4)) + #category labels (for eventual y axis)
      geom_text( #data labels
        aes(y = 10, label = paste(s7.outputs.df$score_school_rate, "%", sep = "")), 
        size = 4,
        color = "white") + 
      theme(panel.background = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_line(color = graphgridlinesgrey),
            axis.text.x = element_text(size = 0, color = "white"),
            axis.ticks.x = element_blank(),
            axis.text.y = element_text(size = 15, color = graphlabelsgrey),
            axis.ticks.y = element_blank()
      ) +     
      coord_flip()
    s7.graph
    
    pptx.j <- addPlot(pptx.j,
                      fun = print,
                      x = s7.graph,
                      height = 3.25,
                      width = 8,
                      offx = 1,
                      offy = 2)
    
    #Notes
    s7.notes <- pot("Documented above are the percent of school respondents who answered that they use the following practices either 'most of the time' or 'always.' Please see page 5 for a list of complete prompts.",
                    notes.format)
    pptx.j <- addParagraph(pptx.j,
                           s7.notes,
                           height = 1.39,
                           width = 8.47,
                           offx = 0.83,
                           offy = 5.28,
                           par.properties = parProperties(text.align ="left", padding = 0)
    )
    
    #Page number
    pptx.j <- addPageNumber(pptx.j)
    
    writeDoc(pptx.j, file = target.file.j) #test Slide build up to this point
  }
  ## SLIDE 8 ## Section Title - CFA
  {  
    pptx.j <- addSlide( pptx.j, slide.layout = 'Section Header')
    
    #s8 Title
    s8.title <- pot("Diving Deeper Into Scales: COMMON FORMATIVE ASSESSMENT",
                    section.title.format)
    pptx.j <- addParagraph(pptx.j, 
                           s8.title, 
                           height = 2.63,
                           width = 7.76,
                           offx = 1.25,
                           offy = 2.48,
                           par.properties=parProperties(text.align="left", padding=0)
    )
    
    #Page number
    pptx.j <- addPageNumber(pptx.j)
    
    writeDoc(pptx.j, file = target.file.j) #test Slide build up to this point
  }
  ## Slide 9 ## CFA Scale Performance
  {
    pptx.j <- addSlide( pptx.j, slide.layout = 'S2')
    
    #Title
    s9.title <- pot("CFA Scale Performance",title.format)
    pptx.j <- addParagraph(pptx.j, 
                           s9.title, 
                           height = 0.89,
                           width = 8.47,
                           offx = 0.83,
                           offy = 0.18,
                           par.properties=parProperties(text.align="left", padding=0)
    )
    
    #Viz
    s9.ft <- FlexTable(
      data = s9.outputs.df,
      header.columns = TRUE,
      add.rownames = FALSE,
      
      header.cell.props = cellProperties(background.color = purpleheader, border.style = "none", vertical.align = "bottom"),
      header.text.props = textProperties(color = "white", font.size = 11, font.weight = "bold"),
      header.par.props = parProperties(text.align = "center"),
      body.cell.props = cellProperties(background.color = "white", border.style = "none"), 
      body.text.props = textProperties(font.size = 12, font.weight = "bold", color = notesgrey)
      
    )
    
    s9.ft[,] <- borderProperties(color = "white")
    s9.ft <- setFlexTableWidths(s9.ft, widths = c(2, rep(1.5,ncol(s9.outputs.df)-1)))      
    s9.ft <- setZebraStyle(s9.ft, odd = purpleshade, even = "white" ) 
    s9.ft[,2:(ncol(s9.outputs.df))] <- parProperties(text.align="center")
    
    pptx.j <- addFlexTable(pptx.j, 
                           s9.ft, 
                           height = 2.75,
                           width = 8,
                           offx = 1.22,
                           offy = 2.0,
                           par.properties = parProperties(text.align="left", padding = 0)
    )
    s9.ft
    
    #Notes
    s9.notes <- pot("Prompt Text:
                    1. (use cfa) I use common formative assessments aligned to the Missouri Learning Standards.
                    2. (all in cfa) All students in my classroom participate in common formative assessments, including students with disabilities.
                    3. (student reviews cfa) Each student reviews his/her results of common formative assessments with a teacher.
                    4. (cfa used to plan) I use the results from common formative assessment to plan for re-teaching and/or future instruction.",
                    textProperties(color = notesgrey, font.size = 11)
    )
    
    pptx.j <- addParagraph(pptx.j,
                           s9.notes,
                           height = 3.3,
                           width = 8.3,
                           offx = 1.22,
                           offy = 3.25,
                           par.properties = parProperties(text.align ="left", padding = 0)
    )
    
    #Page number
    pptx.j <- addPageNumber(pptx.j)
    
    writeDoc(pptx.j, file = target.file.j) #test Slide build up to this point
  }
  ## SLIDE 10 ## CFA Scale Rates of Implementation
  {  
    pptx.j <- addSlide( pptx.j, slide.layout = 'S2')
    
    #Title
    s10.title <- pot("CFA Scale Rates of Implementation",title.format.small)
    pptx.j <- addParagraph(pptx.j, 
                           s10.title, 
                           height = 0.89,
                           width = 8.47,
                           offx = 0.83,
                           offy = 0.85,
                           par.properties=parProperties(text.align="left", padding=0)
    )
    
    #Viz
    s10.graph <- ggplot(data = s10.outputs.df, aes(x = s10.outputs.df$category[order(s10.outputs.df$category, decreasing = TRUE)])) + 
      geom_bar(
        aes(y = score_school_rate), 
        stat="identity", 
        fill = titlegreen, 
        width = 0.8) + 
      ylim(0,100) +
      labs(x = "", y = "") +                 #axis labels
      scale_x_discrete(labels = substring(s10.headers.v[order(s10.headers.v, decreasing = TRUE)],4)) +
      geom_text(                                                                          #data labels
        aes(y = 10, label = paste(s10.outputs.df$score_school_rate, "%", sep = "")), 
        size = 4,
        color = "white") + 
      theme(panel.background = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_line(color = graphgridlinesgrey),
            axis.text.y = element_text(size = 15, color = graphlabelsgrey),
            axis.text.x = element_blank(),
            axis.ticks = element_blank()
      ) +     
      coord_flip() 
    
    pptx.j <- addPlot(pptx.j,
                      fun = print,
                      x = s10.graph,
                      height = 3.25,
                      width = 8,
                      offx = 1,
                      offy = 2)
    
    #Notes
    s10.notes <- pot("Documented above are the percent of school respondents who answered that they use the following practices either “most of the time” or “always.” Please see page 8 for a list of complete prompts.",
                     notes.format)
    pptx.j <- addParagraph(pptx.j,
                           s10.notes,
                           height = 1.39,
                           width = 8.47,
                           offx = 0.83,
                           offy = 5.28,
                           par.properties = parProperties(text.align ="left", padding = 0)
    )
    
    #Page number
    pptx.j <- addPageNumber(pptx.j)  
    
    writeDoc(pptx.j, file = target.file.j) #test Slide build up to this point
  }
  ## SLIDE 11 ## Section Title - DBDM
  {  
    pptx.j <- addSlide( pptx.j, slide.layout = 'Section Header')
    
    #s11 Title
    s11.title <- pot("Diving Deeper Into Scales: DATA-BASED DECISION-MAKING",
                     section.title.format)
    pptx.j <- addParagraph(pptx.j, 
                           s11.title, 
                           height = 2.63,
                           width = 7.76,
                           offx = 1.25,
                           offy = 2.48,
                           par.properties=parProperties(text.align="left", padding=0)
    )
    
    #Page number
    pptx.j <- addPageNumber(pptx.j)
    
    writeDoc(pptx.j, file = target.file.j) #test Slide build up to this point
  }
  ## Slide 12 ## DBDM Scale Performance
  {
    pptx.j <- addSlide( pptx.j, slide.layout = 'S2')
    
    #Title
    s12.title <- pot("DBDM Scale Performance",title.format)
    pptx.j <- addParagraph(pptx.j, 
                           s12.title, 
                           height = 0.89,
                           width = 8.47,
                           offx = 0.83,
                           offy = 0.18,
                           par.properties=parProperties(text.align="left", padding=0)
    )
    
    #Viz
    s12.ft <- FlexTable(
      data = s12.outputs.df,
      header.columns = TRUE,
      add.rownames = FALSE,
      
      header.cell.props = cellProperties(background.color = purpleheader, border.style = "none", vertical.align = "bottom"),
      header.text.props = textProperties(color = "white", font.size = 11, font.weight = "bold"),
      header.par.props = parProperties(text.align = "center"),
      body.cell.props = cellProperties(background.color = "white", border.style = "none"), 
      body.text.props = textProperties(font.size = 12, font.weight = "bold", color = notesgrey)
    )
    
    s12.ft[,] <- borderProperties(color = "white")
    s12.ft <- setFlexTableWidths(s12.ft, widths = c(1.8, rep(1.3,ncol(s12.outputs.df)-1)))      
    s12.ft <- setZebraStyle(s12.ft, odd = purpleshade, even = "white" ) 
    s12.ft[,2:(ncol(s12.outputs.df))] <- parProperties(text.align="center")
    
    pptx.j <- addFlexTable(pptx.j, 
                           s12.ft, 
                           height = 2.75,
                           width = 8,
                           offx = 0.82,
                           offy = 2.0,
                           par.properties = parProperties(text.align="left", padding = 0)
    )
    
    #Notes
    s12.notes <- pot("Prompt Text:
                     1. (use cfa) I use common formative assessments aligned to the Missouri Learning Standards.
                     2. (all in cfa) All students in my classroom participate in common formative assessments, including students with disabilities.
                     3. (student reviews cfa) Each student reviews his/her results of common formative assessments with a teacher.
                     4. (cfa used to plan) I use the results from common formative assessment to plan for re-teaching and/or future instruction.",
                     textProperties(color = notesgrey, font.size = 11)
    )
    
    pptx.j <- addParagraph(pptx.j,
                           s12.notes,
                           height = 3.3,
                           width = 9.57,
                           offx = 0.82,
                           offy = 3.55,
                           par.properties = parProperties(text.align ="left", padding = 0)
    )
    
    #Page number
    pptx.j <- addPageNumber(pptx.j)  
    
    writeDoc(pptx.j, file = target.file.j) #test Slide build up to this point
  }  
  ## SLIDE 13 ## DBDM Scale Rates of Implementation
  {  
    pptx.j <- addSlide( pptx.j, slide.layout = 'S2')
    
    #Title
    s13.title <- pot("DBDM Scale Rates of Implementation",title.format.small)
    pptx.j <- addParagraph(pptx.j, 
                           s13.title, 
                           height = 0.89,
                           width = 8.47,
                           offx = 0.83,
                           offy = 0.4,
                           par.properties=parProperties(text.align="left", padding=0)
    )
    
    #Viz
    s13.graph <- ggplot(data = s13.outputs.df, aes(x = s13.outputs.df$category[order(s13.outputs.df$category, decreasing = TRUE)])) + 
      geom_bar(
        aes(y = score_school_rate), 
        stat="identity", 
        fill = titlegreen, 
        width = 0.8) + 
      ylim(0,100) +
      labs(x = "", y ="") +                 #axis labels
      scale_x_discrete(labels = substring(s13.headers.v[order(s13.headers.v, decreasing = TRUE)], 4)) +
      geom_text(                                                                          #data labels
        aes(y = 10, label = paste(s13.outputs.df$score_school_rate, "%", sep = "")), 
        size = 4,
        color = "white") + 
      theme(panel.background = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_line(color = graphgridlinesgrey),
            axis.text.x = element_blank(),
            axis.text.y = element_text(size = 15, color = graphlabelsgrey),
            axis.ticks = element_blank()
      ) +     
      coord_flip() 
    
    pptx.j <- addPlot(pptx.j,
                      fun = print,
                      x = s13.graph,
                      height = 3.25,
                      width = 8,
                      offx = 1,
                      offy = 2)
    
    #Notes
    s13.notes <- pot("Documented above are the percent of school respondents who answered that they use the following practices either “most of the time” or “always.” Please see page 11 for a list of complete prompts.",
                     notes.format)
    pptx.j <- addParagraph(pptx.j,
                           s13.notes,
                           height = 1.39,
                           width = 8.47,
                           offx = 0.83,
                           offy = 5.4,
                           par.properties = parProperties(text.align ="left", padding = 0)
    )
    
    #Page number
    pptx.j <- addPageNumber(pptx.j)
    
    writeDoc(pptx.j, file = target.file.j) #test Slide build up to this point
  }
  ## SLIDE 14 ## Section Title - Leadership
  {  
    pptx.j <- addSlide( pptx.j, slide.layout = 'Section Header')
    
    #s14 Title
    s14.title <- pot("Diving Deeper Into Scales: LEADERSHIP",
                     section.title.format)
    pptx.j <- addParagraph(pptx.j, 
                           s14.title, 
                           height = 2.63,
                           width = 7.76,
                           offx = 1.25,
                           offy = 2.48,
                           par.properties=parProperties(text.align="left", padding=0)
    )
    #Page number
    pptx.j <- addPageNumber(pptx.j)
    
    writeDoc(pptx.j, file = target.file.j) #test Slide build up to this point
  }
  ## Slide 15 ## Leadership Scale Performance
  {
    pptx.j <- addSlide( pptx.j, slide.layout = 'S2')
    
    #Title
    s15.title <- pot("Leadership Scale Performance",title.format)
    pptx.j <- addParagraph(pptx.j, 
                           s15.title, 
                           height = 0.89,
                           width = 10.47,
                           offx = 0.83,
                           offy = 0.18,
                           par.properties=parProperties(text.align="left", padding=0)
    )
    
    #Viz
    s15.ft <- FlexTable(
      data = s15.outputs.df,
      header.columns = TRUE,
      add.rownames = FALSE,
      
      header.cell.props = cellProperties(background.color = purpleheader, border.style = "none", vertical.align = "bottom"),
      header.text.props = textProperties(color = "white", font.size = 11, font.weight = "bold"),
      header.par.props = parProperties(text.align = "center"),
      body.cell.props = cellProperties(background.color = "white", border.style = "none"), 
      body.text.props = textProperties(font.size = 12, font.weight = "bold", color = notesgrey)
    )
    
    s15.ft[,] <- borderProperties(color = "white")
    s15.ft <- setFlexTableWidths(s15.ft, widths = c(1.8, rep(1.4,ncol(s15.outputs.df)-1)))      
    s15.ft <- setZebraStyle(s15.ft, odd = purpleshade, even = "white" ) 
    s15.ft[,2:(ncol(s15.outputs.df))] <- parProperties(text.align="center")
    s15.ft$header <- parProperties(text.align = "center")
    
    pptx.j <- addFlexTable(pptx.j, 
                           s15.ft, 
                           height = 2.75,
                           width = 9,
                           offx = 0.82,
                           offy = 2.0,
                           par.properties = parProperties(text.align="left", padding = 0)
    )
    
    #Notes
    s15.notes <- pot("Prompt Text:
                     1. (leaders manage) Building leader(s) effectively manage initiatives and expectations placing a focus on improving educational practices.
                     2. (teacher to teacher feedback) Building leadership supports the opportunity for teacher-to-teacher observation and feedback.
                     3. (leader committed) My building administrator(s) show(s) they are committed to implementing a core set of effective instructional practices in building classrooms.
                     4. (leader active) The building leader(s) actively problem-solve(s) with my team.",
                     textProperties(color = notesgrey, font.size = 11)
    )
    
    pptx.j <- addParagraph(pptx.j,
                           s15.notes,
                           height = 3.3,
                           width = 8.5,
                           offx = 0.82,
                           offy = 3.5,
                           par.properties = parProperties(text.align ="left", padding = 0)
    )
    
    #Page number
    pptx.j <- addPageNumber(pptx.j)
    
    writeDoc(pptx.j, file = target.file.j) #test Slide build up to this point
  }  
  ## SLIDE 16 ## Leadership Scale Rates of Implementation
  {  
    pptx.j <- addSlide( pptx.j, slide.layout = 'S2')
    
    #Title
    s16.title <- pot("Leadership Scale Rates of Implementation",title.format.small)
    pptx.j <- addParagraph(pptx.j, 
                           s16.title, 
                           height = 0.89,
                           width = 8.47,
                           offx = 0.83,
                           offy = 0.4,
                           par.properties=parProperties(text.align="left", padding=0)
    )
    
    #Viz
    s16.graph <- ggplot(data = s16.outputs.df, aes(x = s16.outputs.df$category[order(s16.outputs.df$category, decreasing = TRUE)])) + 
      geom_bar(
        aes(y = score_school_rate), 
        stat="identity", 
        fill = titlegreen, 
        width = 0.8) + 
      ylim(0,100) +
      labs(x = "", y = "") +                 #axis labels
      scale_x_discrete(labels = substring(s16.headers.v[order(s16.headers.v, decreasing = TRUE)],4)) +
      geom_text(                                                                          #data labels
        aes(y = 10, label = paste(s16.outputs.df$score_school_rate, "%", sep = "")), 
        size = 4,
        color = "white") + 
      theme(panel.background = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_line(color = graphgridlinesgrey),
            axis.text.x = element_blank(),
            axis.text.y = element_text(size = 15, color = graphlabelsgrey),
            axis.ticks = element_blank()
      ) +     
      coord_flip() 
    
    pptx.j <- addPlot(pptx.j,
                      fun = print,
                      x = s16.graph,
                      height = 3.25,
                      width = 8,
                      offx = 1,
                      offy = 2)
    
    #Notes
    s16.notes <- pot("Documented above are the percent of school respondents who answered that they use the following practices either “agree” or “strongly agree.” Please see page 14 for a list of complete prompts.",
                     notes.format)
    pptx.j <- addParagraph(pptx.j,
                           s16.notes,
                           height = 1.39,
                           width = 8.47,
                           offx = 0.83,
                           offy = 5.4,
                           par.properties = parProperties(text.align ="left", padding = 0)
    )
    
    #Page number
    pptx.j <- addPageNumber(pptx.j)
    
    writeDoc(pptx.j, file = target.file.j) #test Slide build up to this point
  }
  ## SLIDE 17 ## Section Title - Professional Development
  {  
    pptx.j <- addSlide( pptx.j, slide.layout = 'Section Header')
    
    #s17 Title
    s17.title <- pot("Diving Deeper Into Scales: PROFESSIONAL DEVELOPMENT",
                     section.title.format)
    pptx.j <- addParagraph(pptx.j, 
                           s17.title, 
                           height = 2.63,
                           width = 7.76,
                           offx = 1.25,
                           offy = 2.48,
                           par.properties=parProperties(text.align="left", padding=0)
    )
    
    #Page number
    pptx.j <- addPageNumber(pptx.j)
    
    writeDoc(pptx.j, file = target.file.j) #test Slide build up to this point
  }
  ## Slide 18 ## Professional Development Scale Performance
  {
    pptx.j <- addSlide( pptx.j, slide.layout = 'S2')
    
    #Title
    s18.title <- pot("PD Scale Performance",title.format)
    pptx.j <- addParagraph(pptx.j, 
                           s18.title, 
                           height = 0.89,
                           width = 8.47,
                           offx = 0.83,
                           offy = 0.18,
                           par.properties=parProperties(text.align="left", padding=0)
    )
    
    #Viz
    s18.ft <- FlexTable(
      data = s18.outputs.df,
      header.columns = TRUE,
      add.rownames = FALSE,
      
      header.cell.props = cellProperties(background.color = purpleheader, border.style = "none", vertical.align = "bottom"),
      header.text.props = textProperties(color = "white", font.size = 12, font.weight = "bold"),
      header.par.props = parProperties(text.align = "center"),
      body.cell.props = cellProperties(background.color = "white", border.style = "none"), 
      body.text.props = textProperties(font.size = 11, font.weight = "bold", color = notesgrey)
    )
    
    s18.ft[,] <- borderProperties(color = "white")
    s18.ft <- setFlexTableWidths(s18.ft, widths = c(1.8, rep(1.5,ncol(s18.outputs.df)-1)))      
    s18.ft <- setZebraStyle(s18.ft, odd = purpleshade, even = "white" ) 
    s18.ft[,2:(ncol(s18.outputs.df))] <- parProperties(text.align="center")
    
    pptx.j <- addFlexTable(pptx.j, 
                           s18.ft, 
                           height = 2.75,
                           width = 8,
                           offx = 0.82,
                           offy = 2.0,
                           par.properties = parProperties(text.align="left", padding = 0)
    )
    
    #Notes
    s18.notes <- pot("Prompt Text:
                     1. (pd instruction) I participate in professional development where I learn to improve my instructional practices.
                     2. (coaching instruction) I receive coaching to facilitate my  implementation of evidence-based instructional practices.
                     3. (pd monitor) I participate in professional development where I learn how to monitor student progress.
                     4. (teacher feedback instruction) I receive feedback about my classroom instruction from other teachers.",
                     textProperties(color = notesgrey, font.size = 11)
    )
    
    pptx.j <- addParagraph(pptx.j,
                           s18.notes,
                           height = 3.3,
                           width = 8,
                           offx = 0.82,
                           offy = 3.5,
                           par.properties = parProperties(text.align ="left", padding = 0)
    )
    
    #Page number
    pptx.j <- addPageNumber(pptx.j)
    
    writeDoc(pptx.j, file = target.file.j) #test Slide build up to this point
  }  
  ## SLIDE 19 ## Professional Development Scale Rates of Implementation
  {  
    pptx.j <- addSlide( pptx.j, slide.layout = 'S2')
    
    #Title
    s19.title <- pot("PD Scale Rates of Implementation",title.format.small)
    pptx.j <- addParagraph(pptx.j, 
                           s19.title, 
                           height = 0.89,
                           width = 8.47,
                           offx = 0.83,
                           offy = 0.4,
                           par.properties=parProperties(text.align="left", padding=0)
    )
    
    #Viz
    s19.graph <- ggplot(data = s19.outputs.df, aes(x = s19.outputs.df$category[order(s19.outputs.df$category, decreasing = TRUE)])) + 
      geom_bar(
        aes(y = score_school_rate), 
        stat="identity", 
        fill = titlegreen, 
        width = 0.8) + 
      ylim(0,100) +
      labs(x = "", y = "") +                 #axis labels
      scale_x_discrete(labels = substring(s19.headers.v[order(s19.headers.v, decreasing = TRUE)],4)) +
      geom_text(                                                                          #data labels
        aes(y = 10, label = paste(s19.outputs.df$score_school_rate, "%", sep = "")), 
        size = 4,
        color = "white") + 
      theme(panel.background = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_line(color = graphgridlinesgrey),
            axis.text.x = element_blank(),
            axis.text.y = element_text(size = 15, color = graphlabelsgrey),
            axis.ticks = element_blank()
      ) +     
      coord_flip() 
    
    pptx.j <- addPlot(pptx.j,
                      fun = print,
                      x = s19.graph,
                      height = 3.25,
                      width = 8,
                      offx = 1,
                      offy = 2)
    
    #Notes
    s19.notes <- pot("Documented above are the percent of school respondents who answered that they use the following practices either “agree” or “strongly agree.” Please see page 17 for a list of complete prompts.",
                     notes.format)
    pptx.j <- addParagraph(pptx.j,
                           s19.notes,
                           height = 1.39,
                           width = 8.47,
                           offx = 0.83,
                           offy = 5.4,
                           par.properties = parProperties(text.align ="left", padding = 0)
    )
    
    #Page number
    pptx.j <- addPageNumber(pptx.j)
    
    writeDoc(pptx.j, file = target.file.j) #test Slide build up to this point
  }
  ## SLIDE 20 ## Recent Progress
  {  
    pptx.j <- addSlide( pptx.j, slide.layout = 'S2')
    
    #Title
    s20.title <- pot("Recent Progress",title.format)
    pptx.j <- addParagraph(pptx.j, 
                           s20.title, 
                           height = 0.89,
                           width = 8.47,
                           offx = 0.83,
                           offy = 0.85,
                           par.properties=parProperties(text.align="left", padding=0)
    )
    
    #Viz
    s20.outputs.df$category <- factor(s20.outputs.df$category, levels = s20.outputs.df$category)
    
    s20.graph <- ggplot(data = s20.outputs.df, aes(x = rev(category))) + 
      geom_bar(
        aes(y = avg.progress), 
        stat="identity", 
        fill = purplegraphshade, 
        width = 0.8) + 
      ylim(0,100) +
      labs(x = "", y = "") +
      scale_x_discrete(labels = c("Common Formative Assessment",
                                  "Effective Teaching and Learning",
                                  "Collaborative Data Teaming",
                                  "Data-based Decision-making"
      ) %>% rev) +
      geom_text(aes(
        y = 17, 
        label = s20.outputs.df$avg.progress %>% round(., digits = 1) %>% format(., nsmall = 1),
      ), 
      size = 4,
      color = "white") + 
      theme(panel.background = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_line(color = graphgridlinesgrey),
            axis.text.x = element_blank(),
            axis.text.y = element_text(size = 15, color = notesgrey),
            axis.ticks.y = element_blank(),
            axis.ticks.x = element_blank()
      ) +     
      coord_flip() 
    
    s20.graph
    
    pptx.j <- addPlot(pptx.j,
                      fun = print,
                      x = s20.graph,
                      height = 2.85,
                      width = 6.39,
                      offx = 1.8,
                      offy = 2.16)
    
    #Notes
    s20.notes <- pot("Finally, we asked participants to rate the progress of their building in five key areas between the start of the current school year and March of 2018. A value of 50 represents things are about the same; anything greater than 50 refers to progress in the positive direction.",
                     notes.format)
    
    pptx.j <- addParagraph(pptx.j,
                           s20.notes,
                           height = 1.39,
                           width = 8.47,
                           offx = 0.83,
                           offy = 5.28,
                           par.properties = parProperties(text.align ="left", padding = 0)
    )
    
    #Page number
    pptx.j <- addPageNumber(pptx.j)
    
    writeDoc(pptx.j, file = target.file.j) #test Slide build up to this point
  }  
  
  setTxtProgressBar(progress.bar.i, 100*i/maxrow)      
} #END OF LOOP J BY SCHOOL

end_time <- Sys.time()
proc.time <- end_time - start_time
proc.time  
  