#0000000000000000000000000000000000000000000000000000000#
#      	CWIS Automation for MMD                   	    #
#0000000000000000000000000000000000000000000000000000000#

# 0-SETUP -----------------------------------------------------------
  
  #INITIAL SETUP
    rm(list=ls()) #Remove lists
    options(java.parameters = "- Xmx8g") #helps r not to fail when importing large xlsx files with xlsx package
    #TODO: MAKE UTILS MEMORY FUNCTIONS
      #MEASURE MEMORY USAGE OF OBJECTS
      #FUNCTION TO LIST MEMORY USAGE OF LARGEST OBJECTS IN CURRENT ENVIRONMENT
      #WARN WHEN APPROACHING DEFINED MEMORY USAGE LIMITS AND, IF INSIDE OF A LOOP, BREAK THE LOOP
    
    
    #Section & Code Clocking
      sections.all.starttime <- Sys.time()
      section0.starttime <- sections.all.starttime
  
  # ESTABLISH BASE DIRECTORIES
  
    #M900
      working.dir <- "C:\\Users\\willi\\Google Drive\\1. FLUX CONTRACTS - CURRENT\\2016-09 EXT Missouri\\3. MO GDRIVE\\8. CWIS\\2019-06 District Dashboards"
      rproj.dir <- "C:\\Users\\willi\\Documents\\GIT PROJECTS\\CWIS-automation"

    #Thinkpad T470
      #working.dir <- "G:\\My Drive\\1. FLUX CONTRACTS - CURRENT\\2016-09 EXT Missouri\\3. Missouri - GDRIVE\\8. CWIS\\2018-12 Green Reports Phase 6"
      #rproj.dir <- "C:\\Users\\WNF\\Documents\\Git Projects\\CWIS-automation"
    
    #Check Directories
      working.dir <- if(!dir.exists(working.dir)){choose.dir()}else{working.dir}
      rproj.dir <- if(!dir.exists(rproj.dir)){choose.dir()}else{rproj.dir}
    
    #Source Tables Directory (raw data, configs, etc.)
      source.tables.dir <- paste(working.dir,"\\3_source_tables\\", sep = "")
      if(dir.exists(source.tables.dir)){ 
        print("source.tables.dir exists.")
      }else{
        print("source.tables.dir does not exist yet.")
      }
      print(source.tables.dir)
    
  # LOAD UTILS FUNCTIONS
      
    setwd(rproj.dir)
    source("utils_wnf.r")
    
  # LOAD LIBRARIES/PACKAGES
    
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
    library(flextable)
    library(XLConnect)
    
    #Section Clocking
      section0.duration <- Sys.time() - section0.starttime
      section0.duration

# 0-SETUP OUTPUTS -----------------------------------------------------------
  #start_time: sys.time for code
  #working.dir: working directory - Google Drive folder "2018-08 Green Reports"
  #rproj.dir: directory for R project; also contains source data, additional function scripts, and config tables.
  #source.tables.dir: directory with raw data, configs, etc.

# 1-IMPORT & CONFIGS -----------------------------------------
  
  #Section Clocking
    section1.starttime <- Sys.time()
      
  #Source Import Functions
    setwd(rproj.dir)
    source("1-import_functions.r")
  
  #Import Config Tables
    configs.ss <- gs_key("1dWVAe2AjWLFQzbfOTpnUon02UdTYU7xaOl3m1UtWjGc",verbose = TRUE) 
    
    #Import all tables from config google sheet as tibbles
      all.configs.ls <- GoogleSheetLoadAllWorksheets(configs.ss)
    
    #Assign each table to its own tibble object
      ListToTibbleObjects(all.configs.ls) #Converts list elements to separate tibble objects names with
                                          #their respective sheet names with ".tb" appended
    
  #Extract global configs from tibble as their own character objects
    TibbleToCharObjects(config.global.tb)
    sample.print <- #Convert sample.print to TRUE/FALSE
      ifelse(sample.print == "true", TRUE, FALSE)
  
    domains <- strsplit(domains, ",") %>% unlist %>% as.vector
    
  #Import Responses table (main data, imported as data frame)
    setwd(source.tables.dir)
    
    resp1.tb <- read.csv(
      file =  
        MostRecentlyModifiedFilename(
          title.string.match = main.data.file.name.character.string,
          file.type = "csv",
          dir = source.tables.dir
        ),
      stringsAsFactors = FALSE,
      header = TRUE
    ) %>% as_tibble(.)
    
  #Section Clocking
    section1.duration <- Sys.time() - section1.starttime
    section1.duration
    Sys.time() - sections.all.starttime

# 1-IMPORT OUTPUTS -----------------------------------------
  #config.global.tb
    #Objects extracted from config.global.tb
      #report.unit
      #report.version
      #data.year
      #data.semester
      #sample.print
      #sample.size
  #config.ans.opt.tb
  #config.slide.types.tb
  #config.graph.types.tb
  #config.table.types.tb
  #config.pot.tb
  #buildings.tb
  #questions.tb
  #resp1.tb - main responses dataset which will need extensive cleaning and organization in next sections
  #resp.comparison.tb - comparison time period response dataset


# 2-CLEANING --------
  
  #Section Clocking
    section2.starttime <- Sys.time()
    
  #Source Cleaning Functions
    cleaning.source.dir <- paste(rproj.dir,"2-Cleaning/", sep = "")
    setwd(rproj.dir)
    source("2-cleaning_functions.r")
    
    
  #RESPONSES (round 1)  
    
    #Names/Column Headers
      resp2.tb <- 
        LowerCaseNames(resp1.tb) %>%  #Lower-case all variable names
        ReplaceNames(., "school", "building") %>% #Replace "school" with "building" in column names
        ReplaceNames(., "ï..year", "year") %>%
        ReplaceNames(., "id", "resp.id") %>%
        as_tibble()

      names(resp2.tb) <- #replace domain names that have 'c' in front of them for stome reason with regular domain names
        mgsub(
            pattern = paste("c",domains, sep = ""), 
            replacement = domains, 
            x = names(resp2.tb), 
            print.replacements = FALSE
          )
      
      names(resp2.tb) <- SubRepeatedCharWithSingleChar(string.vector = names(resp2.tb), char = ".")
      resp2.tb <- LowerCaseCharVars(resp2.tb) #Lower-case all character variable data
    
    #Add variables: building.id, building.level 
      resp2.tb %<>% mutate(building.id = paste(district,building,sep=".") %>% gsub(" |\\/", ".", .))
        
      resp2.tb %<>%
        mutate(
          building.level = 
            left_join(
              resp2.tb %>% select(building.id),
              buildings.tb %>% select(building.id, building.level),
              by = "building.id"
            ) %>% 
            select(building.level) %>%
            unlist %>% as.vector %>% tolower
        )
      
    #Row Filters
      
      #Filter out blank schools
        resp3.tb <- 
          resp2.tb %>% 
          filter(building != "")
        
      #Building Restrictions
        low.response.nums.filter.tb <- #Filter out building-periods with less than 6 responses at baseline
          resp3.tb %>%
          group_by(building.id,year) %>%
          summarize(x = length(resp.id)) %>%
          mutate(
            filter.baseline = ifelse(year == "baseline" & x < 6, FALSE, TRUE),
            filter.201718 = ifelse(year == "2017-2018" & x < 6, FALSE, TRUE),
            filter.201819 = ifelse(year == "2018-2019" & x < 6, FALSE, TRUE),
            filter.combined = all(filter.baseline, filter.201718, filter.201819)
          )
        
        resp4.tb <- 
          left_join(resp3.tb, low.response.nums.filter.tb %>% select(building.id, filter.combined), by = "building.id") %>%
          filter(filter.combined)
    
    #Reshape to Long Data by response to CWIS question
      cwis.varnames <- FilterVector(grepl(paste0(domains, collapse = "|"), names(resp3.tb)), names(resp3.tb))
      resp5.tb <-
        melt( #Melt to long data frame for all cwis vars
          resp4.tb,
          id.vars = names(resp4.tb)[!names(resp4.tb) %in% cwis.varnames], 
          measure.vars = cwis.varnames
        ) %>% 
        filter(!is.na(value)) %>% #remove rows with no answer for cwis vars
        MoveColsLeft(., c("resp.id","district")) %>% #Rearrange columns: resp.id and district at the front
        as_tibble()
      
      #Add domain variables
        resp6.tb <- 
          left_join(
            x = resp5.tb,
            y = questions.tb %>% select(var.id, domain, practice),
            by = c("variable"="var.id")
          ) %>%
          left_join(
            x = ., 
            y = domains.tb,
            by = c("domain" = "domain.id")
          )
        
      #Restrict Data to sample of user-defined size if doing sample print
        resp7.tb <- 
          RestrictDataToSample(
            tb = resp6.tb,
            report.unit = report.unit,
            sample.print = sample.print,
            sample.group.unit = sample.group.unit,
            sample.size = sample.size
          )
        
        names(resp7.tb)[names(resp7.tb) == as.vector(report.unit)] <- "unit.id"

        resp.long.tb <- resp7.tb
        
        unit.ids.sample <-
          resp7.tb %>%
          select(unit.id) %>%
          unique %>%
          unlist %>% as.vector
          
  #Section Clocking
    section2.duration <- Sys.time() - section2.starttime
    section2.duration
    Sys.time() - sections.all.starttime

# 2-CLEANING OUTPUTS ------------------          
  #resp.long.tb
    
# 3-CONFIGS (TAB AND TABLE CONFIG TABLES) ------------------
  
  #Section Clocking
    section3.startime <- Sys.time()
          
  #Load Configs Functions
    setwd(rproj.dir)
    source("3-configs_functions.r")
   
  #EXPAND CONFIG TABLES FOR EACH unit.id ACCORDING TO LOOPING VARIABLES
  
  ###                          ###    
# ### LOOP "b" BY REPORT.UNIT  ###
  ###                          ###
    
  #Loop Outputs 
    config.tables.ls <- list()
    config.tabs.ls <- list()
  
  #Loop Measurement - progress bar & timing
    progress.bar.b <- txtProgressBar(min = 0, max = 100, style = 3)
    maxrow.b <- length(unit.ids.sample)
    b.loop.startime <- Sys.time()
  
  #b <- 1 #LOOP TESTER (19 = "Raytown C-2")
  #for(b in c(1,2)){   #LOOP TESTER
  for(b in 1:length(unit.ids.sample)){   #START OF LOOP BY REPORT UNIT
  
    loop.start.time.b <- Sys.time()
    
    #Create unit.id.b (for this iteration)
      unit.id.b <- unit.ids.sample[b]
    
    #Print loop messages
      if(b == 1){print("FORMING TAB, GRAPH, AND TABLE CONFIG TABLES...")}
      #print(
      #  paste(
      #    "Loop num: ", b,", Report id: ",unit.id.b,
      #    ", Pct. complete:", round(100*b/length(unit.ids.sample), 2), "%"
      #  )
      #)
    
    #Create data frames for this loop - restrict to unit.id id i  
      resp.long.tb.b <- 
        resp.long.tb %>% filter(unit.id == unit.id.b)
      
    #Tab config table for this report unit
      config.tabs.ls[[b]] <-
        tibble(
          tab.type.id = 4,
          tab.type.name = "Building Overview",
          loop.id = resp.long.tb.b$building.id %>% unique
        ) %>%
        rbind(
          config.tab.types.tb %>% filter(tab.type.id != 4) %>% select(tab.type.id, tab.type.name) %>% mutate(loop.id = NA),
          .
        )
         
    #Tables config table for this report unit
      config.tables.ls[[b]] <- 
        full_join(
          config.tabs.ls[[b]],
          config.table.types.tb,
          by = "tab.type.id"
        )
      
    setTxtProgressBar(progress.bar.b, 100*b/maxrow.b)
    
  } # END OF LOOP 'b' BY REPORT.UNIT
  
  #Loop Measurement - progress bar & timing
    b.loop.duration <- Sys.time() - b.loop.startime
    close(progress.bar.b)
    #b.loop.duration
    
  #Section Clocking
    section3.duration <- Sys.time() - section3.startime
    section3.duration
    Sys.time() - sections.all.starttime

# 3-CONFIGS (TAB AND TABLE CONFIG TABLES) OUTPUTS ------------------
  #unit.ids.sample: vector with all report unit names in resp.long.tb (length = 19 for baseline data)
  #config.tabs.ls
    #[[report.unit]]
      #data frame where each line represents a tab
  #config.tables.ls
    #[[report.unit]]
      #data frame where each line represents a table
 
# 4-SOURCE DATA TABLES --------------------------------------------
  
  #Section Clocking
    section4.starttime <- Sys.time()
    
  #Load Configs Functions
    setwd(rproj.dir)
    source("4-source data tables functions.r")
  
  ###                          ###    
  ### LOOP "c" BY REPORT UNIT  ###
  ###                          ###
  
  #Loop outputs
    tabledata.ls <- list()
  
  #Loop Measurement - progress bar & timing
    progress.bar.c <- txtProgressBar(min = 0, max = 100, style = 3)
    maxrow.c <- config.tables.ls %>% sapply(., dim) %>% sapply(`[[`,1) %>% unlist %>% sum
    c.loop.startime <- Sys.time()
    
  c <- 1 #LOOP TESTER 
  #for(c in tabr.unit.ids.sample){   #LOOP TESTER
  #for(c in 1:length(unit.ids.sample)){   #START OF LOOP BY DISTRICT
    
    #Loop Inputs (both graphs and tables)
      unit.id.c <- unit.ids.sample[c]

      resp.long.tb.c <- 
        resp.long.tb %>%
        filter(unit.id == unit.id.c)
    
    #Print loop messages
      if(c == 1){print("Forming input data tables for graphs...")}
      #print(
      #  paste(
      #    "LOOP 'C' -- Loop num: ", c,", Report id: ",unit.id.c,
      #    ", Pct. complete:", round(100*c/length(unit.ids.sample), 2), "%"
      #  )
      #)
    
    ###                    ###
#   ### LOOP "d" BY TABLE  ###
    ###                    ###
    
    #Loop Inputs
      config.tables.df.c <- config.tables.ls[[c]]
      tabledata.ls.d <- list()
    
    #if(all(is.na(config.tables.df.c))){
      
    }else{
      
      d <- 2
      #for(d in 1:3){ #LOOP TESTER
      #for(d in 1:nrow(config.tables.df.c)){
        
        #Print loop messages
          #print(
          #  paste(
          #    "LOOP 'D' TABLE -- Loop num: ", d,
          #    ", Pct. complete:", round(100*d/dim(config.tables.df.c)[1], 2), "%"
          #  )
          #)
        
        config.tables.df.d <- config.tables.df.c[d,]
        
        #Define table aggregation formula
          row.header.formula <- 
            strsplit(config.tables.df.d$row.header.varname, ",") %>% unlist %>% as.vector %>% 
            {if(is.na(.)) "." else .} %>%
            {if(length(.)==1) . else paste(., collapse = "+")}
        
          col.header.formula <- 
            strsplit(config.tables.df.d$col.header.varname, ",") %>% unlist %>% as.vector %>% 
            {if(is.na(.)) "." else .} %>%
            {if(length(.)==1) . else paste(., collapse = "+")}
          
          table.formula.d <- 
            paste(
              row.header.formula,
              "~",
              col.header.formula,
              sep = ""
            ) %>% 
            as.formula

        #Define table filtering vector
          
          #TODO: adjust for state averages - either calculate once and insert into all or add a way to tell
          #code to pull in full dataset, not just district data (default inside look)
          
          if(is.na(config.tables.df.d$filter.varname)){
            table.filter.vector <- rep(TRUE, nrow(resp.long.tb.c))
          }else{
            table.filter.vector <- 
              resp.long.tb.c %>%
              select(config.tables.df.d$filter.varname) %>%
              unlist %>% as.vector %>%
              grepl(config.tables.df.d$filter.values, .)
          }
          
        #Form final data frame
          table.d <-  
            resp.long.tb.c %>%
            filter(table.filter.vector) %>%
            dcast(
              ., 
              formula = table.formula.d, 
              value.var = "value", 
              fun.aggregate = function(x){mean(x, na.rm = TRUE) %>% round(., digits = 1)}
            ) %>%
            .[,names(.)!= "NA"]
           
          #tabledata.df.d <-  
            #resp.long.tb.c %>%
            #table.data.filter.fun(dat = ., config.input = config.tables.df.d) %>%
            #summarize.table.fun(dat = ., config.input = config.tables.df.d) %>%
            #FirstTableOperations(tb = ., iterations = c(1))
        
      }   
         
          
        tabledata.ls.d[[d]] <- tabledata.df.d
        
      } ### END OF LOOP "d" BY TABLE ###
      
      names(tabledata.ls.d) <- config.tables.df.c$domain %>% RemoveNA() %>% as.character %>% c("role",.)
      tabledata.ls[[c]] <- tabledata.ls.d
    
    } ### END OF LOOP "d" BY TABLE
      
  } ### END OF LOOP "c" BY REPORT UNIT     
  
  #Loop Measurement - progress bar & timing
    c.loop.duration <- Sys.time() - c.loop.startime
    close(progress.bar.c)  
    #c.loop.duration
      
  #Section Clocking
    section4.duration <- Sys.time() - section4.starttime
    section4.duration
    Sys.time() - sections.all.starttime


# 4-SOURCE DATA TABLES OUTPUTS --------------------------------------------
  #graphdata.ls.c
    #[[report unit]]
    #data frame where each line represents a graph
  #tabledata.ls
    #[[report.unit]]
    #data frame where each line represents a table


# 5-OBJECT CREATION (GRAPHS & TABLES) ------------------------------------
  
  #Code Clocking
    section5.starttime <- Sys.time()
    
  #Load Configs Functions
    setwd(rproj.dir)
    source("5-object creation functions.r")
  
  
  ###                       ###    
# ### LOOP "f" BY DISTRICT  ###
  ###                       ###
  
  #Loop Progress Tracking
    progress.bar.f <- txtProgressBar(min = 0, max = 100, style = 3)
    maxrow.f <- length(unit.ids.sample)
    
  #Loop Outputs
    graphs.ls.f <- list()
    #tables.ls.f <- list()
  
  #f <- 1 #LOOP TESTER
  #for(f in 1:2){ #LOOP TESTER
  for(f in 1:length(unit.ids.sample)){
    
    #Loop units  
      unit.id.f <- unit.ids.sample[f]
      config.graphs.df.f <- config.graphs.ls.b[[f]]
      graphdata.ls.f <- graphdata.ls.c[[f]]
    
    #Print loop messages
      if(f == 1){print("FORMING GRAPHS & TABLES IN GGPLOT...")}
      #print(
      #  paste(
      #    "LOOP 'F' -- Loop num: ", f,", Loop id: ",unit.id.f,
      #    ", Pct. complete:", round(100*f/length(unit.ids.sample), 2), "%"
      #  )
      #)
    
    ###                       ###    
#   ### LOOP "g" BY GRAPH     ###
    ###                       ###
    
    #Loop output object(s)
    graphs.ls.g <- list()
    maxrow.g <- length(graphdata.ls.f)
    
    #g <- which(config.graphs.df.f$graph.type.id == "e")[1] #LOOP TESTER
    #for(g in 1:2) #LOOP TESTER
    for(g in 1:length(graphdata.ls.f))
      local({ #Necessary to avoid annoying and confusing ggplot lazy evaluation problem (see journal)
        
        #Print loop messages for bug checking
            #print(
            #  paste0(
            #    "LOOP 'g' -- Loop num: ", g,
            #    ", Pct. complete:", round(100*g/length(graphdata.ls.f), 2), "%"
            #  )
            #)
            #cprint(graphdata.df.g)
            #print(config.graphs.df.g)
        
        #Redefine necessary objects in local environment
          g<-g #same as above
        
        #CONFIG DATA TABLE
          config.graphs.df.g <- config.graphs.df.f[g,] %>% as.data.frame()
          
          #Making new [shortened] objects that will get a lot of use in graph formation; 
            x.varnames.g <- 
              config.graphs.df.g$x.varnames %>% strsplit(., ",") %>% unlist
            
            graph.header.varname <- x.varnames.g[1]
            graph.group.by.varnames <- if(length(x.varnames.g)==1){NULL}else{x.varnames.g[2:length(x.varnames.g)]}
           
        #GRAPH DATA TABLE
          graphdata.df.g <- graphdata.ls.f[[g]]
          
          #Manual ordering of graphdata.df.g
            order.ls <- 
              StringSplitVectorIntoList(
                vector = config.graphs.df.g$x.var.order,
                list.element.names = strsplit(config.graphs.df.g$x.varnames, ",") %>% unlist,
                list.level.split.char = ";",
                within.element.split.char = ","
              )
            
            #Special cases - formatting text so matches exactly for domain and answer option
              #order.ls[names(order.ls) == "domain"] <- #domain names to all caps
                #order.ls[names(order.ls) == "domain"] %>% lapply(., toupper)
              
              order.ls[names(order.ls) == "answer"] <-
                order.ls[names(order.ls) == "answer"] %>% unlist %>% as.numeric %>% list
            
            #Order graph data table by result  
              graphdata.df.g <-
                ManualOrderTableByVectorsWithValuesCorrespondingToVariableInTable(
                  tb = graphdata.df.g,
                  tb.order.varnames = names(order.ls),
                  ordering.vectors.list = order.ls
                )
            
           #Inserting corrected scale for graphs that have Answer Options along the bottom
            if(!is.null(graph.group.by.varnames) && "answer" %in% names(graphdata.df.g)){
              graphdata.df.g <- 
                left_join(graphdata.df.g,config.ans.opt.tb, by = c("answer" = "ans.num"))
              
              if(config.graphs.df.g$domain %in% c("lead","pd")){
                graphdata.df.g <- 
                  graphdata.df.g %>% 
                  select(graph.group.by.varnames, ans.text.agreement.num, measure, avg)
                graph.header.varname <- "ans.text.agreement.num"
              }else{
                graphdata.df.g <- 
                  graphdata.df.g %>% 
                  select(graph.group.by.varnames, ans.text.freq.num, measure, avg)
                graph.header.varname <- "ans.text.freq.num"
              }
            }
           
            #Capitalize headers in graphdata.df.g, all-caps for domain, upper-case first letter for everything else
             graphdata.df.g[,!names(graphdata.df.g) %in% c("domain","answer","measure","avg")] <- 
                graphdata.df.g[,!names(graphdata.df.g) %in% c("domain","answer","measure","avg")] %>% 
                apply(., c(1,2), FirstLetterCap_MultElements)
             
             graphdata.df.g[names(graphdata.df.g) == "domain"] <- 
               graphdata.df.g[names(graphdata.df.g) == "domain"] %>%
               apply(., c(1:2), toupper)
        
        ### BASE GRAPH FORMATION WITH GGPLOT2 ###
        
          #Base Graph
            graph.1 <- 
              FormBaseGraphObject.DataAndTheme( 
                dat = graphdata.df.g 
              )

          #Adding Columns (Clustered or Non-Clustered)
            
            #Define Fill Values
              if(
                strsplit(config.graphs.df.g$graph.fill, ",") %>% 
                  unlist %>% trimws %>% length %>% equals(1)
              ){
                graph.fill.g <- 
                  config.graphs.df.g$graph.fill %>% 
                  rep(., nrow(graphdata.df.g))
              }else{
                graph.fill.g <- 
                  strsplit(config.graphs.df.g$graph.fill, ",") %>% 
                  unlist %>% trimws %>% rev %>%
                  rep(., nrow(graphdata.df.g)/2)
              }

            #Add columns
              graph.2 <- 
                AddColsToGraph(
                  base.graph.input = graph.1,
                  dat = graphdata.df.g,
                  graph.orientation = config.graphs.df.g$graph.type.orientation,
                  graph.header.varname = graph.header.varname,
                  graph.group.by.varnames = graph.group.by.varnames,
                  graph.fill = c("#c7b7c7","#603356") %>% rep(., nrow(graphdata.df.g)/2),
                  print.graph = FALSE
                )
              
          #Manually order x-axis according to configs
            graph.3 <- 
              graph.2 + 
              scale_x_discrete(
                limits = levels(
                  factor(
                    graphdata.df.g[,names(graphdata.df.g) == graph.header.varname] %>% 
                      unlist %>% unique %>% as.vector, 
                    levels = graphdata.df.g[,names(graphdata.df.g) == graph.header.varname] %>% 
                      unlist %>% unique %>% as.vector %>% rev
                  )
                )
              )
          
          #Add Graph Averages (as error bar)
            #NOTE: does not depend on config.graphs.df.g - taken care of with if statement outside function
            if(!is.na(config.graphs.df.g$avg.level)){
              graph.4 <-
                AddGraphAverages(
                  base.graph.input = graph.3,
                  dat = graphdata.df.g,
                  graph.header.varname = graph.header.varname,
                  graph.group.by.varnames = graph.group.by.varnames,
                  avg.bar.color = config.graphs.df.g$avg.bar.color,
                  dat.configs = config.graphs.df.g,
                  print.graph = FALSE 
                )
            }else{
              graph.4 <- graph.3
            }
            
          #Add data labels 
          
            #Graph label data frame
              graph.labels.df <- 
                CreateGraphLabels(
                  dat = graphdata.df.g, 
                  dat.measure.varname = "measure", 
                  height.ratio.threshold = 8.2,
                  dat.configs = config.graphs.df.g
                )
            
            #Add Data labels to graph
              graph.5 <-
                AddGraphDataLabels(
                  base.graph.input = graph.4,
                  dat = graphdata.df.g,
                  graph.header.varname = graph.header.varname,
                  graph.group.by.varnames = graph.group.by.varnames,
                  graph.orientation = config.graphs.df.g$graph.type.orientation,
                  dat.labels = graph.labels.df,
                  label.font.size = 4,
                  print.graph = FALSE
                )
            
          #Final step: graph orientation - flip for bar charts
            graph.g <- 
              GraphOrientation(
                base.graph.input = graph.5,
                graph.orientation = config.graphs.df.g$graph.type.orientation
              )
              
        graphs.ls.g[[g]] <<- graph.g
       
        
      })  ### END OF LOOP "g" BY GRAPH ###

      graphs.ls.f[[f]] <- graphs.ls.g
    
    ###                       ###    
#   ### LOOP "g" BY TABLE     ###
    ###                       ###
    {
    #Loop output object(s)
      #tables.ls.g <- list()
    
    #g <- 1 #LOOP TESTER
    #for(g in 1:2){ #LOOP TESTER
    #for(g in 1:length(tabledata.ls[[f]])){
      
      #Prep Loop Inputs
        #if(dim(tabledata.ls[[f]][[g]])[1] == 0){
        #  tabledata.ls[[f]][[g]][1,] <- rep(0, dim(tabledata.ls[[f]][[g]])[2]) 
        #}
        
        #tabledata.df.g <- tabledata.ls[[f]][[g]]
        #config.tables.df.g <- config.tables.df.c[g,]
    
      #Print loop messages for bug checking
        #print(
        #  paste0(
        #    "LOOP 'g' -- Loop num: ", g,
        #    ", Pct. complete:", round(100*g/length(tabledata.ls[[f]]), 2), "%"
        #  )
        #)
        #print(tabledata.df.g)
        #print(config.tables.df.g)
        
      #Create FlexTable Object
         #ft.g <- FlexTable(
          #data = tabledata.df.g,
          #header.columns = TRUE,
          #add.rownames = FALSE,
          
          #header.cell.props = cellProperties(background.color = "#5F3356", border.style = "none"), #TODO:Should put into configs instead of specifying in code
          #header.text.props = textProperties(
          #  color = "white", 
          #  font.size = 15,
          #  font.family = "Century Gothic",
          #  font.weight = "bold"),
          #header.par.props = parProperties(text.align = "center"),
          #body.cell.props = cellProperties(background.color = "white", border.style = "none"),
          #body.text.props = textProperties(
          #  color = "#515151",
          #  font.size = 15,
          #  font.family = "Century Gothic"
          #)
        #)
        
        #if(g == 1){
          #ft.g[dim(tabledata.df.g)[1],] <- 
            #chprop(
              #textProperties(
                #font.weight = "bold",
                #font.size = 18,
                #font.family = "Century Gothic"
              #)
            #) #Bold text on last line (totals)
          #ft.g[,1] <- chprop(parProperties(text.align = "center"))
          #ft.g <- setFlexTableWidths(ft.g, widths = c(4, rep(6,dim(tabledata.df.g)[2]-1)))      
          
        #}
        
        #if(g != 1){
        #  ft.g[,1] <- chprop(parProperties(text.align = "right"))
        #}
        
        #ft.g[1:dim(tabledata.df.g)[1],2:dim(tabledata.df.g)[2]] <- #Center align numbers in all but first column
        #  chprop(parProperties(text.align = "center")) 
        #ft.g <- setZebraStyle(ft.g, odd = "#D0ABD6", even = "white" ) 
        
        #tables.ls.g[[g]] <- ft.g
        
    #} ### END OF LOOP "g" BY TABLE ###
  
    #names(tables.ls.g) <- c("role","etlp","cfa","dbdm","pd","lead") #TODO:WAS CAUSING PROBLEMS WITH ORDERING OF TABLES ON SLIDES BECAUSE HAD NOT BEEN UPDATED TO NEW ORDER OF domainS
    #tables.ls.f[[f]] <- tables.ls.g
    }
      
    setTxtProgressBar(progress.bar.f, 100*f/maxrow.f)
    
  } ### END OF LOOP "f" BY REPORT.UNIT
  close(progress.bar.f)
    
   #Section Clocking
    section5.duration <- Sys.time() - section5.starttime
    section5.duration
    Sys.time() - sections.all.starttime



# 5-OBJECT CREATION (GRAPHS & TABLES) OUTPUTS ------------------------------------
  #graphs.ls.f
    #[[report.unit]]
      #ggplot object
  #tables.ls.f
    #[[report.unit]]
      #FlexTable object


# 6-EXPORT -----------------------------------------------
  
  #Code Clocking
    section6.starttime <- Sys.time()
    
  #Load Configs Functions
    setwd(rproj.dir)
    source("6-powerpoints export functions.r")
  
  
  # POWERPOINT GLOBAL CONFIGURATIONS
  
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
    
    #Text formatting
      title.format <- textProperties(color = titlegreen, font.size = 48, font.weight = "bold", font.family = "Century Gothic")
      title.format.small <- textProperties(color = titlegreen, font.size = 40, font.weight = "bold", font.family = "Century Gothic")
      subtitle.format <- textProperties(color = notesgrey, font.size = 28, font.weight = "bold", font.family = "Century Gothic")
      section.title.format <- textProperties(color = "white", font.size = 48, font.weight = "bold", font.family = "Century Gothic")
      notes.format <- textProperties(color = notesgrey, font.size = 14, font.family = "Century Gothic")
      setwd(source.tables.dir)
  
  ###                          ###    
# ### LOOP "h" BY REPORT UNIT  ###
  ###                          ###
  
  #Progress Bar
    progress.bar.h <- txtProgressBar(min = 0, max = 100, style = 3)
    maxrow.h <- sapply(config.slides.ls.b, dim) %>% sapply(`[[`,1) %>% unlist %>% sum
    printed.reports.ls <- list()
  
  #h <- 1 #LOOP TESTER
  #for(h in ceiling(runif(5,1,length(config.slides.ls.b)))){
  for(h in 1:length(config.slides.ls.b)){ #LOOP TESTER
    
    #Reading 'Cadre' so it can be added to file name
      #cadre.h <- 
      #  buildings.tb %>% 
      #  filter(buildings.tb %>% select(report.unit) %>% equals(unit.ids.sample[h])) %>% 
      #  select(cadre) %>% 
      #  unlist %>% 
      #  FirstLetterCap_OneElement()
    
    #Set up target file
      template.file <- paste(source.tables.dir,
                             "template_purple reports.pptx",
                             sep = "")
      if(sample.print){
        file.name.h <- 
          paste(
            #cadre.h,
            #"_",
            h,
            "_",
            unit.ids.sample[h],
            "_",
            gsub(":",".",Sys.time()) %>% substr(., 15,19),
            ".pptx", 
            sep=""
          ) 
      }else{
        file.name.h <- 
          paste(
            "CWIS Repeated Meas_",
            FirstLetterCap_MultElements(unit.ids.sample[h]),
            "_",
            gsub(":",".",Sys.Date()),
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
        mutate(graph.id = 1:nrow(config.graphs.ls.b[[h]]))#config.graphs.ls.b[[h]] %>% .[,ncol(config.graphs.ls.b[[h]])] %>% seq_along(.))
      
      config.tables.df.h <- config.tables.ls[[h]]
      
      unit.id.h <- unit.ids.sample[h]
      district.h <- strsplit(unit.id.h, "_") %>% unlist %>% .[1] %>% toupper()
      school.h <- strsplit(unit.id.h, "_") %>% unlist %>% .[2] %>% toupper()
      config.slides.df.h <- config.slides.ls.b[[h]]
      
      graphs.ls.h <- graphs.ls.f[[h]]
      #tables.ls.h <- tables.ls.f[[h]]
    
    ###                     ###    
#   ### LOOP "i" BY SLIDE   ###
    ###                     ###
    
    #i <- 4 #LOOP TESTER
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
          loop.varnames <- 
            config.graphs.df.h %>% 
            filter(config.graphs.df.h$slide.type.id == slide.type.id.i) %>%
            select(names(config.slide.df.i)[grep("loop", names(config.slide.df.i))]) %>%
            apply(., 2, unique) %>%
            unlist %>% as.vector %>% RemoveNA
          
          config.graphs.df.i <-
            config.graphs.df.h %>%
            filter(config.graphs.df.h$slide.type.id == slide.type.id.i)
         
          if(length(loop.varnames) >= 1){
            config.graphs.df.i <- 
              config.graphs.df.i %>%
              filter(
                config.graphs.df.i %>% 
                select(loop.varnames[1]) %>% 
                equals(config.slide.df.i[,loop.varnames[1]] %>% unlist)
              )
          }
          
          if(length(loop.varnames) >= 2){
            config.graphs.df.i <- 
              config.graphs.df.i %>%
              filter(
                config.graphs.df.i %>% 
                select(loop.varnames[2]) %>% 
                equals(config.slide.df.i[,loop.varnames[2]] %>% unlist)
              )
          }
          
          if(length(loop.varnames) >= 3){
            config.graphs.df.i <- 
              config.graphs.df.i %>%
              filter(
                config.graphs.df.i %>% 
                select(loop.varnames[3]) %>% 
                equals(config.slide.df.i[,loop.varnames[3]] %>% unlist)
              )
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
            
            graph.k <- graphs.ls.h[config.graphs.df.i$graph.id[k]]
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
        
        
      #ADD TABLES
        {
        #TODO: Will want to generalize so can add more than one table to each slide if necessary
        #config.tables.df.i <- config.tables.df.h %>% 
        #  filter(slide.type.id == slide.type.id.i)
        
        #if(dim(config.tables.df.i)[1] != 0 && !is.na(config.tables.df.i$table.type.id)){
          
        #  if(is.na(config.slide.df.i$domain)){
        #    config.tables.df.i <- config.tables.df.i[is.na(config.tables.df.i$domain),]
        #  }else{
        #    config.tables.df.i <- config.tables.df.i[config.tables.df.i$domain == config.slide.df.i$domain,]
        #  }
          
        #  if(i == 2){
        #    ft.i <- tables.ls.f[[h]][[1]]
        #  }else{
        #    ft.i <- tables.ls.f[[h]][[which(names(tables.ls.f[[h]])==config.tables.df.i$domain)]]
        #  }
        #  
        #  ppt.h <- addFlexTable(ppt.h, 
        #                        ft.i, 
        #                        height = config.tables.df.i$height,
        #                        width = config.tables.df.i$width,
        #                        offx = config.tables.df.i$offx,
        #                        offy = config.tables.df.i$offy
        #                        #par.properties=parProperties(text.align="center", padding=0)
        #  )
        }
        
      #ADD POT OBJECTS
        
        ###                         ###    
#       ### LOOP "j" BY POT OBJECT  ###
        ###                         ###
        
        config.pot.i <- config.pot.types.tb[config.pot.types.tb$slide.type.id == slide.type.id.i,]
        
        if(any(!is.na(config.pot.i$domain))){
          config.pot.i <- filter(config.pot.i, grepl(as.character(config.slide.df.i$domain), config.pot.i$domain))
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
              ifelse(!is.na(config.pot.i$content.dynamic[j])," ",""),
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
      setTxtProgressBar(progress.bar.h, 100*h/length(unit.ids.sample))
      
    } #END OF LOOP "i" BY SLIDE
    
    writeDoc(ppt.h, file = target.path.h) #Write complete pptx object to file
    
    print(h)
    printed.reports.ls[[h]] <- unit.ids.sample[h]
  
  } # END OF LOOP "h" BY REPORT.UNIT      
  
  close(progress.bar.h)      

 #Section Clocking
    section6.duration <- Sys.time() - section6.starttime
    section6.duration
    Sys.time() - sections.all.starttime

windows()        



# 2.5-TEST TABLE OUTPUTS ------------------   
  
  #Table 1: Average Response by Domain
    building.domain.mean.value.tb <- 
      resp6.tb %>%
      dcast(
        ., 
        formula = building.id ~ domain, 
        value.var = "value", 
        fun.aggregate = function(x){mean(x, na.rm = TRUE) %>% round(., digits = 1)}
      ) %>%
      .[,names(.)!= "NA"]
      
    domain.district.mean.value.v <-
      resp5.tb %>%
      group_by(., domain) %>%
      summarize(x = mean(value)) %>%
      select(x) %>%
      unlist %>% as.vector %>%
      round(., digits = 1) %>%
      as.matrix() %>% t
    
    domain.state.mean.value.v <- 
      domain.district.mean.value.v + 
      rnorm(length(domain.district.mean.value.v), mean = 0, sd = sd(domain.district.mean.value.v)) %>%
      round(., digits = 1) %>%
      as.matrix() %>% t
  
  #Table 3: Average Response by Practice (CFA)
  
    building.cfa.practice..mean.value.tb <-
      resp5.tb %>%
      filter(domain == "cfa") %>%
      dcast(
        .,
        formula = building ~ variable,
        value.var = "value",
        fun.aggregate = function(x){mean(x, na.rm = TRUE) %>% round(., digits = 1)}
      )
    
    practice.cfa.district.mean.value.v <-
      resp5.tb %>%
      filter(domain == "cfa") %>%
      group_by(., variable) %>%
      summarize(x = mean(value)) %>%
      select(x) %>%
      unlist %>% as.vector %>%
      round(., digits = 1) %>%
      as.matrix() %>% t
    
    practice.cfa.state.mean.value.v <- 
      practice.cfa.district.mean.value.v + 
      rnorm(length(practice.cfa.district.mean.value.v), mean = 0, sd = sd(practice.cfa.district.mean.value.v)) %>%
      round(., digits = 1) %>%
      as.matrix() %>% t
  
    
  #Establish Outputs Directory
    outputs.dir <- 
      paste(
        working.dir,
        "\\4_outputs\\",
        gsub(":",".",Sys.time()), 
        sep = ""
      )
          
      dir.create(
        outputs.dir,
        recursive = TRUE
      )
      
      setwd(outputs.dir)
              
  #Set up target file
    template.file <- 
      paste(
        source.tables.dir,
        "dashboard_template.xlsx",
        sep = ""
      )
    
    file.name.h <- 
      paste(
        "test.output_",
        gsub(":",".",Sys.time()),
        ".xlsx",
        sep = ""
      )
      
    target.path.h <- 
      paste(
        outputs.dir,
        "\\",
        file.name.h,
        sep=""
      ) 
    
    file.copy(template.file, target.path.h)
  
  #1-Time Export of Long Data to do timed experiment for creating dashboard
    setwd(outputs.dir) 
    
    write.csv(
      resp5.tb,
      file = "farmington_longdata.csv",
    )
    
  #Write table to file
    setwd(outputs.dir)
    wb <- loadWorkbook(file.name.h, create = FALSE)
    setStyleAction(wb, XLC$"STYLE_ACTION.NONE")
    
    #Table 1
      writeWorksheet(
        object = wb, 
        data = building.domain.mean.value.tb,
        sheet = "District Overview (vs district)",
        startRow = 3,
        startCol = 1,
        header = TRUE
      )
      
      writeWorksheet(
        object = wb, 
        data = domain.district.mean.value.v,
        sheet = "District Overview (vs district)",
        startRow = 19,
        startCol = 2,
        header = FALSE
      )
      
      writeWorksheet(
        object = wb, 
        data = domain.state.mean.value.v,
        sheet = "District Overview (vs district)",
        startRow = 20,
        startCol = 2,
        header = FALSE
      )
      
    #Table 3
      writeWorksheet(
        object = wb, 
        data = building.cfa.practice..mean.value.tb,
        sheet = "District Overview (vs district)",
        startRow = 3,
        startCol = 20,
        header = TRUE
      )
      
      writeWorksheet(
        object = wb, 
        data = practice.cfa.district.mean.value.v,
        sheet = "District Overview (vs district)",
        startRow = 19,
        startCol = 21,
        header = FALSE
      )
      
      writeWorksheet(
        object = wb, 
        data = practice.cfa.state.mean.value.v,
        sheet = "District Overview (vs district)",
        startRow = 20,
        startCol = 21,
        header = FALSE
      )
      
  saveWorkbook(wb)
