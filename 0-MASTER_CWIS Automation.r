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
  
      # Figure out what machine code is running on
      if(dir.exists("C:\\Users\\willi")){m900 <- TRUE}else{m900 <- FALSE}
      
      # Set Working Directory and R Project Directory
      if(m900){  
        #M900
          wd <- "C:\\Users\\willi\\Google Drive\\1. FLUX CONTRACTS - CURRENT\\2016-09 EXT Missouri\\3. MO GDRIVE\\8. CWIS\\2019-06 District Dashboards\\"
          rproj.dir <- "C:\\Users\\willi\\Documents\\GIT PROJECTS\\CWIS-automation\\"
      }else{
        #Thinkpad T470
          wd <- "G:\\My Drive\\1. FLUX CONTRACTS - CURRENT\\2016-09 EXT Missouri\\3. MO GDRIVE\\8. CWIS\\2019-06 District Dashboards\\"
          rproj.dir <- "C:\\Users\\WNF\\Documents\\GIT PROJECTS\\CWIS-automation\\"
      }
    
    #Check Directories
      wd <- if(!dir.exists(wd)){choose.dir()}else{wd}
      rproj.dir <- if(!dir.exists(rproj.dir)){choose.dir()}else{rproj.dir}
    
    #Source Tables Directory (raw data, configs, etc.)
      source.tables.dir <- paste(wd,"\\3_source_tables\\", sep = "")
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
  #wd: working directory - Google Drive folder "2018-08 Green Reports"
  #rproj.dir: directory for R project; also contains source data, additional function scripts, and config tables.
  #source.tables.dir: directory with raw data, configs, etc.

# 1-IMPORT -----------------------------------------
  
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
    
  #Establish Outputs Directory
    outputs.parent.folder <- 
      paste(
        wd,
        "4_outputs\\",
        sep  = ""
      )
    
    if(sample.print){
      outputs.dir <- 
        paste(
          outputs.parent.folder,
          gsub(":",".",Sys.time()), 
          sep = ""
        )
      
      dir.create(
        outputs.dir,
        recursive = TRUE
      )
    }
    
    if(!sample.print){
      setwd(outputs.parent.folder)
      
      most.recent.output.folder <-
        list.dirs()[grepl("FULL PRINT",list.dirs())] %>%
        .[order(.)] %>%
        .[length(.)]
    }
    
    if(!sample.print & grepl(Sys.Date(), most.recent.output.folder)){ #Set same outputs directory as before if most recent full print output 
      #is from the same date and has some reports already in it
      setwd(most.recent.output.folder)
      
      districts.that.already.have.reports <-
        list.files()[!grepl("desktop.ini", list.files())] %>%
        str_split(., "_") %>%
        lapply(., `[[`, 2) %>%
        unlist
      
      outputs.dir <- 
        paste(
          outputs.parent.folder,
          most.recent.output.folder,
          sep = ""
        )
      
    }else{ #If from different day, start full print over
      
      districts.that.already.have.reports <- ""
      
      outputs.dir <- 
        paste(
          wd,
          "\\4_outputs\\",
          gsub(":",".",Sys.time()),
          sep  = ""
        )
      
      dir.create(
        outputs.dir,
        recursive = TRUE
      )
      
    }
    
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
    
    resp2.tb %<>% 
      ReplaceNames(
        ., 
        current.names = names(resp2.tb)[names(resp2.tb) == as.vector(report.unit)], 
        new.names =  "unit.id"
      )
    
    names(resp2.tb) <- SubRepeatedCharWithSingleChar(string.vector = names(resp2.tb), char = ".")
    
  #Lower-case all character variable data
    resp2.tb <- LowerCaseCharVars(resp2.tb)
  
  #Add variables: building.id, building.name building.level 
    resp2.tb %<>% mutate(building.id.raw = paste(unit.id,building,sep=".") %>% gsub(" |\\/", ".", .))
    
    resp2.tb <-
      left_join(
        resp2.tb,
        buildings.tb %>% select(building.id.raw, building.id, building.name, building.level),
        by = "building.id.raw"
      )
    
    resp2.tb$building.level <- Proper(resp2.tb$building.level)
    
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
        inner_join(
          resp3.tb, 
          low.response.nums.filter.tb %>% select(building.id, year, filter.combined), 
          by = c("year","building.id")
        ) %>%
        filter(filter.combined)
      
  #Column Filters (only columns necessary for producing reports)
    
    resp5.tb <-
      resp4.tb %>%
      SelectColsIn(
        ., 
        "NOT.IN",
        questions.tb %>% filter(necessary.in.final.data == 0) %>% select(var.id) %>% unlist %>% as.vector
      )
  
  #Reshape to Long Data by response to CWIS question
    cwis.varnames <- FilterVector(grepl(paste0(domains, collapse = "|"), names(resp5.tb)), names(resp5.tb))
    resp6.tb <-
      melt( #Melt to long data frame for all cwis vars
        resp5.tb,
        id.vars = names(resp5.tb)[!names(resp5.tb) %in% cwis.varnames], 
        measure.vars = cwis.varnames
      ) %>% 
      filter(!is.na(value)) %>% #remove rows with no answer for cwis vars
      MoveColsLeft(., c("resp.id","unit.id")) %>% #Rearrange columns: resp.id and unit.id at the front
      as_tibble()
    
  #ADDING NEW USEFUL VARIABLES ----
    
    #Add domain variables
      resp7.tb <- 
        left_join(
          x = resp6.tb,
          y = questions.tb %>% select(var.id, domain, practice),
          by = c("variable"="var.id")
        ) %>%
        left_join(
          x = ., 
          y = domains.tb,
          by = c("domain" = "domain.id")
        )
    
    #Add proficiency dummy variable
      resp8.tb <- 
        resp7.tb %>%
        mutate(
          is.proficient = ifelse(value >= 4, 1, 0)
        )
      
    #Add variable for baseline, most recent, and next-to-most-recent years for each unit.id
      resp8.tb$year[resp8.tb$year == "baseline"] <- "0000"
      
#FUNCTION - could be made into function to designate alphabetic first/last/penultimate by group
      year.var.helper.tb <-
        resp8.tb %>%
        select(year, unit.id) %>%
        unique 
      
      year.var.helper.ls <- list()
      
      for(i in 1:length(unique(year.var.helper.tb$unit.id))){
        unit.id.i <- unique(year.var.helper.tb$unit.id)[i]
        year.var.helper.tb.i <- 
          year.var.helper.tb %>% 
          filter(unit.id == unit.id.i) %>%
          arrange(year) %>% 
          mutate(
            num.measurements = nrow(.),
            is.baseline = 0,
            is.most.recent = 0, 
            is.current = 0,
            is.current.or.most.recent = 0,
            is.current.or.baseline = 0
          )
        
        year.var.helper.tb.i$is.baseline[1] <- 1
        year.var.helper.tb.i$is.current[nrow(year.var.helper.tb.i)] <- 1
        year.var.helper.tb.i$is.most.recent[nrow(year.var.helper.tb.i)-1] <- 1
        year.var.helper.tb.i$is.current.or.most.recent[
          year.var.helper.tb.i$is.current == 1 | year.var.helper.tb.i$is.most.recent ==1
        ] <- 1
        year.var.helper.tb.i$is.current.or.baseline[
          year.var.helper.tb.i$is.current == 1 | year.var.helper.tb.i$is.baseline ==1
          ] <- 1
        
        year.var.helper.ls[[i]] <- year.var.helper.tb.i
      }
      
      year.var.helper.tb <- do.call(rbind, year.var.helper.ls)
      
      resp9.tb <- 
        left_join(
          resp8.tb,
          year.var.helper.tb,
          by = c("year", "unit.id")
        ) %>% 
        filter(num.measurements > 1)

  #RESTRICT DATA TO SAMPLE OF USER-DEFINED SIZE IF DOING SAMPLE PRINT ----
    if(sample.print){
      is.valid.sample <- FALSE
      while(!is.valid.sample){
        
        resp10.tb <- 
          RestrictDataToSample(
            tb = resp9.tb,
            report.unit = "unit.id",
            sample.print = sample.print,
            sample.group.unit = "unit.id",
            sample.size = sample.size
          )
        
        is.valid.sample <- 
          ifelse(
            dcast(
              data = resp10.tb, 
              formula = unit.id ~ .,
              fun.aggregate = function(x){length(unique(x))},
              value.var = "building.id"
            ) %>% 
            select(".") %>%
            unlist %>% as.vector %>%
            is_weakly_less_than(max.building.count) %>% 
            all,
            TRUE,
            FALSE
          )
      }
    }
    
    if(!sample.print){
      resp10.tb <- 
        resp9.tb %>%
        filter(!unit.id %in% districts.that.already.have.reports) %>%
        filter(unit.id %in% (unit.id %>% unique %>% .[5]))
    }

    resp.long.tb <- resp10.tb
    
    unit.ids.sample <-
      resp.long.tb %>%
      select(unit.id) %>%
      unique %>%
      unlist %>% as.vector
          
  #Section Clocking
    section2.duration <- Sys.time() - section2.starttime
    section2.duration
    Sys.time() - sections.all.starttime

# 2-CLEANING OUTPUTS ------------------          
  #resp.long.tb
    
# 3-CONFIGS (TAB, TABLE, TEXT CONFIG TABLES) ------------------
  
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
    config.tabs.ls <- list()
    config.tables.ls <- list()
    config.text.ls <- list()

  
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
    
    #Create data frames for this loop - restrict to unit.id id i  
      resp.long.tb.b <- 
        resp.long.tb %>% filter(unit.id == unit.id.b)
      
    #Other useful inputs for forming config tables
      district.name.v <- 
        resp.long.tb.b$unit.id %>% unique %>% unlist %>% as.vector %>%
        str_split(., "-") %>% lapply(., Proper) %>% unlist %>% as.vector
      
      if(length(district.name.v) > 1){
        district.name.v[length(district.name.v)] <- district.name.v[length(district.name.v)] %>% toupper()
      }
      
      district.name <- district.name.v %>% paste(., collapse = "-")
      
      tab4.loopvarname <- 
        config.tab.types.tb %>% 
        select(tab.loop.var.1) %>% 
        unlist %>% unique %>% RemoveNA
      
      building.names <- resp.long.tb.b %>% select(tab4.loopvarname) %>% unlist %>% unique
      
      building.names.tb <- 
        tibble(
          tab.type.id = 4,
          loop.id = building.names
        )
      
    #Tab config table for this report unit
      
      config.tabs.ls[[b]] <-
        tibble(
          tab.type.id = 4,
          tab.type.name = "Building Summary",
          loop.id = building.names 
        ) %>%
        rbind(
          config.tab.types.tb %>% select(tab.type.id, tab.type.name) %>% mutate(loop.id = NA) %>% filter(tab.type.id != 4),
          .
        ) %>%
        mutate(
          tab.name = 
            c(
              tab.type.name[1:3],
              paste(
                "Building Summary (",
                1:length(building.names),
                ")",
                sep = ""
              )
            )
        )
         
    #Tables config table for this report unit
      config.tables.ls[[b]] <- 
        full_join(
          config.tabs.ls[[b]],
          config.table.types.tb,
          by = "tab.type.id"
        )
      
    #Text configs table for this report unit
      
      config.text.ls[[b]] <-
        full_join(
          config.text.types.tb,
          building.names.tb,
          by = "tab.type.id"
        ) %>% 
        full_join(
          .,
          config.tabs.ls[[b]],
          by = c("tab.type.id","loop.id")
        ) %>%
        mutate(text.value = district.name) %>%
        mutate(
          text.value = 
            ifelse(text.type == "building", loop.id, text.value)
        ) %>%
        mutate(text.value = Proper(text.value))
        
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

# 3-CONFIGS (TAB, TABLE, TEXT CONFIG TABLES) OUTPUTS ------------------
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
    tables.ls <- list()
  
  #Loop Measurement - progress bar & timing
    progress.bar.c <- txtProgressBar(min = 0, max = 100, style = 3)
    nrows.c <- lapply(config.tables.ls, nrow) %>% unlist
    c.loop.startime <- Sys.time()
  
  #Building Level Order
    building.level.order.v <- c("Elem.","High","Middle","Technology Ctr.","Other")
    
  #c <- 1 #LOOP TESTER 
  #for(c in tabr.unit.ids.sample){   #LOOP TESTER
  for(c in 1:length(unit.ids.sample)){   #START OF LOOP BY unit.id
    
    #Loop Inputs (both graphs and tables)
      unit.id.c <- unit.ids.sample[c]
    
    #Print loop messages
      if(c == 1){print("Forming tables for export...")}
    
    #Tabs 1 & 2 ----
      ###                                   ###
  #   ### LOOP "d" BY TABLE FOR TABS 1 & 2  ###
      ###                                   ###
      
      #Loop Inputs
        config.tables.input.tb <- 
          config.tables.ls[[c]] %>% 
          filter(!is.na(table.type.id)) %>%
          filter(tab.type.id %in% c(1,2))
        tables.tab12.ls <- list()
        
        #d <- 2
        #for(d in 1:3){ #Loop Tester
        for(d in 1:nrow(config.tables.input.tb)){ ### START OF LOOP "d" BY TABLE ###
          
          config.tables.tb.d <- config.tables.input.tb[d,]
          
          #print(
          #  paste(
          #    "LOOP 'd' -- Loop num: ", d,
          #    ", Report id: ",unit.id.c,
          #    ", Tab: ", config.tables.tb.d$tab.type.name[d],
          #    ", Table: ", config.tables.tb.d$table.type.name[d],
          #    ", Pct. complete: ", round(100*d/nrow(config.tables.tb.d), 2), "%",
          #    sep = ""
          #  )
          #)
          
          
          #Define table aggregation formula
            table.formula.d <-
              DefineTableRowColFormula(
                row.header.varnames = strsplit(config.tables.tb.d$row.header.varname, ",") %>% unlist %>% as.vector,
                col.header.varnames = strsplit(config.tables.tb.d$col.header.varname, ",") %>% unlist %>% as.vector
              )
          
          #Define table filtering vector
            table.filter.v <-
              DefineTableFilterVector(
                tb = resp.long.tb,
                filter.varnames = config.tables.tb.d$filter.varname %>% strsplit(., ";") %>% unlist %>% as.vector,
                filter.values = config.tables.tb.d$filter.values %>% strsplit(., ";") %>% unlist %>% as.vector
              )
            
          #Define Table Aggregation Function
            table.aggregation.function <-
              function(
                x
              ){
                allowed.functions <- c("count", "count.unique", "mean", "display.unique")
                
                if(!config.tables.tb.d$aggregate.function %in% allowed.functions){
                  stop(
                    paste(
                      "Aggregate function must be one of allowed functions: ", 
                      paste0(allowed.functions, collapse = ", "),
                      sep = ""
                    )
                  )
                }
                
                if(config.tables.tb.d$aggregate.function == "count"){
                  result <- length(x)
                }
                
                if(config.tables.tb.d$aggregate.function == "count.unique"){
                  result <- length(unique(x))
                }
                
                if(config.tables.tb.d$aggregate.function == "mean"){
                  result <- mean(x, na.rm = TRUE)
                }
                
                if(config.tables.tb.d$aggregate.function == "display.unique"){
                  result <- x %>% unique %>% unlist %>% as.vector
                }
                
                return(result)
              }
            
          #Form final data frame
            table.d <-  
              resp.long.tb %>%
              filter(table.filter.v) %>%
              dcast(
                ., 
                formula = table.formula.d, 
                value.var = config.tables.tb.d$value.varname, 
                fun.aggregate = table.aggregation.function
              ) %>%
              .[,names(.)!= "NA"]
            
          #Modifications for specific tables
            
            if(grepl("building.level", table.formula.d) %>% any){
              table.d <- 
                left_join(
                  building.level.order.v %>% as.data.frame %>% ReplaceNames(., ".", "building.level"), 
                  table.d,
                  by = "building.level"
                )
            }
            
            if(!config.tables.tb.d$row.header){  #when don't want row labels
              table.d <- table.d %>% select(names(table.d)[-1])
            }
            
           
        
          tables.tab12.ls[[d]] <- table.d
          
          
          progress.bar.value.c <- 
            ifelse(c==1, 0, 1) %>%
            rep(., nrows.c[1:(c-1)] %>% length) %>%
            multiply_by(nrows.c[1:(c-1)]) %>%
            add(d) %>% 
            divide_by(sum(nrows.c)) %>% 
            multiply_by(100) 
            
          setTxtProgressBar(progress.bar.c, progress.bar.value.c)
          
        } ### END OF LOOP "d" BY TABLE ###
        
        names(tables.tab12.ls) <- 
          paste(
            rep(unit.id.c, length(tables.tab12.ls)),
            ".",
            c(1:length(tables.tab12.ls)),
            ".",
            config.tables.tb.d$table.type.name, 
            sep = ""
          ) %>%
          gsub("-", " ", .) %>%
          gsub(" ", ".", .) %>%
          SubRepeatedCharWithSingleChar(., ".") %>%
          tolower
      
    #Tab 3 ----
        
      #Loop Inputs
        tables.tab3.ls <- list()
      
      #State Average Table - Last School Year vs. Current
        tables.tab3.ls[[1]] <-
          resp.long.tb %>% 
          filter(
            unit.id == unit.id.c & 
              is.current.or.most.recent == 1
          ) %>%
          SplitColReshape.ToLong(
            df = ., 
            id.varname = "resp.id", 
            split.varname = "domain", 
            split.char = ","
          ) %>%
          as_tibble() %>%
          dcast(
            data = .,
            formula = domain ~ year,
            fun.aggregate = mean,
            value.var = "value"
          ) %>%
          mutate(
            Trend = .[,3] - .[,2]
          ) %>%
          TransposeTable(., keep.first.colname = FALSE)
        
        tables.tab3.ls[[1]] %<>%
          apply(
            X = tables.tab3.ls[[1]][,2:ncol(tables.tab3.ls[[1]])], 
            MARGIN = 2, 
            FUN = as.numeric
          ) %>%
          cbind(
            tables.tab3.ls[[1]][,1],
            .
          ) %>%
          ReplaceNames(., "Var.1", "")
        
        tables.tab3.ls[[1]][1,1] <- "Previous School Year"
          
      
      #Building Average Table - Last School year vs. Current  
        tables.tab3.ls[[2]] <- 
          resp.long.tb %>% 
          filter(
            unit.id == unit.id.c & 
              is.current.or.most.recent == 1
          ) %>%
          SplitColReshape.ToLong(
            df = ., 
            id.varname = "resp.id", 
            split.varname = "domain", 
            split.char = ","
          ) %>%
          as_tibble() %>%
          dcast(
            data = .,
            formula = building.name + domain ~ year,
            fun.aggregate = mean,
            value.var = "value"
          ) %>%
          mutate(
            Trend = .[,4] - .[,3]
          ) %>%
          melt(
            ., 
            id.vars = c("building.name","domain")
          ) %>%
          dcast(
            ., 
            formula = building.name + variable ~ domain
          )
        
        #tables.tab3.ls[[2]][1,1] <- "Previous School Year"
        tables.tab3.ls[[2]]$variable %<>%
          as.character %>%
          gsub("0000", "Prev. School Year", .)

      #State Average Table - Baseline vs. Current
        tables.tab3.ls[[3]] <-
          resp.long.tb %>% 
          filter(
            unit.id == unit.id.c & 
              is.current.or.baseline == 1
          ) %>%
          SplitColReshape.ToLong(
            df = ., 
            id.varname = "resp.id", 
            split.varname = "domain", 
            split.char = ","
          ) %>%
          as_tibble() %>%
          dcast(
            data = .,
            formula = domain ~ year,
            fun.aggregate = mean,
            value.var = "value"
          ) %>%
          mutate(
            Trend = .[,3] - .[,2]
          ) %>%
          TransposeTable(., keep.first.colname = FALSE)
        
        tables.tab3.ls[[3]] %<>%
          apply(
            X = tables.tab3.ls[[3]][,2:ncol(tables.tab3.ls[[3]])], 
            MARGIN = 2, 
            FUN = as.numeric
          ) %>%
          cbind(
            tables.tab3.ls[[3]][,1],
            .
          ) %>%
          ReplaceNames(., "Var.1", "")
        
        tables.tab3.ls[[3]][1,1] <- "Baseline"
        
      #Building Average Table - Baseline year vs. Current  
        tables.tab3.ls[[4]] <- 
          resp.long.tb %>% 
          filter(
            unit.id == unit.id.c & 
              is.current.or.baseline == 1
          ) %>%
          SplitColReshape.ToLong(
            df = ., 
            id.varname = "resp.id", 
            split.varname = "domain", 
            split.char = ","
          ) %>%
          as_tibble() %>%
          dcast(
            data = .,
            formula = building.name + domain ~ year,
            fun.aggregate = mean,
            value.var = "value"
          ) %>%
          mutate(
            Trend = .[,4] - .[,3]
          ) %>%
          melt(
            ., 
            id.vars = c("building.name","domain")
          ) %>%
          dcast(
            ., 
            formula = building.name + variable ~ domain
          )
        
        tables.tab3.ls[[4]]$variable %<>%
          as.character %>%
          gsub("0000", "Baseline", .)
      
    #Tab 4+ (Building Summaries) ----
        ###                                   ###
    #   ### LOOP "e" BY TABLE FOR TABs 4+  ###
        ###                                   ###
        
        #Loop Inputs
          config.tables.input.tb <- 
            config.tables.ls[[c]] %>% 
            filter(!is.na(table.type.id)) %>%
            filter(tab.type.id %in% c(4)) %>%
            filter(!is.na(loop.id))
          tables.tab4.ls <- list()
          
        #e <- 3
        for(e in 1:nrow(config.tables.input.tb)){ ### START OF LOOP "e" BY TABLE ###
          
          config.tables.tb.e <- config.tables.input.tb[e,]
          
          #Define table aggregation formula
            table.formula.e <-
              DefineTableRowColFormula(
                row.header.varnames = strsplit(config.tables.tb.e$row.header.varname, ",") %>% unlist %>% as.vector,
                col.header.varnames = strsplit(config.tables.tb.e$col.header.varname, ",") %>% unlist %>% as.vector
              )
          
          #Define table filtering vector
            loop.id.e <- config.tables.tb.e$loop.id
            table.filter.v <-
              DefineTableFilterVector(
                tb = resp.long.tb,
                filter.varnames = config.tables.tb.e$filter.varname %>% strsplit(., ";") %>% unlist %>% as.vector,
                filter.values = config.tables.tb.e$filter.values %>% strsplit(., ";") %>% unlist %>% as.vector
              )
            
            if(table.filter.v %>% not %>% all){
              tables.tab4.ls[[e]] <- ""
              next()
            }
            
          #Define Table Aggregation Function
            table.aggregation.function <-
              function(x){
                if(config.tables.tb.e$aggregate.function == "count"){
                  result <- length(x)
                }
                
                if(config.tables.tb.e$aggregate.function == "count.unique"){
                  result <- length(unique(x))
                }
                
                if(config.tables.tb.e$aggregate.function == "mean"){
                  result <- mean(x, na.rm = TRUE)
                }
                
                return(result)
              }
          
          #Form final data frame
            table.e <-  
              resp.long.tb %>%
              filter(table.filter.v) %>%
              dcast(
                ., 
                formula = table.formula.e, 
                value.var = config.tables.tb.e$value.varname, 
                fun.aggregate = table.aggregation.function
              ) %>%
              .[,names(.)!= "NA"] %>%
              mutate(
                Trend = .[,3] - .[,2]
              )
          
          #Modifications for specific tables
            if(grepl("building.level", table.formula.e) %>% any){
              table.e <- 
                left_join(
                  building.level.order.v %>% as.data.frame %>% ReplaceNames(., ".", "building.level"), 
                  table.e,
                  by = "building.level"
                )
            }
            
            if(!config.tables.tb.e$row.header){  #when don't want row labels
              table.e <- table.e %>% select(names(table.e)[-1])
            }
            
            tables.tab4.ls[[e]] <- table.e
          
          
          progress.bar.value.c <- 
            ifelse(c==1, 0, 1) %>%
            rep(., nrows.c[1:(c-1)] %>% length) %>%
            multiply_by(nrows.c[1:(c-1)]) %>%
            add(d) %>% 
            divide_by(sum(nrows.c)) %>% 
            multiply_by(100) 
          
          setTxtProgressBar(progress.bar.c, progress.bar.value.c)
          
        } ### END OF LOOP "e" BY TABLE ###
      
    tables.ls[[c]] <- c(tables.tab12.ls, tables.tab3.ls, tables.tab4.ls)
      
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
  #tables.ls
    #[[report.unit]]
    #data frame where each line represents a table


# 5-EXPORT -----------------------------------------------
  
  #EXPORT SETUP: CLOCKING, LOAD FUNCTIONS, ESTABLISH OUTPUTS DIRECTORY ----
    #Code Clocking
      section6.starttime <- Sys.time()
      
    #Load Configs Functions
      setwd(rproj.dir)
      source("6-powerpoints export functions.r")
    
  #EXPORT OF LONG DATA (if in global configs) ----
    if(export.long.data %>% as.logical){
      
      setwd(outputs.dir) 
      
      data.output.filename <- 
         paste(
          "longdata_",
          gsub(":",".",Sys.time()),
          ".csv",
          sep = ""
        )
      
      write.csv(
        resp.long.tb,
        file = data.output.filename
      )
    
    }
  
  #EXPORT TO EXCEL REPORTS - LOOP 'h' BY REPORT UNIT ----
        
    ###                          ###    
  # ### LOOP "h" BY REPORT UNIT  ###
    ###                          ###
    
    #Progress Bar
      progress.bar.h <- txtProgressBar(min = 0, max = 100, style = 3)
      maxrow.h <- tables.ls %>% lengths %>% sum
      #printed.reports.ls <- list()
    
    #h <- 1 #LOOP TESTER
    #for(h in ceiling(runif(5,1,length(unit.ids.sample)))){
    for(h in 1:length(unit.ids.sample)){ 
      
      unit.id.h <- unit.ids.sample[h]  
                    
      #Set up target file
        template.file <- 
          paste(
            source.tables.dir,
            "dashboard_template.xlsx",
            sep = ""
          )
        
        if(sample.print){
          
          file.name.h <- 
             paste(
              "district dashboard_",
              unit.id.h,
              "_",
              gsub(":",".",Sys.time()),
              ".xlsx",
              sep = ""
            )
          
        }else{
           file.name.h <- 
            paste(
              "district dashboard_",
              unit.id.h,
              ".xlsx",
              sep = ""
            )
        }
        
        target.path.h <- 
            paste(
              outputs.dir,
              "\\",
              file.name.h,
              sep=""
            ) 
        
        file.copy(template.file, target.path.h)
        
        print(file.name.h)
      
  
      ###                                   ###    
  #   ###   LOOP "i" BY TABLE FOR TABS 1-3  ###
      ###                                   ###
      
      config.tables.ls.h <- config.tables.ls[[h]]   
      setwd(outputs.dir)
      wb <- loadWorkbook(file.name.h, create = FALSE)
      setStyleAction(wb, XLC$"STYLE_ACTION.NONE")
      nrow.tabs.1.through.3 <- nrow(config.tables.ls[[h]] %>% filter(tab.type.id < 4))
      nrow.tab.4 <- nrow(config.tables.ls[[h]] %>% filter(tab.type.id == 4))
      
      #i <- 79 #LOOP TESTER
      for(i in 1:nrow.tabs.1.through.3){  
        
        #print(
        #  paste(
        #    "Loop: ",
        #    i,
        #    ". Tab name: ",
        #    config.tables.ls.h$tab.type.name[i],
        #    ". Table name: ",
        #    names(tables.ls[[h]])[i],
        #    sep = ""
        #  )
        #)
        
        setTxtProgressBar(
          progress.bar.h, 
          i %>% divide_by(nrow.tabs.1.through.3 + nrow.tab.4) %>% multiply_by(100)
        )
        
        writeWorksheet(
          object = wb, 
          data = tables.ls[[h]][[i]],
          sheet = config.tables.ls.h$tab.type.name[i],
          startRow = config.tables.ls.h$startrow[i],
          startCol = config.tables.ls.h$startcol[i],
          header = config.tables.ls.h$header[i],
          rownames = config.tables.ls.h$row.header[i]
        )
        
      } # END OF LOOP 'i' BY TABLE
        

      ###                                               ###    
  #   ###   LOOP "j" BY TABLE FOR BUILDING SUMMARY TABS ###
      ###                                               ###
      
      config.tab4.tb <- 
        config.tables.ls[[h]] %>% 
        filter(tab.type.id == 4 & !is.na(loop.id))
      
      #j = 2 #LOOP TESTER
      for(j in 1:nrow(config.tab4.tb)){
        setTxtProgressBar(
          progress.bar.h, 
          nrow.tabs.1.through.3 %>% add(j) %>% divide_by(nrow.tabs.1.through.3 + nrow.tab.4) %>% multiply_by(100)
        )
####
        #loop.id.j <- config.tab4.tb$loop.id[j]
        #allowed.buildings <- config.tab4.tb$loop.id %>% unique %>% .[1:2]
        #if(!loop.id.j %in% allowed.buildings){next()} #TODO: REMOVE ONCE FINISHED WITH BUILDING OVERVIEW SHEETS
####    
        
        building.num <- config.tab4.tb$loop.id %>% unique %>% equals(config.tab4.tb$loop.id[j]) %>% which
        
        building.base.sheetname <- 
          getSheets(wb) %>% 
          .[grepl("Building Summary", .)] %>% 
          .[building.num]
        
          #if(!existsSheet(wb, config.tab4.tb$loop.id[j])){
            
            #cloneSheet(
            #  wb,
            #  sheet = "Building Summary",
            #  name = config.tab4.tb$loop.id[j]
            #)
          #}
        
        #Write tables to building worksheet
          writeWorksheet(
            object = wb, 
            data = tables.ls[[h]][[i+j]],
            sheet = building.base.sheetname,
            startRow = config.tab4.tb$startrow[j],
            startCol = config.tab4.tb$startcol[j],
            header = config.tab4.tb$header[j],
            rownames = config.tab4.tb$row.header[j]
          )
          
        #Change sheet name to building name
          #building.final.sheetname <- config.tab4.tb$loop.id[j]
          
          #renameSheet(
          #  object = wb,
          #  sheet = building.base.sheetname,
          #  newName = building.final.sheetname
          #)
        
      } #END OF LOOP 'j' BY TABLE & TAB OF BUILDING OVERVIEW TABS (TYPE 4)
      
      #Delete any extra building summary tabs
        building.summary.tabs.with.data <- 
          getSheets(wb) %>% 
          .[grepl("Building Summary", .)] %>% 
          assign("building.summary.tabs", ., pos = 1) %>%
          .[1:length(config.tab4.tb$loop.id %>% unique)]
        
        extra.building.summary.tabnames <-
          building.summary.tabs[!building.summary.tabs %in% building.summary.tabs.with.data]
####
        #.[3] #TODO: REMOVE ONCE FINISHED WITH BUIDLING OVERVIEW SHEETS
####
        
        for(k in 1:length(extra.building.summary.tabnames)){
          removeSheet(
            object = wb, 
            sheet = extra.building.summary.tabnames[k]
          )
        }
        
        ###                           ###
    #   ###   LOOP "m" BY TEXT ITEM   ###
        ###                           ###
        
        config.text.h <- config.text.ls[[h]]
        
        #m = 2 #LOOP TESTER
        for(m in 1:nrow(config.text.h)){
          
          #Write tables to building worksheet
            writeWorksheet(
              object = wb,
              data = config.text.h$text.value[m],
              sheet = config.text.h$tab.name[m],
              startRow = config.text.h$row.num[m],
              startCol = config.text.h$col.num[m],
              header = FALSE,
              rownames = FALSE
            )
          
        } #END OF LOOP 'm' BY TEXT ITEM
        
      saveWorkbook(wb)
    } # END OF LOOP 'h' BY REPORT UNIT

#SIGNAL CODE IS FINISHED BY OPENING A NEW WINDOW      
  windows()
    