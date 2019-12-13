#0000000000000000000000000000000000000000000000000000000#
#      	CWIS Automation for MMD                   	    #
#0000000000000000000000000000000000000000000000000000000#

# 0-SETUP -----------------------------------------------------------
  
  #INITIAL SETUP
    rm(list=ls()) #Remove lists
    options(java.parameters = "- Xmx20g") #helps r not to fail when importing large xlsx files with xlsx package
    #TODO: MAKE UTILS MEMORY FUNCTIONS
      #MEASURE MEMORY USAGE OF OBJECTS
      #FUNCTION TO LIST MEMORY USAGE OF LARGEST OBJECTS IN CURRENT ENVIRONMENT
      #WARN WHEN APPROACHING DEFINED MEMORY USAGE LIMITS AND, IF INSIDE OF A LOOP, BREAK THE LOOP
    
    
    #Section & Code Clocking
      sections.all.starttime <- Sys.time()
  
  # ESTABLISH BASE DIRECTORIES
  
      # Figure out what machine code is running on
      if(dir.exists("C:\\Users\\willi")){m900 <- TRUE}else{m900 <- FALSE}
      
      # Set Working Directory and R Project Directory
      if(m900){  
        #M900
          wd <- "C:\\Users\\willi\\Google Drive\\1. FLUX CONTRACTS - CURRENT\\2016-09 EXT Missouri\\3. MO GDRIVE\\8. CWIS\\2019-11_Phase 10\\"
          rproj.dir <- "C:\\Users\\willi\\Documents\\GIT PROJECTS\\CWIS-automation\\"
      }else{
        #Thinkpad T470
          wd <- "G:\\Meu Drive\\1. FLUX CONTRACTS - CURRENT\\2016-09 EXT Missouri\\3. MO GDRIVE\\8. CWIS\\2019-11_Phase 10\\"
          rproj.dir <- "C:\\Users\\WNF\\Documents\\GIT PROJECTS\\CWIS-automation\\"
      }
    
    #Check Directories
      wd <- if(!dir.exists(wd)){choose.dir()}else{wd}
      rproj.dir <- if(!dir.exists(rproj.dir)){choose.dir()}else{rproj.dir}
    
    #Source Tables Directory (raw data, configs, etc.)
      source.tables.dir <- paste(wd,"3_source_tables\\", sep = "")
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
    
    #library(rJava)
    LoadCommonPackages()
    library(flextable)
    library(XLConnect)
    library(tictoc)
    library(microbenchmark)
    
    #Section Clocking
      #section0.duration <- Sys.time() - section0.starttime
      #section0.duration

# 1-IMPORT -----------------------------------------
  
  #Section Clocking
    #section1.starttime <- Sys.time()
    tic("1-Import") 
    
  #Source Import Functions
    setwd(rproj.dir)
    source("1-import_functions.r")
  
  #Import Config Tables
    configs.ss <- gs_key("1_QqIHuAlUfYXX-qC9yMRRpOkgpudJT9-bETY1k0D19k",verbose = TRUE) 
    
    #Import all tables from config google sheet as tibbles
      all.configs.ls <- GoogleSheetLoadAllWorksheets(configs.ss)
    
    #Assign each table to its own tibble object
      ListToTibbleObjects(all.configs.ls) #Converts list elements to separate tibble objects names with
                                          #their respective sheet names with ".tb" appended
    
  #Extract global configs from tibble as their own character objects
    TibbleToCharObjects(config.global.tb)
    sample.print <- #Convert sample.print to TRUE/FALSE
      ifelse(sample.print == "true", TRUE, FALSE)
    
    #Specific formatting & definitions for global configs
      add.to.last.full.print <-
        ifelse(add.to.last.full.print == "true", TRUE, FALSE)
    
      domains <- strsplit(domains, ",") %>% unlist %>% as.vector
      
      is.overview <- ifelse(dashboard.or.overview == "overview", TRUE, FALSE)
      
      previous.school.year <-
        current.school.year %>%
        strsplit(., "-") %>%
        unlist() %>%
        as.numeric() %>%
        subtract(1) %>%
        as.character %>%
        paste0(., collapse = "-")
    
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
    
    setwd(outputs.parent.folder)
    
    most.recent.output.folder <-
      list.dirs() %>%
      .[order(.)] %>%
      .[length(.)]
    
    if(sample.print){  #scenario 1 - sample print
      outputs.dir <- 
        paste(
          outputs.parent.folder,
          gsub(":",".",Sys.time()), 
          sep = ""
        )
    }
    
    if(!sample.print & !add.to.last.full.print){  #scenario 2 - full print not adding to last full print
      setwd(outputs.parent.folder)
      
      districts.that.already.have.reports <- vector() %>% as.character()
      
      outputs.dir <- 
        paste(
          outputs.parent.folder,
          gsub(":",".",Sys.time()), 
          "_FULL",
          sep = ""
        )
    }
    
    if(!sample.print & add.to.last.full.print){ #scenario 3 - ful#is from the same date and has some reports already in it
      
      most.recent.full.output.folder <-
        list.dirs() %>%
        .[grepl("FULL", list.dirs())] %>%
        .[order(.)] %>%
        .[length(.)]
      
      setwd(most.recent.full.output.folder)
      
      districts.that.already.have.reports <-
        list.files()[!grepl("desktop.ini", list.files())] %>%
        str_split(., "_") %>%
        lapply(., `[[`, 2) %>%
        unlist %>%
        gsub(".xlsx","",.)
      
      outputs.dir <- 
        paste(
          outputs.parent.folder,
          most.recent.output.folder,
          sep = ""
        )
      
    }
    
  #Section Clocking
    #section1.duration <- Sys.time() - section1.starttime
    #section1.duration
    #Sys.time() - sections.all.starttime
    toc()

# 2-CLEANING -----------------------------------------
  
  #Section Clocking
    tic("2-CLEANING")
    
  #Source Cleaning Functions
    cleaning.source.dir <- paste(rproj.dir,"2-Cleaning/", sep = "")
    setwd(rproj.dir)
    source("2-cleaning_functions.r")
    
  #Names/Column Headers
    resp2.tb <- 
      LowerCaseNames(resp1.tb) %>%  #Lower-case all variable names
      ReplaceNames(., "school", "building") %>% #Replace "school" with "building" in column names
      #ReplaceNames(., "ï..year", "year") %>% #not necessary in latest 2019 data
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
  
  #Add variables: building.id, building.name, building.level 
    resp2.tb %<>% mutate(building.id.raw = paste(unit.id,building,sep=".") %>% gsub(" |\\/", ".", .))
    
    resp2.tb <- 
      left_join(
        resp2.tb,
        buildings.tb %>% select(district, building.id.raw, building.id, building.name, building.level),
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
          filter.previous.school.year = ifelse(year == previous.school.year & x < 6, FALSE, TRUE),
          filter.current.school.year = ifelse(year == current.school.year & x < 6, FALSE, TRUE),
          filter.combined = all(filter.baseline, filter.previous.school.year, filter.current.school.year)
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
    
  #Filter out responses for DBDM practice 5 (see Missouri Journal entry 2019-11-08)
    resp6.tb %<>%
      filter(variable != "dbdm_visualrepresetations")
    
  #ADDING NEW USEFUL VARIABLES ----
    
    #Add proficiency dummy variable
      resp7.tb <- 
        resp6.tb %>%
        mutate(
          is.proficient = ifelse(value >= 4, 1, 0)
        )
      
    #Add variable for baseline, most recent, and next-to-most-recent years for each unit.id
      resp7.tb$year[resp7.tb$year == "baseline"] <- "0000"
      
#FUNCTION - could be made into function to designate alphabetic first/last/penultimate by group
      
      year.var.helper.tb <-
        resp7.tb %>%
        select(year, unit.id, building) %>%
        unique %>%
        mutate(
          is.baseline = ifelse(year == "0000", 1, 0),
          is.most.recent = ifelse(year == previous.school.year, 1, 0),
          is.current = ifelse(year == current.school.year, 1, 0)
        ) %>%
        mutate(
          is.baseline.or.most.recent = ifelse(is.baseline == 1 | is.most.recent == 1, 1, 0),
          is.baseline.or.current = ifelse(is.baseline == 1 | is.current == 1, 1, 0),
          is.most.recent.or.current = ifelse(is.most.recent == 1 | is.current == 1, 1, 0)
        )
      
      resp8.tb <- 
        left_join(
          resp7.tb,
          year.var.helper.tb,# %>% select(building),
          by = c("year", "unit.id", "building")
        ) %>% 
        mutate(variable = as.character(variable))
   
  #FORM FINAL DATASETS - (A) SPLITCOLRESHAPED BY DOMAIN AND (B) RESTRICTED TO SAMPLE ----
    
    #Restrict whole dataset to districts with following condition: 
      #50% or more of buildings in the district that are not `other` or `district office` 
      #have at least one response in the current period
      
      dashboard.restriction.tb <-
        resp8.tb %>% 
        select(resp.id, year, unit.id, building, building.id) %>%
        #filter(!building %in% c("district office", "other")) %>% #get rid of rows for 'other' and 'district office'
        filter(year == current.school.year) %>% #filter for current school year 
        dcast( #reshape into table of response counts by building.id
          ., 
          formula = unit.id + building.id ~ ., 
          value.var = "resp.id", 
          fun.aggregate = function(x){length(unique(x))}
        ) %>%
        as_tibble() %>%
        select(-unit.id) %>%
        ReplaceNames(
          ., 
          current.names = names(.)[length(names(.))],
          new.names = "resp.count.current.school.year"
        ) %>%
        left_join( #join with buildings table so have all buildings and response counts for current school year
          buildings.tb %>% select(district, building.id) %>% filter(!duplicated(building.id)), 
          ., 
          by = "building.id"
        ) %>%
        filter(!grepl("district.office|other", building.id)) %>% #get rid of rows for 'other' and 'district office' buildings
        dcast( #reshape into table of percentage of buildings with responses in current school year by district
          .,
          formula = district ~ .,
          value.var = 'resp.count.current.school.year',
          fun.aggregate = function(x){mean(!is.na(x))}
        ) %>%
        mutate(is.dashboard = . >= 0.5) %>% #add logical variable for those districts with >= 50% of buildings with responses in current school year
        ReplaceNames(
          ., 
          current.names = names(.)[2],
          new.names = "pct.bldgs.w.responses.in.current.schyr"
        ) %>%
        as_tibble()
      
      
      overview.restriction.base.tb <-
        resp8.tb %>% 
        select(district, building.id, is.baseline, is.most.recent, is.current, is.baseline.or.current) %>%
        unique
        #assign("overview.restriction.base.tb", ., pos = 1) %>%
        
      building.has.baseline <- 
        overview.restriction.base.tb %>%
        dcast(district + building.id ~ is.baseline, value.var = "building.id", fun.aggregate = function(x){length(unique(x))}) %>%
        mutate(building.has.baseline = .[,ncol(.)] > 0) %>%
        select(building.id, building.has.baseline)
      
      building.has.current <- 
        overview.restriction.base.tb %>%
        dcast(district + building.id ~ is.current, value.var = "building.id", fun.aggregate = function(x){length(unique(x))}) %>%
        mutate(building.has.current = .[,ncol(.)] > 0)  %>%
        select(building.id, building.has.current)
      
      building.has.midline <- 
        overview.restriction.base.tb %>%
        dcast(
          district + building.id ~ is.most.recent + is.baseline.or.current, 
          value.var = "building.id", fun.aggregate = function(x){length(unique(x))}
        ) %>%
        mutate(building.has.midline = .[,ncol(.)] > 0) %>%
        select(building.id, building.has.midline)
    
    building.meets.condition.1 <- 
      left_join(
        buildings.tb,
        building.has.baseline,
        by = "building.id"
      ) %>%
      left_join(
        ., 
        building.has.current,
        by = "building.id"
      ) %>%
      left_join(
        ., 
        building.has.midline,
        by = "building.id"
      ) %>%
      mutate(building.meets.condition.1 = building.has.current & building.has.baseline & !building.has.midline) %>%
      #select(building.has.baseline, building.has.current, building.has.midline, building.meets.condition.1) %>% unique
      select(district, building.id, building.has.baseline, building.has.current, building.has.midline, building.meets.condition.1)
        
        
      
      
      #filter(is.baseline == 1) %>%
      #  dcast(district ~)
        
        
      #  filter(is.most.recent == 0 & is.baseline.or.current == 1) %>%
      #  dcast(., district + building.id ~ is.baseline + is.current) %>%
      #  mutate(condition.1 = !is.na(.[,ncol(.)] + .[,ncol(.)-1])) %>%
      #  dcast(district ~ condition.1) %>%
      #  mutate(condition.1 = .[ncol(.)] > 0)
        
        
      #overview.restriction.tb <- 
        
      #  mutate(
      #    condition.1 = if(is.baseline == 1)
      #  )
      #  filter(
      #    is.most.recent == 1 & is.baseline.or.current == 0
      #    is.baseline
      #  ) %>%
      ##  select(building.id) %>%
      #  mutate(is.overview = FALSE) %>%
      #  left_join(
      #    buildings.tb,
      #    .,
      #    by = "building.id"
      #  ) %>%
      #  dcast(district ~ is.overview, value.var = "building.id", fun.aggregate = function(x){length(unique(x))}) %>%
      #  mutate(is.overview = ifelse(.[,2] == 0 & .[,3] > 0, TRUE, FALSE)) %>%
      #  select(district, is.overview)
      
        #filter(.[,2])
        #unique
        #select(building.id, year) %>%
        #dcast(., building.id ~ year, value.var = "year", fun.aggregate = length) %>%
        #mutate(
        #  is.overview = 
        #    .[,ncol(.)] > 0 & 
        #    .[,ncol(.)-1] == 0 &
        #    .[,ncol(.)-2] > 0
        #) %>%
        #dcast(., district ~ is.overview, value.var = "is.overview", fun.aggregate = length) %>%
        #mutate(is.overview = .[,ncol(.)] == 1)
      
      resp9.tb <-   
        left_join(
          resp8.tb,
          dashboard.restriction.tb,
          by = "district"
        ) %>%
        left_join(
          ., 
          overview.restriction.tb,
          by = "district"
        )
      
      buildings.tb %<>%
        left_join(
          .,
          dashboard.restriction.tb,
          by = "district"
        )
     
    #Full dataset - no splitcolreshape by domain
      resp.full.nosplit.tb <- 
        resp9.tb
      
    #Full dataset - splitcolreshape by domain (for some practices where answers count towards ETL and CFA)
      resp.full.split.tb <-
        left_join(
          x = resp9.tb,
          y = questions.tb %>% select(var.id, domain, practice),
          by = c("variable"="var.id")
        ) %>%
        SplitColReshape.ToLong(
          df = ., 
          id.varname = "resp.id" ,
          split.varname = "domain",
          split.char = ","
        ) %>% 
        as_tibble() %>%
        left_join(
          x = ., 
          y = domains.tb,
          by = c("domain" = "domain.id")
        )
      
    #Restricted datasets (depending on whether printing dashboards or overviews)
      if(is.overview){
        resp.restricted.nosplit.tb <- 
          resp.full.nosplit.tb %>%
          filter(is.overview == TRUE)
        
        resp.restricted.split.tb <-
          resp.full.split.tb %>%
          filter(is.overview == TRUE)
      }else{
        resp.restricted.nosplit.tb <- 
          resp.full.nosplit.tb %>%
          filter(is.dashboard == TRUE)
        
        resp.restricted.split.tb <-
          resp.full.split.tb %>%
          filter(is.dashboard == TRUE)
      }
    
    #Sample Datasets  
    
      #Scenario 1 - sample print
        if(sample.print){ 
          is.valid.sample <- FALSE
          while(!is.valid.sample){
            
            resp.sample.nosplit.tb <- 
              RestrictDataToSample(
                tb = resp.restricted.nosplit.tb,
                report.unit = "unit.id",
                sample.print = sample.print,
                sample.group.unit = "unit.id",
                sample.size = sample.size
              )
            
            resp.sample.split.tb <- 
              left_join(
                x = resp.sample.nosplit.tb,
                y = questions.tb %>% select(var.id, domain, practice),
                by = c("variable"="var.id")
              ) %>%
              SplitColReshape.ToLong(
                df = ., 
                id.varname = "resp.id" ,
                split.varname = "domain",
                split.char = ","
              ) %>% 
              as_tibble() %>%
              left_join(
                x = ., 
                y = domains.tb,
                by = c("domain" = "domain.id")
              )
            
            is.valid.sample <- 
              ifelse(
                dcast(
                  data = resp.sample.nosplit.tb, 
                  formula = unit.id ~ .,
                  fun.aggregate = function(x){length(unique(x))},
                  value.var = "building.id"
                ) %>% 
                select(".") %>%
                unlist %>% as.vector %>%
                is_weakly_less_than(as.numeric(max.building.count)) %>% 
                all,
                TRUE,
                FALSE
              )
          }
          
          unit.ids.sample <-
            resp.sample.nosplit.tb %>%
            select(unit.id) %>%
            unique %>%
            unlist %>% as.vector
          
        }
        
      #Scenario 2 - full print (fresh, not adding to previous full print)  
        if(!sample.print & !add.to.last.full.print){
            
          resp.sample.nosplit.tb <- 
            resp.restricted.nosplit.tb
          
          resp.sample.split.tb <- 
            resp.restricted.split.tb
          
          unit.ids.sample <-
            resp.sample.nosplit.tb %>%
            select(unit.id) %>%
            unique %>%
            unlist %>% as.vector
          
        }
        
      #Scenario 3 - full print (adding to previous full print)  
        if(!sample.print & add.to.last.full.print){
          is.valid.sample <- FALSE
          while(!is.valid.sample){
            
            unit.ids.sample <-
              resp.restricted.nosplit.tb %>%
              select(unit.id) %>%
              unique %>%
              unlist %>% as.vector %>%
              setdiff(., districts.that.already.have.reports) %>%
              .[1:sample.size] %>%
              RemoveNA
            
            is.valid.sample <-
              unit.ids.sample %in% districts.that.already.have.reports %>% not %>% all
            
            print(is.valid.sample)
            if(!is.valid.sample){next()}
            
            resp.sample.nosplit.tb <- 
              resp.restricted.nosplit.tb %>%
              filter(unit.id %in% unit.ids.sample)
            
            resp.sample.split.tb <- 
              left_join(
                x = resp.sample.nosplit.tb,
                y = questions.tb %>% select(var.id, domain, practice),
                by = c("variable"="var.id")
              ) %>%
              SplitColReshape.ToLong(
                df = ., 
                id.varname = "resp.id" ,
                split.varname = "domain",
                split.char = ","
              ) %>% 
              as_tibble() %>%
              left_join(
                x = ., 
                y = domains.tb,
                by = c("domain" = "domain.id")
              )
          }
        }

    #resp.long.tb <- resp10.tb
    
  #Section Clocking
    toc()
    Sys.time() - sections.all.starttime

# 3-CONFIGS (TAB, TABLE, TEXT CONFIG TABLES) ------------------
  
  #Section Clocking
    tic("3-Configs")
          
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
    
    #Create unit.id.b (for this iteration)
      unit.id.b <- unit.ids.sample[b]
    
    #Print loop messages
      if(b == 1){print("FORMING TAB, GRAPH, AND TABLE CONFIG TABLES...")}
    
    #Create data frames for this loop - restrict to unit.id id i  
      resp.long.tb.b <- 
        resp.sample.nosplit.tb%>% filter(unit.id == unit.id.b)
      
    #Other useful inputs for forming config tables
      
      #District Name
        district.name.v <- 
          resp.long.tb.b$unit.id %>% unique %>% unlist %>% as.vector %>%
          str_split(., "-") %>% lapply(., Proper) %>% unlist %>% as.vector
        
        if(length(district.name.v) > 1){
          district.name.v[length(district.name.v)] <- district.name.v[length(district.name.v)] %>% toupper()
        }
        
        district.name <- district.name.v %>% paste(., collapse = "-")
      
      #District Overview Indicator (if have only data from one year)
        #district.overview <- resp.long.tb.b$year %>% unique %>% length %>% is_less_than(2) 
      
      #Tab 4 Loop Variable & Building Names
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
        ) %>%
        mutate(
          is.state.table = !grepl("unit.id", filter.varname),
          is.domain.table = grepl("domain", filter.varname)|grepl("domain", row.header.varname)|grepl("domain",col.header.varname)
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
        mutate(
          text.value =  
            ifelse(text.type == "report.version", report.version, text.value)
        ) %>%
        mutate(text.value = Proper(text.value))
        
    setTxtProgressBar(progress.bar.b, 100*b/maxrow.b)
    
  } # END OF LOOP 'b' BY REPORT.UNIT
  
  close(progress.bar.b)
    
  #Section Clocking
    toc()
    Sys.time() - sections.all.starttime

# 4-SOURCE DATA TABLES --------------------------------------------
  
  #Section Clocking
    tic("Section 4 duration")
        
  #Load Configs Functions
    setwd(rproj.dir)
    source("4-source data tables functions.r")
    
  #STATE AVERAGE TABLES ----
    
    #Tabs 1 & 2 State Averages ----

      #Loop Inputs
        config.tables.tab12.input.tb <- 
          config.tables.ls[[1]] %>% 
          filter(!is.na(table.type.id)) %>%
          filter(grepl("1|2", tab.type.id)) %>%
          filter(is.state.table) %>%
          OrderDfByVar(., order.by.varname = "tab.type.id", rev = FALSE) %>%
          as_tibble()
        
        tables.tab12.state.ls <- list()
      
      #d <- 4
      for(d in 1:nrow(config.tables.tab12.input.tb)){ ### START OF LOOP "d" BY TABLE ###
        
        #Print loop messages
        print(paste("TABS 1 & 2 LOOP - Loop #: ", d, " - Pct. Complete: ", 100*d/nrow(config.tables.tab12.input.tb), sep = ""))
        
        #Define table configs for loop
        config.tables.tb.d <- config.tables.tab12.input.tb[d,]
        
        #CREATE TABLE
        
          #Tab 1 Tables (newly created)
            if(config.tables.tb.d$tab.type.id == 1){
              
              #Define table aggregation formula
                table.formula.d <-
                  DefineTableRowColFormula(
                    row.header.varnames = strsplit(config.tables.tb.d$row.header.varname, ",") %>% unlist %>% as.vector,
                    col.header.varnames = strsplit(config.tables.tb.d$col.header.varname, ",") %>% unlist %>% as.vector
                  )
    
              #Define table source data
                filter.varnames.d <- config.tables.tb.d$filter.varname %>% strsplit(., ";") %>% unlist %>% as.vector
                filter.values.d <- config.tables.tb.d$filter.values %>% strsplit(., ";") %>% unlist %>% as.vector
              
                is.state.table <- ifelse(!"unit.id" %in% filter.varnames.d, TRUE, FALSE)
                is.domain.table <- grepl("domain", c(filter.varnames.d, table.formula.d)) %>% any
              
              #STATE data with NO domains in formula
                if(is.state.table & !is.domain.table){table.source.data <- resp.full.nosplit.tb}
              
              #STATE data WITH domains in formula
                if(is.state.table & is.domain.table){table.source.data <- resp.full.split.tb}
              
              #NON-STATE data NO domains in formula
                if(!is.state.table & !is.domain.table){table.source.data <- resp.sample.nosplit.tb}
              
              #NON-STATE data WITH domains in formula
                if(!is.state.table & is.domain.table){table.source.data <- resp.sample.split.tb}
              
              #Define table filtering vector
                table.filter.v <-
                  DefineTableFilterVector(
                    tb = table.source.data,
                    filter.varnames = filter.varnames.d,
                    filter.values = filter.values.d
                  )
    
              #Form final data frame
                table.d <-  
                  table.source.data %>%
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
            }
          
          #Tab 2 Tables (copies of tab 1)
            if(config.tables.tb.d$tab.type.id == 2){
              table.d <- 
                tables.tab12.state.ls[
                  tables.tab12.state.ls %>%
                    lapply(
                      .,
                      function(x){
                        (
                          x$configs$tab.type.id %>%
                            unlist %>% as.vector %>%
                            equals(1)
                        ) &
                          (
                            x$configs$table.type.id %>%
                              unlist %>% as.vector() %>%
                              equals(config.tables.tb.d$table.type.id)
                          )
                      }
                    ) %>%
                    unlist %>% as.vector
                  ] %>%
                .[[1]] %>% 
                .[["table"]]
            }
          
        #Table Storage
          table.d.storage.index <- length(tables.tab12.state.ls) %>% add(1)
          tables.tab12.state.ls[[table.d.storage.index]] <- list()
          tables.tab12.state.ls[[table.d.storage.index]]$configs <- config.tables.tb.d
          tables.tab12.state.ls[[table.d.storage.index]]$table <- table.d
        
      } ### END OF LOOP "d" BY TABLE ###
      
      #Finalizing loop outputs  
        names(tables.tab12.state.ls) <- 
          paste(
            #rep(unit.id.c, length(tables.tab12.state.ls)),
            #".",
            c(1:length(tables.tab12.state.ls)),
            ".",
            config.tables.tb.d$table.type.name, 
            sep = ""
          ) %>%
          gsub("-", " ", .) %>%
          gsub(" ", ".", .) %>%
          SubRepeatedCharWithSingleChar(., ".") %>%
          tolower
    
    
    #Tab 3 State Averages ----
      #Current vs. previous school year
        tab3.state.avg.current.vs.previous.school.year <-
          resp.full.split.tb %>% 
          filter(
            is.most.recent.or.current == 1
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
            formula = domain ~ is.current,
            fun.aggregate = mean,
            value.var = "value"
          ) %>%
          ReplaceNames(., current.names = c("0","1"), new.names = c("Previous School Year","Current Year")) %>%
          mutate(
            Trend = .[,3] - .[,2]
          ) %>%
          TransposeTable(., keep.first.colname = FALSE)
        
        tab3.state.avg.current.vs.previous.school.year %<>%
          apply(
            X = tab3.state.avg.current.vs.previous.school.year[,2:ncol(tab3.state.avg.current.vs.previous.school.year)], 
            MARGIN = 2, 
            FUN = as.numeric
          ) %>%
          cbind(
            tab3.state.avg.current.vs.previous.school.year[,1],
            .
          ) %>%
          ReplaceNames(., "Var.1", "")
        
        tab3.state.avg.current.vs.previous.school.year[1,1] <- "Prev. School Year"
        
      #Current vs. baseline
        tab3.state.avg.current.vs.baseline <- 
          resp.full.split.tb %>% 
          filter(
            is.baseline.or.current == 1
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
            formula = domain ~ is.current,
            fun.aggregate = mean,
            value.var = "value"
          ) %>%
          ReplaceNames(., current.names = c("0","1"), new.names = c("Baseline","Current Year")) %>%
          mutate(
            Trend = .[,3] - .[,2]
          ) %>%
          TransposeTable(., keep.first.colname = FALSE)
        
        tab3.state.avg.current.vs.baseline %<>%
          apply(
            X = tab3.state.avg.current.vs.baseline[,2:ncol(tab3.state.avg.current.vs.baseline)], 
            MARGIN = 2, 
            FUN = as.numeric
          ) %>%
          cbind(
            tab3.state.avg.current.vs.baseline[,1],
            .
          ) %>%
          ReplaceNames(., "Var.1", "") 
        
        tab3.state.avg.current.vs.baseline[1,1] <- "Baseline"
  
  #DISTRICT/BUILDING TABLES ----  
        
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
    for(c in 1:length(unit.ids.sample)){   #START OF LOOP BY unit.id
      #Loop Prep----
        #Loop timing
          tic("REPORT LOOP DURATION",c)
        
        #Loop Inputs (both graphs and tables)
          unit.id.c <- unit.ids.sample[c]
        
        #Print loop messages
          if(c == 1){print("Forming tables for export...")}
          print("REPORT LOOP")
          print(paste("Loop #: ", c, " - Pct. Complete: ", 100*c/length(unit.ids.sample),sep = ""))
          print(paste("Unit id: ", unit.ids.sample[c]))
          
          #district.overview <- 
      
      #Tabs 1 & 2 ----
       
        #Loop Inputs
          config.tables.tab12.input.tb <- 
            config.tables.ls[[c]] %>% 
            filter(!is.na(table.type.id)) %>%
            filter(grepl("1|2", tab.type.id)) %>%
            filter(!is.state.table) %>%
            OrderDfByVar(., order.by.varname = "tab.type.id", rev = FALSE) %>%
            as_tibble()
          
          tables.tab12.ls <- list()
        
        #d <- 4
        for(d in 1:nrow(config.tables.tab12.input.tb)){ ### START OF LOOP "d" BY TABLE ###
          
          #Loop timing
            #tic("Tabs 1 & 2 loop iteration:", d)
          
          #Print loop messages
            print(paste("TABS 1 & 2 LOOP - Loop #: ", d, " - Pct. Complete: ", 100*d/nrow(config.tables.tab12.input.tb), sep = ""))
          
          #Define table configs for loop
            config.tables.tb.d <- config.tables.tab12.input.tb[d,]
          
          #CREATE TABLE
            if(config.tables.tb.d$tab.type.id == 1){
              
              #Define table aggregation formula
                table.formula.d <-
                  DefineTableRowColFormula(
                    row.header.varnames = strsplit(config.tables.tb.d$row.header.varname, ",") %>% unlist %>% as.vector,
                    col.header.varnames = strsplit(config.tables.tb.d$col.header.varname, ",") %>% unlist %>% as.vector
                  )

              #Define table source data
                filter.varnames.d <- config.tables.tb.d$filter.varname %>% strsplit(., ";") %>% unlist %>% as.vector
                filter.values.d <- config.tables.tb.d$filter.values %>% strsplit(., ";") %>% unlist %>% as.vector
                
                is.state.table <- ifelse(!"unit.id" %in% filter.varnames.d, TRUE, FALSE)
                is.domain.table <- grepl("domain", c(filter.varnames.d, table.formula.d)) %>% any
                
                #STATE data with NO domains in formula
                  if(is.state.table & !is.domain.table){table.source.data <- resp.full.nosplit.tb}
                
                #STATE data WITH domains in formula
                  if(is.state.table & is.domain.table){table.source.data <- resp.full.split.tb}
                
                #NON-STATE data NO domains in formula
                  if(!is.state.table & !is.domain.table){table.source.data <- resp.sample.nosplit.tb}
                
                #NON-STATE data WITH domains in formula
                  if(!is.state.table & is.domain.table){table.source.data <- resp.sample.split.tb}
                
              #Define table filtering vector
                table.filter.v <-
                  DefineTableFilterVector(
                    tb = table.source.data,
                    filter.varnames = filter.varnames.d,
                    filter.values = filter.values.d
                  )

              #Form final data frame
                if(table.filter.v %>% not %>% all){
                  table.d <- ""
                }else{
                  table.d <-  
                  table.source.data %>%
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
                }
            }
          
            if(config.tables.tb.d$tab.type.id == 2){
              table.d <- 
                tables.tab12.ls[
                  tables.tab12.ls %>%
                  lapply(
                    .,
                    function(x){
                      (
                        x$configs$tab.type.id %>%
                          unlist %>% as.vector %>%
                          equals(1)
                      ) &
                        (
                          x$configs$table.type.id %>%
                            unlist %>% as.vector() %>%
                            equals(config.tables.tb.d$table.type.id)
                        )
                    }
                  ) %>%
                  unlist %>% as.vector
                ] %>%
                .[[1]] %>% 
                .[["table"]]
            }
  
          #Table Storage
            table.d.storage.index <- length(tables.tab12.ls) %>% add(1)
            tables.tab12.ls[[table.d.storage.index]] <- list()
            tables.tab12.ls[[table.d.storage.index]]$configs <- config.tables.tb.d
            tables.tab12.ls[[table.d.storage.index]]$table <- table.d
          
          #tic.log(format = TRUE)
          #toc(log = TRUE, quiet = TRUE)
          
        } ### END OF LOOP "d" BY TABLE ###
        
        #Finalizing loop outputs  
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
        
        if(!is.overview){
          
          #Print status
            print("Tab 3 calculations begun...")
            
          #Loop Inputs
            config.tab3.tb <-   
              config.tables.ls[[c]] %>% 
              filter(!is.na(table.type.id)) %>%
              filter(grepl("3", tab.type.id)) %>%
              OrderDfByVar(., order.by.varname = "table.type.id", rev = FALSE)
            
            tables.tab3.ls <- list()
          
          #State Average Table - Last School Year vs. Current
            tables.tab3.ls[[1]] <- list()
            tables.tab3.ls[[1]]$configs <- config.tab3.tb[1,]
            tables.tab3.ls[[1]]$table <- tab3.state.avg.current.vs.previous.school.year  
          
          #Building Average Table - Last School year vs. Current  
            tables.tab3.ls[[2]] <- list()
            tables.tab3.ls[[2]]$configs <- config.tab3.tb[2,]
            tab3.bldg.current.vs.previous.school.year <- 
              resp.sample.split.tb %>% 
              filter(
                unit.id == unit.id.c & 
                  is.most.recent.or.current == 1
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
              )
  
            if(nrow(tab3.bldg.current.vs.previous.school.year) < 1){
              tab3.bldg.current.vs.previous.school.year <- "" 
            }else{
              
              if(ncol(tab3.bldg.current.vs.previous.school.year) < 4){ # if missing previous school year data, add 4th column with NAs
                tab3.bldg.current.vs.previous.school.year %<>%
                  mutate(
                    `Prev. School Year` = NA,
                    Trend = NA
                  )
              }
              
              if(ncol(tab3.bldg.current.vs.previous.school.year) == 4){ # add trend column
                tab3.bldg.current.vs.previous.school.year %<>%
                  mutate(
                    Trend = .[,4] - .[,3]
                  )
              }
              
              tab3.bldg.current.vs.previous.school.year %<>% # transpose table
                melt(
                  ., 
                  id.vars = c("building.name","domain")
                ) %>%
                dcast(
                  ., 
                  formula = building.name + variable ~ domain
                )
              
              tab3.bldg.current.vs.previous.school.year$variable <-
                c("Prev. School Year", current.school.year, "Trend")
            }
            
            tables.tab3.ls[[2]]$table <- tab3.bldg.current.vs.previous.school.year
    
          #State Average Table - Baseline vs. Current
            tables.tab3.ls[[3]] <- list()
            tables.tab3.ls[[3]]$configs <- config.tab3.tb[3,]
            tables.tab3.ls[[3]]$table <- tab3.state.avg.current.vs.baseline
              
            
          #Building Average Table - Baseline year vs. Current  
            tables.tab3.ls[[4]] <- list()
            tables.tab3.ls[[4]]$configs <- config.tab3.tb[4,]
            tab3.bldg.current.vs.baseline <- 
              resp.sample.split.tb %>% 
              filter(
                unit.id == unit.id.c & 
                  is.baseline.or.current == 1
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
              )
            
            if(ncol(tab3.bldg.current.vs.baseline) < 4){
              tab3.bldg.current.vs.baseline %<>%
                mutate(
                  `Prev. School Year` = NA,
                  Trend = NA
                )
            }
            
            if(ncol(tab3.bldg.current.vs.baseline) == 4){
              tab3.bldg.current.vs.baseline %<>%
                mutate(
                  Trend = .[,4] - .[,3]
                )
            }
            
            tab3.bldg.current.vs.baseline %<>%
              melt(
                ., 
                id.vars = c("building.name","domain")
              ) %>%
              dcast(
                ., 
                formula = building.name + variable ~ domain
              )
            
            tab3.bldg.current.vs.baseline$variable %<>%
              as.character %>%
              gsub("0000", "Baseline", .)
            
            tables.tab3.ls[[4]]$table <- tab3.bldg.current.vs.baseline
        } #end of if statement for producing tab 4 tables only if not a district overview report
      
      #Tab 4+ (Building Summaries) ----
        
        if(!is.overview){  
          #Loop timing
            #tic("Tab 4 duration:")  
            
          #Loop Inputs
            config.tables.tab4.input.tb <- 
              config.tables.ls[[c]] %>% 
              filter(!is.na(table.type.id)) %>%
              filter(tab.type.id %in% c(4)) %>%
              filter(!is.na(loop.id))
            tables.tab4.ls <- list()
            
          #e <- 1
          for(e in 1:nrow(config.tables.tab4.input.tb)){ ### START OF LOOP "e" BY TABLE ###
              
              #Print loop messages
                print(paste("TAB 4 LOOP - Loop #: ", e, " - Pct. Complete: ", 100*e/nrow(config.tables.tab4.input.tb), sep = ""))
              
<<<<<<< HEAD
              if(table.filter.v %>% any){
                table.e <-  
                  table.source.data %>%
                  filter(table.filter.v) %>%
                  dcast(
                    ., 
                    formula = table.formula.e, 
                    value.var = config.tables.tb.e$value.varname, 
                    fun.aggregate = table.aggregation.function
                  ) %>%
                  .[,names(.)!= ""]
                
                trend.all.na <- 
                  ncol(table.e) %>% is_less_than(3) %>%
                  ifelse(
                    .,
                    TRUE,
                    c(
                      class(table.e[,ncol(table.e)-1]) == "factor",
                      IsError(table.e[,ncol(table.e)] - table.e[,ncol(table.e)-1]) %>% all,
                      all(is.na(table.e[,ncol(table.e)] - table.e[,ncol(table.e)-1]))
                    ) %>% any
                  )
                  
                  
                if(trend.all.na){ # add trend
                  table.e$trend <- rep(NA, nrow(table.e))
                }else{
                  table.e$trend <- table.e[,ncol(table.e)] - table.e[,ncol(table.e)-1]
                }
=======
              config.tables.tb.e <- config.tables.tab4.input.tb[e,]
              
              #Define table aggregation formula
                table.formula.e <-
                  DefineTableRowColFormula(
                    row.header.varnames = strsplit(config.tables.tb.e$row.header.varname, ",") %>% unlist %>% as.vector,
                    col.header.varnames = strsplit(config.tables.tb.e$col.header.varname, ",") %>% unlist %>% as.vector
                  )
                
              #Define table source data
                filter.varnames.e <- config.tables.tb.e$filter.varname %>% strsplit(., ";") %>% unlist %>% as.vector
                filter.values.e <- config.tables.tb.e$filter.values %>% strsplit(., ";") %>% unlist %>% as.vector
                
                is.state.table <- ifelse(!"unit.id" %in% filter.varnames.e, TRUE, FALSE)
                is.domain.table <- grepl("domain", c(filter.varnames.e, table.formula.e)) %>% any
                
                #STATE data with NO domains in formula
                  if(is.state.table & !is.domain.table){table.source.data <- resp.full.nosplit.tb}
                
                #STATE data WITH domains in formula
                  if(is.state.table & is.domain.table){table.source.data <- resp.full.split.tb}
                
                #NON-STATE data NO domains in formula
                  if(!is.state.table & !is.domain.table){table.source.data <- resp.sample.nosplit.tb}
                
                #NON-STATE data WITH domains in formula
                  if(!is.state.table & is.domain.table){table.source.data <- resp.sample.split.tb}
                
              #Define table filtering vector
                loop.id.e <- config.tables.tb.e$loop.id
                table.filter.v <-
                  DefineTableFilterVector(
                    tb = table.source.data,
                    filter.varnames = filter.varnames.e,
                    filter.values = filter.values.e
                  )
>>>>>>> 8da574fc489d5bd9a165921efeb89ba8f2b6213d
              
              #Create table itself
                if(table.filter.v %>% not %>% all){
                  table.e <- ""
                }
                
                if(table.filter.v %>% any){
                  table.e <-  
                    table.source.data %>%
                    filter(table.filter.v) %>%
                    dcast(
                      ., 
                      formula = table.formula.e, 
                      value.var = config.tables.tb.e$value.varname, 
                      fun.aggregate = table.aggregation.function
                    ) %>%
                    .[,names(.)!= ""]
                
                  if(class(table.e[,ncol(table.e)-1]) == "factor" || IsError(table.e[,ncol(table.e)] - table.e[,ncol(table.e)-1])){ # add trend
                    table.e$trend <- rep(NA, nrow(table.e))
                  }else{
                    table.e$trend <- table.e[,ncol(table.e)] - table.e[,ncol(table.e)-1]
                  }
                
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
                }
                
              #Table storage
                #if(table.e == ""){print(e)}
                tables.tab4.ls[[e]] <- list()
                tables.tab4.ls[[e]]$configs <- config.tables.tb.e
                tables.tab4.ls[[e]]$table <- table.e
              
            } ### END OF LOOP "e" BY TABLE ###
        
        } #end of if statement for producing tab 4 tables only if not a district overview report
          
      #Assemble final outputs for district ----  
<<<<<<< HEAD
            tables.ls[[c]] <- c(tables.tab12.state.ls, tables.tab12.ls, tables.tab3.ls, tables.tab4.ls)
=======
        tables.ls[[c]] <- 
            c(
              tables.tab12.state.ls, 
              tables.tab12.ls, 
              if(exists("tables.tab3.ls")){tables.tab3.ls}else{},
              if(exists("tables.tab4.ls")){tables.tab4.ls}else{}
            )
          
        if(!is.overview & !exists("tables.tab3.ls")){stop("Missing 'tables.tab3.ls'.")}
        if(!is.overview & !exists("tables.tab4.ls")){stop("Missing 'tables.tab4.ls'.")}  
>>>>>>> 8da574fc489d5bd9a165921efeb89ba8f2b6213d
      
      toc(log = TRUE, quiet = TRUE)  
    } ### END OF LOOP "c" BY REPORT UNIT     
        
  #Section Clocking
    toc(log = TRUE, quiet = FALSE)
    Sys.time() - sections.all.starttime


# 5-EXPORT -----------------------------------------------
  #EXPORT SETUP: CLOCKING, LOAD FUNCTIONS, ESTABLISH OUTPUTS DIRECTORY ----
    #Code Clocking
      section6.starttime <- Sys.time()
      
    #Load Configs Functions
      setwd(rproj.dir)
      #source("6-powerpoints export functions.r")
      
    #Create Outputs Directory
      if(sample.print){
        outputs.dir <- 
          paste(
            outputs.parent.folder,
            gsub(":",".",Sys.time()), 
            sep = ""
          )
      }
      
      dir.create(
        outputs.dir,
        recursive = TRUE
      )
    
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
      #Remove all objects except those needed to print (patch on memory errors)
        #rm(
        #  list = 
        #    setdiff(
        #      ls(), 
        #      c("sections.all.starttime","RemoveNA","unit.ids.sample", "tables.ls","config.text.ls","source.tables.dir","outputs.dir","sample.print")
        #    )
        #)
      
      #Define function to write workbooks inside of loop h
        WriteDistrictWorkbook <- 
          function(
            district.tables.list,
            district.text.list,
            district.file.name
          ){
            
            #Define config tables for district
              config.tables.h <- 
                district.tables.list %>% 
                lapply(., `[[`, 1) %>% 
                do.call(rbind, .) %>%
                mutate(
                  is.overview.table = ifelse(tab.type.id %in% c(1,2), TRUE, FALSE),
                  config.id = 1:nrow(.)
                )
              
              if(is.overview){config.tables.h <- config.tables.h %>% filter(is.overview.table)} #filter out tables not in tabs 1 & 2 if printing district overview
            
            #Define tables list for district
              if(is.overview){
                district.tables.list %<>% .[config.tables.h$config.id[config.tables.h$is.overview.table == TRUE]]
              }
              
            wb <- loadWorkbook(file.name.h, create = FALSE)
            setStyleAction(wb, XLC$"STYLE_ACTION.NONE")
            
            building.names.for.district <- config.tables.h %>% select(loop.id) %>% unlist %>% unique %>% RemoveNA
            
            #i <- 1 #LOOP TESTER
            for(i in 1:length(district.tables.list)){  
              
              #Loop inputs
                if(
                  (district.tables.list[[i]]$table %>% dim %>% length %>% equals(1)) && (district.tables.list[[i]]$table %>% equals(""))
                ){
                  table.i <- ""
                }else{
                  table.i <- district.tables.list[[i]]$table
                }
                configs.i <- district.tables.list[[i]]$configs
                
                #if(configs.i$tab.type.id == 4){ #customize tab name if need be for building summaries
                #  building.num <- configs.i$loop.id %>% unique %>% equals(building.names.for.district) %>% which
                #  
                #  configs.i$tab.name <- 
                #    getSheets(wb) %>% 
                #    .[grepl("Building Summary", .)] %>% 
                #    .[building.num]
                #}
                
              #Print loop messages
                #print(paste("Loop i #: ", i, " - Table: ", configs.i$table.type.name, sep = ""))
              
              #Write Worksheets
                writeWorksheet(
                  object = wb, 
                  data = table.i,
                  sheet = configs.i$tab.name,
                  startRow = configs.i$startrow,
                  startCol = configs.i$startcol,
                  header = configs.i$header,
                  rownames = configs.i$row.header
                )
              
            } # END OF LOOP 'i' BY TABLE
            
            ###                           ###
            ###   LOOP "m" BY TEXT ITEM   ###
            ###                           ###
            
            #Define source table to print text items (filter if just printing District Overviews)
            if(is.overview){
              config.text.h <- district.text.list %>% filter(tab.type.id %in% c(1,2))
            }else{
              config.text.h <- district.text.list
            }
            
            #m = 2 #LOOP TESTER
            for(m in 1:nrow(config.text.h)){
              #print(paste("Loop m #:", m, " - Pct. Complete: ", 100*m/nrow(config.text.h), sep = ""))
              
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
            print(paste("WORKBOOK SAVED. File: ", file.name.h, " - Pct. complete: ", 100*h/length(unit.ids.sample), sep = ""))
          }  
      
    ###                          ###    
  # ### LOOP "h" BY REPORT UNIT  ###
    ###                          ###
    
    #Progress Bar
      if(exists("districts.that.already.have.reports")){print(districts.that.already.have.reports)}
      print(unit.ids.sample)
      progress.bar.h <- txtProgressBar(min = 0, max = 100, style = 3)
      maxrow.h <- tables.ls %>% lengths %>% sum
      
    #h <- 1 #LOOP TESTER
    for(h in 1:length(unit.ids.sample)){ 
      
      unit.id.h <- unit.ids.sample[h]  
                    
      #Set up target file
        if(is.overview){
          template.file <- 
            paste(
              source.tables.dir,
              "overview_template.xlsx",
              sep = ""
            )
        }else{
          template.file <- 
            paste(
              source.tables.dir,
              "dashboard_template.xlsx",
              sep = ""
            )
        }
      
      #Define template file
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
              "_",
              report.version,
              ".xlsx",
              sep = ""
            )
        }
      
      #Define target file path & copy template into new file path to be overwritten as report  
        target.path.h <- 
            paste(
              outputs.dir,
              "\\",
              file.name.h,
              sep=""
            ) 
        
        file.copy(template.file, target.path.h)
        
        print(file.name.h)
    
      #Write Workbook
        setwd(outputs.dir)
        
        WriteDistrictWorkbook(
          district.tables.list = tables.ls[[h]] ,
          district.text.list = config.text.ls[[h]],
          district.file.name = file.name.h
      )

    } # END OF LOOP 'h' BY REPORT UNIT

# 6-WRAP UP -----------------------------------------------
  #Code Clocking
    code.runtime <- Sys.time() %>% subtract(sections.all.starttime) %>% round(., 2)
      
    print(
      paste(
        "Total Runtime for ", 
        length(unit.ids.sample), 
        " reports: ", 
        code.runtime,
        sep = ""
      )
    )
  
    #implied.total.runtime.for.all.reports <- 
    #  resp.full.nosplit.tb$unit.id %>% 
    #  unique %>% length %>% 
    #  divide_by(length(unit.ids.sample)) %>% 
    #  multiply_by(code.runtime)
    
    #print(
    #  paste(
    #    "Implied total runtime for all reports (min): ",
    #    implied.total.runtime.for.all.reports,
    #    sep = ""
    #  )
    #)
    
#SIGNAL CODE IS FINISHED BY OPENING A NEW WINDOW      
  windows()
    