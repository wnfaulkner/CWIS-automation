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
      m900 <- !(Sys.info()[which(row.names(as.data.frame(Sys.info())) == "nodename")] == "LAPTOP-NIDLDLA7")
      
      # Set Working Directory and R Project Directory
      if(m900){  
        #M900
          wd <- "C:\\Users\\WNF\\Google Drive\\1. FLUX PROJECTS - CURRENT\\2016-09 EXT Missouri\\3. MO GDRIVE\\8. CWIS\\2020-04 Phase 11\\"
          rproj.dir <- "C:\\Users\\WNF\\Documents\\GIT PROJECTS\\CWIS-automation\\"
      }else{
        #Thinkpad T470
          wd <- "G:\\Meu Drive\\1. FLUX PROJECTS - CURRENT\\2016-09 EXT Missouri\\3. MO GDRIVE\\8. CWIS\\2020-04 Phase 11\\"
          rproj.dir <- "C:\\Users\\WNF\\Documents\\GIT PROJECTS\\GIT_2016-09 EXT Missouri_CWIS-automation"
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
    source("2016-09 EXT Missouri_CWIS-automation functions.r")
    
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
    library(tibble)
    
    #Section Clocking
      #section0.duration <- Sys.time() - section0.starttime
      #section0.duration

# 1-IMPORT -----------------------------------------
  
  #Section Clocking
    #section1.starttime <- Sys.time()
    tic("1-Import") 
    
  #Source Import Functions
    setwd(rproj.dir)

  #Import Config Tables
    configs.ss <- gs_key("1Bbs7ITPpCjB73ZEp2PLJuWyEAvG0X7XEMMR6wTUb-PM", verbose = TRUE) 
    
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
        if(current.school.year == "0000"){
          "0000"  
        }else{
          current.school.year %>%
          strsplit(., "-") %>%
          unlist() %>%
          as.numeric() %>%
          subtract(1) %>%
          as.character %>%
          paste0(., collapse = "-")
        }
    
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
    
  #Import resp.full.split if have saved it on a previous run
    if(file.exists("resp.full.split.tb.csv") && m900){
      resp.full.split.tb <-
      read.csv(file = "resp.full.split.tb.csv") %>%
      as_tibble()
    }
    
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

  #BUILDINGS CONFIG TABLE ----
    buildings.tb %<>%
      LowerCaseCharVars(.) %>%
      mutate(
        district =
          district %>% gsub(" ", ".", .) %>%
          SubRepeatedCharWithSingleChar(., ".")
      )
    
  #RESPONSE TABLE  ----
    #Lower-case all character variable data
      resp2.tb <- LowerCaseCharVars(resp1.tb)  
      
    #Names/Column Headers
      resp2.tb %<>% 
        LowerCaseNames(.) %>%  #Lower-case all variable names
        ReplaceNames( #Replace specific column names
          ., 
          current.names = c("school", "id", "schoolid"), 
          new.names = c("building.name", "resp.id", "building.id")
        ) %>% 
        as_tibble()
  
      names(resp2.tb) <- #replace domain names that have 'c' in front of them for stome reason with regular domain names
        mgsub(
            pattern = paste("c",domains, sep = ""), 
            replacement = domains, 
            x = names(resp2.tb), 
            print.replacements = FALSE
          )
      
      names(resp2.tb) <- SubRepeatedCharWithSingleChar(string.vector = names(resp2.tb), char = ".") # get rid of any double periods in names
      
      #Add building ids for District Offices and Other
        resp2.tb %<>%
          mutate(
            district = 
              district %>% gsub(" ", ".", .) %>%
              SubRepeatedCharWithSingleChar(., ".")
          )
          
        resp.district.office.other.tb <- 
          resp2.tb %>%
          filter(building.name %in% c("district office", "other"))
        
        for(i in 1:nrow(resp.district.office.other.tb)){
          building.name.i <- resp.district.office.other.tb$building.name[i]
          building.id.end.i <- if(building.name.i == "district office"){"0001"}else{"0002"}
          district.i <- resp.district.office.other.tb$district[i]
          
          building.id.i <- #create new building id from district id plus '0001' for district office and '0002' for other
            buildings.tb %>% 
            filter(district == district.i) %>%
            select(building.id) %>%
            UnlistVector() %>%
            RemoveNA %>%
            .[1] %>%
            substr(., 1, 5) %>%
            paste(., building.id.end.i, sep = "")
          
          resp2.tb$building.id[ #insert into response table
            resp2.tb$building.name == building.name.i & 
            resp2.tb$district == district.i
          ] <- building.id.i 
          
          buildings.tb$building.id[ #insert into buildings config table
            buildings.tb$district == district.i & 
            buildings.tb$building.name == building.name.i
          ] <- building.id.i 
          
          if(!(building.id.i %in% buildings.tb$building.id & building.id.i %in% buildings.tb$building.id)){print(i)}
        }
        
      #Replace district and building.name variables from response data with official designations from DESE
        resp2.tb <- 
          left_join(
            resp2.tb %>% select(-c("district","building.name")),
            buildings.tb %>% select(building.id, district, building.name, building.level),
            by = "building.id"
          )
        
        resp2.tb$building.level <- Proper(resp2.tb$building.level)
        
      #Assign variable that is the report unit the name 'unit.id'  
        resp2.tb %<>% 
          ReplaceNames(
            ., 
            current.names = names(resp2.tb)[names(resp2.tb) == as.vector(report.unit)], 
            new.names =  "unit.id"
          )

    #Row Filters
      
      #Filter out blank schools
        resp3.tb <- 
          resp2.tb %>% 
          filter(building.name != "")
        
      #Building Restrictions
        if(dashboard.or.overview == "dashboard"){
          buildings.to.keep <- 
            resp3.tb %>%
            dcast(., building.id ~ year, value.var = "resp.id", fun.aggregate = length) %>%
            mutate(
              filter.baseline = baseline %>% is_greater_than(0),
              filter.most.recent = apply(.[,ncol(.)-1] %>% as.data.frame(), 1, sum) %>% is_greater_than(10),
              filter.other.school.years = apply(.[,2:(ncol(.)-1)] %>% as.data.frame(), 1, sum) %>% is_greater_than(0),
              filter.combined = filter.baseline & filter.most.recent & filter.other.school.years
            ) %>%
            filter(filter.combined) %>%
            select(building.id) %>%
            UnlistVector()
          
          resp4.tb <- 
            resp3.tb %>%
            filter(building.id %in% buildings.to.keep)
        }
        
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
          select(year, building.id) %>%
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
          year.var.helper.tb,
          by = c("year", "building.id")
        ) %>% 
        mutate(variable = as.character(variable))
      
   
  #FORM FINAL DATASETS - RESTRICTED TO SAMPLE ---- 
    #Not necessary in Phase 11 as restrictions simplified and data already filtered above during cleaning
    
    #Restrict whole dataset to districts with following condition: 
      #50% or more of buildings in the district that are not `other` or `district office` 
      #have at least one response in the current period
      
      #dashboard.restriction.tb <-
      #  resp8.tb %>% 
      #  select(resp.id, year, unit.id, building.name, building.id) %>%
      #  filter(year == current.school.year) %>% #filter for current school year 
      #  dcast( #reshape into table of response counts by building.id
      #    ., 
      #    formula = unit.id + building.id ~ ., 
      #    value.var = "resp.id", 
      #    fun.aggregate = function(x){length(unique(x))}
      #  ) %>%
      #  as_tibble() %>%
      #  select(-unit.id) %>%
      #  ReplaceNames(
      #    ., 
      #    current.names = names(.)[length(names(.))],
      #    new.names = "resp.count.current.school.year"
      #  ) %>%
      #  left_join( #join with buildings table so have all buildings and response counts for current school year
      #    buildings.tb %>% select(district, building.id) %>% filter(!duplicated(building.id)), 
      #    ., 
      #    by = "building.id"
      #  ) %>%
      #  filter(!grepl("district.office|other", building.id)) %>% #get rid of rows for 'other' and 'district office' buildings
      #  dcast( #reshape into table of percentage of buildings with responses in current school year by district
      #    .,
      #    formula = district ~ .,
      #    value.var = 'resp.count.current.school.year',
      #    fun.aggregate = function(x){mean(!is.na(x))}
      #  ) %>%
      #  mutate(is.dashboard = . >= 0.5) %>% #add logical variable for those districts with >= 50% of buildings with responses in current school year
      #  ReplaceNames(
      #    ., 
      #    current.names = names(.)[2],
      #    new.names = "pct.bldgs.w.responses.in.current.schyr"
      #  ) %>%
      #  as_tibble()
      
      
      if(dashboard.or.overview == "overview"){
        overview.restriction.base.tb <-
          buildings.tb %>%
          mutate(
            is.overview = FALSE,
            constant = "x"
          )
          
        overview.restriction.base.tb$is.overview[overview.restriction.base.tb$cohort == 3] <- TRUE
        
        overview.restriction.tb <-
          dcast(overview.restriction.base.tb, district ~ constant, value.var = "is.overview", fun.aggregate = any) %>%
          ReplaceNames(., current.names = "x", new.names = "is.overview") %>%
          as_tibble()
      }
        
      if(exists("dashboard.restriction.tb")){
        resp9.tb <-   
          left_join(
            resp8.tb,
            dashboard.restriction.tb,
            by = "district"
          )
        
        buildings.tb %<>%
        left_join(
          .,
          dashboard.restriction.tb,
          by = "district"
        ) 
        
      }else{
        resp9.tb <- resp8.tb
      }
      
      if(exists("overview.restriction.tb")){
        resp9.tb <-
          left_join(
            resp9.tb, 
            overview.restriction.tb,
            by = "district"
          )
        
        buildings.tb %<>%
          left_join(
          ., 
          overview.restriction.tb,
          by = "district"
        )
      }else{}
      
      
    #Full dataset - no splitcolreshape by domain
      resp.full.nosplit.tb <- 
        resp8.tb %>%
        left_join(
          x = resp8.tb,
          y = questions.tb %>% select(var.id, domain),
          by = c("variable"="var.id")
        ) 
        
      
    #Restricted datasets (depending on whether printing dashboards or overviews)
      #if(is.overview){
      #  resp.restricted.nosplit.tb <- 
      #    resp.full.nosplit.tb %>%
      #    filter(is.overview == TRUE)
      #  
      #  resp.restricted.split.tb <-
      #    resp.full.split.tb %>%
      #    filter(is.overview == TRUE)
      #}else{
      #  resp.restricted.nosplit.tb <- 
      #    resp.full.nosplit.tb %>%
      #    filter(is.dashboard == TRUE)
      #  
      #  resp.restricted.split.tb <-
      #    resp.full.split.tb %>%
      #    filter(is.dashboard == TRUE)
      #}
        
  #FORM FINAL DATASETS - SPLICOLRESHAPE ----
      
    #Full dataset - splitcolreshape by domain (for some practices where answers count towards ETL and CFA)
      if(!exists("resp.full.split.tb")){
        resp.full.split.tb <-
          resp.full.nosplit.tb %>%
          SplitColReshape.ToLong(
            df = ., 
            id.varname = "resp.id" ,
            split.varname = "domain",
            split.char = ","
          ) %>% 
          left_join(
            x = ., 
            y = domains.tb,
            by = c("domain" = "domain.id")
          ) %>%
          left_join(
            x = ., 
            y = practices.tb %>% SplitColReshape.ToLong(., id.varname = "practice.id", split.varname = "domain.id", split.char = ","),
            by = c("variable" = "practice.id")
          ) %>%
          select(-domain.id) %>%
          ReplaceNames(., current.names = "practice.abbrv", new.names = "practice") %>%
          as_tibble()
      } 
    
  #FORM SAMPLE DATASETS ----  
    
    #Scenario 1 - sample print
      
      if(sample.print){ 
        is.valid.sample <- FALSE
        while(!is.valid.sample){
          
          resp.sample.nosplit.tb <- 
            RestrictDataToSample(
              tb = resp.full.nosplit.tb,
              report.unit = "unit.id",
              sample.print = sample.print,
              sample.group.unit = "unit.id",
              sample.size = sample.size
            ) #%>% filter(filter.combined)
          
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
        
        resp.sample.split.tb <- 
          resp.full.split.tb %>%
          filter(unit.id %in% unit.ids.sample)
        
      }
      
    #Scenario 2 - full print (fresh, not adding to previous full print)  
      if(!sample.print & !add.to.last.full.print){
          
        resp.sample.nosplit.tb <- 
          resp.full.nosplit.tb #%>% filter(filter.combined)
        
        resp.sample.split.tb <- 
          resp.full.split.tb #%>% filter(filter.combined)
        
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
            resp.full.nosplit.tb %>%
            select(unit.id) %>%
            unique %>%
            UnlistVector %>%
            setdiff(., districts.that.already.have.reports) %>%
            .[1:sample.size] %>%
            RemoveNA
          
          is.valid.sample <-
            unit.ids.sample %in% districts.that.already.have.reports %>% not %>% all
          
          print(is.valid.sample)
          if(!is.valid.sample){next()}
          
          resp.sample.nosplit.tb <- 
            resp.full.nosplit.tb %>%
            filter(unit.id %in% unit.ids.sample)
          
          resp.sample.split.tb <- 
            resp.full.split.tb %>%
            filter(unit.id %in% unit.ids.sample)
        }
      }

  #Section Clocking
    toc()
    Sys.time() - sections.all.starttime

# 3-CONFIGS (TAB, TABLE, TEXT CONFIG TABLES) ------------------
  
  #Section Clocking
    tic("3-Configs")
          
  #Load Configs Functions
    setwd(rproj.dir)

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
      
      #Building Summary Loop Variable & Building Names
        building.summaries.loopvarname <- 
          config.tab.types.tb %>% 
          select(tab.loop.var.1) %>% 
          unlist %>% unique %>% RemoveNA
        
        building.names <- resp.long.tb.b %>% select(building.summaries.loopvarname) %>% unlist %>% unique
        
        building.names.tb <- 
          tibble(
            tab.type.name = "Building Summary",
            loop.id = building.names
          )
      
    #Tab config table for this report unit
      
      config.tabs.ls[[b]] <-
        tibble(
          tab.type.name = "Building Summary",
          loop.id = building.names 
        ) %>%
        rbind(
          config.tab.types.tb %>% select(tab.type.name) %>% mutate(loop.id = NA) %>% filter(tab.type.name != "Building Summary"),
          .
        ) %>%
        mutate(
          tab.name = 
            c(
              tab.type.name[1:4],
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
          by = "tab.type.name"
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
          by = "tab.type.name"
        ) %>% 
        full_join(
          .,
          config.tabs.ls[[b]],
          by = c("tab.type.name","loop.id")
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

  #STATE AVERAGE TABLES ----
    
    #Overview Tabs State Averages ----

      #Loop Inputs
        config.tables.overviews.input.tb <- 
          config.tables.ls[[1]] %>% 
          filter(!is.na(table.type.name) & grepl("Overview", tab.type.name) & is.state.table) %>%
          filter(tab.type.name != "District Overview (vs state)") %>%
          as_tibble()
        
        tables.overviews.state.ls <- list()
      
      #d <- 14
      for(d in 1:nrow(config.tables.overviews.input.tb)){ ### START OF LOOP "d" BY TABLE ###
        
        #Print loop messages
          print(paste("OVERVIEW TABS LOOP - Loop #: ", d, " - Pct. Complete: ", 100*d/nrow(config.tables.overviews.input.tb), sep = ""))
        
        #Define table configs for loop
          config.tables.tb.d <- config.tables.overviews.input.tb[d,]
        
        #CREATE TABLES
        
          #CWIS Response Tab Tables
            if(config.tables.tb.d$tab.type.name == "CWIS Responses"){
              
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
                
              #Table Storage
                table.d.storage.index <- length(tables.overviews.state.ls) %>% add(1)
                tables.overviews.state.ls[[table.d.storage.index]] <- list()
                tables.overviews.state.ls[[table.d.storage.index]]$configs <- config.tables.tb.d
                tables.overviews.state.ls[[table.d.storage.index]]$table <- table.d
                
            } #End of if statement for CWIS Response Tab
        
          #District Overview (vs district) Tables
            if(grepl("District Overview", config.tables.tb.d$tab.type.name)){
              
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
                
              #Table Storage
                
                #Store table for 'vs district' tab
                  table.d.storage.index <- length(tables.overviews.state.ls) %>% add(1)
                  tables.overviews.state.ls[[table.d.storage.index]] <- list()
                  tables.overviews.state.ls[[table.d.storage.index]]$configs <- config.tables.tb.d
                  tables.overviews.state.ls[[table.d.storage.index]]$table <- table.d
                
                #Store table for 'vs state' tab
                  table.d.storage.index <- length(tables.overviews.state.ls) %>% add(1)
                  tables.overviews.state.ls[[table.d.storage.index]] <- list()
                  
                  tables.overviews.state.ls[[table.d.storage.index]]$configs <- 
                    config.tables.ls[[1]] %>% 
                    filter(
                      table.type.name == config.tables.tb.d$table.type.name,
                      startrow == config.tables.tb.d$startrow,
                      startcol == config.tables.tb.d$startcol,
                      table.type.id != config.tables.tb.d$table.type.id
                    )
                  
                  tables.overviews.state.ls[[table.d.storage.index]]$table <- table.d
                
            } #End of if statement for District Overview Tabs
        
      } ### END OF LOOP "d" BY TABLE ###
      
      #Finalizing loop outputs  
        names(tables.overviews.state.ls) <- 
          paste(
            c(1:length(tables.overviews.state.ls)),
            ".",
            config.tables.tb.d$table.type.name, 
            sep = ""
          ) %>%
          gsub("-", " ", .) %>%
          gsub(" ", ".", .) %>%
          SubRepeatedCharWithSingleChar(., ".") %>%
          tolower
    
    
    #Buildings Over Time - State Averages ----
      if(!is.overview){
        
        #Current vs. previous school year
          buildings.over.time.state.avg.current.vs.previous.school.year <-
            resp.full.split.tb %>% 
            filter(
              is.most.recent.or.current == 1
            ) %>%
            as_tibble() %>%
            dcast(
              data = .,
              formula = domain ~ is.current,
              fun.aggregate = mean,
              value.var = "value"
            ) %>%
            mutate(
              Trend = `1` %>% subtract(`0`)
            ) %>%
            ReplaceNames(., current.names = c("0","1"), new.names = c(previous.school.year,current.school.year)) %>%
            TransposeTable(., keep.first.colname = FALSE) %>%
            apply(., c(1,2), trimws) 
          
          buildings.over.time.state.avg.current.vs.previous.school.year %<>%
            apply(
              X = buildings.over.time.state.avg.current.vs.previous.school.year[,2:ncol(buildings.over.time.state.avg.current.vs.previous.school.year)], 
              MARGIN = 2, 
              FUN = as.numeric
            ) %>%
            as_tibble() %>%
            mutate(
              period = buildings.over.time.state.avg.current.vs.previous.school.year[,1]
            ) %>%
            MoveColsLeft(., "period")
            
          buildings.over.time.state.avg.current.vs.previous.school.year$period <-
            gsub(previous.school.year, "Prev. School Year", buildings.over.time.state.avg.current.vs.previous.school.year$period)
          
        #Current vs. baseline
          buildings.over.time.state.avg.current.vs.baseline <- 
            resp.full.split.tb %>% 
            filter(
              is.baseline.or.current == 1
            ) %>%
            as_tibble() %>%
            dcast(
              data = .,
              formula = domain ~ is.current,
              fun.aggregate = mean,
              value.var = "value"
            ) %>%
            mutate(
              Trend = `1` %>% subtract(`0`)
            ) %>%
            ReplaceNames(., current.names = c("0","1"), new.names = c("Baseline",current.school.year)) %>%
            TransposeTable(., keep.first.colname = FALSE) %>%
            apply(., c(1,2), trimws) 
          
          buildings.over.time.state.avg.current.vs.baseline %<>%
            apply(
              X = buildings.over.time.state.avg.current.vs.baseline[,2:ncol(buildings.over.time.state.avg.current.vs.baseline)], 
              MARGIN = 2, 
              FUN = as.numeric
            ) %>%
            as_tibble() %>%
            mutate(
              period = buildings.over.time.state.avg.current.vs.baseline[,1]
            ) %>%
            MoveColsLeft(., "period")
        
        } #end of if statement to execute code only if not an overview report.
        
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
      building.level.order.v <- c("Elem.","Middle","High","Technology Ctr.","Other")
        
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
      
      #CWIS Responses Tab ----
          
        tables.cwis.responses.ls <- list(NULL)
          
        #Config Input
          tables.cwis.responses.ls[[1]]$configs <- 
            config.tables.ls[[c]] %>% 
            filter(!is.na(table.type.id)) %>%
            filter(grepl("CWIS Responses", tab.type.name)) %>%
            filter(!is.state.table) %>%
            as_tibble()
          
        #Unit Response Table
          tables.cwis.responses.ls[[1]]$table <-
            resp.sample.nosplit.tb %>%
            filter(unit.id == unit.id.c) %>%
            dcast(., formula = building.name ~ year, value.var = "resp.id", fun.aggregate = length) %>%
            ReplaceNames(., c("building.name","0000"), c("Building", "Baseline"))
          
      #Overview Tabs ----
       
        #Loop Inputs
          config.tables.overviews.input.tb <- 
            config.tables.ls[[c]] %>% 
            filter(!is.na(table.type.id)) %>%
            filter(grepl("vs district", tab.type.name)) %>%
            filter(!is.state.table) %>%
            as_tibble()
          
          tables.overview.ls <- list()
          max.d <- config.tables.overviews.input.tb %>% nrow
        
        #d <- 29
        for(d in 1:max.d){ ### START OF LOOP "d" BY TABLE ###
          
          #Loop timing
            #tic("Tabs 1 & 2 loop iteration:", d)
          
          #Print loop messages
            print(paste("OVERVIEW TABS LOOP - Loop #: ", d, " - Pct. Complete: ", 100*d/max.d, sep = ""))
          
          #Define table configs for loop
            config.tables.tb.d <- config.tables.overviews.input.tb[d,]

          #CREATE TABLE
              
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
            
          #Table Storage
              
            #Store table for 'vs district' tab
              table.d.storage.index <- length(tables.overview.ls) %>% add(1)
              tables.overview.ls[[table.d.storage.index]] <- list()
              tables.overview.ls[[table.d.storage.index]]$configs <- config.tables.tb.d
              tables.overview.ls[[table.d.storage.index]]$table <- table.d
              
            #Store table for 'vs state' tab
              table.d.storage.index <- length(tables.overview.ls) %>% add(1)
              tables.overview.ls[[table.d.storage.index]] <- list()
              
              tables.overview.ls[[table.d.storage.index]]$configs <- 
                config.tables.ls[[1]] %>% 
                filter(
                  table.type.name == config.tables.tb.d$table.type.name,
                  startrow == config.tables.tb.d$startrow,
                  startcol == config.tables.tb.d$startcol,
                  table.type.id != config.tables.tb.d$table.type.id
                )
              
              tables.overview.ls[[table.d.storage.index]]$table <- table.d
          
          #tic.log(format = TRUE)
          #toc(log = TRUE, quiet = TRUE)
          
        } ### END OF LOOP "d" BY TABLE ###
          
          #Finalize names
            names(tables.overview.ls) <- 
              paste(
                rep(unit.id.c, length(tables.overview.ls)),
                ".",
                c(1:length(tables.overview.ls)),
                ".",
                config.tables.tb.d$table.type.name, 
                sep = ""
              ) %>%
              gsub("-", " ", .) %>%
              gsub(" ", ".", .) %>%
              SubRepeatedCharWithSingleChar(., ".") %>%
              tolower
        
      #Buildings Over Time ----
        
        if(!is.overview){
          
          #Print status
            #print("Buildings Over Time calculations begun...")
            
          #Loop Inputs
            config.buildings.over.time.tb <-   
              config.tables.ls[[c]] %>% 
              filter(!is.na(table.type.name)) %>%
              filter(grepl("Over Time", tab.type.name))

            tables.buildings.over.time.ls <- list()
          
          #State Average Table - Last School Year vs. Current
            tables.buildings.over.time.ls[[1]] <- list()
            tables.buildings.over.time.ls[[1]]$configs <- config.buildings.over.time.tb %>% filter(table.type.id == 82)
            tables.buildings.over.time.ls[[1]]$table <- buildings.over.time.state.avg.current.vs.previous.school.year  
          
          #Building Average Table - Last School year vs. Current  
            tables.buildings.over.time.ls[[2]] <- list()
            tables.buildings.over.time.ls[[2]]$configs <- config.buildings.over.time.tb %>% filter(table.type.id == 83)
            buildings.over.time.bldg.current.vs.previous.school.year <- 
              resp.sample.split.tb %>% 
              filter(
                unit.id == unit.id.c & 
                  is.most.recent.or.current == 1
              ) %>%
              dcast(
                data = .,
                formula = building.name + domain ~ year,
                fun.aggregate = mean,
                value.var = "value"
              )
  
            if(nrow(buildings.over.time.bldg.current.vs.previous.school.year) < 1){
              buildings.over.time.bldg.current.vs.previous.school.year <- "" 
            }else{
              
              if(ncol(buildings.over.time.bldg.current.vs.previous.school.year) < 4){ # if missing previous school year data, add 4th column with NAs
                buildings.over.time.bldg.current.vs.previous.school.year %<>%
                  mutate(
                    `Prev. School Year` = NA,
                    Trend = NA
                  ) %>%
                  ReplaceNames(., "Prev. School Year", previous.school.year)
              }
              
              if(ncol(buildings.over.time.bldg.current.vs.previous.school.year) == 4){ # add trend column
                buildings.over.time.bldg.current.vs.previous.school.year %<>%
                  mutate(
                    Trend = .[,4] - .[,3]
                  )
              }
              
              buildings.over.time.bldg.current.vs.previous.school.year %<>% # transpose table
                melt(
                  ., 
                  id.vars = c("building.name","domain")
                ) %>%
                dcast(
                  ., 
                  formula = building.name + variable ~ domain
                )
              
              buildings.over.time.bldg.current.vs.previous.school.year <-
                ManualOrderTableByVectorsWithValuesCorrespondingToVariableInTable(
                  tb = buildings.over.time.bldg.current.vs.previous.school.year,
                  tb.order.varnames = "variable",
                  ordering.vectors.list = 
                    list(
                      c(previous.school.year,current.school.year, "Trend")
                    )
                ) %>%
                OrderDfByVar(., order.by.varname = "building.name", rev = FALSE)

            }
            
            tables.buildings.over.time.ls[[2]]$table <- buildings.over.time.bldg.current.vs.previous.school.year
    
          #State Average Table - Baseline vs. Current
            tables.buildings.over.time.ls[[3]] <- list()
            tables.buildings.over.time.ls[[3]]$configs <- config.buildings.over.time.tb %>% filter(table.type.id == 84)
            tables.buildings.over.time.ls[[3]]$table <- buildings.over.time.state.avg.current.vs.baseline
              
            
          #Building Average Table - Baseline year vs. Current  
            tables.buildings.over.time.ls[[4]] <- list()
            tables.buildings.over.time.ls[[4]]$configs <- config.buildings.over.time.tb %>% filter(table.type.id == 85)
            buildings.over.time.bldg.current.vs.baseline <- 
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
            
            if(ncol(buildings.over.time.bldg.current.vs.baseline) < 4){
              buildings.over.time.bldg.current.vs.baseline %<>%
                mutate(
                  `Prev. School Year` = NA,
                  Trend = NA
                )
            }
            
            if(ncol(buildings.over.time.bldg.current.vs.baseline) == 4){
              buildings.over.time.bldg.current.vs.baseline %<>%
                mutate(
                  Trend = .[,4] - .[,3]
                )
            }
            
            buildings.over.time.bldg.current.vs.baseline %<>%
              melt(
                ., 
                id.vars = c("building.name","domain")
              ) %>%
              dcast(
                ., 
                formula = building.name + variable ~ domain
              )
            
            buildings.over.time.bldg.current.vs.baseline$variable %<>%
              as.character %>%
              gsub("0000", "Baseline", .)
            
            tables.buildings.over.time.ls[[4]]$table <- buildings.over.time.bldg.current.vs.baseline
        } #end of if statement for producing building summary tables only if not a district overview report
      
      #Building Summary Tabs ----
        
        if(!is.overview){  
          #Loop timing
            #tic("Building Summary Tabs Duration:")  
            
          #Loop Inputs
            config.tables.building.summaries.input.tb <- 
              config.tables.ls[[c]] %>% 
              filter(!is.na(table.type.name)) %>%
              filter(tab.type.name == "Building Summary") %>%
              filter(!is.na(loop.id))
            tables.building.summaries.ls <- list()
            
          #e <- 2
          for(e in 1:nrow(config.tables.building.summaries.input.tb)){ ### START OF LOOP "e" BY TABLE ###
              
              #Print loop messages
                print(paste("BUILDING SUMMARY LOOP - Loop #: ", e, " - Pct. Complete: ", 100*e/nrow(config.tables.building.summaries.input.tb), sep = ""))
              
              #Define Configs for this loop
                config.tables.tb.e <- config.tables.building.summaries.input.tb[e,]
              
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
              
              #Create table itself
                if(table.filter.v %>% not %>% all){
                  table.e <- ""
                  
                  #Table storage
                    #if(table.e == ""){print(e)}
                    tables.building.summaries.ls[[e]] <- list()
                    tables.building.summaries.ls[[e]]$configs <- config.tables.tb.e
                    tables.building.summaries.ls[[e]]$table <- table.e
                    
                  next()
                }
              
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
                
                #Add missing practices
                  if(
                    !is.na(config.tables.tb.e$row.header.varname) && 
                    config.tables.tb.e$row.header.varname == "practice"
                  ){
                    table.e %<>%
                      left_join(
                        ., 
                        practices.tb %>% select(practice.abbrv, practice.long),
                        by = c("practice" = "practice.abbrv")
                      ) %>%
                      select(-practice) %>%
                      ReplaceNames(., "practice.long", "practice") %>%
                      MoveColsLeft("practice")
                      
                      
                      
                  #    mutate(
                  #      practice = practice.long %>% tolower
                  #    )
                  #  
                  #  domain.e <- 
                  #    config.tables.tb.e$filter.values %>%
                  #    strsplit(., ";") %>%
                  #   UnlistVector() %>% 
                  #    .[. %in% domains]
                  #  
                  #  practices.e <- 
                  ##    practices.tb %>%
                  #    filter(grepl(domain.e, practices.tb$domain.id)) %>%
                  #    select(practice.long) %>%
                  #    UnlistVector() %>% 
                  #    tolower() %>%
                  #    as.data.frame %>%
                  #    ReplaceNames(., names(.), "practice")
                  #  
                  #  table.e %<>%
                  #    left_join(
                  #      practices.e,
                  #      ., 
                  #      by = "practice"
                  #    )
                  }
                
                #Add columns with no data if necessary
                  
                  full.names.e <-
                    if(grepl("is.most.recent", config.tables.tb.e$filter.varname)){
                      c(previous.school.year, current.school.year)
                    }else{
                      c("0000", current.school.year)
                    }
                  
                  add.names.e <- full.names.e[full.names.e %!in% names(table.e)]
                  
                  table.e[add.names.e] <- NA
                  
                  table.e %<>%
                    ReplaceNames(., ".", "practice") %>%
                    .[,order(names(.))] %>%
                    MoveColsLeft(., "practice") %>%
                    OrderDfByVar(., "practice", rev = FALSE)
                
                #Add trend variable
                  if(
                    class(table.e[,ncol(table.e)-1]) == "factor" || 
                    IsError(table.e[,ncol(table.e)] - table.e[,ncol(table.e)-1]) %in% c(FALSE,NA) %>% all %>% not
                  ){ 
                    table.e$trend <- rep(NA, nrow(table.e))
                  }else{
                    table.e$trend <- table.e[,ncol(table.e)] %>% subtract(table.e[,ncol(table.e)-1]) %>% UnlistVector()
                  }
                  
               #print(table.e)
              
              #Modifications for specific tables
                #if(grepl("building.level", table.formula.e) %>% any){
                #  table.e <- 
                #    left_join(
                #      building.level.order.v %>% as.data.frame %>% ReplaceNames(., ".", "building.level"), 
                #      table.e,
                #      by = "building.level"
                #    )
                #}
              
                
                if(!config.tables.tb.e$row.header){  #when don't want row labels
                  table.e <- table.e %>% select(names(table.e)[-1])
                }
              
            #Table storage
              #if(table.e == ""){print(e)}
              tables.building.summaries.ls[[e]] <- list()
              tables.building.summaries.ls[[e]]$configs <- config.tables.tb.e
              tables.building.summaries.ls[[e]]$table <- table.e
              
          } ### END OF LOOP "e" BY TABLE ###
        
        } #end of if statement for producing building summary tables only if not a district overview report
          
      #Assemble final outputs for district ----  

        tables.ls[[c]] <- 
            c(
              tables.cwis.responses.ls,
              tables.overviews.state.ls, 
              tables.overview.ls, 
              if(exists("tables.buildings.over.time.ls")){tables.buildings.over.time.ls}else{},
              if(exists("tables.building.summaries.ls")){tables.building.summaries.ls}else{}
            )
          
        if(!is.overview & !exists("tables.buildings.over.time.ls")){stop("Missing 'tables.buildings.over.time.ls'.")}
        if(!is.overview & !exists("tables.building.summaries.ls")){stop("Missing 'tables.building.summaries.ls'.")}  

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

    ###                          ###    
  # ### LOOP "h" BY REPORT UNIT  ###
    ###                          ###
    
    #Progress Bar
      if(exists("districts.that.already.have.reports")){print(districts.that.already.have.reports)}
      print(unit.ids.sample)
      progress.bar.h <- txtProgressBar(min = 0, max = 100, style = 3)
      maxrow.h <- tables.ls %>% lengths %>% sum
      
    #h <- 1 #LOOP TESTER
    for(h in 1:length(tables.ls)){ 
      
      unit.id.h <- unit.ids.sample[h]  
                    
      #Set up target file
        if(is.overview){
          template.file <- 
            paste(
              source.tables.dir,
              MostRecentlyModifiedFilename(
                title.string.match = "overview_template",
                file.type = "xlsx",
                dir = source.tables.dir
              ),
              sep = ""
            )
        }else{
          template.file <- 
            paste(
              source.tables.dir,
              MostRecentlyModifiedFilename(
                title.string.match = "dashboard_template",
                file.type = "xlsx",
                dir = source.tables.dir
              ),
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
        
        WriteReportWorkbook(
          district.tables.list = tables.ls[[h]],
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
    