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
      #TODO: MAKE UTILS FUNCTIONS THAT 
        #(A) DESIGNATE SYS.TIME() 'BOOKMARKS' OR 'WAYPOINTS'
        #(B) PRINT A TABLE OF WAYPOINTS IN ROWS AND COLUMNS REPRESENTING
          #(1) TIME SINCE MOST RECENT WAYPOINT
          #(2) CUMULATIVE TIME SINCE FIRST WAYPOINT
          #(3) TIME SINCE MOST RECENT WAYPOINT/TIME SINCE FIRST WAYPOINT (PERCENTAGE)
    
      sections.all.starttime <- Sys.time()
      section0.starttime <- sections.all.starttime
  
  # ESTABLISH BASE DIRECTORIES
  
    #M900
      working.dir <- "C:\\Users\\willi\\Google Drive\\1. FLUX CONTRACTS - CURRENT\\2016-09 EXT Missouri\\3. MO GDRIVE\\8. CWIS\\2019-04 CWIS Auto Phase 8"
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
    library(pander)
    library(proftools)
    library(jsonlite)
    library(rlang)
    library(extrafont)
    extrafont::loadfonts(device="win")
    
    #Special for ReporteRs package because it has been archived (https://stackoverflow.com/questions/24194409/how-do-i-install-a-package-that-has-been-archived-from-cran)
      
      #Install Dependencies
        if( !require( rJava ) ) install.packages("rJava")
        if( !require( ggplot2 ) ) install.packages("ggplot2")
        if( !require( base64 ) ) install.packages("base64")
        if( !require( highlight ) ) install.packages("highlight")
        if( !require( devtools ) ) install.packages("devtools")
        if( !require( rvg ) ) install.packages("rvg")
        if( !require( gdtools ) ) install.packages("gdtools")
        if( !require( png ) ) install.packages("png")
        if( !require( R.Utils ) ) install.packages("R.utils")
        if( !require( ReporteRsjars ) ) install.packages("ReporteRsjars")
        if( !require( jsonlite ) ) install.packages("jsonlite")
        if( !require( rlang ) ) install.packages("rlang")
      
      #Install Archived Packages: ReporteRsjars & ReporteRs  
        if( !require( ReporteRsjars ) ) devtools::install_github("davidgohel/ReporteRsjars")
        if( !require( ReporteRs ) ) devtools::install_github("davidgohel/ReporteRs")
    
    #Section Clocking
      section0.duration <- Sys.time() - section0.starttime
      section0.duration

# 0-SETUP OUTPUTS -----------------------------------------------------------
  #start_time: sys.time for code
  #working.dir: working directory - Google Drive folder "2018-08 Green Reports"
  #rproj.dir: directory for R project; also contains source data, additional function scripts, and config tables.
  #source.tables.dir: directory with raw data, configs, etc.

# 1-IMPORT -----------------------------------------
  
  #Section Clocking
    section1.starttime <- Sys.time()
      
  #Source Import Functions
    setwd(rproj.dir)
    source("1-import_functions.r")
  
  #Import Config Tables
    configs.ss <- gs_key("1wukfxVKxx8vXq1ZsU1_eqr3Yh0e00u8WLqepOe9I9YU",verbose = TRUE) 
    
    #Import all tables from config google sheet as tibbles
      all.configs.ls <- GoogleSheetLoadAllWorksheets(configs.ss)
    
    #Assign each table to its own tibble object
      ListToTibbleObjects(all.configs.ls) #Converts list elements to separate tibble objects names with
                                          #their respective sheet names with ".tb" appended
    
  #Extract global configs from tibble as their own character objects
    TibbleToCharObjects(config.global.tb)
    sample.print <- #Convert sample.print to TRUE/FALSE
      ifelse(
        sample.print == "true",
        TRUE,
        FALSE
      )

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
  
  #config.graph.types.tb
    config.graph.types.tb <- AddSlideTypeCols(config.graph.types.tb) #Add slide.type columns via inner-join
    
  #config.table.types.tb
    config.table.types.tb <- AddSlideTypeCols(config.table.types.tb) #Add slide.type columns via inner-join
    
  #config.pot.tb
    config.pot.types.tb <- AddSlideTypeCols(config.pot.types.tb) #Add slide.type columns via inner-join
    config.pot.types.tb$color <- config.pot.types.tb$color %>% gsub("x","",.)  #Removing 'x' from colors #TODO: 
    
  #buildings.tb
    buildings.tb <- LowerCaseCharVars(buildings.tb) #Lower-case all content 
  
  #config.ans.opt.tb
    config.ans.opt.tb <- LowerCaseCharVars(config.ans.opt.tb)
    
  #RESPONSES (round 1)  
    resp1.tb <- LowerCaseNames(resp1.tb)  #Lower-case all variable names
    names(resp1.tb) <- SubRepeatedCharWithSingleChar(string.vector = names(resp1.tb), char = ".")
    resp1.tb <- LowerCaseCharVars(resp1.tb) #Lower-case all data
    data.from.qualtrics.logical <- #Define whether data coming in through Qualtrics or SurveyGizmo
      ifelse(names(resp1.tb)[1] == "startdate", TRUE, FALSE)
    
  #q.branched.tb (round 1)
    q.branched.tb <- LowerCaseNames(questions.tb)    #Lower-case all variable names
    names(q.branched.tb) <- SubRepeatedCharWithSingleChar(string.vector = names(q.branched.tb), char = ".") #Replace any number of repeated periods with a single period
    q.branched.tb <- LowerCaseCharVars(q.branched.tb)  #Lower-case all content
    
    q.branched.tb <- #Restrict to rows for questions for this year/semester 
      q.branched.tb[ 
        as.character(q.branched.tb$year) == data.year & #year
        tolower(q.branched.tb$semester) == data.semester #semester
        ,]
   
    #SURVEYGIZMO-SPECIFIC
      #Set up variable 'raw.var.id' so can replace response variable names with short names
      q.branched.tb$raw.var.id <- SubRepeatedCharWithSingleChar(string.vector = q.branched.tb$raw.var.id, char = ".")  
    
    #QUALTRICS: add 'x' to questions so match export exactly
      #TODO: couldn't just remove it from column names? Wouldn't that be more efficient?
    
  #RESPONSES (round 2)
    
    #QUALTRICS-SPECIFIC: 
      #Remove extra header rows
        resp2.tb <- 
          RemoveExtraHeaderRowsBasedOnStartChar(
            tb = resp1.tb, 
            header.rownum = 1,
            search.colname = names(resp1.tb)[1],
            search.char = "{"
          )
      
      #TODO:Slider vars:
        #convert to integer (based on min/max, e.g. cutoff points at 1.5, 2.5, 3.5)
        #convert to text 
        #convert to binary
        
    #SURVEYGIZMO-SPECIFIC: 
      #Replace names of resp1.df with short names from q.branched.tb
        names(resp2.tb) <- 
          IndexMatchToVectorFromTibble(
            vector = names(resp2.tb),
            lookup.tb = q.branched.tb,
            match.colname = "raw.var.id",
            replacement.vals.colname = "var.id",
            mult.replacements.per.cell = FALSE
          )

    #BOTH DATA SOURCES
      #Add id column which is either unique district name or unique building_district combo &
        #filter out rows with nothing in columns necessary to define unit.id 
        #(e.g. district and building)
        
        resp3.tb <- CreateUnitIDCol(
          tb = resp2.tb,
          id.unit = report.unit,
          remove.blanks = "ANY.MISSING",
          paste.char = "_"
        )
      
      #Filter out district office & blank schools
        resp4.tb <- 
          resp3.tb %>% 
          filter(!grepl("district office", building)) %>%
          filter(building != "")
        
      #TODO:Filter out test responses
          
      #Restrict rows 
        #Data to sample of user-defined size if doing sample print
         resp5.tb <- RestrictDataToSample(
            tb = resp4.tb,
            report.unit = report.unit,
            sample.print = sample.print,
            sample.group.unit = sample.group.unit,
            sample.size = sample.size
          )

      #Restrict columns to those necessary in final data
        necessary.colnames <- 
          q.branched.tb %>% 
          filter(tolower(necessary.in.final.data) == "yes") %>%
          select(var.id) %>%
          unlist %>%
          RemoveNA %>% 
          c(., "unit.id")
         
         resp6.tb <-
           SelectColsIn(resp5.tb, "IN", necessary.colnames)
          
      #Rearrange columns
        cwis.varnames.branched <- #CWIS response variables to right, all others first
          names(resp6.tb)[names(resp6.tb) %in% q.branched.tb$var.id[!is.na(q.branched.tb$module)]]
         
        resp7.tb <-
          resp6.tb[ ,
                   c(
                     which(!(names(resp6.tb) %in% cwis.varnames.branched)),
                     which((names(resp6.tb) %in% cwis.varnames.branched))
                   )
          ]
        
        resp8.tb <-  #'resp.id' and 'id' in first column 
          resp7.tb[ ,
            c(
              grep("resp.id|unit.id", names(resp7.tb)),
              which(!grepl("resp.id|unit.id", names(resp7.tb)))
            )
          ]
      
      #Add variables for: building.id, school.level
        resp8.tb$building.id <- paste(resp8.tb$district, "_", resp8.tb$building, sep = "")
        
        resp9.tb <-
          left_join(
            resp8.tb,
            buildings.tb %>% select(building.id,building.level),
            by = c("building.id")
          )
        
      #Unbranch columns
         
        if(q.branched.tb$unbranched.var.id %>% is.na %>% all){
          
          resp10.tb <- resp9.tb
          q.unbranched.tb <- q.branched.tb
          cwis.varnames.unbranched <- cwis.varnames.branched
          
        }else{
          
          #Response data
            resp10.tb <- 
              Unbranch(
                data.tb = resp9.tb,
                data.id.varname = "resp.id",
                var.guide.tb = q.branched.tb,
                current.names.colname = "var.id",
                unbranched.names.colname = "unbranched.var.id"
              ) %>% .[[1]] 
            
          #Unbranched Questions table  
            q.unbranched.tb <- 
              Unbranch(
                data.tb = resp9.tb,
                data.id.varname = "resp.id",
                var.guide.tb = q.branched.tb,
                current.names.colname = "var.id",
                unbranched.names.colname = "unbranched.var.id"
              ) %>% .[[2]] %>%
              filter(., necessary.in.final.data == "yes") #filter to questions necessary to final data
          
            cwis.varnames.unbranched <- 
              q.unbranched.tb$var.id[!is.na(q.unbranched.tb$module)]
        }
          
      #Data type conversions for CWIS vars
        #Text vars (freq & agreement): 
          #Preserve text; add numbers (e.g. "Always" becomes "1. Always")
            #TODO: Turn this into a function that applies a concatenation according to a lookup table
            #recode.varnames <- 
            #  q.unbranched.tb$var.id[grepl("frequency|agreement",q.unbranched.tb$scale.type)]
            
            
        #Recode for CWIS 
          #1. add numbers to text > store; 
          #2. convert text to integers > store; 
          #3. compile non-cwis, cwis text, & cwis integer vars into one tibble
          #TODO: turn into custom CWIS function
            
          recode.addnums.tb <- #add numbers to text variables
            RecodeIndexMatch(
              tb = SelectColsIn(resp10.tb, "IN", c("resp.id", cwis.varnames.unbranched)),
              lookup.tb = config.ans.opt.tb,
              match.colname = "ans.text.freq",
              replacement.vals.colname = "ans.text.freq.num",
              na.replacement = ""
            ) %>%
            RecodeIndexMatch(
              tb = .,
              lookup.tb = config.ans.opt.tb,
              match.colname = "ans.text.agreement",
              replacement.vals.colname = "ans.text.agreement.num",
              na.replacement = ""
            )
      
          recode.num.tb <- #convert to integer
            RecodeIndexMatch(
              tb = recode.addnums.tb,
              lookup.tb = config.ans.opt.tb,
              match.colname = "ans.text.freq.num",
              replacement.vals.colname = "ans.num"
            ) %>%
            RecodeIndexMatch(
              tb = .,
              lookup.tb = config.ans.opt.tb,
              match.colname = "ans.text.agreement.num",
              replacement.vals.colname = "ans.num"
            ) 
          
          names(recode.num.tb)[!(names(recode.num.tb) %in% "resp.id")] <-
            paste(SelectNamesIn(recode.num.tb, "NOT.IN", "resp.id"), "_num", sep = "")
            
          #TODO: convert to binary - move to table formation?
          #TODO: 1. use external config.ans.opt to allow adding new scales
          #TODO: 2. create standard mapping function to convert any number of integer choices to any standard
              #number of allowed answer options.
          
        #Complete Questions Table for Long Data (adding "_num" vars)
          q.long1.tb <- q.unbranched.tb[q.unbranched.tb$var.id %in% cwis.varnames.unbranched,]
          q.long1.tb$var.id <- paste0(q.long1.tb$var.id, "_num")
          q.long.tb <- rbind(q.unbranched.tb, q.long1.tb) %>% as_tibble()
          
      #Synthesize final response data tables 
        
        #Wide table for export
          resp.wide.tb <- 
            left_join(
              SelectColsIn(resp10.tb, "NOT.IN", cwis.varnames.unbranched),
              recode.addnums.tb,
              by = "resp.id"
            ) %>%
            left_join(
              ., 
              recode.num.tb,
              by = "resp.id"
            )
        
        #Long table
          resp.long1.tb <- 
            melt( #reshape cwis vars (numeric and text) from wide to long
              data = resp.wide.tb,
              id.vars = 
                SelectNamesIn(
                  resp.wide.tb,
                  "NOT.IN", 
                  c(cwis.varnames.unbranched, paste0(cwis.varnames.unbranched, "_num"))
                ),
              variable.name = "question",
              value.name = "answer",
              stringsAsFactors = FALSE,
              na.rm = TRUE
            ) %>%
            .[,names(.) %in% #filter out columns unnecessary for analysis 
              c(
                "question",
                "answer",
                "unit.id",
                "building.id",
                "building.level",
                q.unbranched.tb %>% 
                  filter(q.unbranched.tb$necessary.for.reports == "yes") %>% select(var.id) %>% unlist
              )
            ] %>%
            as_tibble()
                 
          #Add module and practice variables for looping 
            #TODO: need to abstract? Just for module variable or are there others?
            
            resp.long.tb <-
              left_join(
                resp.long1.tb,
                q.long.tb %>% select(var.id, module, practice),
                by = c("question" = "var.id")
              )
            
          #Split Col Reshape on Module column so don't have "cfa,etlp" values
            resp.long.tb <-
              SplitColReshape.ToLong(
                df = resp.long.tb,
                id.varname = "resp.id",
                split.varname = "module",
                split.char = ","
              ) 
            
          #Convert answer column to numeric
            resp.long.tb$answer <- resp.long.tb$answer %>% substr(., 1,1) %>% as.numeric()
            
          #Filter out rows with no answer
            resp.long.tb <- 
              resp.long.tb %>% 
              filter(!is.na(answer)) %>% 
              filter(answer != "") %>% 
              as_tibble
            
          #Add answer.id variable to be able to distinguish each row
            resp.long.tb$answer.id <-
              1:nrow(resp.long.tb)
              
          #Create variable to permit pivoting comparing one district with the rest
            #isolate.district.name
            #resp.long.tb$special.district <-
            #  ifelse(grepl(isolate.district.name,resp.long.tb$district),isolate.district.name,"other")
              
              
        #Establish Outputs Directory
          if(sample.print){
            outputs.dir <- 
              paste(
                working.dir,
                "\\4_outputs\\",
                gsub(":",".",Sys.time()), 
                sep = ""
              )
          }else{
            outputs.dir <- 
              paste(
                working.dir,
                "\\4_outputs\\",
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
          
        #Write Wide Data to CSV File
          widedata.file.name <- 
            paste( 
              "widedata_",
              gsub(":",".",Sys.time()),
              ".csv", 
              sep=""
            )
          
          write.csv(
            resp.wide.tb,
            file = widedata.file.name,
            row.names = FALSE
          )
          
        #Write Long Data to CSV File
          longdata.file.name <- 
            paste( 
              "longdata_",
              gsub(":",".",Sys.time()),
              ".csv", 
              sep=""
            )
          
          write.csv(
            resp.long.tb,
            file = longdata.file.name,
            row.names = FALSE
          )
          
  #Section Clocking
    section2.duration <- Sys.time() - section2.starttime
    section2.duration
    Sys.time() - sections.all.starttime

# 2-CLEANING OUTPUTS ------------------          
  #config.ans.opt.tb
  #config.global.tb
  #config.slide.types.tb
  #config.graph.types.tb
  #config.table.types.tb
  #config.pot.types.tb
  #config.ans.opt.tb
  #buildings.tb
  #resp.wide.tb
  #resp.long.tb
  
  #NO CHANGES
    #config.ans.opt.tb
    #config.global.tb
    #config.slide.types.tb
    
# 3-CONFIGS (SLIDE, GRAPH, AND TABLE CONFIG TABLES) ------------------
  
  #Section Clocking
    section3.startime <- Sys.time()
          
  #Load Configs Functions
    setwd(rproj.dir)
    source("3-configs_functions.r")
   
  #Expand Config Tables for each district according to looping variables
  
  ###                          ###    
# ### LOOP "b" BY REPORT.UNIT  ###
  ###                          ###
  
  #Loop Inputs
    unit.ids.sample <- resp.wide.tb$unit.id %>% unique 
    
  #Loop Outputs 
    config.graphs.ls.b <- list()
    config.tables.ls.b <- list()
    config.slides.ls.b <- list()
  
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
      if(b == 1){print("FORMING SLIDE, GRAPH, AND TABLE CONFIG TABLES...")}
      #print(
      #  paste(
      #    "Loop num: ", b,", Report id: ",unit.id.b,
      #    ", Pct. complete:", round(100*b/length(unit.ids.sample), 2), "%"
      #  )
      #)
    
    #Create data frames for this loop - restrict to district id i  
      resp.long.tb.b <- 
        resp.long.tb %>% filter(unit.id == unit.id.b)
    
    #Graphs config table for this report unit
      if(nrow(config.graph.types.tb) == 0){
        config.graphs.ls.b[[b]] <- NA
      }else{
        config.graphs.ls.b[[b]] <- 
          loop.expander.fun(
            configs = config.graph.types.tb, 
            loop.varnames = c("slide.loop.var.1","slide.loop.var.2","slide.loop.var.3"), 
            manual.order.varnames = c("slide.order.1","slide.order.2","slide.order.3"),
            collate.varnames = c("slide.section.1","slide.section.2","slide.section.3"),
            source.data = resp.long.tb.b
          )
      }
      #config.graphs.ls.b[[b]] <- remove.district.office.fun(config.graphs.df)
    
    #Tables config table for this report unit
      if(nrow(config.table.types.tb) == 0){
        config.tables.ls.b[[b]] <- NA
      }else{
        config.tables.ls.b[[b]] <-
          loop.expander.fun(
            configs =  config.table.types.tb, 
            loop.varnames = c("slide.loop.var.1","slide.loop.var.2","slide.loop.var.3"), 
            manual.order.varnames = c("slide.order.1","slide.order.2","slide.order.3"),
            collate.varnames = c("slide.section.1","slide.section.2","slide.section.3"),
            source.data = resp.long.tb.b
          )
      }
    
    #Slide config table for this report unit
      config.slides.ls.b[[b]] <- 
        loop.expander.fun(
          configs = config.slide.types.tb,
          loop.varnames = c("slide.loop.var.1","slide.loop.var.2","slide.loop.var.3"), 
          manual.order.varnames = c("slide.order.1","slide.order.2","slide.order.3"),
          collate.varnames = c("slide.section.1","slide.section.2","slide.section.3"),
          source.data = resp.long.tb.b
        )
    
      #config.slides.ls.b[[b]] <- remove.district.office.fun(config.slides.df)
    
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

# 3-CONFIGS (SLIDE, GRAPH, AND TABLE CONFIG TABLES) OUTPUTS ------------------
  #unit.ids.sample: vector with all report unit names in resp.long.tb (length = 19 for baseline data)
  #config.slides.ls.b
    #[[report.unit]]
      #data frame where each line represents a slide
  #config.tables.ls.b
    #[[report.unit]]
      #data frame where each line represents a table
  #config.graphs.ls.b
    #[[report.unit]]
      #data frame where each line represents a graph


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
    graphdata.ls.c <- list()
    tabledata.ls.c <- list()
  
  #Loop Measurement - progress bar & timing
    progress.bar.c <- txtProgressBar(min = 0, max = 100, style = 3)
    maxrow.c <- config.graphs.ls.b %>% sapply(., dim) %>% sapply(`[[`,1) %>% unlist %>% sum
    c.loop.startime <- Sys.time()
    
  #c <- 1 #LOOP TESTER 
  #for(c in slider.unit.ids.sample){   #LOOP TESTER
  for(c in 1:length(unit.ids.sample)){   #START OF LOOP BY DISTRICT
    
    #Loop Inputs (both graphs and tables)
      unit.id.c <- unit.ids.sample[c]
      district.c <- 
        resp.long.tb %>% 
        filter(unit.id == unit.id.c) %>% 
        select(district) %>% 
        unique %>% 
        unlist %>% 
        RemoveNA()
      
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
#   ### LOOP "d" BY GRAPH  ###
    ###                    ###
    
    #Loop Inputs
      config.graphs.df.c <- config.graphs.ls.b[[c]]
      graphdata.ls.d <- list()
    
    #d <- which(config.graphs.df.c$graph.type.id == "i")[1]
    #for(d in 1:2){ #LOOP TESTER
    for(d in 1:nrow(config.graphs.df.c)){
      
      #Print loop messages
        #print(
        #  paste(
        #    "LOOP 'D' GRAPH -- Loop num: ", d,
        #    ", Pct. complete:", round(100*d/dim(config.graphs.df.c)[1], 2), "%"
        #  )
        #)
      
      config.graphs.df.d <- config.graphs.df.c[d,]
      
      group_by.d <- config.graphs.df.d$x.varnames %>% 
        strsplit(., ",") %>% 
        unlist
      
      #Form Graph Data Table
        graphdata.ls <-
          list(
            axis.cat.labels =  #Define category names that will go along axis of bar graph - module, practice
              DefineAxisCategories(
                tb = resp.long.tb,
                cat.colnames = 
                  config.graphs.df.d %>% select("x.varnames") %>% unlist %>% as.character %>%
                  strsplit(., ",") %>% unlist %>% as.character
              ),
            graphtable.d =  #Data table with measurements
              GraphDataRestriction(
                tb =  resp.long.tb.c,
                id.varname = "answer.id",
                tb.config = config.graphs.df.d
              ) %>%
              SummarizeDataByGroups(
                tb = .,
                group.varnames = group_by.d,
                summarize.varname = config.graphs.df.d$summarize.varname %>% unlist %>% as.vector,
                summarize.fun = config.graphs.df.d$summarize.fun %>% unlist %>% as.vector
              ),
            graph.avgs.d = #Averages (using average.level from configs)
              GroupedAveragesByLevel(
                tb = resp.long.tb,
                group.varnames = group_by.d,
                summarize.varname = config.graphs.df.d$summarize.varname %>% unlist %>% as.vector,
                summarize.fun = config.graphs.df.d$summarize.fun %>% unlist %>% as.vector,
                avg.level = config.graphs.df.d$avg.level,
                tb.restriction.value = district.c
              )
          )
        
        graphdata.df.d <- 
          Reduce(function(x,y){left_join(x,y, by = group_by.d)}, graphdata.ls) %>%
          ReplaceNames(
            df = ., 
            current.names = names(.), 
            new.names = c(names(.)[1:(length(names(.))-2)],"measure","avg")
          )
      
      #Store Results  
        graphdata.ls.d[[d]] <- graphdata.df.d
        names(graphdata.ls.d)[[d]] <- config.graphs.df.d$graph.type.name
      
      #Progress Bar
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
    
    if(is.na(config.tables.df.c)){
      
    }else{
      
      #d <- 2
      #for(d in 1:2){ #LOOP TESTER
      for(d in 1:dim(config.tables.df.c)[1]){
        
        #Print loop messages
          #print(
          #  paste(
          #    "LOOP 'D' TABLE -- Loop num: ", d,
          #    ", Pct. complete:", round(100*d/dim(config.tables.df.c)[1], 2), "%"
          #  )
          #)
        
        config.tables.df.d <- config.tables.df.c[d,]
        
        #Form final data frame
          tabledata.df.d <-  
            resp.long.tb.c %>%
            table.data.filter.fun(dat = ., config.input = config.tables.df.d) %>%
            summarize.table.fun(dat = ., config.input = config.tables.df.d) %>%
            FirstTableOperations(tb = ., iterations = c(1))
        
        tabledata.ls.d[[d]] <- tabledata.df.d
        
      } ### END OF LOOP "d" BY TABLE ###
      
      names(tabledata.ls.d) <- config.tables.df.c$module %>% RemoveNA() %>% as.character %>% c("role",.)
      tabledata.ls.c[[c]] <- tabledata.ls.d
    
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
  #tabledata.ls.c
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
    tables.ls.f <- list()
  
  f <- 1 #LOOP TESTER
  #for(f in 1:2){ #LOOP TESTER
  #for(f in 1:length(unit.ids.sample)){
    
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
    
    g <- 1 #LOOP TESTER
    #for(g in 1:2) #LOOP TESTER
    #for(g in 1:length(graphdata.ls.f))
      #local({ #Necessary to avoid annoying and confusing ggplot lazy evaluation problem (see journal)
        
        #Redefine necessary objects in local environment
          g<-g #same as above
        
        #GRAPH DATA & CONFIGS DATA FRAMES
          
          #Select tables from lists produced in previous sections
            graphdata.df.g <- graphdata.ls.f[[g]] %>% as.data.frame()
            config.graphs.df.g <- config.graphs.df.f[g,] %>% as.data.frame()
          
          #Print loop messages for bug checking
            #print(
            #  paste0(
            #    "LOOP 'g' -- Loop num: ", g,
            #    ", Pct. complete:", round(100*g/length(graphdata.ls.f), 2), "%"
            #  )
            #)
            #cprint(graphdata.df.g)
            #print(config.graphs.df.g)
        
        #CLEANING DATA & CONFIGS
            
          #Making new [shortened] objects that will get a lot of use in graph formation; 
            graph.group.by.varnames <- 
              names(graphdata.df.g)[1:(which(names(graphdata.df.g) == "measure")-1)]
            
          #Capitalize headers in graphdata.df.g, all-caps for module, upper-case first letter for everything else
          
            if(names(graphdata.df.g)[1] %>% grepl("module",.)){
              graphdata.df.g[,1:(which(names(graphdata.df.g) == "measure")-1)] <- 
                graphdata.df.g[,1:(which(names(graphdata.df.g) == "measure")-1)] %>% 
                apply(., c(1:2), toupper)
            }else{
              graphdata.df.g[,1:(which(names(graphdata.df.g) == "measure")-1)] <- 
                graphdata.df.g[,1:(which(names(graphdata.df.g) == "measure")-1)] %>% 
                apply(., c(1:2), FirstLetterCap_MultElements)
            }
          
          #Inserting corrected scale for graphs that have Answer Options along the bottom
            if(!is.null(graph.group.by.varnames) && "answer" %in% graph.group.by.varnames){
              graphdata.df.g <- left_join(graphdata.df.g,config.ans.opt.tb, by = c("answer" = "ans.num"))#graphdata.df.g[order(graphdata.df.g[,2]),]
              
              if(config.graphs.df.g$module %in% c("LEAD","PD")){
                graphdata.df.g <- 
                  graphdata.df.g %>% 
                  select(graph.group.by.varnames, ans.text.agreement, measure, avg)
                graph.cat.varname <- "ans.text.agreement"
              }else{
                graphdata.df.g <- 
                  graphdata.df.g %>% 
                  select(graph.group.by.varnames, ans.text.agreement, measure, avg)
                graph.cat.varname <- "ans.text.freq"
              }
            }
        
        ### BASE GRAPH FORMATION WITH GGPLOT2 ###
        
          #Base Graph
            graph.1 <- 
              FormBaseGraphObject.DataAndTheme( 
                dat = graphdata.df.g 
              )

          #Adding Columns (Clustered or Non-Clustered)
            #Define Fill Values
              if(strsplit(config.graphs.df.g$graph.fill, ",") %>% unlist %>% trimws %>% length %>% equals(1)){
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
                  graph.group.by.varnames = graph.group.by.varnames,
                  graph.fill = graph.fill.g,
                  print.graph = FALSE
                )
        
          #Add data labels 
          
            #Graph label data frame
              graph.labels.df <- 
                create.graph.labels.fun(
                  dat = graphdata.df.g, 
                  dat.measure.varname = "measure", 
                  height.ratio.threshold = 8.2,
                  dat.configs = config.graphs.df.g
                )
          
            #Add Data labels to graph
              graph.3 <-
                AddGraphDataLabels(
                  base.graph.input = graph.2,
                  dat = graphdata.df.g,
                  dat.labels = graph.labels.df,
                  label.font.size = 4,
                  print.graph = FALSE
                )  
            
          #Add Graph Averages (as error bar)
            #NOTE: does not depend on config.graphs.df.g - taken care of with if statement outside function
            if(!is.na(config.graphs.df.g$avg.level)){
              graph.4 <-
                AddGraphAverages(
                  base.graph.input = graph.3,
                  dat = graphdata.df.g,
                  avg.bar.color = config.graphs.df.g$avg.bar.color,
                  dat.configs = config.graphs.df.g,
                  print.graph = FALSE 
                )
            }else{
              graph.4 <- graph.3
            }
          
          #Final graph formatting & edits: correct category order, finalize orientation as column or bar
            #NOTE: depends on config.graphs.df.g (used within function to decide whether to flip to bar)
            graph.g <- 
              FinalGraphFormatting(
                base.graph.input = graph.4,
                dat = graphdata.df.g,
                dat.configs = config.graphs.df.g,
                print.graph = FALSE
              )
              
        graphs.ls.g[[g]] <<- graph.g
       
        
      })  ### END OF LOOP "g" BY GRAPH ###

      graphs.ls.f[[f]] <- graphs.ls.g
    
    ###                       ###    
#   ### LOOP "g" BY TABLE     ###
    ###                       ###
    
    #Loop output object(s)
      tables.ls.g <- list()
    
    #g <- 1 #LOOP TESTER
    #for(g in 1:2){ #LOOP TESTER
    for(g in 1:length(tabledata.ls.c[[f]])){
      
      #Prep Loop Inputs
        if(dim(tabledata.ls.c[[f]][[g]])[1] == 0){
          tabledata.ls.c[[f]][[g]][1,] <- rep(0, dim(tabledata.ls.c[[f]][[g]])[2]) 
        }
        
        tabledata.df.g <- tabledata.ls.c[[f]][[g]]
        config.tables.df.g <- config.tables.df.c[g,]
    
      #Print loop messages for bug checking
        #print(
        #  paste0(
        #    "LOOP 'g' -- Loop num: ", g,
        #    ", Pct. complete:", round(100*g/length(tabledata.ls.c[[f]]), 2), "%"
        #  )
        #)
        #print(tabledata.df.g)
        #print(config.tables.df.g)
        
      #Create FlexTable Object
         ft.g <- FlexTable(
          data = tabledata.df.g,
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
          ft.g[dim(tabledata.df.g)[1],] <- 
            chprop(
              textProperties(
                font.weight = "bold",
                font.size = 18,
                font.family = "Century Gothic"
              )
            ) #Bold text on last line (totals)
          ft.g[,1] <- chprop(parProperties(text.align = "center"))
          #ft.g <- setFlexTableWidths(ft.g, widths = c(4, rep(6,dim(tabledata.df.g)[2]-1)))      
          
        }
        
        if(g != 1){
          ft.g[,1] <- chprop(parProperties(text.align = "right"))
        }
        
        ft.g[1:dim(tabledata.df.g)[1],2:dim(tabledata.df.g)[2]] <- #Center align numbers in all but first column
          chprop(parProperties(text.align = "center")) 
        ft.g <- setZebraStyle(ft.g, odd = "#D0ABD6", even = "white" ) 
        
        tables.ls.g[[g]] <- ft.g
        
    } ### END OF LOOP "g" BY TABLE ###
  
    names(tables.ls.g) <- c("role","etlp","cfa","dbdm","pd","lead") #TODO:WAS CAUSING PROBLEMS WITH ORDERING OF TABLES ON SLIDES BECAUSE HAD NOT BEEN UPDATED TO NEW ORDER OF MODULES
    tables.ls.f[[f]] <- tables.ls.g
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


# 6-POWERPOINTS & EXPORT -----------------------------------------------
  
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
  
  #h <- 10 #LOOP TESTER
  #for(h in ceiling(runif(5,1,length(config.slides.ls.b)))){
  for(h in 1:length(config.slides.ls.b)){ #LOOP TESTER
    
    #Reading 'Cadre' so it can be added to file name
      cadre.h <- 
        buildings.tb %>% 
        filter(report.id == unit.ids.sample[h]) %>% 
        select(cadre) %>% 
        unlist %>% 
        FirstLetterCap_OneElement()
    
    #Set up target file
      template.file <- paste(source.tables.dir,
                             "template_green reports.pptx",
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
            #cadre.h,
            #"_",
            unit.ids.sample[h],
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
      unit.id.h <- unit.ids.sample[h]
      district.h <- strsplit(unit.id.h, "_") %>% unlist %>% .[1] %>% toupper()
      school.h <- strsplit(unit.id.h, "_") %>% unlist %>% .[2] %>% toupper()
      config.slides.df.h <- config.slides.ls.b[[h]]
      
      graphs.ls.h <- graphs.ls.f[[h]]
      tables.ls.h <- tables.ls.f[[h]]
    
    ###                     ###    
#   ### LOOP "i" BY SLIDE   ###
    ###                     ###
    
    #i <- 2 #LOOP TESTER
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
        
        config.pot.i <- config.pot.types.tb[config.pot.types.tb$slide.type.id == slide.type.id.i,]
        
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


