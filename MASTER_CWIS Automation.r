#########################################################
##### 	CWIS Automation for MMD                   	#####
#########################################################


# 0-SETUP -----------------------------------------------------------
  
  #INITIAL SETUP
    rm(list=ls()) #Remove lists
    options(java.parameters = "- Xmx8g") #helps r not to fail when importing large xlsx files with xlsx package
    
    
    #Record code start time for processing time calculations
      start_time <- Sys.time()
  
  # ESTABLISH BASE DIRECTORIES
  
    #M900
      working.dir <- "C:/Users/willi/Google Drive/1. FLUX CONTRACTS - CURRENT/2016-09 EXT Missouri Education/3. Missouri Education - GDRIVE/8. CWIS/2018-12 Green Reports Phase 6/"
      rproj.dir <- "C:/Users/willi/Documents/GIT PROJECTS/CWIS-automation/"
      
    #Thinkpad T470
      #working.dir <- "G:/My Drive/1. FLUX CONTRACTS - CURRENT/2016-09 EXT Missouri Education/3. Missouri Education - GDRIVE/8. CWIS/2018-12 Green Reports Phase 6/"
      #rproj.dir <- "C:/Users/WNF/Documents/Git Projects/CWIS-automation"
    
    #Source Code Directory
      rproj.dir <- rproj.dir #paste(rproj.dir,"2_source_code/",sep="") #Changed back to using 'Documents' folder after attempting to move project into Google Drive but running into problems
    
    #Source Resources Director (raw data)
      source.resources.dir <- paste(working.dir,"3_source_resources/", sep = "")
    
    #Source Inputs (configs)
      source.inputs.dir <- paste(working.dir,"4_source_inputs/",sep="")
  
  # LOAD SOURCE CODE
      
    setwd(rproj.dir)
    source("utils_wnf.r")
    source("CWIS_custom.r")
    
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
    library(proftools)
    library(jsonlite)
    library(rlang)
    library(extrafont)
    extrafont::loadfonts(device="win")
    
# 0-SETUP OUTPUTS -----------------------------------------------------------
  #start_time: sys.time for code
  #working.dir: working directory - Google Drive folder "2018-08 Green Reports"
  #rproj.dir: directory for R project; also contains source data, additional function scripts, and config tables.
  #source.resources.dir: directory with raw data
  #source.inputs.dir: directory with config tables and powerpoint template

# 1-IMPORT -----------------------------------------
  
  #Source Import Functions
    import.source.dir <- paste(rproj.dir,"1-Import/", sep = "")
    setwd(import.source.dir)
    source("import_functions.r")
  
  #Import Config Tables
    configs.ss <- gs_key("1ku_OC9W87ut6W1qrdpFeYBlWlPN5X4fGHJ3h1k0HrOA",verbose = TRUE) 
    
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
    
    setwd(source.resources.dir)
    
    resp1.tb <- read.csv(
      file =  
        MostRecentlyModifiedFilename(
          title.string.match = "CWIS",
          file.type = "csv",
          dir = source.resources.dir
        ),
      stringsAsFactors = FALSE,
      header = TRUE
    ) %>% as_tibble(.)

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
  #q.branched.tb
  #resp1.tb (initial responses dataset which will need extensive cleaning and organization in next sections)


# 2-CLEANING --------
    
  #Source Cleaning Functions
    cleaning.source.dir <- paste(rproj.dir,"2-Cleaning/", sep = "")
    setwd(cleaning.source.dir)
    source("cleaning_functions.r")
  
  #config.graph.types.tb
    config.graph.types.tb <- AddSlideTypeCols(config.graph.types.tb) #Add slide.type columns via inner-join
    
  #config.table.types.tb
    config.table.types.tb <- AddSlideTypeCols(config.table.types.tb) #Add slide.type columns via inner-join
    
  #config.pot.tb
    config.pot.types.tb <- AddSlideTypeCols(config.pot.types.tb) #Add slide.type columns via inner-join
    config.pot.types.tb$color <- config.pot.types.tb$color %>% gsub("x","",.)  #Removing 'x' from colors
    
  #buildings.tb
    #TODO:Correct misspelling 'bucahanan' to 'buchanan'
    buildings.tb <- LowerCaseCharVars(buildings.tb)#Lower-case all content
  
  #config.ans.opt.tb
    config.ans.opt.tb <- LowerCaseCharVars(config.ans.opt.tb)
    
  #RESPONSES (round 1)  
    resp1.tb <- LowerCaseNames(resp1.tb)  #Lower-case all variable names
    names(resp1.tb) <- SubRepeatedCharWithSingleChar(string.vector = names(resp1.tb), char = ".")
    resp1.tb <- LowerCaseCharVars(resp1.tb) #Lower-case all data
    data.from.qualtrics.logical <- #Define whether data coming in through Qualtrics or SurveyGizmo
      ifelse(names(resp1.tb)[1] == "startdate", TRUE, FALSE)
    
  #q.branched.tb (round 1)
    q.branched.tb <- #Restrict to rows for questions for this year/semester 
      questions.tb[ 
        as.character(questions.tb$year) == data.year & #year
        tolower(questions.tb$semester) == data.semester #semester
        ,]
    q.branched.tb <- LowerCaseNames(q.branched.tb)    #Lower-case all variable names
    names(q.branched.tb) <- SubRepeatedCharWithSingleChar(string.vector = names(q.branched.tb), char = ".") #Replace any number of repeated periods with a single period
    q.branched.tb <- LowerCaseCharVars(q.branched.tb)  #Lower-case all content
   
    
    #SURVEYGIZMO-SPECIFIC
      #Set up variable 'row.1' so can replace response variable names with short names
      q.branched.tb$row.1 <- SubRepeatedCharWithSingleChar(string.vector = q.branched.tb$row.1, char = ".")  
    
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
      
      #TODO:Rename important variables with names that make sense
      
      #TODO:Slider vars:
        #convert to integer (based on min/max, e.g. cutoff points at 1.5, 2.5, 3.5)
        #convert to text 
        #convert to binary
        
    #SURVEYGIZMO-SPECIFIC: 
      #Replace names of resp1.df with short names from q.branched.tb
        names(resp2.tb) <- IndexMatchToVectorFromTibble(
          vector = names(resp2.tb),
          lookup.tb = q.branched.tb,
          match.colname = "row.1",
          replacement.vals.colname = "var.id",
          mult.replacements.per.cell = FALSE
        )

    #BOTH DATA SOURCES
      #Add id column which is either unique district name or unique building_district combo &
      #Filter out rows with nothing in columns necessary to define report.id (e.g. district and building) 
        resp3.tb <- CreateUnitIDCol(
          tb = resp2.tb,
          id.unit = report.unit,
          additional.colnames = c("district"),
          remove.blanks = "ANY.MISSING",
          paste.char = "_"
        )
      
      #Filter out district office rows
        resp4.tb <- resp3.tb %>% filter(!grepl("district office", resp3.tb$id))
        
      #TODO:Filter out test responses
          
      #Restrict rows 
        #Data to sample of user-defined size if doing sample print
         resp5.tb <- RestrictDataToSample(
            tb = resp4.tb,
            report.unit = report.unit,
            sample.group.unit = sample.group.unit,
            sample.size = sample.size
          )

      #restrict columns 
        #Necessary in final data
        necessary.colnames <- 
           q.branched.tb %>% 
           filter(tolower(necessary.in.final.data) == "yes") %>%
           select(var.id) %>%
           unlist %>%
           RemoveNA
         
         resp6.tb <-
           SelectColsIn(resp5.tb, "IN", necessary.colnames)
          
      #Rearrange columns
        #TODO: Use 'SelectColIn' Function to rearrange columns according to names that meet a TRUE/FALSE condition
        cwis.varnames.branched <- #CWIS response variables to right, all others first
          names(resp6.tb)[names(resp6.tb) %in% q.branched.tb$var.id[!is.na(q.branched.tb$module)]]
         
        resp7.tb <-
          resp6.tb[ ,
                   c(
                     which(!(names(resp6.tb) %in% cwis.varnames.branched)),
                     which((names(resp6.tb) %in% cwis.varnames.branched))
                   )
          ]
        
        resp8.tb <-  #Response.id in first column 
          resp7.tb[ ,
            c(
              grep("resp.id|id", names(resp7.tb)),
              which(!grepl("resp.id|id", names(resp7.tb)))
            )
          ]
        
      #Unbranch columns
        
        #Response data
          resp9.tb <- 
            Unbranch(
              data.tb = resp8.tb,
              data.id.varname = "resp.id",
              var.guide.tb = q.branched.tb,
              current.names.colname = "var.id",
              unbranched.names.colname = "branch.master.var.id"
            ) %>% .[[1]] 
        
        #Questions table
          q.unbranched.tb <- 
            Unbranch(
              data.tb = resp8.tb,
              data.id.varname = "resp.id",
              var.guide.tb = q.branched.tb,
              current.names.colname = "var.id",
              unbranched.names.colname = "branch.master.var.id"
            ) %>% .[[2]] %>%
            filter(., necessary.in.final.data == "yes") #filter to questions necessary to final data

        cwis.varnames.unbranched <- 
          q.unbranched.tb$var.id[!is.na(q.unbranched.tb$module)]
          
      #Data type conversions for CWIS vars
        #Text vars (freq & agreement): 
          #Preserve text; add numbers (e.g. "Always" becomes "1. Always")
            #TODO: Turn this into a function that applies a concatenation according to a lookup table
            recode.varnames <- 
              q.unbranched.tb$var.id[grepl("frequency|agreement",q.unbranched.tb$scale.type)]
            
            
        #Recode for CWIS 
            #1. add numbers to text > store; 2. convert text to integers > store; 3. compile non-cwis, cwis text, & cwis integer vars into one tibble
            #TODO: turn into custom CWIS function
            recode.addnums.tb <- #add numbers to text variables
              RecodeIndexMatch(
                tb = SelectColsIn(resp9.tb, "IN", c("resp.id", recode.varnames)),
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
            #recode.binary.tb <- 

          #TODO: 1. use external config.ans.opt to allow adding new scales
          #TODO: 2. create standard mapping function to convert any number of integer choices to any standard
              #number of allowed answer options.
      
      #Create useful objects
        #Vectors for selecting CWIS answer variables
      
      #Synthesize final response data tables 
        #Wide table for export
          resp.wide.tb <- 
            left_join(
              SelectColsIn(resp9.tb, "NOT.IN", cwis.varnames.unbranched),
              recode.addnums.tb,
              by = "resp.id"
            ) %>%
            left_join(
              ., 
              recode.num.tb,
              by = "resp.id"
            )
        
        #Long table
          report.data.varnames <- q.unbranched.tb$var.id[q.unbranched.tb$necessary.for.reports == "yes"]
          resp.wide.report.tb <- resp.wide.tb[, names(resp.wide.tb) %in% report.data.varnames]
          resp.long.tb <- 
            melt(
              data = resp.wide.report.tb,
              id.vars = SelectNamesIn(resp.wide.report.tb,"NOT.IN", cwis.varnames.unbranched),
              variable.name = "question",
              value.name = "answer",
              stringsAsFactors = FALSE,
              na.rm = TRUE
            ) %>% as_tibble()
      
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
            resp.wide.tb,
            file = unbranched.file.name,
            row.names = FALSE
          )
          
    #q.branched.tb (round 2)
      #Re-do question table so no extraneous rows for roles that are now unbranched
    
    


  #NO CHANGES  
    #config.ans.opt.tb
    #config.global.tb
    #config.slide.types.tb
    
# 4-CONFIGS (SLIDE, GRAPH, AND TABLE CONFIG TABLES) ------------------
  
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
          configs = config.graph.types.tb, 
          loop.varname = c("slide.loop.var.1","slide.loop.var.2","slide.loop.var.3"), 
          collate.varname = "slide.section.1",
          source.data = resp.long.df.b
        )
      
      config.graphs.ls.b[[b]] <- remove.district.office.fun(config.graphs.df)
    
    #Tables config table for this report unit
      config.tables.df <-
        loop.expander.fun(
          configs =  config.table.types.tb, 
          loop.varname = c("slide.loop.var.1","slide.loop.var.2","slide.loop.var.3"), 
          collate.varname = "slide.section.1",
          source.data = resp.long.df.b
        )
      
      config.tables.ls.b[[b]] <- remove.district.office.fun(config.tables.df)
    
    #Slide config table for this report unit
      config.slides.df <- 
        loop.expander.fun(
          configs = config.slide.types.tb,
          loop.varnames = c("slide.loop.var.1","slide.loop.var.2","slide.loop.var.3"),
          collate.varname = "slide.section.1",
          source.data = resp.long.df.b  
        )
    
      config.slides.ls.b[[b]] <- remove.district.office.fun(config.slides.df)
    
    
    setTxtProgressBar(progress.bar.b, 100*b/maxrow.b)
    
  } # END OF LOOP 'b' BY REPORT.UNIT
  close(progress.bar.b)

# 4-CONFIGS (SLIDE, GRAPH, AND TABLE CONFIG TABLES) OUTPUTS ------------------
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


# 5-SOURCE DATA TABLES --------------------------------------------

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
      
      #Create data frame "all.cats.df.d" of all possible answers for x-axis (role, module, data.year, answer)
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

# 5-SOURCE DATA TABLES OUTPUTS --------------------------------------------
  #tabledata.ls.c
    #[[report.unit]]
    #data frame where each line represents a table
  #graphdata.ls.c
    #[[report unit]]
    #data frame where each line represents a graph



# 6-OBJECT CREATION (GRAPHS & TABLES) ------------------------------------

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
            graphdata.df.g <- left_join(graphdata.df.g,config.ans.opt.tb, by = c("answer" = "ans.num"))#graphdata.df.g[order(graphdata.df.g[,2]),]
            
            if(config.graphs.df.g$module %in% c("LEAD","PD")){
              graphdata.df.g <- graphdata.df.g %>% select(data.year, ans.text.agreement, measure.var, avg)
              graph.cat.varname <- "ans.text.agreement"
            }else{
              graphdata.df.g <- graphdata.df.g %>% select(data.year, ans.text.freq, measure.var, avg)
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
          #TODO: Need to make so can group on arbitrary variable with arbitrary number of groups and sub-groups. Right now can only two groups of 2 (e.g. data.year in Repeated Measures)
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
            data.year = c("Baseline","2017-18"),
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

# 6-OBJECT CREATION (GRAPHS & TABLES) OUTPUTS ------------------------------------
  
  #graphs.ls.f
    #[[report.unit]]
      #ggplot object
  #tables.ls.f
    #[[report.unit]]
      #FlexTable object


# 7-POWERPOINTS & EXPORT -----------------------------------------------
  
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




