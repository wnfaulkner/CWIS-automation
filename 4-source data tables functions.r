#############################################################
#####       4-source data tables functions              #####
#############################################################

source("utils_wnf.r")



# GRAPHS --------------------------  

#Define names of categories that will go along bottom of graph
  #Test Inputs
    #source.table = resp.long.tb
    #config.table = config.tables.df.d
    #config.varname = "x.varname"
  
  DefineAxisCategories <- function(
    source.table,
    config.table,
    config.varname
  ){
    cat.colname <- config.table %>% select(config.varname) %>% unlist %>% as.character
    
    if(is.na(cat.colname)){
      result <- NA
      warning(paste0("Config table column '", config.varname, "' is NA."))
    }
    
    if(!is.na(cat.colname)){
      
      if(cat.colname %in% "practice"){
        source.table <- source.table[grep(config.table$module, source.table$module),]
      }
      
      result <-
        UniqueVariableValues(
          varnames = cat.colname, 
          tb = source.table
        ) %>%
        strsplit(., ",") %>%
        unlist %>%
        unique %>%
        RemoveNA(.) %>%
        .[order(.)]
    }
    
    return(result)
  }

#Selecting variable names that will be used in graph/table calculations
    #TODO: add parameters for questions table (which one to use), names of variables in there to select
    #Test Inputs
      #config.input <- config.graphs.df.d
      #data.input <- resp.long.tb
    
    GraphVarnamesInData <- function(config.input, data.input){
      
      varname.i <- config.input %>% select(x.varname.1)
      
      if(is.na(varname.i)){
        all.cats.ls[[i]] <- ""
        next()
      }
      
      if(varname.i == "answer"){
        module.varnames <- 
          q.long.tb %>% 
          filter(module == config.input$module) %>% 
          select(var.id) %>% 
          unlist %>% 
          setdiff(., names(slider.vars.df))
        
        result <- 
          data.input$question %in% module.varnames %>% 
          data.input$answer[.] %>% 
          unique %>%
          .[.!=""] %>%
          RemoveNA() %>%
          .[.!=""]
      }
      
      if(varname.i == "module"){
        result <- "module"
      }
      
      if(varname.i == "practice"){
        result <- 
          q.long.tb %>% 
          filter(module == config.input$module) %>% 
          select(var.id) %>% 
          unique %>% 
          unlist %>%
          RemoveNA() %>%
          .[.!=""]
      }
      
      if(varname.i == "role"){
        result <- 
          data.input %>%
          select(varname.i) %>%
          unique %>% 
          unlist %>%
          RemoveNA() %>%
          .[.!=""]
      }
      
      #all.cats.ls[[i]] <- result %>% as.data.frame %>% ReplaceNames(df = ., current.names = ".", new.names = "all.cats")
      return(result)
    }  
  
  
  #Data restriction - district vs. unit.id
    #TODO:NEED TO GENEARALIZE: IF REPORT.UNIT IS DISTRICT AND GRAPH DATA.LEVEL IS DISTRICT, THIS WORKS, 
      #BUT NOT IF REPORT.UNIT IS unit.id AND DATA.LEVEL IS DISTRICT.
    
    GraphDataRestriction <- function(tb){
      
      if(config.graphs.df.d$data.level == "district"){
        y <- tb
      }
      
      if(config.graphs.df.d$data.level == "building"){
        y <- 
          tb %>% 
          filter(unit.id == unit.id.c) 
      }
      
      z <- y[grep("_num", y$question),]
      
      if(!is.na(config.graphs.df.d$slide.loop.var.1)){
        if(config.graphs.df.d$slide.loop.var.1 == "module"){
          z <- z %>% filter(module == config.graphs.df.d$module)
        }
      }
      
      if(is.na(config.graphs.df.d$data.restriction)){
        result <- z
      }
      
      if(!is.na(config.graphs.df.d$data.restriction)){
        result <- 
          z %>% 
          filter(
            z[,names(z) == config.graphs.df.d$data.restriction] == 
              config.graphs.df.d[,names(config.graphs.df.d) == config.graphs.df.d$data.restriction]
          )
      }
      
      return(result)
    }
  
  #Data Summarize GRAPHS - participation vs. implementation vs. performance 
    #Test inputs
    #config.input <- config.graphs.df.d
    #data.input <-  resp.long.tb.c %>% GraphDataRestriction %>% group_by(!!! syms(group_by.d))
    
    summarize.graph.fun <- function(config.input, data.input){
      
      data.input <- data.input[grep("_num", data.input$question),]
      
      if(config.input$data.measure == "participation"){
        result <- 
          dplyr::summarize(data.input, measure.var =  length(unique(resp.id)))
      }
      
      if(config.input$data.measure == "implementation"){
        result <- 
          data.input %>% 
          dplyr::summarize(measure.var = mean(as.numeric(answer), na.rm = TRUE)) %>%
          as.data.frame(., stringsAsFactors = FALSE)
      }
      
      if(config.input$data.measure == "performance"){
        result <- data.input %>%
          dplyr::summarize(measure.var = as.character(length(unique(resp.id))))
      }
      
      if(config.input$data.measure == "average performance"){
        result <- 
          data.input %>%
          filter(grepl("_num",question)) %>%
          dplyr::summarize(., measure.var =  mean(as.numeric(answer), na.rm = TRUE))
      }
      
      return(result)
    }
    
  #Restriction function for graph average data
    #TODO: THESE TWO FUNCTIONS ARE VERY SIMILAR TO THE ONES ABOVE WHICH HAVE BEEN CHANGED SO NOW NEED TO SPECIFY "config.input" BUT
      #HAVE NOT MADE THOSE CHANGES HERE YET. PROBABLY COULD ROLL UP INTO ONE OR TWO FUNCTIONS.
    
    #Test Inputs
      #x <- resp.long.tb
    
    avg.data.restriction.fun <- function(x){
      
      if(avg.level == "district"){
        result <- x %>% filter(district == district.c)
      }
      
      #if(avg.level == "building"){
      #  y <- 
      #    x %>% 
      #    filter(district == unique(resp.long.tb$district[resp.long.tb$unit.id == unit.id.c])) 
      #}
      
      #z <- y %>% filter(!is.na(y[,names(y)==group_by.d])) #TODO:Might want to make flexible - i.e. add a parameter which allows user to include NA
      
      #if(!config.graphs.df.d$data.restriction=="module" | is.na(config.graphs.df.d$data.restriction)){ 
        #TODO:Should look into a better way to deal with this restriction, think about input tables
      #  result <- z
      #}
      
      #if(config.graphs.df.d$data.restriction=="module" & !is.na(config.graphs.df.d$data.restriction)){
      #  result <- 
      #    z %>%
      #    filter(
      #      z[,names(z)==config.graphs.df.d$data.restriction] == 
      #        config.graphs.df.d[,names(config.graphs.df.d)==config.graphs.df.d$data.restriction]
      #    )
      #}
      
      return(result)
    }
    
  #Summary Function for Graph Averages
    #Test Inputs
      #x<-resp.long.tb %>% avg.data.restriction.fun(.) %>% group_by(!!! syms(group_by.d))
    
    summarize.avg.fun <- function(x){
      
      if(config.graphs.df.d$data.measure == "participation"){
        result <- x %>%
          dplyr::summarize(avg = length(unique(resp.id))/length(unique(school.id)))#participation
      }
      
      if(config.graphs.df.d$data.measure == "implementation"){
        result <- x %>% 
          filter(.,impbinary == 1) %>%
          dplyr::summarize(., avg = mean(as.numeric(answer), na.rm = TRUE))#implementation
        
      }
      
      if(config.graphs.df.d$data.measure == "performance"){
        result <- x %>%
          filter(impbinary == 0, !is.na(answer)) %>%
          dplyr::summarize(avg = length(unique(resp.id))/length(unique(school.id)))
      }
      
      if(config.graphs.df.d$data.measure == "average performance"){
        result <- 
          x %>%
          filter(grepl("_num",question)) %>%
          dplyr::summarize(., measure.var.avg =  mean(as.numeric(answer), na.rm = TRUE))
      }
      
      return(result)
    }

# TABLES --------------------------

#Filtering Table Data 
  #TODO: NEED TO GENERALIZE: IF REPORT.UNIT IS DISTRICT AND table DATA.LEVEL IS DISTRICT, THIS WORKS, BUT NOT IF REPORT.UNIT IS 
  #BUILDING AND DATA.LEVEL IS DISTRICT.
  
  #Test Inputs
    #data.input = resp.long.tb.c
    #config.input = config.tables.df.d
  
  table.data.filter.fun <- function(data.input, config.input){

    #data.input <- data.input[grep("_num", data.input$question),] 
    
    #Restrict data according to 'filter' variable in configs
      if(is.na(config.input$filter)){ #
        result1 <- data.input
      }
        
      if(!config.input$filter %in% c("building","district")){
        stop("Configuration 'filter' is neither 'building' nor 'district.' Check input.")
      }
      
      if(config.input$filter == "building"){
        result1 <- data.input %>% filter(unit.id == unit.id.c)
      }
      
      if(config.input$filter == "district"){
        result1 <- datap.input %>% filter(district == district.c)
      }
    
    #Additional filtering for Loop Var(s)
      if(is.na(config.input$module)){
        result <- result1
      }else{
        result <- result1 %>% filter(module == config.input$module)
      }
    
    return(result)
  }
    
#Define table x and y headers from table
  #Test Inputs
    #configs = config.tables.df.d
    #configs.header.varname = "x.varname"
    #y.varname = "y.varname"
    
  DefineHeaders <- function(
    configs,
    configs.header.varname
  ){
    
    varname <- configs %>% select(configs.header.varname) %>% unlist %>% as.vector
    
    if(is.na(varname)){
      headers <- NA
      warning("Varname is NA; returning NA for headers.")
    }else{
    
      varname.in.resp.wide.names <- varname %in% names(resp.wide.tb)
      
      if(varname.in.resp.wide.names){
        headers <- 
          resp.wide.tb %>%
          select(varname) %>%
          unlist %>%
          as.vector %>%
          unique %>%
          RemoveNA %>%
          .[. != ""]
      }
      
      if(!varname.in.resp.wide.names){
        headers <- 
          resp.long.tb %>%
          .[!grepl("_num", resp.long.tb$question),] %>%
          filter(module == configs$module) %>%
          select(varname) %>%
          unique %>%
          unlist %>%
          as.vector %>%
          RemoveNA %>%
          .[. !=""]
      }
    }
    
    return(headers)
  }

#Data Summarize TABLES - participation vs. implementation vs. performance 
  #TODO:
    #1. EVENTUALLY WILL NEED TO GENERALIZE THIS FUNCTION SO CAN TAKE AN ARBITRARY NUMBER OF CATEGORIES AS INPUT
    #   RIGHT NOW CAN ONLY TAKE TWO AND ONE OF THEM MUST BE 'YEAR,' AND THAT NOT EVEN IN CURRENT VERSION (SEE FINAL COMMAND COMMENTED OUT).
    #2. ALSO, RIGHT NOW WHEN SELECTING 'practice' IT LOOKS FOR THE CHARACTER SUBSTRING OCCURENCE IN THE 'module' VARIABLE WITH GREPL
    #   EVENTUALLY WILL WANT TO DO A STRINGSPLIT AND EXACT MATCH IN CASE THERE ARE MODULES THAT CONTAIN THE CHARACTERSTRINGS OF 
    #   OTHER MODULES (E.G. IF THERE WAS A MODULE 'CFAM' AND 'CFA' THEN THE FUNCTION WOULD PICK UP BOTH WHEN LOOKING FOR JUST 'CFA').
    
  #Test inputs
    #config.input <- config.tables.df.d
    #data.input <-  
    #  resp.long.tb.c %>% 
    #  table.data.filter.fun(data.input = ., config.input = config.tables.df.d)
    
  summarize.table.fun <- function(
    config.input, 
    data.input
  ){
    #Define all possible headers for both axes
      x.headers <- 
        DefineHeaders( configs = config.input, configs.header.varname = "x.varname") %>% 
        as.data.frame %>%
        ReplaceNames(
          .,
          names(.),
          config.input %>% select(x.varname) %>% unlist %>% as.character
        )
      
      x.headers.v <- x.headers %>% unlist %>% as.vector %>% RemoveNA
      
      y.headers <- 
        DefineHeaders( configs = config.input, configs.header.varname = "y.varname") %>% 
        as.data.frame %>%
        ReplaceNames(
          .,
          names(.),
          config.input %>% select(y.varname) %>% unlist %>% as.character
        )
      
      if(is.na(x.headers) && is.na(y.headers)){
        stop("X and Y headers are NA. Check config.table.")
      }
    
    #Build table with data and all x/y headers
      if(dim(data.input)[1] == 0){
        tb1 <-
          data.frame(matrix(0, nrow = nrow(y.headers), ncol = nrow(x.headers))) %>%
          cbind(y.headers, .) %>%
          ReplaceNames(
            .,
            names(.),
            c(names(.)[1],x.headers.v)
          )
      }else{
        dcast.formula <- paste0(names(y.headers),"~",names(x.headers))
        
        tb1 <- #table with all data and all y headers
          #data.input[!grepl("_num", data.input$question),] %>%
          melt(data.input, id.vars = names(data.input)) %>% #melt
          reshape2::dcast( #case
            data = .,
            formula = as.formula(dcast.formula), 
            value.var = "resp.id",
            fun.aggregate = function(x){length(unique(x))}
          )
      }
      
      #Add missing y headers, if any
        tb2 <- 
          right_join( #add in all possible y categories
            tb1, 
            y.headers, 
            by = config.input %>% select(y.varname) %>% unlist %>% as.character
          )
      
      #Add missing x headers, if any
        extra.x.cols <- x.headers.v[!(x.headers.v %in% names(tb1))]
        
        if(length(extra.x.cols) > 0){
          tb3 <- 
            matrix(0, nrow = nrow(tb2), ncol = length(extra.x.cols)) %>%
            as.data.frame() %>%
            ReplaceNames(
              ., 
              names(.),
              extra.x.cols
            )
          tb4 <- cbind(tb2, tb3)
        }else{
          tb4 <- tb2
        }
      
      #Order X headers      
        #TODO: Generalize
        #tb4 <-
        #  tb3[,order(names)]
        
      #Order Y headers  
        tb5 <- 
          ManualOrderTableByVectorsWithValuesCorrespondingToVariableInTable( #reorder y headers
            tb = tb4, 
            tb.order.varnames = names(tb4)[names(tb4) %in% names(y.headers)],
            ordering.vectors.list = list(config.input$y.varname.order %>% strsplit(., ",") %>% unlist)
          )
      
      #Capitalize first letter of x headers
        tb6 <-
          ReplaceNames( #Capitalize x headers
            df = tb5,
            current.names = names(tb5),
            new.names = FirstLetterCap_MultElements(names(tb5))
          )
        
      #Capitalize first letter of y headers      
        tb6[,1] <- 
          FirstLetterCap_MultElements(tb6[,1])
      
      #Replace NA with 0
        tb6[is.na(tb6)] <- 0
      
      return(tb6)
  }    
  
#Operations for first table only
  #Test inputs
    #tb = tb5
    #iterations = c(1)
  
  FirstTableOperations <- function(tb, iterations){
    if(d %in% iterations){
      
      #Filter out district admin
        tb1 <- tb %>% filter(Role != "District Administrator")
      
      #Add totals row (only first table) 
        #TODO: make into parameter in configs tables
        tb2 <-
          rbind( 
            tb1,
            c("Total",sum(select(tb1, names(tb1)[2:length(names(tb1))])))
          )
      
      #Replace second column name with "Num. Responses"
        names(tb2)[2] <- "Num. Responses"
    
      return(tb2) 
        
    }else{
      
      return(tb)
      
    }
  }
  
  
  