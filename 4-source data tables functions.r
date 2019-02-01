#############################################################
#####       4-source data tables functions              #####
#############################################################

source("utils_wnf.r")

#Data restriction - district vs. report.id
  
  #TODO:NEED TO GENEARALIZE: IF REPORT.UNIT IS DISTRICT AND GRAPH DATA.LEVEL IS DISTRICT, THIS WORKS, BUT NOT IF REPORT.UNIT IS 
  #report.id AND DATA.LEVEL IS DISTRICT.
  
  graph.data.restriction.fun <- function(x){
    
    if(config.graphs.df.d$data.level == "district"){
      y <- x
    }
    
    if(config.graphs.df.d$data.level == "building"){
      y <- 
        x %>% 
        filter(report.id == report.id.c) 
    }
    
    if(is.na(config.graphs.df.d$data.restriction)){
      result <- y
    }
    
    if(!is.na(config.graphs.df.d$data.restriction)){
      result <- 
        y %>% 
        filter(
          y[,names(y) == config.graphs.df.d$data.restriction] == 
            config.graphs.df.d[,names(config.graphs.df.d) == config.graphs.df.d$data.restriction]
        )
    }
    
    return(result)
  }
  
#Restriction function for graph average data
  
  #TODO: THESE TWO FUNCTIONS ARE VERY SIMILAR TO THE ONES ABOVE WHICH HAVE BEEN CHANGED SO NOW NEED TO SPECIFY "config.input" BUT
  #   HAVE NOT MADE THOSE CHANGES HERE YET. PROBABLY COULD ROLL UP INTO ONE OR TWO FUNCTIONS.
  
  #Test Inputs
  #x <- resp.long.df
  
  avg.data.restriction.fun <- function(x){
    
    if(config.graphs.df.d$data.level == "district"){
      y <- x
    }
    
    if(config.graphs.df.d$data.level == "building"){
      y <- 
        x %>% 
        filter(district == unique(resp.long.df$district[resp.long.df$report.id == report.id.c])) 
    }
    
    z <- y %>% filter(!is.na(y[,names(y)==group_by.d])) #TODO:Might want to make flexible - i.e. add a parameter which allows user to include NA
    
    if(!config.graphs.df.d$data.restriction=="module" | is.na(config.graphs.df.d$data.restriction)){ 
      #TODO:Should look into a better way to deal with this restriction, think about input tables
      result <- z
    }
    
    if(config.graphs.df.d$data.restriction=="module" & !is.na(config.graphs.df.d$data.restriction)){
      result <- 
        z %>%
        filter(
          z[,names(z)==config.graphs.df.d$data.restriction] == 
            config.graphs.df.d[,names(config.graphs.df.d)==config.graphs.df.d$data.restriction]
        )
    }
    
    return(result)
  }
  
#Summary Function for Graph Averages
  #Test Inputs
  #x<-resp.long.df %>% avg.data.restriction.fun(.) %>% group_by(!!! syms(group_by.d))
  
  summarize.avg.fun <- function(x){
    
    if(config.graphs.df.d$data.measure == "participation"){
      result <- x %>%
        dplyr::summarize(avg = length(unique(responseid))/length(unique(school.id)))#participation
    }
    
    if(config.graphs.df.d$data.measure == "implementation"){
      result <- x %>% 
        filter(.,impbinary == 1) %>%
        dplyr::summarize(., avg = mean(as.numeric(answer), na.rm = TRUE))#implementation
      
    }
    
    if(config.graphs.df.d$data.measure == "performance"){
      result <- x %>%
        filter(impbinary == 0, !is.na(answer)) %>%
        dplyr::summarize(avg = length(unique(responseid))/length(unique(school.id)))
    }
    
    if(config.graphs.df.d$data.measure == "average performance"){
      result <- 
        x %>%
        filter(grepl("_num",question)) %>%
        dplyr::summarize(., measure.var.avg =  mean(as.numeric(answer), na.rm = TRUE))
    }
    
    return(result)
  }



#Filtering Table Data 
  #TODO:NEED TO GENEARALIZE: IF REPORT.UNIT IS DISTRICT AND table DATA.LEVEL IS DISTRICT, THIS WORKS, BUT NOT IF REPORT.UNIT IS 
  #BUILDING AND DATA.LEVEL IS DISTRICT.
  
  table.data.filter.fun <- function(data.input, config.input){
    
    if(is.na(config.input$module)){
      y <- x
    }else{
      y <- x %>% filter(module == config.input$module) %>% filter()
    }
    
    y <- y %>% filter(table.q == 1) 
    
    if(is.na(config.input$filter)){ #
      result <- y
    }else{
      
      if(!config.input$filter %in% c("building","district")){
        stop("Configuration 'filter' is neither 'building' nor 'district.' Check input.")
      }
      
      if(config.input$filter == "building"){
        result <- y %>% filter(report.id == report.id.c)
      }
      
      if(config.input$filter == "district"){
        result <- y %>% filter(district == district.c)
      }
      
    }
    return(result)
  }

  
#Selecting variable names that will be used in graph/table calculations
  #TEST INPUTS
    #config.input <- config.graphs.df.d
    #data.input <- resp.long.df
    
  GraphVarnamesInData <- function(config.input, data.input) {
    varname.i <- config.input %>% select(x.varname.1)
    
    if(is.na(varname.i)){
      all.cats.ls[[i]] <- ""
      next()
    }
    
    if(varname.i == "answer"){
      module.varnames <- 
        q.unbranched.df %>% 
        filter(module == config.input$module) %>% 
        select(row.1) %>% 
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
        q.unbranched.df %>% 
        filter(module == config.input$module) %>% 
        select(row.1) %>% 
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
    
#Data Summarize - participation vs. implementation vs. performance 
  #Test inputs
  #config.input <- config.graphs.df.d
  #data.input <-  resp.long.df.c %>% graph.data.restriction.fun %>% group_by(!!! syms(group_by.d))
  
  summarize.graph.fun <- function(config.input, data.input){
    if(config.input$data.measure == "participation"){
      result <- 
        dplyr::summarize(data.input, measure.var =  length(unique(responseid)))
    }
    
    if(config.input$data.measure == "implementation"){
      result <- 
        data.input %>% 
        filter(impbinary == 1) %>%
        dplyr::summarize(measure.var = mean(as.numeric(answer), na.rm = TRUE)) %>%
        as.data.frame(., stringsAsFactors = FALSE)
    }
    
    if(config.input$data.measure == "performance"){
      result <- data.input %>%
        filter(impbinary == 0, !is.na(answer)) %>%
        dplyr::summarize(measure.var = as.character(length(unique(responseid))))
    }
    
    if(config.input$data.measure == "average performance"){
      result <- 
        data.input %>%
        filter(grepl("_num",question)) %>%
        dplyr::summarize(., measure.var =  mean(as.numeric(answer), na.rm = TRUE))
    }
    
    return(result)
  }
  
#Data Summarize - participation vs. implementation vs. performance 
  #Test inputs
  #config.input <- config.tables.df.d
  #data.input <-  resp.long.df.c %>% table.data.filter.fun %>% group_by(!!! syms(config.tables.df.d$summary.var))
  
  #TODO:
  #1. EVENTUALLY WILL NEED TO GENERALIZE THIS FUNCTION SO CAN TAKE AN ARBITRARY NUMBER OF CATEGORIES AS INPUT
  #   RIGHT NOW CAN ONLY TAKE TWO AND ONE OF THEM MUST BE 'YEAR,' AND THAT NOT EVEN IN CURRENT VERSION (SEE FINAL COMMAND COMMENTED OUT).
  #2. ALSO, RIGHT NOW WHEN SELECTING 'practice' IT LOOKS FOR THE CHARACTER SUBSTRING OCCURENCE IN THE 'module' VARIABLE WITH GREPL
  #   EVENTUALLY WILL WANT TO DO A STRINGSPLIT AND EXACT MATCH IN CASE THERE ARE MODULES THAT CONTAIN THE CHARACTERSTRINGS OF 
  #   OTHER MODULES (E.G. IF THERE WAS A MODULE 'CFAM' AND 'CFA' THEN THE FUNCTION WOULD PICK UP BOTH WHEN LOOKING FOR JUST 'CFA').
  
  
  summarize.table.fun <- function(config.input, data.input){
    #na.replace <- function(x, na.replacement){x[is.na(x)] <- na.replacement} #TODO:This didn't work, but may not need after generalizing.
    
    result.1 <- melt(data.input, id.vars = names(data.input)) 
    
    if(is.na(config.input$x.varname)){ #TODO: finalize modifications (see line 1001 of MASTER file)
      result <-
        reshape2::dcast(
          data = result.1,
          formula = role ~ report.id,
          value.var = "responseid",
          fun.aggregate = function(x){length(unique(x))}
        ) %>%
        right_join(
          ., 
          all.cats.ls.d$y, 
          by = c("role" = "all.cats")
        ) %>%
        filter(role != "District Administrator") %>%
        .[c(2,3,5,6,7,4,8,1),] %>%
        #.[c(1,2,3,4,6,7,8,5),] %>%
        ReplaceNames(
          df = .,
          current.names = report.id.c,
          new.names = "num. responses"
        ) %>%
        ReplaceNames(
          df = .,
          current.names = names(.),
          new.names = FirstLetterCap_MultElements(names(.))
        ) %>%
        rbind(
          .,
          c("Total",sum(select(., "Num. Responses"), na.rm = TRUE))
        )
      result[is.na(result)] <- 0
      
      return(result)
    }else{
      
      #Draft table (have to merge with all.cats to make sure have every column and row represented)
      result.2 <- 
        reshape2::dcast(
          data = result.1, 
          formula = 
            unlist(data.input[names(data.input) == config.input$y.varname]) ~ 
            unlist(data.input[names(data.input) == config.input$x.varname]),#syms(paste(config.input$x.var,"~",config.input$y.var,sep="")), 
          value.var ="responseid",
          fun.aggregate = length
        ) %>% 
        ReplaceNames(
          df = .,
          current.names = "unlist(data.input[names(data.input) == config.tables.df.d$y.varname])",
          new.names = "all.cats"
        ) 
      
      #Add all.cats to rows (y axis) 
      result.3 <- 
        right_join(
          result.2, 
          all.cats.ls.d$y, 
          by = "all.cats"
        )
      
      ##Add all.cats to columns (x axis)
      missing.cats <- unlist(all.cats.ls.d$x)[!unlist(all.cats.ls.d$x) %in% names(result.3)] %>% as.character
      
      result.4 <- 
        matrix(
          ncol = length(missing.cats),
          nrow = dim(result.3)[1]
        ) %>%
        as_tibble() %>%
        ReplaceNames(
          df = .,
          current.names = names(.),
          new.names = missing.cats
        ) %>%
        cbind(result.3, .) %>%
        OrderDfByVar(
          df = .,
          order.by.varname = "all.cats",
          rev = TRUE
        ) %>%
        ReplaceNames(
          df = .,
          current.names = "all.cats",
          new.names = FirstLetterCap_OneElement(config.input$y.varname)
        )
      
      return(result.4)
    }
  }    