#############################################################
#####       4-source data tables functions              #####
#############################################################

source("utils_wnf.r")

# GRAPHS --------------------------  

  #Define names of categories that will go along bottom of graph
    #Test Inputs
      #tb = resp.long.tb
      #config.table = config.graphs.df.d
      #config.varname = "x.varnames"
    
    DefineAxisCategories <- function(
      tb,
      cat.colnames
    ){

      if(all(is.na(cat.colnames))){
        result <- NA
        warning(paste0("Config table column '", config.varname, "' is NA."))
      }
      
      if(any(cat.colnames %in% "practice")){
        tb <- tb[grep(tb.config$module, tb$module),]
      }
        
      result <-
        UniqueCombnFromColnames(
          varnames = cat.colnames, 
          df = tb
        ) 
      
      return(result)
    }
  
  #Data restriction - district vs. unit.id
    #TODO:NEED TO GENEARALIZE: IF REPORT.UNIT IS DISTRICT AND GRAPH DATA.LEVEL IS DISTRICT, THIS WORKS, 
      #BUT NOT IF REPORT.UNIT IS BUILDING AND DATA.LEVEL IS DISTRICT.
    
    #Test Inputs
      #tb = resp.long.tb.c
      #id.varname = "answer.id"
      #tb.config = config.graphs.df.d
      
    GraphDataRestriction <- function(
      tb,
      id.varname,
      tb.config
    ){
      
      loop.varnames <- 
        tb.config %>% 
        select(slide.loop.var.1, slide.loop.var.2, slide.loop.var.3) %>% 
        unlist %>% unique %>% RemoveNA
      
      if(all(is.na(loop.varnames))|length(loop.varnames) == 0){
        
        result <- tb %>% select(c(id.varname,unit.id,loop.varnames,group_by.d, tb.config$summarize.varname,practice,answer))
        return(result)
        warning("No loop varnames. Returning data as-is.")
      
      }else{
      
        restrictions.ls <-
          UniqueValsFromColnames(
            df = tb.config,
            varnames = loop.varnames
          )
        
        output.ls <- list()
        for(i in 1:length(restrictions.ls)){
          output.ls[[i]] <- 
            tb %>% 
            select(names(restrictions.ls)[i]) %>% 
            equals(restrictions.ls[[i]]) %>% 
            tb[.,] %>% select(id.varname) %>% 
            unique %>% unlist %>% as.vector
        }
        
        restricted.resp.ids <-
          Reduce(intersect, output.ls)
          
        result <- 
          tb %>% 
          filter(tb$answer.id %in% restricted.resp.ids) %>%
          select(c(id.varname,unit.id,loop.varnames,group_by.d, tb.config$summarize.varname,practice,answer))
        
        return(result)
      }
    }
  
  #Data Summarize GRAPHS - participation vs. implementation vs. performance 
    #Test inputs
      #tb = 
      #  GraphDataRestriction(
      #      tb =  resp.long.tb.c,
      #      id.varname = "answer.id",
      #      tb.config = config.graphs.df.d
      #    )
      #group.varnames = group_by.d
      #summarize.varname = config.graphs.df.d$summarize.varname %>% unlist %>% as.vector
      #summarize.fun = config.graphs.df.d$summarize.fun %>% unlist %>% as.vector
    
    SummarizeDataByGroups <- 
      function(
        tb,
        group.varnames,
        summarize.varname,
        summarize.fun
      ){

        summarize.fun <- gsub("x",summarize.varname,summarize.fun)
        
        result <-
          tb %>% 
          group_by(.dots = group.varnames) %>%
          summarise_(
            .dots = setNames(summarize.fun,"measure")
          )
        
        return(result)
      }
    
  #Restriction function for graph average data
    #TODO: THESE TWO FUNCTIONS ARE VERY SIMILAR TO THE ONES ABOVE WHICH HAVE BEEN CHANGED SO NOW NEED TO SPECIFY "dat.config" BUT
      #HAVE NOT MADE THOSE CHANGES HERE YET. PROBABLY COULD ROLL UP INTO ONE OR TWO FUNCTIONS.
    
    #Test Inputs
      #tb = resp.long.tb
      #graph.type.id = config.graphs.df.d$graph.type.id
      #group.varnames = group_by.d
      #summarize.varname = config.graphs.df.d$summarize.varname %>% unlist %>% as.vector
      #summarize.fun = config.graphs.df.d$summarize.fun %>% unlist %>% as.vector
      #avg.level = config.graphs.df.d$avg.level
      #tb.restriction.value = district.c
    
    GroupedAveragesByLevel <- function(
      tb, #source data table
      graph.type.id, #STUPID type c making this function not abstracted because have to add a grouping by building. Might be able to generalize later using loop variables
      group.varnames, #varnames on which averages will be grouped
      summarize.varname,
      summarize.fun,
      avg.level = NULL, #name of the variable for filtering data (if necessary)
      tb.restriction.value = NULL #value to match in avg.level variable to restrict data (if necessary)
    ){
      
      if(is.na(avg.level)){
        avg.level <- NULL
      }
      
      if(is.null(avg.level)){
        result <-
          SummarizeDataByGroups(
            tb = tb,
            group.varnames = c(group.varnames),
            summarize.varname = "answer",
            summarize.fun = "length(x)"
          ) %>% 
          select(group.varnames) %>%
          mutate(avg = NA)
        return(result)
      }
      
      #Form avg. tables under different scenarios
      
        #When summary function is length(unique(x)) 'count.unique'
          #if(graph.type.id == "c"){
          #  group.varnames <- c("building.id",group.varnames)
          #}
            
          if(avg.level == "district"){ #If avg.level is relevant, restrict data to rows where avg.level var == tb.restriction.value
            tb <- tb[tb[,names(tb) ==avg.level] == tb.restriction.value,]
          } #when avg.level is district, restrict source data table to matching district
          
          if(grepl("length\\(unique\\(x",summarize.fun)){
            output1.tb <- 
              SummarizeDataByGroups(
                tb = tb,
                group.varnames = c("district",if(graph.type.id == "c"){"building.id"}, group.varnames),
                summarize.varname = summarize.varname,
                summarize.fun = summarize.fun
              ) 
            
            result <- 
              SummarizeDataByGroups(
                tb = output1.tb,
                group.varnames = c(group.varnames[]),
                summarize.varname = "measure",
                summarize.fun = "mean(x)"
              ) %>% 
              ReplaceNames(., current.names = "measure", new.names = "avg")
          }
          
        #When summary function is mean(x) 'mean'
          if(grepl("mean\\(x",summarize.fun)){
            result <- 
              SummarizeDataByGroups(
                tb = tb,
                group.varnames = c(group.varnames),
                summarize.varname = "answer",
                summarize.fun = summarize.fun
              ) %>% 
              ReplaceNames(., current.names = "measure", new.names = "avg")
          }
        
      return(result)
    } 

# TABLES --------------------------

  #Define Table Formula
    DefineTableRowColFormula <- 
      function(
        row.header.varnames,
        col.header.varnames
      ){
        row.header.formula <- 
          row.header.varnames %>% 
          {if(is.na(.)) "." else .} %>%
          {if(length(.)==1) . else paste(., collapse = "+")}
      
        col.header.formula <- 
          col.header.varnames %>% 
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
        
        return(table.formula.d)
      }
  
  #Define Filtering Vector
    DefineTableFilterVector <-
      function(
        filter.varnames,
        filter.values
      ){
        
        if(length(filter.varnames) != length(filter.values)){stop("Filter varnames and filter values vectors must be the same length.")}
        
        table.filters.ls <- list()
        
        for(e in 1:length(filter.varnames)){
          
          filter.varname.e <- filter.varnames[e]
          
          filter.value.e <- 
            ifelse(
              filter.varname.e == "unit.id",
              as.object(filter.values[e]),
              filter.values[e]
            )
          
          if(
            !is.na(filter.varname.e) && 
            is.na(filter.value.e)
          ){
            print(paste("ERROR: filter variable specified but missing filter values. c = ", c, "; d = ", d, ". Skipping to next loop iteration."))
            next()
          }
          
          table.filters.ls[[e]] <- #regular filter for unit.id data based on filter column and value(s)
            resp.long.tb %>%
            select(filter.varname.e) %>%
            unlist %>% as.vector %>%
            grepl(filter.value.e, .)
        }
        
        table.filter.v <- 
          do.call(cbind, table.filters.ls) %>% as_tibble() %>%
          mutate(table.filter.v = apply(., 1, function(x){x %>% equals(TRUE) %>% all})) %>%
          select(table.filter.v) %>%
          unlist %>% as.vector()
        
        return(table.filter.v)
      }

  
  