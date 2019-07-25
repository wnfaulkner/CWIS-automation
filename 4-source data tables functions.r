#############################################################
#####       4-source data tables functions              #####
#############################################################

source("utils_wnf.r")

# TABLES --------------------------

  #Define Table Formula
    DefineTableRowColFormula <- 
      function(
        row.header.varnames,
        col.header.varnames
      ){
        row.header.formula <- 
          row.header.varnames %>% 
          {if(all(is.na(.))) "." else .} %>%
          {if(length(.)==1) . else paste(., collapse = "+")}
      
        col.header.formula <- 
          col.header.varnames %>% 
          {if(all(is.na(.))) "." else .} %>%
          {if(length(.)==1) . else paste(., collapse = "+")}
        
        table.formula <- 
          paste(
            row.header.formula,
            "~",
            col.header.formula,
            sep = ""
          ) %>% 
          as.formula
        
        return(table.formula)
      }
  
  #Define Filtering Vector
    DefineTableFilterVector <-
      function(
        tb,
        filter.varnames,
        filter.values
      ){
        
        if(length(filter.varnames) != length(filter.values)){stop("Filter varnames and filter values vectors must be the same length.")}
        
        table.filters.ls <- list()
        
        #tic("Loop total duration")
        for(e in 1:length(filter.varnames)){
          
          filter.varname.e <- filter.varnames[e]
          
          filter.value.e <- 
            ifelse(
              grepl("\\.id", filter.values[e]),
              as.object(filter.values[e]),
              filter.values[e]
            )
          
          #if(
          #  !is.na(filter.varname.e) && 
          #  is.na(filter.value.e)
          #){
          #  print(paste("ERROR: filter variable specified but missing filter values. c = ", c, "; d = ", d, ". Skipping to next loop iteration."))
          #  next()
          #}
          
          table.filters.ls[[e]] <- #regular filter for unit.id data based on filter column and value(s)
            tb %>%
            select(filter.varname.e) %>%
            unlist %>% as.vector %>%
            grepl(filter.value.e, .)
          
          #if(table.filters.ls[[e]] %>% not %>% all){
          #  warning(
          #    paste(
          #      "Filtering variable '",
          #      filter.varname.e, 
          #      "' for value '",
          #      filter.value.e,
          #      "' returning no rows."
          #    )
          #  )
          #}
        }
        #toc(log = TRUE, quiet = FALSE)
        
        #tic("Final creation of filtor vector")
        table.filter.v <- 
          do.call(cbind, table.filters.ls) %>% as_tibble() %>%
          mutate(table.filter.v = apply(., 1, function(x){all(x)})) %>%
          select(table.filter.v) %>%
          unlist %>% as.vector()
        #toc(log = TRUE, quiet = FALSE)
        
        if(table.filter.v %>% not %>% all){warning("Table filter returning no rows.")}
        
        return(table.filter.v)
      }

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
  