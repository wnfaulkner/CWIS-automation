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
        tb,
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
            tb %>%
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

  
  