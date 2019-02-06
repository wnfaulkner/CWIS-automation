#############################################################
#####       3-configs_functions				            #####
#############################################################

source("utils_wnf.r")

#School-level slides should not include an iteration for the District Office 
  remove.district.office.fun <- function(x){
    if(report.unit != "district" & !grepl("district office", unit.id.b)){
      #print("Report unit is 'building' and the unit.id for this loop does not contain 'district office.' Returning input with no changes.")
      return(x)
    }
    
    if(report.unit != "district" & grepl("district office", unit.id.b)){
      #print("Report unit is 'building' and the unit.id for this loop contains 'district office.' Skipping to next loop")
      return(x)
    }
    
    if(report.unit == "district"){
      x[!(grepl("school", x$slide.loop.var) & (x$school %>% SubNA(.,"")) == "District Office"),] %>% 
        return(.)
    }
  }

#Loop Expander for creating full config tables
  #Test Inputs
    #configs = config.graph.types.tb
    #loop.varnames = c("slide.loop.var.1","slide.loop.var.2","slide.loop.var.3") 
    #manual.order.varnames = c("slide.order.1","slide.order.2","slide.order.3")
    #collate.varnames = c("slide.section.1","slide.section.2","slide.section.3")
    #source.data = resp.long.tb  

  
  #TODO: make so can handle recursive loops, sections, ordering
  
  loop.expander.fun <- function(
    configs, 
    loop.varnames, 
    manual.order.varnames,
    collate.varnames, 
    source.data
  ){
    
    output.ls <- list()
    
    #c = 2 #LOOP TESTER: NO LOOPS
    #c = 3 #LOOP TESTER: ONE LOOP VAR
    #c = 4 #LOOP TESTER: TWO LOOP VARS
    #for(c in 2:3){
    for(c in 1:dim(configs)[1]){
      
      #c.list.c <- list(c=c)
      configs.c <- configs[c,]
      
      #Make data frame with configurations repeated out across all unique combinations of loop.varname(s) in source.data      
      
        loop.varnames.c <- configs[c,names(configs) %in% loop.varnames] %>% 
          as.matrix %>% 
          as.vector %>% 
          RemoveNA()
        
        if(length(loop.varnames.c) == 0){
          output.ls[[c]] <- configs[c,]
          next()
        }
        
        output.c <- #initial output data frame
          UniqueCombnFromColnames(source.data,loop.varnames.c) %>%
          cbind(configs.c,.)
        
        #output.c <- output1.c[order(match(order.c, output1.c$module)),] 
          
        output.ls[[c]] <- output.c #store loop output
        
    } ### END OF LOOP "C" BY ROW OF CONFIG INPUT ###
    
    output1.df <- rbind.fill(output.ls)
    
    #Manually Ordering Result (if necessary)
      if(is.na(configs %>% select(manual.order.varnames[1]) %>% unique) %>% all){
        result <- output1.df
      }else{
        manual.order.varname <- #TODO: Put in config table?
          UniqueValsFromColnames(
            df = output1.df,
            varnames = "slide.loop.var.1"
          ) %>% unlist %>% as.vector
        
        ordering.vector <- 
          configs %>% 
          select(manual.order.varnames[1]) %>% 
          unique %>% 
          unlist %>% 
          RemoveNA %>% 
          strsplit(., ",") %>% 
          unlist
        
        result <- 
          ManualOrderTableByVectorWithValuesCorrespondingToVariableInTable(
            tb = output1.df,
            tb.order.varname = manual.order.varname,
            ordering.vector = ordering.vector,
            suppressWarnings()
          )
      }
    
    return(result)
    
  } #END OF LOOP EXPANDER FUNCTION
  
