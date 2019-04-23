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
    #c = 9 #LOOP TESTER: ONE LOOP VAR
    #c = 4 #LOOP TESTER: TWO LOOP VARS
    #for(c in 2:3){
    for(c in 1:dim(configs)[1]){
      
      #c.list.c <- list(c=c)
      configs.c <- configs[c,]
      
      #Make data frame with configurations repeated out across all unique combinations of loop.varname(s) in source.data      
      
        loop.varnames.c <- 
          configs[c,names(configs) %in% loop.varnames] %>% 
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
        
        output.ls[[c]] <- output.c #store loop output
        
    } ### END OF LOOP "C" BY ROW OF CONFIG INPUT ###
    
    output1.df <- rbind.fill(output.ls)
    
    #Collate & Manual Order 
    
      output2.ls <- list()
      #i = 11
      for(i in 1:length(unique(output1.df$slide.section.1))){
        
        slide.section.i <- unique(output1.df$slide.section.1)[i]
        tb.i <- output1.df %>% filter(slide.section.1 == slide.section.i) %>% as_tibble()
        loop.varnames.i <- 
          configs[configs$slide.section.1 == slide.section.i,names(configs) %in% loop.varnames] %>% 
          unlist %>% as.vector %>% RemoveNA() %>% unique
        
        if(length(loop.varnames.i) == 0){
          output2.ls[[i]] <- tb.i
          next()
        }
      
      #Manually Ordering Result (if necessary)
        manual.order.ls <-
          UniqueValsFromColnames(
            df = tb.i,
            varnames = manual.order.varnames[1:length(loop.varnames.i)]
          )
        
        #Ordering vector for sub-sections
          if(length(loop.varnames.i) == 1){
            order.v.i <-
              order(
                tb.i$slide.section.1,
                tb.i %>% select(loop.varnames.i[1]) %>% unlist %>% as.vector %>% .[order(match(.,manual.order.ls[[1]]))]
              )
          }
          if(length(loop.varnames.i) == 2){
            order.v.i <-
              order(
                tb.i$slide.section.1,
                tb.i %>% select(loop.varnames.i[1]) %>% unlist %>% as.vector %>% .[order(match(.,manual.order.ls[[1]]))], 
                tb.i$slide.section.2,
                tb.i %>% select(loop.varnames.i[2]) %>% unlist %>% as.vector %>% .[order(match(.,manual.order.ls[[2]]))]
              )
          }
          if(length(loop.varnames.i) == 3){
            order.v.i <-
              order(
                tb.i$slide.section.1,
                tb.i %>% select(loop.varnames.i[1]) %>% unlist %>% as.vector %>% .[order(match(.,manual.order.ls[[1]]))],
                tb.i$slide.section.2,
                tb.i %>% select(loop.varnames.i[2]) %>% unlist %>% as.vector %>% .[order(match(.,manual.order.ls[[2]]))],
                tb.i$slide.loop.var.3,
                tb.i %>% select(loop.varnames.i[3]) %>% unlist %>% as.vector %>% .[order(match(.,manual.order.ls[[3]]))]
              )
          }
          
        output2.ls[[i]] <- tb.i[order.v.i,]
      }
    
      result <- do.call(rbind, output2.ls) %>% as_tibble()
  
    return(result)
   
  } #END OF LOOP EXPANDER FUNCTION
  
