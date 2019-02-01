#############################################################
#####       3-configs_functions				            #####
#############################################################

source("utils_wnf.r")

#School-level slides should not include an iteration for the District Office 
  remove.district.office.fun <- function(x){
    if(report.unit != "district" & !grepl("district office", report.id.b)){
      #print("Report unit is 'building' and the report.id for this loop does not contain 'district office.' Returning input with no changes.")
      return(x)
    }
    
    if(report.unit != "district" & grepl("district office", report.id.b)){
      #print("Report unit is 'building' and the report.id for this loop contains 'district office.' Skipping to next loop")
      return(x)
    }
    
    if(report.unit == "district"){
      x[!(grepl("school", x$slide.loop.var) & (x$school %>% SubNA(.,"")) == "District Office"),] %>% 
        return(.)
    }
  }

#Loop Expander for creating full config tables
  #Test Inputs
    #configs = config.slide.types.tb
    #loop.varnames = c("slide.loop.var.1","slide.loop.var.2","slide.loop.var.3")
    #collate.varname = "slide.section.1"
    #source.data = resp.long.tb.b  

  loop.expander.fun <- function(
    configs, 
    loop.varnames, 
    collate.varname, 
    source.data
  ){
    
    output.ls <- list()
    
    #c = 4 #LOOP TESTER: NO LOOPS
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
        
        output.ls[[c]] <- 
          UniqueCombnFromColnames(resp.long.tb.b,loop.varnames.c) %>%
          cbind(configs.c,.)
      
    } ### END OF LOOP "C" BY ROW OF CONFIG INPUT ###
    
    output.df <- rbind.fill(output.ls)
    
    #Collate Report Sub-Sections
      manual.order.1 <- 
        output.df$slide.order.1 %>% 
        unique %>% 
        RemoveNA() %>% 
        strsplit(.,",") %>% 
        unlist %>%
        as.data.frame(.) %>%
        ReplaceNames(
          ., 
          current.names = names(.), 
          new.names = "module" #TODO: Not abstracted
        )
      
      output.df <- full_join(manual.order.1, output.df)
      output.df <- 
        output.df[
          order(output.df$slide.section.1),
          c(2:length(names(output.df)),1)
          ]
    
    return(output.df)
    
  } #END OF LOOP EXPANDER FUNCTION
  
