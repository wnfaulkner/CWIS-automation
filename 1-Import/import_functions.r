#########################################################
##### 	1-IMPORT FUNCTIONS                         	#####
#########################################################

  #Load all worksheets from a Google Sheet Object
    #Test Inputs
      #gs <- configs.ss
    
    GoogleSheetLoadAllWorksheets <- function(gs) {
    
    ws_num <- gs$n_ws
    ws_names <- gs$ws %>% select(ws_title) %>% unlist
    
    result.ls <- list()
    
    for(i in 1:ws_num){
      result.ls[[i]] <- 
        gs_read(
          gs, 
          ws = ws_names[i], 
          range = NULL,
          literal = TRUE
        )
      
      #names(result.ls[[i]]) <- ws_names[i]
    
    }
    
    names(result.ls) <- ws_names
    
    return(result.ls)
    
  } 
  
  #Assign all elements in a list into separate tibble objects, assigning them names of their respective list elements  
    #Test Inputs
      #list <- configs.ls
      
    ListToTibbleObjects <- function(list){
      for(i in 1:length(list)){
        
        object.name.i <- paste(names(list)[i], ".tb", sep = "")
        
        assign(
          object.name.i,
          list[[i]],
          pos = 1
        )
        
        print(paste(i, ": ", object.name.i, sep = ""))
      }
    }
    
  
  #Assign values in second column of a table to character string objects named from first column in a table
    #object names replace a space with a "." and lower case all characters
    #Test Inputs
      #tibble <- config.global.tb
        
    TibbleToCharObjects <- function(tibble){
      for(i in 1:dim(tibble)[1]){
        
        object.name.i <- tibble[i,1] %>% unlist %>% tolower(.) %>% gsub(" ", ".", .)
        
        assign(
          object.name.i,
          tibble[i,tolower(names(tibble)) == "value"] %>% unlist %>% tolower,
          pos = 1
        )
        
        print(paste(i, ": ", object.name.i, sep = ""))
      }  
    }
 
