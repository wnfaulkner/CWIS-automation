# ESTABLISH DIRECTORIES ---------------------------------------------------

  library(magrittr)
  
  #Load Utils
    source("C:/Users/willi/Documents/GIT PROJECTS/CWIS-automation/utils_wnf.r")

  #Test inputs
    comp <- "M900"
    base.dir <- "Documents/GIT PROJECTS/CWIS-automation"
    append.dir <- "1-Import"

  MakeSubDir <- function(comp, base.dir, append.dir){
      
      #Parameter checks
                                  #TODO: make util functions for missing and incorrectly specified(?) parameters for functions in general
        #Missing: comp
          if(missing(comp) && (!missing(base.dir) | !missing(append.dir))){ #prompt to fill in 'comp' if missing 'comp' but have either base.dir or append.dir
            missing.prompt <- 
              paste(
                "No computer name specified. You may input '",
                paste(allowed.comps.v, collapse = "' or '"),
                "':"
              )
            comp <- readline(missing.comp.prompt)
          }
        
        #Incorrectly specified: comp
          allowed.vals.v <- c("M900","T470")
          
          while(!(comp %in% allowed.vals.v)){
            incorrectly.specifed.prompt <- 
              paste(
                "Object 'comp' incorrectly defined as:",
                comp,
                ". Please input either '",
                paste(allowed.vals.v, collapse = "' or '"),
                "'"
              )
            comp <- readline(incorrectly.specifed.prompt)
          }
        
        #Missing: base.dir
          if(missing(base.dir) && !missing(append.dir)){
            missing.prompt <- "No base.dir specified. Please input a base.dir:"
            base.dir <- readline(missing.prompt)
          }  
      
      #Form full base.dir using comp and base.dir parameter
        if(comp == "M900"){
          comp.dir <- "C:/Users/willi/"
        }
        
        if(comp == "T470"){
          comp.dir <- "C:/Users/WNF/"
        }
        
        if(substr(base.dir,1,1) == "/"){
          base.dir <- substr(base.dir, 2, nchar(base.dir))
        }
        
        comp.base.dir <- 
          paste(
            comp.dir,
            base.dir,
            sep = ""
          ) %>% 
          gsub("/ ", "/", .) %>%
          gsub(" /", "/", .) %>%
          gsub("//", "/", .)
          
      #Paramater checks: base.dir
                                  #TODO: Make so if directories not specified, can browse for directory
       
        #Incorrectly specified: base.dir
          if(IsError(.expr = setwd(comp.base.dir))){
            
            while(IsError(.expr = setwd(comp.base.dir))){
              incorrectly.specified.prompt <- 
                paste(
                  "Can't find the directory specified: '",
                  base.dir,
                  "'. Please specify base.dir again:",
                  sep = ""
                )
              
              base.dir <- readline(incorrectly.specified.prompt)
              
              comp.base.dir <- 
                paste(
                  comp.dir,
                  base.dir,
                  sep = ""
                ) %>% 
                gsub("/ ", "/", .) %>%
                gsub(" /", "/", .) %>%
                gsub("//", "/", .)
            }
          }
       
      #Assemble final directory
        
          
          
    
    
  } 
    
    if(comp == "M900")
    
    
    #M900
      working.dir <- "C:/Users/willi/Google Drive/1. FLUX CONTRACTS - CURRENT/2016-09 EXT Missouri Education/3. Missouri Education - GDRIVE/8. CWIS/2018-12 Green Reports Phase 6/"
      rproj.dir <- "C:/Users/willi/Documents/GIT PROJECTS/CWIS-automation"
    
    #Thinkpad T470
      #working.dir <- "G:/My Drive/1. FLUX CONTRACTS - CURRENT/2016-09 EXT Missouri Education/3. Missouri Education - GDRIVE/8. CWIS/2018-12 Green Reports Phase 6/"
      #rproj.dir <- "C:/Users/WNF/Documents/Git Projects/CWIS-automation"
    
    dir.ls <- list(
      working.dir = working.dir
      rproj.dir = rproj.dir
    )
    
    return(dir.ls)
  }
 
  #Source Code Directory
    #source.code.dir <- rproj.dir #paste(rproj.dir,"2_source_code/",sep="") #Changed back to using 'Documents' folder after attempting to move project into Google Drive but running into problems
  
  #Source Resources Director (raw data)
    #source.resources.dir <- paste(working.dir,"3_source_resources/", sep = "")
  
  #Source Inputs (configs)
    #source.inputs.dir <- paste(working.dir,"4_source_inputs/",sep="")

    
