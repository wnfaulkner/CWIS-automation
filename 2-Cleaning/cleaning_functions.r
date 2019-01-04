#########################################################
##### 	2-CLEANING FUNCTIONS                        #####
#########################################################

  #Add slide.type columns via inner-join
    AddSlideTypeCols <- function(config.table){
      result <- 
        inner_join(config.slide.types.tb, config.table, by = "slide.type.id", all.x = FALSE)
    }
   

