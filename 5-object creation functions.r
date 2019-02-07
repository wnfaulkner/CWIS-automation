#############################################################
#####       5-object creation functions		            #####
#############################################################

source("utils_wnf.r")

#Graph Label Heights (defined based on ratio of tallest to shortest columns)
  #Test Inputs
    #df = graphdata.df.g
    #measure.var = "measure.var"
    #height.ratio.threshold = 8.2

create.graph.labels.fun <- function(df, measure.var, height.ratio.threshold){
  
  if(!is.data.frame(as.data.frame(df))){stop("Input cannot be coerced into data frame.")}
  
  df <- as.data.frame(df)
  
  var <- df[,names(df) == measure.var] %>% as.matrix %>% as.vector(.,mode = "numeric")
  
  #Label Heights
  
    min <- min(var, na.rm = TRUE)
    max <- max(var, na.rm = TRUE)
    height.ratio.threshold <- height.ratio.threshold
    height.ratio <- ifelse(max == 0, 0, max/min)
  
    #print(paste("Max: ",max,"  Min: ",min,"  Ratio: ", height.ratio, "  Ratio threshold: ",height.ratio.threshold,sep = ""))
  
  if(height.ratio < height.ratio.threshold){ 
    graph.labels.heights.v <- rep(min/2, length(var)) #if ratio between min and max height below threshold, all labels are minimum height divided by 2
    above.label.vectorposition <- max/var > height.ratio.threshold
  }
  
  if((min == 0 && max !=0) | height.ratio >= height.ratio.threshold){
    graph.labels.heights.v <- vector(length = length(var))
    above.label.vectorposition <- max/var > height.ratio.threshold
    above.label.vectorposition[is.na(above.label.vectorposition)] <- TRUE
    var[is.na(var)] <- 0
    graph.labels.heights.v[above.label.vectorposition] <-   #labels for columns below threshold, position is height of bar plus 1/10 of max bar height 
      var[above.label.vectorposition] + max/10
    graph.labels.heights.v[graph.labels.heights.v == 0] <-    #labels for columns above threshold, position is height of smallest bar divided by 2
      min(var[!above.label.vectorposition])/2
  }
  
  #Label Text
    
    if(config.graphs.df.g$data.measure == "implementation"){
      graph.labels.text.v <- as.character(100*var %>% round(., 2)) %>% paste(.,"%",sep="")
    }else{
      graph.labels.text.v <- var %>% as.numeric %>% round( ., 1) %>% sprintf("%.1f",.) %>% trimws(., which = "both") 
    }
    graph.labels.text.v[df[,names(df) == measure.var] %>% as.matrix %>% as.vector(.,mode = "numeric") %>% is.na(.)] <- "No Responses"
  
  #Label visibility
    
    graph.labels.alpha.v <- 1 #ifelse(var != 0, 1, 0)  
  
  #Label color for graph.type.e
    
    if(config.graphs.df.g$graph.type.id == "e"){
      graph.labels.color.v <- rep(c("#000000","#FFFFFF"),length(df[,1])/2) %>% rev
    }else{
      graph.labels.color.v <- rep("#FFFFFF",100)[1:length(df[,1])]
    }
    graph.labels.color.v[var==0] <- "#000000"
    graph.labels.color.v[above.label.vectorposition] <- "#000000"
    graph.labels.color.v <- graph.labels.color.v %>% rev
    
    result <- data.frame(
      graph.labels.text = graph.labels.text.v,
      graph.labels.heights = graph.labels.heights.v,
      graph.labels.alpha.v = graph.labels.alpha.v,
      graph.labels.color = graph.labels.color.v,
      stringsAsFactors = FALSE
    )
  
  #print(paste("Graph Label Heights: ",paste(graph.labels.heights.v, collapse = ", "),sep=""))
  return(result)
}

