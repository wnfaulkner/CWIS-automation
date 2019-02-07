#############################################################
#####       5-object creation functions		            #####
#############################################################

source("utils_wnf.r")

#Graph Base Formation: forms graph object with data, alpha, and theme
  FormBaseGraphObject.DataAndTheme <- function(data.input){
    
    graph.base <- 
      
      ggplot( 
        data = data.input,
        alpha = alpha
      ) + 
      
      theme(
        panel.background = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(size = 20, color = "#5a6b63"),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 12)
      )
    
    return(graph.base)
  }

#Add columns to graph: 
  #first column of data becomes x-axis categories, 
  #measure.var = height (y values), 
  #graph.fill = fill
  #Transparency (alpha) automatically set to 0 - fully opaque
  #Test Inputs
    #base.graph.input = graph.1
    #data.input = graphdata.df.g
    #graph.headers.varname = names(graphdata.df.g)[!grepl("measure", names(graphdata.df.g))]
    #graph.group.by.var = graph.group.by.var
    #graph.fill = graph.fill.g
    #print.graph = TRUE
  
  AddColsToGraph <- function(
    base.graph.input, #a base graph ggplot object with data and alpha defined (e.g. resulting from function above)
    data.input, #the graph data frame with x-axis labels in column 1 and bar heights in a column named 'measure.var'
    graph.headers.varname, #name of variable in data.input that will form graph headers
    graph.group.by.var, #[for stacked graphs only] a data frame of the grouping variable extracted from input data earlier in code
    graph.fill, #a vector of hex color values with same length as nrow(data.input)
    print.graph = FALSE #TRUE/FALSE: if true, prints graph in new window. FALSE = default.
  ){
    #Checking Parameter Inputs
      if(nrow(data.input) != length(graph.fill)){
        print(graph.fill)
        stop(
          paste0(
            "Length of graph.fill (", 
            length(graph.fill), 
            ") is different from number of rows in data.input (",
            nrow(data.input),
            ")."
          )
        )
      }
      
      if(!(print.graph %in% c(TRUE,FALSE))){
        stop(
          print(
            paste0(
              "print.graph (currently set to: ",print.graph,") must be logical TRUE/FALSE."
            )
          )
        )
      }
    #Produce Inputs for Final Result
      headers <- data.input[,names(data.input) == graph.headers.varname] #vector of column/bar headers
    
    #Produce Final Result  
      if(is.null(graph.group.by.var)){
        graph.w.cols <-
          base.graph.input +
          
          geom_bar(
            aes(x = headers, 
                y = measure.var %>% as.numeric
            ),
            fill = graph.fill.g,
            alpha = 1,
            position = "dodge", 
            stat = "identity",
            show.legend = FALSE
          )
      }else{ #TODO: WILL LIKELY NEED EDITING WHEN NEED TO PRODUCE STACKED BAR/COLUMN CHARTS AGAIN
        graph.w.cols <-
          base.graph.input +
          
          geom_bar(
            aes(x = headers, 
                y = measure.var %>% as.numeric,
                group = graph.group.by.var, 
                fill = factor(graph.group.by.var)
            ),
            alpha = 1,
            position = "dodge", 
            stat = "identity"
          )
      }
    
    #Return/Print Results  
      if(print.graph){
        windows()
        print(graph.w.cols)
      }
      
      return(graph.w.cols)
      
  }
  
#Graph Label Heights (defined based on ratio of tallest to shortest columns)
  #Test Inputs
    #dat = graphdata.df.g
    #measure.var = "measure.var"
    #height.ratio.threshold = 8.2
  #TODO: STANDARDIZE ALL FUNCTIONS SO DATA TABLE INPUT (WHETHER TIBBLE OR DATA FRAME) PARAMETER IS "dat"

create.graph.labels.fun <- function(dat, measure.var, height.ratio.threshold){
  
  if(!is.data.frame(as.data.frame(dat))){stop("Input cannot be coerced into data frame.")}
  
  dat <- as.data.frame(dat)
  var <- dat[,names(dat) == measure.var] %>% as.matrix %>% as.vector(.,mode = "numeric")
  
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
    graph.labels.text.v[dat[,names(dat) == measure.var] %>% as.matrix %>% as.vector(.,mode = "numeric") %>% is.na(.)] <- "No Responses"
  
  #Label visibility
    
    graph.labels.alpha.v <- 1 #ifelse(var != 0, 1, 0)  
  
  #Label color for graph.type.e
    
    if(config.graphs.df.g$graph.type.id == "e"){
      graph.labels.color.v <- rep(c("#000000","#FFFFFF"),length(dat[,1])/2) %>% rev
    }else{
      graph.labels.color.v <- rep("#FFFFFF",100)[1:length(dat[,1])]
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

#Add data labels to columns in graph
  #Test Inputs
    #base.graph.input = graph.2
    #data.labels.dat = graph.labels.df
    #label.font.size = 4
    
  AddGraphDataLabels <- function(
    base.graph.input,
    data.labels.dat,
    label.font.size,
    print.graph = FALSE
  ){
    #Checking & Cleaning Parameter Inputs
      if(!(print.graph %in% c(TRUE,FALSE))){
        stop(
          print(
            paste0(
              "print.graph (currently set to: ",print.graph,") must be logical TRUE/FALSE."
            )
          )
        )
      }
    
      label.font.size <- as.numeric(label.font.size)
    
    #Produce Final Result  
      graph.w.datalabels <- 
        base.graph.input +
        geom_text( 
          aes(                                                          
            y = data.labels.dat$graph.labels.heights, 
            x = headers,
            label = data.labels.dat$graph.labels.text,
            #alpha = data.labels.dat$graph.labels.alpha.v,
            group = graphdata.df.g[,1]
          ),
          alpha = data.labels.dat$graph.labels.alpha.v,
          color = data.labels.dat$graph.labels.color,
          size = label.font.size,
          fontface = "bold",
          position = position_dodge(width = 1),
          show.legend = FALSE
        )
    #Return/Print Results  
      if(print.graph){
        windows()
        print(graph.w.cols)
      }
      
    return(graph.w.datalabels)
  }
  
  
  
  
  
  
  
  