#############################################################
#####       5-object creation functions		            #####
#############################################################

source("utils_wnf.r")

#Graph Base Formation: forms graph object with data, alpha, and theme
  FormBaseGraphObject.DataAndTheme <- function(dat){
    
    graph.base <- 
      
      ggplot( 
        data = dat,
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
  #Test Inputs
    #base.graph.input = graph.1
    #dat = graphdata.df.g
    #graph.group.by.varnames = graph.group.by.varnames
    #graph.fill = graph.fill.g
    #print.graph = TRUE
  
  AddColsToGraph <- function(
    base.graph.input, #a base graph ggplot object with data and alpha defined (e.g. resulting from function above)
    dat, #the graph data frame with x-axis labels in column 1 and bar heights in a column named 'measure.var'
    graph.group.by.varnames, #[for stacked graphs only] a data frame of the grouping variable extracted from input data earlier in code
    graph.fill, #a vector of hex color values with same length as nrow(dat)
    print.graph = FALSE #TRUE/FALSE: if true, prints graph in new window. FALSE = default.
  ){
    #Checking Parameter Inputs
      if(nrow(dat) != length(graph.fill)){
        print(graph.fill)
        stop(
          paste0(
            "Length of graph.fill (", 
            length(graph.fill), 
            ") is different from number of rows in dat (",
            nrow(dat),
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
    
    #Produce Inputs for Final Results
      headers.varname <- graph.group.by.varnames[graph.group.by.varnames != "time.period"]

    #Produce Final Result  
      if(is.null(graph.group.by.varnames)){
        graph.w.cols <-
          base.graph.input +
          
          geom_bar(
            aes(x = dat[,names(dat) == headers.varname], 
                y = measure %>% as.numeric
            ),
            fill = graph.fill,
            alpha = 1,
            position = "dodge", 
            stat = "identity",
            show.legend = FALSE
          )
      }else{ #TODO: WILL LIKELY NEED EDITING WHEN NEED TO PRODUCE STACKED BAR/COLUMN CHARTS AGAIN
        graph.w.cols <-
          base.graph.input +
          
          geom_bar(
            aes(x = dat[,names(dat) == headers.varname], 
                y = measure %>% as.numeric,
                group = dat %>% select(graph.group.by.varnames[1]) %>% unlist %>% as.vector, 
                fill = graph.fill
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
  #NOTE: WHEN WITHIN local() COMMAND AND USING A FUNCTION SOURCED FROM ANOTHER FILE, CANNOT USE OBJECTS
    #NOT DEFINED IN FUNCTION PARAMETERS. THIS FUNCTION USES THE CONFIG TABLE BUT DOES NOT HAVE IT AS A
    #PARAMETER, SO THROWS AN ERROR: 
      #"Error in create.graph.labels.fun(dat = graphdata.df.g, measure.var = "measure.var",  : 
      #object 'config.graphs.df.g' not found"
  
  #Test Inputs
    #dat = graphdata.df.g
    #dat.measure.varname = "measure"
    #height.ratio.threshold = 8.2
    #dat.configs = config.graphs.df.g
  
  create.graph.labels.fun <- function(
    dat, 
    dat.measure.varname, 
    height.ratio.threshold,
    dat.configs
  ){
    
    if(!is.data.frame(as.data.frame(dat))){stop("Input cannot be coerced into data frame.")}
    
    dat <- as.data.frame(dat)
    var <- dat[,names(dat) == dat.measure.varname] %>% as.matrix %>% as.vector(.,mode = "numeric")
    
    if(var %>% is.na %>% all){
      graph.labels.heights.v <- rep(1, length(var)) #Label Heights
      graph.labels.text.v <- rep("No data", length(var)) #Label Text
      above.label.vectorposition <- rep(TRUE, length(var)) #Defines color to make label
    }else{
    
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
        graph.labels.heights.v <- vector(length = length(var), mode = "numeric")
        above.label.vectorposition <- max/var > height.ratio.threshold
        above.label.vectorposition[is.na(above.label.vectorposition)] <- TRUE
        graph.labels.heights.v[above.label.vectorposition] <-   #labels for columns below threshold, position is height of bar plus 1/10 of max bar height 
          var[above.label.vectorposition] + max/10
        graph.labels.heights.v[graph.labels.heights.v == 0] <-    #labels for columns above threshold, position is height of smallest bar divided by 2
          min(var[!above.label.vectorposition])/2
        graph.labels.heights.v[which(is.na(var))] <- max/3  #"No Responses" labels at max/3 height
        
      }
      
    #Label Text
      
      if(grepl("mean",dat.configs$summarize.fun)){
        graph.labels.text.v <- as.character(100*var %>% round(., 2)) %>% paste(.,"%",sep="")
      }else{
        graph.labels.text.v <- 
          var %>% 
          as.numeric %>% 
          round( ., 1) %>% 
          sprintf("%.1f",.) %>% 
          trimws(., which = "both") 
      }
      graph.labels.text.v[
        dat[,
          names(dat) == dat.measure.varname] %>% 
          as.matrix %>% 
          as.vector(.,mode = "numeric") %>% 
          is.na(.) %>% 
          which
      ] <- "No Responses"
    }
  
    #Label visibility
      
      graph.labels.alpha.v <- 1 #ifelse(var != 0, 1, 0)  
    
    #Label color for graph.type.e
      
      if(dat.configs$graph.type.id == "e"){
        graph.labels.color.v <- rep(c("#000000","#FFFFFF"),length(dat[,1])/2) %>% rev
      }else{
        graph.labels.color.v <- rep("#FFFFFF",100)[1:length(dat[,1])]
      }
      graph.labels.color.v[which(graph.labels.text.v %in% c("0%", "No Responses"))] <- "#000000"
      graph.labels.color.v[which(above.label.vectorposition)] <- "#000000"
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
    #graph.headers = headers
    #dat = graphdata.df.g
    #dat.labels = graph.labels.df
    #label.font.size = 4
    #print.graph = TRUE
    
  AddGraphDataLabels <- function(
    base.graph.input,
    dat,
    dat.labels,
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
     
    #Produce Inputs for Final Results
      headers.varname <- graph.group.by.varnames[graph.group.by.varnames != "time.period"]
   
    #Produce Final Result  
      graph.w.datalabels <- 
        base.graph.input +
        geom_text( 
          aes(                                                          
            y = dat.labels$graph.labels.heights, 
            x = dat[,names(dat) == headers.varname],
            label = dat.labels$graph.labels.text,
            group = dat[,1]
          ),
          alpha = dat.labels$graph.labels.alpha.v,
          color = dat.labels$graph.labels.color,
          size = label.font.size,
          fontface = "bold",
          position = position_dodge(width = 1),
          show.legend = FALSE
        )
    #Return/Print Results  
      if(print.graph){
        windows()
        print(graph.w.datalabels)
      }
      
      return(graph.w.datalabels)
  }

#Add Graph Averages (as error bars)
  #Test Inputs
    #base.graph.input = graph.3
    #dat = graphdata.df.g
    #avg.bar.color = config.graphs.df.g$avg.bar.color
    #dat.configs = config.graphs.df.g
    #print.graph = TRUE
    
  AddGraphAverages <- function(
    base.graph.input,
    dat,
    avg.bar.color,
    dat.configs,
    print.graph = FALSE
  ){
     #Produce Inputs for Final Results
        headers.varname <- names(dat)[!grepl("measure", names(dat))]
        headers <- dat[,names(dat) == headers.varname]
        
        #Average bar opacity (alpha)
          dat$avg.alpha <- 
            ifelse(
              is.na(dat.configs$graph.group.by.varnamess)||is.null(dat.configs$graph.group.by.varnamess),
              1,
              rep(c(0.8,0.0),nrow(dat))
            )
     
      #Produce Final Results
        if(dat$measure.var.avg %>% is.na %>% all){
          graph.w.averages <- base.graph.input
        }else{
          graph.w.averages <- 
          
            base.graph.input +
          
            geom_errorbar(
              aes(
                x = headers,
                #group = dat[[graph.cat.varname]],
                ymin = dat$measure.var.avg, 
                ymax = dat$measure.var.avg,
                alpha = dat$avg.alpha
              ), 
              position = position_dodge(width = 1), # 1 is dead center, < 1 moves towards other series, >1 away from it
              color = avg.bar.color, 
              width = 1,
              size = 2,
              #alpha = 1,
              show.legend = FALSE
            )
        }
          
    #Return/Print Results  
      if(print.graph){
        windows()
        print(graph.w.averages)
      }
      
      return(graph.w.averages)
  }
  
  
    
#Final graph formatting & edits: correct category order, finalize orientation as column or bar
  #Test Inputs
    #base.graph.input = graph.4
    #graph.headers = headers
    #dat = graphdata.df.g 
    #dat.configs = config.graphs.df.g
    #print.graph = TRUE
  
  FinalGraphFormatting <- function(
    base.graph.input,
    #graph.headers,
    dat,
    dat.configs,
    print.graph = FALSE
    
  ){
    #List with all potential header vectors in correct order
      #NOTE: SHOULD BE OBSOLETE - NOW SHOULD ALREADY BE DEFINED IN CONFIGS TABLE
      #graph.cat.order.ls <-
      #  list(
      #    data.year = c("Baseline","2017-18"),
      #    school.level = c("Elem.","Middle","High","Mult.","Other"),
      #    role = c("Special Educator","Classroom Teacher","Instructional Coach","School Counselor","School Social Worker","Building Administrator","Other"),
      #    module = c("ETLP", "CFA","DBDM","LEAD","PD"),
      #    ans.text.freq = c("Always","Most of the time","About half the time","Sometimes","Never"),
      #    ans.text.agreement = c("Strongly Agree","Agree","Neutral","Disagree","Strongly Disagree"),
      #    practice = if("practice" %in% names(graphdata.df.g)){graphdata.df.g$practice}else{""} #TODO:When moving this out of loop, will need to generalize for all module practices
      #  )
        
    #Produce Inputs for Final Results
      headers.varname <- names(dat)[!grepl("measure", names(dat))]
      headers <- dat[,names(dat) == headers.varname] 
      
      #Factor vector with levels in order they will need to be to get column/bar ordering right
        #When graphs are bar as opposed to columns, have to reverse order because the coord_flip() 
        #command does a mirror image
        if(dat.configs$graph.type.orientation == "bar"){
          graph.order.g <- headers %>% factor(., levels = headers %>% rev)
        }else{
          graph.order.g <- headers %>% factor(., levels = headers %>% rev)        
        }
    
    #Graph category axis ordering
      graph.w.orderedaxis <- 
        base.graph.input + 
        scale_x_discrete(limits=levels(graph.order.g))
    
    #GRAPH ORIENTATION
      if(dat.configs$graph.type.orientation == "bar"){
        graph.final <- 
          graph.w.orderedaxis +
          coord_flip() +
          theme(
            axis.text.x = element_blank(),
            axis.text.y = element_text(
              size = 20, 
              #family = "Century Gothic",
              color = "#5a6b63",
              hjust = 1)
          )
      }else{
        graph.final <- graph.w.orderedaxis
      }
    
     #Return/Print Results  
      if(print.graph){
        windows()
        print(graph.final)
      }
      
      return(graph.final)
  }
  
#GRAPH CREATION FUNCTION: uses all functions above
  #Test Inputs
    #dat = graphdata.df.g
    
  CreateGraph <- function(
    dat
  ){
   
  }
    
  