#############################################################
#####       CWIS AUTOMATION CUSTOM FUNCTIONS            #####
#############################################################

source("utils_wnf.r")

  #Loop Expander for creating full config tables
    #Function input testers
      #configs = config.slidetypes.tb
      #loop.varnames = c("slide.loop.var.1","slide.loop.var.2","slide.loop.var.3")
      #collate.varname = "slide.section.1"
      #source.data = resp.long.df.b  
  
  loop.expander.fun <- function(configs, loop.varnames, collate.varname, source.data){
    output.ls <- list()
    
    #c = 4 #LOOP TESTER: NO LOOPS
    #c = 3 #LOOP TESTER: ONE LOOP VAR
    #c = 4 #LOOP TESTER: TWO LOOP VARS
    #for(c in 2:3){
    for(c in 1:dim(configs)[1]){
      
      c.list.c <- list(c=c)
      configs.c <- configs[c,]
      
      #Make data frame with configurations repeated out across all unique combinations of loop.varname(s) in source.data      
      
      loop.varnames.c <- configs[c,names(configs) %in% loop.varnames] %>% 
        as.matrix %>% 
        as.vector %>% 
        remove.na.from.vector()
      
      if(length(loop.varnames.c) == 0){
        output.ls[[c]] <- configs[c,]
        next()
      }
      
      output.ls[[c]] <- 
        unique.combn.from.colnames(resp.long.df.b,loop.varnames.c) %>%
        cbind(configs.c,.)
      
    } ### END OF LOOP "C" BY ROW OF CONFIG INPUT ###
    
    output.df <- rbind.fill(output.ls)
    
    #Collate Report Sub-Sections
    
    manual.order.1 <- 
      output.df$slide.order.1 %>% 
      unique %>% 
      remove.na.from.vector() %>% 
      strsplit(.,",") %>% 
      unlist %>%
      as.data.frame(.) %>%
      replace.names(
        ., 
        current.names = names(.), 
        new.names = "module" #TODO: Not abstracted
      )
    
    
    #full_join(manual.order.1, output.df)
    output.df <- full_join(manual.order.1, output.df)
    output.df <- 
      output.df[
        order(output.df$slide.section.1),
        c(2:length(names(output.df)),1)
        ]
    
    #x<-output.df[
    #  order(
    #    output.df$slide.section.1,        # output.df$module),] %>% full_join(manual.order.1, ., by = "module")#.[match(manual.order.1,output.df$module),]
    #    output.df$module, 
    #    output.df$slide.section.2,
    #    output.df$slide.section.3
    #  )
    #  ,]
    
    
    #if(!missing(collate.varname)){
    
    #Form inputs for collating loop: list with sections (collated and non-colated, in order) 
    #collate.section.configs.df <- vector.value.change.positions.fun(output.df[,collate.varname])
    #collate.ls <- list()
    #for(e in 1:nrow(collate.section.configs.df)){
    #  collate.ls[[e]] <- output.df[collate.section.configs.df$start.position[e]:collate.section.configs.df$end.position[e],]
    #}
    
    ###                                          ###
    # Start of loop 'd' by collated report section #
    ###                                          ###
    
    #The following loop takes as input the list of report slides which have just been broken up into an ordered list of
    #collated and non-collated sections. For sections requiring collation, it will replace the list element with the
    #collated version of the slide configurations.
    
    #d = 2 #LOOP TESTER
    #for(d in 1:length(collate.ls)){  
    #for each unique report section:
    #if it doesn't require collation, do nothing
    #select lines of output.df with only that unique slide.loop.collate.section
    #order by looping variable that has same name as slide.loop.var (e.g. 'school') AND by slide.type.position
    #store in list (to be re-attached) to non-collated sections and other collated sections
    
    #Section to collate for this iteration
    #    collate.input.df.d <- collate.ls[[d]]
    
    #Skip iteration of no collation/re-ordering necessary
    #    if(unique(is.na(collate.input.df.d$slide.loop.collate.section))){
    #      next()
    #    }
    
    #Name of loop variable (will be used to order data-frame in b-loop)
    #    loop.var.d <- collate.input.df.d %>%
    #      select(slide.loop.var) %>%
    #      unlist %>%
    #      unique 
    
    #Create collated data frame to replace un-collated one in slide list
    #    collate.ls[[d]] <- 
    #      collate.input.df.d[
    #        order(
    #          collate.input.df.d %>% select(matches(loop.var.d)), 
    #          collate.input.df.d$slide.type.position
    #        )
    #        ,]
    
    #} #END OF LOOP "d" BY COLLATED SECTION
    
    #output.df <- do.call(rbind, collate.ls)
    
    #} #END OF 'IF' STATEMENT FOR WHEN SOME REPORT SECTIONS REQUIRE COLLATING
    
    return(output.df)
    
  } #END OF LOOP EXPANDER FUNCTION
  
  
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
      x[!(grepl("school", x$slide.loop.var) & (x$school %>% na.sub(.,"")) == "District Office"),] %>% 
        return(.)
    }
  }

  
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
  
      