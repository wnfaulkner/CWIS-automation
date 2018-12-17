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
        RemoveNA()
      
      if(length(loop.varnames.c) == 0){
        output.ls[[c]] <- configs[c,]
        next()
      }
      
      output.ls[[c]] <- 
        UniqueCombnFromColnames(resp.long.df.b,loop.varnames.c) %>%
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
    #collate.section.configs.df <- VectorValueChangePositions(output.df[,collate.varname])
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
      x[!(grepl("school", x$slide.loop.var) & (x$school %>% SubNA(.,"")) == "District Office"),] %>% 
        return(.)
    }
  }
  
#Data restriction - district vs. report.id
  
  #TODO:NEED TO GENEARALIZE: IF REPORT.UNIT IS DISTRICT AND GRAPH DATA.LEVEL IS DISTRICT, THIS WORKS, BUT NOT IF REPORT.UNIT IS 
  #report.id AND DATA.LEVEL IS DISTRICT.
  
  graph.data.restriction.fun <- function(x){
    
    if(config.graphs.df.d$data.level == "district"){
      y <- x
    }
    
    if(config.graphs.df.d$data.level == "building"){
      y <- 
        x %>% 
        filter(report.id == report.id.c) 
    }
    
    if(is.na(config.graphs.df.d$data.restriction)){
      result <- y
    }
    
    if(!is.na(config.graphs.df.d$data.restriction)){
      result <- 
        y %>% 
        filter(
          y[,names(y) == config.graphs.df.d$data.restriction] == 
            config.graphs.df.d[,names(config.graphs.df.d) == config.graphs.df.d$data.restriction]
        )
    }
    
    return(result)
  }
  
#Data Summarize - participation vs. implementation vs. performance 
  #Test inputs
  #config.input <- config.graphs.df.d
  #data.input <-  resp.long.df.c %>% graph.data.restriction.fun %>% group_by(!!! syms(group_by.d))
  
  summarize.data.fun <- function(config.input, data.input){
    if(config.input$data.measure == "participation"){
      result <- 
        dplyr::summarize(data.input, measure.var =  length(unique(responseid)))
    }
    
    if(config.input$data.measure == "implementation"){
      result <- 
        data.input %>% 
        filter(impbinary == 1) %>%
        dplyr::summarize(measure.var = mean(as.numeric(answer), na.rm = TRUE)) %>%
        as.data.frame(., stringsAsFactors = FALSE)
    }
    
    if(config.input$data.measure == "performance"){
      result <- data.input %>%
        filter(impbinary == 0, !is.na(answer)) %>%
        dplyr::summarize(measure.var = as.character(length(unique(responseid))))
    }
    
    if(config.input$data.measure == "average performance"){
      result <- 
        data.input %>%
        filter(grepl("_num",question)) %>%
        dplyr::summarize(., measure.var =  mean(as.numeric(answer), na.rm = TRUE))
    }
    
    return(result)
  }
  
#Restriction function for graph average data
  
  #TODO: THESE TWO FUNCTIONS ARE VERY SIMILAR TO THE ONES ABOVE WHICH HAVE BEEN CHANGED SO NOW NEED TO SPECIFY "config.input" BUT
  #   HAVE NOT MADE THOSE CHANGES HERE YET. PROBABLY COULD ROLL UP INTO ONE OR TWO FUNCTIONS.
  
  #Test Inputs
  #x <- resp.long.df
  
  avg.data.restriction.fun <- function(x){
    
    if(config.graphs.df.d$data.level == "district"){
      y <- x
    }
    
    if(config.graphs.df.d$data.level == "building"){
      y <- 
        x %>% 
        filter(district == unique(resp.long.df$district[resp.long.df$report.id == report.id.c])) 
    }
    
    z <- y %>% filter(!is.na(y[,names(y)==group_by.d])) #TODO:Might want to make flexible - i.e. add a parameter which allows user to include NA
    
    if(!config.graphs.df.d$data.restriction=="module" | is.na(config.graphs.df.d$data.restriction)){ 
      #TODO:Should look into a better way to deal with this restriction, think about input tables
      result <- z
    }
    
    if(config.graphs.df.d$data.restriction=="module" & !is.na(config.graphs.df.d$data.restriction)){
      result <- 
        z %>%
        filter(
          z[,names(z)==config.graphs.df.d$data.restriction] == 
            config.graphs.df.d[,names(config.graphs.df.d)==config.graphs.df.d$data.restriction]
        )
    }
    
    return(result)
  }
  
#Summary Function for Graph Averages
  #Test Inputs
  #x<-resp.long.df %>% avg.data.restriction.fun(.) %>% group_by(!!! syms(group_by.d))
  
  summarize.avg.fun <- function(x){
    
    if(config.graphs.df.d$data.measure == "participation"){
      result <- x %>%
        dplyr::summarize(avg = length(unique(responseid))/length(unique(school.id)))#participation
    }
    
    if(config.graphs.df.d$data.measure == "implementation"){
      result <- x %>% 
        filter(.,impbinary == 1) %>%
        dplyr::summarize(., avg = mean(as.numeric(answer), na.rm = TRUE))#implementation
      
    }
    
    if(config.graphs.df.d$data.measure == "performance"){
      result <- x %>%
        filter(impbinary == 0, !is.na(answer)) %>%
        dplyr::summarize(avg = length(unique(responseid))/length(unique(school.id)))
    }
    
    if(config.graphs.df.d$data.measure == "average performance"){
      result <- 
        x %>%
        filter(grepl("_num",question)) %>%
        dplyr::summarize(., measure.var.avg =  mean(as.numeric(answer), na.rm = TRUE))
    }
    
    return(result)
  }

#Filtering Table Data 
  #TODO:NEED TO GENEARALIZE: IF REPORT.UNIT IS DISTRICT AND table DATA.LEVEL IS DISTRICT, THIS WORKS, BUT NOT IF REPORT.UNIT IS 
  #BUILDING AND DATA.LEVEL IS DISTRICT.
  
  table.data.filter.fun <- function(x){
    
    if(is.na(config.tables.df.d$module)){
      y <- x
    }else{
      y <- x %>% filter(module == config.tables.df.d$module) %>% filter()
    }
    
    y <- y %>% filter(table.q == 1) 
    
    if(is.na(config.tables.df.d$filter)){ #
      result <- y
    }else{
      
      if(!config.tables.df.d$filter %in% c("building","district")){
        stop("Configuration 'filter' is neither 'building' nor 'district.' Check input.")
      }
      
      if(config.tables.df.d$filter == "building"){
        result <- y %>% filter(report.id == report.id.c)
      }
      
      if(config.tables.df.d$filter == "district"){
        result <- y %>% filter(district == district.c)
      }
      
    }
    return(result)
  }
  
#Data Summarize - participation vs. implementation vs. performance 
  #Test inputs
  #config.input <- config.tables.df.d
  #data.input <-  resp.long.df.c %>% table.data.filter.fun %>% group_by(!!! syms(config.tables.df.d$summary.var))
  
  summarize.data.fun <- function(config.input, data.input){
    #na.replace <- function(x, na.replacement){x[is.na(x)] <- na.replacement} #TODO:This didn't work, but may not need after generalizing.
    
    result.1 <- melt(data.input, id.vars = names(data.input)) 
    
    if(d == 1){ #TODO:Needs to be generalized - right now just uses number of loop but should be based on configs
      result <-
        reshape2::dcast(
          data = result.1,
          formula = role ~ report.id,
          value.var = "responseid",
          fun.aggregate = function(x){length(unique(x))}
        ) %>%
        right_join(
          ., 
          all.cats.ls.d$y, 
          by = c("role" = "all.cats")
        ) %>%
        filter(role != "District Administrator") %>%
        .[c(2,3,5,6,7,4,8,1),] %>%
        #.[c(1,2,3,4,6,7,8,5),] %>%
        ReplaceNames(
          df = .,
          current.names = report.id.c,
          new.names = "num. responses"
        ) %>%
        ReplaceNames(
          df = .,
          current.names = names(.),
          new.names = FirstLetterCap_MultElements(names(.))
        ) %>%
        rbind(
          .,
          c("Total",sum(select(., "Num. Responses"), na.rm = TRUE))
        )
      result[is.na(result)] <- 0
      
      return(result)
    }else{
      
      #Draft table (have to merge with all.cats to make sure have every column and row represented)
      result.2 <- 
        reshape2::dcast(
          data = result.1, 
          formula = 
            unlist(data.input[names(data.input) == config.tables.df.d$y.varname]) ~ 
            unlist(data.input[names(data.input) == config.tables.df.d$x.varname]),#syms(paste(config.input$x.var,"~",config.input$y.var,sep="")), 
          value.var ="responseid",
          fun.aggregate = length
        ) %>% 
        ReplaceNames(
          df = .,
          current.names = "unlist(data.input[names(data.input) == config.tables.df.d$y.varname])",
          new.names = "all.cats"
        ) 
      
      #Add all.cats to rows (y axis) 
      result.3 <- 
        right_join(
          result.2, 
          all.cats.ls.d$y, 
          by = "all.cats"
        )
      
      ##Add all.cats to columns (x axis)
      missing.cats <- unlist(all.cats.ls.d$x)[!unlist(all.cats.ls.d$x) %in% names(result.3)] %>% as.character
      
      result.4 <- 
        matrix(
          ncol = length(missing.cats),
          nrow = dim(result.3)[1]
        ) %>%
        as_tibble() %>%
        ReplaceNames(
          df = .,
          current.names = names(.),
          new.names = missing.cats
        ) %>%
        cbind(result.3, .) %>%
        OrderDfByVar(
          df = .,
          order.by.varname = "all.cats",
          rev = TRUE
        ) %>%
        ReplaceNames(
          df = .,
          current.names = "all.cats",
          new.names = FirstLetterCap_OneElement(config.tables.df.d$y.varname)
        )
      
      return(result.4)
    }
  }    