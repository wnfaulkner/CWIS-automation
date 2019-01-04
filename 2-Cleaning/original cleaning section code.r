 
    {
      #Graph Types Configs Table
      load.config.graph.types.tb <- gs_read(configs.ss, ws = "graph.types", range = NULL, literal = TRUE) #read.xlsx("graph_configs.xlsx", sheetName = "graph.types",header = TRUE, stringsAsFactors = FALSE) 
      config.graph.types.tb <- 
        inner_join(config.slide.types.tb, load.config.graph.types.tb, by = "slide.type.id", all.x = FALSE) 
      
      #Table Types Configs Table
      load.config.table.types.tb <- gs_read(configs.ss, ws = "table.types", range = NULL, literal = TRUE) #read.xlsx("graph_configs.xlsx", sheetName = "table.types",header = TRUE, stringsAsFactors = FALSE) 
       config.table.types.tb <- 
        inner_join(config.slide.types.tb, config.table.types.tb, by = c("slide.type.id")) 
      
      #Piece-of-text (POT) Config Table
      config.pot.tb <- gs_read(configs.ss, ws = "pot.types", range = NULL, literal = TRUE) #read.xlsx("graph_configs_Jason Altman.xlsx", sheetName = "slide.pot.objects",header = TRUE, stringsAsFactors = FALSE) 
      config.pot.tb$color <- 
        config.pot.tb$color %>% 
        gsub("x","",.)
      
      #Questions Configs Table (imported as list)
      questions.ls <- 	gs_read(configs.ss, ws = "questions", range = NULL, literal = TRUE) %>% as.list() %>% lapply(., tolower)
      questions.tb <- do.call(cbind, questions.ls) %>% as.data.frame(., stringsAsFactors = FALSE)
      
      #Buildings Config Table  
      buildings.tb <- 	
        gs_read(configs.ss, ws = "buildings", range = NULL, literal = TRUE) %>% 
        as.list() %>% 
        lapply(., tolower) %>%
        do.call(cbind, .) %>%
        as_tibble()
      
      buildings.tb$report.id <- mgsub("bucahanan","buchanan",buildings.tb$report.id)
    }  
    

 
    #Restrict questions.tb to only rows for this data.year/data.semester
      questions.sem.df <- 
        questions.tb[
          questions.tb$data.year == data.year & questions.tb$data.semester == data.semester,
        ]
      
    #Add "x" to questions.sem.df$row.1 so they match exactly with Qualtrics export as imported by R
      questions.sem.df$row.1[NumSubstringMatches("_",questions.sem.df$row.1) == 2] <- 
        paste("x",questions.sem.df$row.1[NumSubstringMatches("_",questions.sem.df$row.1) == 2],sep="")
    
    #Remove extra header rows
      dat.startrow <- 
        ifelse(
          any(substr(resp1.df[,1],1,1) == "{"),
          which(substr(resp1.df[,1],1,1) == "{") + 1,
          1
        )
      resp1.df <- resp1.df[dat.startrow:length(resp1.df[,1]),]
    
    #Lower-case all variable names
      names(resp1.df) <- resp1.df %>% names %>% tolower 
    
    #Variable renaming of important variables
      names(resp1.df) <-
        mgsub(
          questions.sem.df$row.1[!is.na(questions.sem.df$q.changename)], 
          questions.sem.df$q.changename[!is.na(questions.sem.df$q.changename)], 
          names(resp1.df)
        )
    
    #Define Report Unit and Report IDs
      if(!report.unit %in% c("building","district")){
        stop("Report unit must be either 'building' or 'district.'")
      }
      
      if(report.unit == "building"){
        report.id.colname <- "building"
      }else{
        report.id.colname <- "district"
      }
      
      report.id.col <- resp1.df[,names(resp1.df) == report.id.colname]
      
      if(report.id.colname %in% c("building","district")){
        
        if(report.id.colname == "district"){
          resp1.df <-
            resp1.df %>%
            mutate(
              report.id =
                resp1.df[,grep(report.id.colname,names(resp1.df))] %>% tolower
            )
        }
        
        if(report.id.colname == "building"){
          resp1.df <- 
            resp1.df %>%
            mutate(
              report.id =
                paste(
                  resp1.df[,names(resp1.df) %in% "district"],
                  resp1.df[,names(resp1.df) %in% report.id.colname],
                  sep = "_"
                ) %>%
                tolower %>%
                replace(., . == "_", "")
            )
        }
        
        resp1.df$report.id <- gsub("\\/"," ",resp1.df$report.id) #in case there is a slash in the school name itself, this replaces it so file storage for ppt works properly
        
        report.ids.all <- #report ids filtering out district office, ids without a district, or blanks
          resp1.df$report.id %>%
          unique %>% 
          FilterVector(
            condition = !grepl("district office",.),
            vector.input = .
          ) %>%
          .[!grepl("_",substr(.,1,1))] %>%
          .[. != ""]
        
      }else{}   #If user has designated district names as "all", code will create reports for all district names present in the data
      
      #Restrict Responses table to produce only a sample of reports unless this is final print
        #Notes: code selects whole districts at random so that it will generate all reports for those 
        #districts. This is important so that you don't get a bunch of scattered districts and the 
        #district averages aren't realistic. The code samples district combinations until it finds one
        #where the number of report ids is equal to the user-defined sample size.
      
        if(sample.print & report.unit == "building"){ #TODO: generalize so will work if report.unit is district
            
          building.counts.df <- 
            resp1.df %>% 
            group_by(district) %>% 
            dplyr::summarize(n_buildings = n_distinct(building)) %>% 
            as.data.frame()
          
          report.ids.sample <- ""
          report.districts.sample <- ""
          unique.report.ids <- resp1.df$report.id %>% unique()
          
          i <- 1
          
          while(length(report.ids.sample) != sample.size){            
            
            report.districts.sample <-
              c(
                report.districts.sample,
                sample(resp1.df$district %>% unique,1)
              ) %>% 
              .[. != ""] %>% 
              tolower
            
            report.ids.sample <- 
              unique.report.ids[grep(paste(report.districts.sample,collapse = "|"),unique.report.ids)] %>%
              .[. != ""] %>% .[!grepl("district office", .)]
            
            if(length(report.ids.sample) > sample.size){
              report.districts.sample <- ""
              report.ids.sample <- ""
            }
            #print(i)
            #print(length(report.ids.sample))
            i = i+1
          }
        }
        
        if(!sample.print & report.unit == "building"){
          report.ids.sample <- report.ids.all
        }
        
        #Restrict response dataset to rows for generated report ids
          resp2.df <- resp1.df[resp1.df$report.id %in% report.ids.sample,]
      
    #Remove extraneous variables
      remove.colnames <- 
        questions.tb %>% 
        filter(tolower(necessary.in.final.data) == "no") %>%
        filter(data.year == data.year) %>%
        filter(data.semester == data.semester) %>%
        select(row.1) %>%
        unlist %>%
        RemoveNA
      
      resp2.df <-
        resp2.df[ , !(names(resp2.df) %in% remove.colnames)]
          
      
  #STACKING COLUMNS SPLIT BY SURVEY BRANCHING
    #"branch" refers to branching questions, so branch0 are columns without branches, and branch1 are columns that are part of branching questions
    #"ans" refers to having answer options, so ans0 are columns without answer options, and ans1 are columns that are part of questions with multiple answer options
    
    branch0.ans0.colnames.v <- names(resp2.df)[NumSubstringMatches("_",names(resp2.df))==0]
    branch0.ans1.colnames.v <- names(resp2.df)[NumSubstringMatches("_",names(resp2.df))==1 & substr(names(resp2.df),1,1) == "q"]
    branch1.ans0.colnames.v <- names(resp2.df)[NumSubstringMatches("_",names(resp2.df))==1 & substr(names(resp2.df),1,1) == "x"]
    branch1.ans1.colnames.v <- names(resp2.df)[NumSubstringMatches("_",names(resp2.df))==2]
    
    #branch.q.colnums.v <- which(resp2.df %>% names %>% substr(.,1,1) == "x")
    
    #Make data frame of base variables (that require no stacking)  
      branch0.df <- 
        resp2.df[ ,                                           
                  names(resp2.df) %in% c("responseid",branch0.ans0.colnames.v,branch0.ans1.colnames.v)      
                  ]              
    
    #Make data fram of variables to be stacked
      branch1.df <- 
        resp2.df[ ,
                  names(resp2.df) %in% c("responseid",branch1.ans0.colnames.v,branch1.ans1.colnames.v) # ResponseId plus all columns whose names begin with "X"
                  ]
      
    #Re-stack & collapse columns that are split up because of survey branching 
    
      #Base names of questions that have multiple branches
      branch1.q.v <- 
        strsplit(c(branch1.ans1.colnames.v,branch1.ans0.colnames.v), "_") %>% 
        unlist %>% 
        .[grep("q",.)] %>% 
        unique 
      
      ###                                                    ###
      # Start of loop 'a' by base question of branched columns #
      ###                                                    ###
      
      #Loop output storage
      q.ls <- list()
      varname.match.ls <- list()
      
      #a <- 1 #for testing loop
      for(a in 1:length(branch1.q.v)){     ### START OF LOOP BY QUESTION; only for questions with branched variables
        
        q.name.a <- branch1.q.v[a]                                 # base question name
        varnames.a <-                                              # all columns in branch0.df that belong to base question number
          names(branch1.df)[names(branch1.df) != "responseid"][
            which(names(branch1.df)[names(branch1.df) != "responseid"] %>% strsplit(.,"_") %>% lapply(., `[[`, 2) %>% unlist == q.name.a)
            ]
        
        if(NumSubstringMatches("_",varnames.a) %>% unique() == 1){ # final column names once branching is collapsed
          q.ans.options.a <- q.name.a
        }else{}
        
        if(NumSubstringMatches("_",varnames.a) %>% unique() == 2){
          q.ans.options.a <-
            paste(q.name.a, 
                  varnames.a %>% strsplit(.,"_") %>% lapply(., `[[`, 3) %>% unique %>% unlist,
                  sep = "_")
        }        
        varname.match.ls[[a]] <- q.ans.options.a
        
        unbranch.a.ls <- list()
        
        ###                                                    ###
        # Start of loop 'b' by base question of branched columns #
        ###                                                    ###
        
        #b = 12
        for(b in 1:length(q.ans.options.a)){    ### START OF LOOP BY ANSWER OPTION
          
          check.varnames.a <- str_sub(names(branch1.df), start = -nchar(q.ans.options.a[b]))
          
          branch.df.b <- #data frame of only columns to be stacked (for this question, for this answer option)
            branch1.df[,
                       c(
                         grep("responseid",names(branch1.df)),
                         grep(q.ans.options.a[b], check.varnames.a)
                       )
                       ]
          
          unbranch.df.b <- #reshaped data frame of stacked columns
            reshape(
              data = branch.df.b, 
              idvar = "responseid",
              timevar = NULL,
              varying = names(branch.df.b)[names(branch.df.b) != "responseid"],
              v.names = q.ans.options.a[b],
              direction = "long"
            ) %>% 
            filter(.[,2] != "") #remove rows with blank answers
          
          unbranch.a.ls[[b]] <- unbranch.df.b
          
        } ### END OF LOOP BY ANSWER OPTION
        
        q.dat.df <- unbranch.a.ls %>% Reduce(function(x, y) full_join(x,y, all = TRUE), .)
        q.ls[[a + 1]] <- q.dat.df
        
      } ### END OF LOOP BY QUESTION
      
    #Re-merge with non-branched variables
      q.ls[[1]] <- branch0.df
      #q.ls <- lapply(q.ls, tolower)
      resp2.df <- q.ls %>% Reduce(function(x, y) full_join(x,y, all = TRUE), .) 
      
  
  #Re-do question table so no extraneous rows for roles that are now unbranched  
    q.unbranched.df <- 
      questions.sem.df[
        questions.sem.df$row.1 %in% 
          c(
            branch0.ans0.colnames.v,
            branch0.ans1.colnames.v,
            varname.match.ls %>% unlist %>% paste("1_",.,sep="")
          )
        ,
        ] #TODO:looks like still uneven numbers - some columns must be missing from questions table, but seems to be columns we don't care about.
    
    q.unbranched.df$row.1[grep("q",q.unbranched.df$row.1)] <- 
      str_extract(
        q.unbranched.df$row.1[grep("q",q.unbranched.df$row.1)], 
        "q[0-9].+"
      )
    
    q.unbranched.df <-
      SplitColReshape.ToLong(
        df = q.unbranched.df,
        id.varname = "row.1",
        split.varname = "module",
        split.char = ","
      )

# 2-CLEANING ROUND 1 (UNBRANCHING) OUTPUTS --------
  #resp2.df - now with all branch variables 'unbranched' (stacked), and having removed two extra header rows
  #questions.sem.df - now only with rows pertaining to this data.year and data.semester
  #config.ans.opt.tb - global answer options table with numerical scale, agreement scale, and frequency scale lined up


# 3-CLEANING ROUND 2 (ADDING USEFUL VARIABLES) ------------------------------
  
  #Lower-Case All Data
    resp3.df <- apply(resp2.df,c(1:2),tolower) %>% as.data.frame(., stringsAsFactors = FALSE)
  
  #Recode role variable
    resp3.df$role <- mgsub("Teacher", "Classroom Teacher", resp3.df$role)
  
  #Recode school & district names
    resp3.df$building <- mgsub("bucahanan","buchanan",resp3.df$building)
    resp3.df$district <- mgsub("bucahanan","buchanan",resp3.df$district)
  
  #Capitalize First Letter of character variables
    resp3.df[,names(resp3.df) %in% c("data.year","role","district","school","school.level")] <- 
      apply(resp3.df[,names(resp3.df) %in% c("data.year","role","district","school","school.level")], 2, FirstLetterCap_MultElements)
  
  
  #Rearrange data columns
    resp3.df <- resp3.df[,   # CWIS response variables last, others first
                         c(which(!grepl("_", names(resp3.df))),
                           grep("_", names(resp3.df)))
                         ]
  
    resp3.df <-  resp3.df[, #Put "responseid" in first column
                          c(grep("responseid", names(resp3.df)),which(!grepl("responseid", names(resp3.df))))
                          ]
    
  #Remove rows with no district or building name
    resp3.df <- resp3.df %>% filter(report.id != "_")
  
  #Add useful variables for analysis 
  
    #Useful vectors for selecting cwis answer variables
      cwis.vars.v <- which(names(resp3.df) %in% q.unbranched.df$row.1[!is.na(q.unbranched.df$module)])
      cwis.varnames.v <- names(resp3.df)[names(resp3.df) %in% q.unbranched.df$row.1[!is.na(q.unbranched.df$module)]]
      cwis.modules.v <- 
        questions.sem.df$module[!is.na(questions.sem.df$module)] %>% 
        unique %>% 
        strsplit(.,"\\/") %>% 
        unlist %>% 
        unique
    
#FUN  #Recode answer option variables as numeric
      numeric.recode.fun <- 
        function(x){
          recode(
            x,
            `always` = 5,
            `most of the time` = 4,
            `about half the time` = 3,
            `sometimes` = 2,
            `never` = 1,
            `strongly agree` = 5,
            `agree` = 4,
            `neutral` = 3,
            `neither agree nor disagree` = 3,
            `neither agree or disagree` = 3,
            `disagree` = 2,
            `strongly disagree` = 1
          )
        }
      
      recode.ansopt.varnames.v <- 
        which(resp3.df %>% 
                apply(., 2, unique) %>%
                sapply(., function(x) {
                  x %in% c("always","most of the time","about half the time","sometimes","never","strongly agree","agree","neutral","disagree","strongly disagree")
                }) %>%
                sapply(., any)) %>%
       names(resp3.df)[.]
      
      num.ansopt.vars.df <- 
        apply(resp3.df[,names(resp3.df) %in% recode.ansopt.varnames.v], 
              2,
              numeric.recode.fun
        ) %>% 
        as.data.frame
      
      names(num.ansopt.vars.df) <- paste(names(resp3.df[,names(resp3.df) %in% recode.ansopt.varnames.v]),"_num", sep = "")
    
#FUN  #Recode Original Answers to add numbers (e.g. "Always" becomes "1. Always")
      addnums.recode.fun <- 
        function(x){
          recode(
            x,
            `always` = '5. always',
            `most of the time` = '4. most of the time',
            `about half the time` = '3. about half the time',
            `sometimes` = "2. sometimes",
            `never` = "1. never",
            `strongly agree` = "5. strongly agree",
            `agree` = "4. agree",
            `neutral` = "3. neutral",
            `neither agree nor disagree` = "3. neutral",
            `neither agree or disagree` = "3. neutral",
            `disagree` = "2. disagree",
            `strongly disagree` = "1. strongly disagree"
          )
        }
      
      recode.addnums.df <- 
        apply(resp3.df[,names(resp3.df) %in% recode.ansopt.varnames.v], 
              2,
              addnums.recode.fun
        ) %>% 
        as.data.frame
      
    #Create 'implementation' binary variables
      binary.ansopt.vars.df <- apply(num.ansopt.vars.df,c(1:2),function(x){ifelse(x >= 3.5,1,0)}) %>% as.data.frame
      names(binary.ansopt.vars.df) <- paste(names(resp3.df[,names(resp3.df) %in% recode.ansopt.varnames.v]),"_binary",sep = "")
    
    #Convert numeric variables in original data to numeric
      resp3.df[,names(resp3.df) %in% NumericVarnames(resp3.df)] <-
        apply(
          resp3.df[,names(resp3.df) %in% NumericVarnames(resp3.df)],
          c(1:2),
          as.numeric
        )
    
    #Converting Slider variables to numeric and binary (according to different max/min/thresholds)
      slider.vars.df <- 
        resp3.df[,
                 names(resp3.df) %in% q.unbranched.df$row.1[!is.na(q.unbranched.df$var.min)]
                 ]
      
      slider.binary.vars.ls <- list()
      slider.num.vars.ls <- list()
    
      for(c in 1:ncol(slider.vars.df)){
        
        colname.c <- names(slider.vars.df)[c]
        
        var.min.c <- 
          q.unbranched.df$var.min[q.unbranched.df$row.1 == colname.c][!is.na(q.unbranched.df$var.min[q.unbranched.df$row.1 == colname.c])] %>% 
          .[1] %>%                 #Once did SplitColReshape on the questions table, have two rows for some questions so have to select first one only.
          as.character %>% 
          as.numeric
        
        var.max.c <- 
          q.unbranched.df$var.max[q.unbranched.df$row.1 == colname.c][!is.na(q.unbranched.df$var.max[q.unbranched.df$row.1 == colname.c])] %>% 
          .[1] %>%
          as.character %>% 
          as.numeric
        
        if(var.min.c == 1 & var.max.c == 5){
          cuts <- c(1.5,2.5,3.5,4.5)
          slider.binary.threshold.c <- 3.5
        }
        
        if(var.min.c == 1 & var.max.c == 10){
          cuts <- seq(
            from = var.min.c,
            to = var.max.c, 
            #by = ((to - from)/(length.out - 1)), 
            length.out = 5)[1:4]
          slider.binary.threshold.c <- 7
        }
        
        if(length(var.min.c == 1 & var.max.c == 5) > 1){
          print(c)
        }
        slider.num.vars.ls[[c]] <- findInterval(slider.vars.df[,c], cuts)+1
        slider.binary.vars.ls[[c]] <- ifelse(slider.vars.df[,c] >= slider.binary.threshold.c, 1, 0)
      }
      
      slider.num.vars.df <- 
        do.call(cbind, slider.num.vars.ls) %>% 
        as.data.frame %>%
        ReplaceNames(
          df = .,
          current.names = names(.),
          new.names = names(slider.vars.df)
        ) 
      
      slider.agreement.varnames.v <- 
        q.unbranched.df$row.1[q.unbranched.df$var.type == "continuous" & q.unbranched.df$scale.type == "agreement"] %>%
        RemoveNA()
      
      slider.freq.varnames.v <-
        q.unbranched.df$row.1[q.unbranched.df$var.type == "continuous" & q.unbranched.df$scale.type == "frequency"] %>%
        RemoveNA()
      
      slider.agreement.df <-
        slider.num.vars.df[,names(slider.num.vars.df) %in% slider.agreement.varnames.v] %>%
        apply(., 2, function(x){
          mgsub(
            pattern = c(5:1),
            replacement = paste(config.ans.opt.tb$ans.num,". ",config.ans.opt.tb$ans.text.agreement, sep = "") %>% tolower,
            x = x
          )
        }) %>%
        as.data.frame()
      
      slider.freq.df <-
        slider.num.vars.df[,names(slider.num.vars.df) %in% slider.freq.varnames.v] %>%
        apply(., 2, function(x){
          mgsub(
            pattern = c(5:1),
            replacement = paste(config.ans.opt.tb$ans.num,". ",config.ans.opt.tb$ans.text.freq, sep = "") %>% tolower,
            x = x
          )
        }) %>%
        as.data.frame()
      
      slider.text.df <- 
        cbind(slider.agreement.df, slider.freq.df) %>%
        ReplaceNames(
          df = .,
          current.names = names(.),
          new.names = paste(names(.),"_text",sep="")
        )
      
      slider.binary.vars.df <- 
        do.call(cbind, slider.binary.vars.ls) %>% 
        as.data.frame %>%
        ReplaceNames(
          df = .,
          current.names = names(.),
          new.names = paste(names(slider.vars.df),"_binary",sep="")
        )
    
  #Create final data frames: 1. Wide; 2. Long for original CWIS data; 3. Long for impbinary data (both long include all original id variables)
    resp.wide.df <- 
      cbind(
        resp3.df[,setdiff(names(resp3.df),names(recode.addnums.df))], 
        recode.addnums.df, 
        num.ansopt.vars.df, 
        binary.ansopt.vars.df,
        slider.num.vars.df %>% ReplaceNames(df = ., current.names = names(.), new.names = paste(names(.),"_num",sep = "")),
        slider.binary.vars.df,
        slider.text.df
      )
    resp.long.df <- 
      melt(
        data = resp.wide.df,
        id.vars = names(resp.wide.df)[!grepl("q[0-9]",names(resp.wide.df))],
        variable.name = "question",
        value.name = "answer",
        stringsAsFactors = FALSE
      )
    resp.long.df$question <- as.character(resp.long.df$question)
    
    #filter.varnames.v <- resp.long.df$question %>% unique %>% FilterVector(grepl("num|binary",.),.) %>% str_extract(., "q[0-9]*_[0-9]*") %>% unique
    #c(branch1.ans0.colnames.v,branch1.ans1.colnames.v) %>% gsub("x[0-9]*\\_","",.) %>% unique      
    #resp.long.df <- 
    #  resp.long.df[!resp.long.df$question %in% filter.varnames.v,]
    
  #Creating additional useful variables for long data frames
  
  #Variable for module
  resp.long.df$q.original <- 
    paste(
      str_extract(resp.long.df$question, "q[0-9]*"),
      ifelse(is.na(str_extract(resp.long.df$question,"_[0-9]")),"",str_extract(resp.long.df$question, "_[0-9]*")),
      sep = ""
    )
  resp.long.df <- 
    left_join(
      resp.long.df, 
      q.unbranched.df[,names(q.unbranched.df) %in% c("row.1","module","practice")], 
      by = c("q.original" = "row.1")
    )  
  
  #Later will matter that we have "cfa,etlp" value in module variable for forming graph data.
  resp.long.df <-
    SplitColReshape.ToLong(
      df = resp.long.df,#[grep(",",resp.long.df$module),],
      id.varname = "responseid",
      split.varname = "module",
      split.char = ","
    ) 
  
  #Variable designating questions to be used in implementation calculations (binary)
    resp.long.df$impbinary <- ifelse(grepl("binary",resp.long.df$question),1,0)
  
  #Variable designating questions to be used in average performance calculations
  #avg.perf.q.varnames.v <-
  #  c(grepl("_num",  )
  #    q.unbranched.df$row.1[!is.na(q.unbranched.df$var.type) & q.unbranched.df$var.type == "integer"],
  #    paste(names(slider.num.vars.df),"_num",sep="")
  #  )
  
  resp.long.df$avg.perf.q <- ifelse(grepl("_num", resp.long.df$question), 1, 0)
  
  #Variable designating questions to be used in tables (bucketing)
  table.q.varnames.v <- 
    c(
      q.unbranched.df$row.1[!is.na(q.unbranched.df$var.type) & q.unbranched.df$var.type == "integer"],
      names(slider.text.df)
    )
  
  resp.long.df$table.q <- ifelse(resp.long.df$question %in% table.q.varnames.v, 1, 0)
  
  #Loop: state average implementation rates [a], results in imp.state.df
  #imp.state.df <- data.frame(cwis.module = cwis.modules.v)
  #a <- 1 # LOOP TESTER
  #for(a in 1:length(cwis.modules.v)){
  #  imp.state.df$imp.rate[imp.state.df[,1]==cwis.modules.v[a]] <- 
  #    impbinary.df[,grep(cwis.modules.v[a], names(impbinary.df))] %>%
  #    apply(., 2, function(x){mean(x, na.rm = TRUE)}) %>% 
  #    mean() 
  #}
  
  
  #Establish Outputs Directory
    if(sample.print){
      outputs.dir <- 
        paste(
          working.dir,
          "5_outputs/",
          gsub(":",".",Sys.time()), 
          sep = ""
        )
    }else{
      outputs.dir <- 
        paste(
          working.dir,
          "5_outputs/",
          gsub(":",".",Sys.time()),
          "_FULL PRINT",
          sep = ""
        )
    }
  
    dir.create(
      outputs.dir,
      recursive = TRUE
    )
    
    setwd(outputs.dir)
  
  #Write Unbranched Data to Excel File
      unbranched.file.name <- 
        paste( 
          "widedata_",
          gsub(":",".",Sys.time()),
          ".csv", 
          sep=""
        )
    
    write.csv(
      resp.wide.df,
      file = unbranched.file.name,
      row.names = FALSE
    )