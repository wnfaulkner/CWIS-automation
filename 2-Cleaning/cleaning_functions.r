#########################################################
##### 	2-CLEANING FUNCTIONS                        #####
#########################################################









  
##############################################################################################################
#COPIED CODE


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