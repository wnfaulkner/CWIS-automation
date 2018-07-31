#Convert classes in data frame back into correct class
  #1. Display names of data.frame with class they are currently
  #2. Prompt for classes of each column; have option to just say 'as-is' (already in correct format)
  #3. As each column is entered, bind and display with column names
  #4. Once all entered, convert to 
  
  test.df <- data.frame(
	character = c("a","ab","222","x")
	)
  
  colclass.v <- c(	"character",
				   "numeric",
				   rep("character",8),
				   "numeric",
				   rep("character",3),
				   "numeric",
				   "date",
				   "numeric",
				   "character",
				   "character",
				   "numeric"					
  )

  cbind(names(byscore.df), colclass.v)

  progress.bar.l <- txtProgressBar(min = 0, max = 100, style = 3)
  progress.bar.l.max <- ncol(byscore.df)					

  for(l in 1:ncol(byscore.df)){ #START OF LOOP BY COLUMN
	
	#Character variables
	if(colclass.v[l] == "character"){
	  byscore.df[,l] <- as.character(byscore.df[,l])
	}else{}
	
	#Numeric variables
	if(colclass.v[l] == "numeric"){
	  byscore.df[,l] <- as.numeric(levels(byscore.df[,l]))[byscore.df[,l]]
	}else{}
	
	#Date variables
	if(colclass.v[l] == "date"){
	  byscore.df[,l] <- as.numeric(as.character(byscore.df[,l]))
	  byscore.df[,l] <- as.Date(byscore.df[,l], origin = "1899-12-30")
	}else{}
	
	setTxtProgressBar(progress.bar.l, 100*l/progress.bar.l.max)	
	
  } #END OF LOOP BY COLUMN
  
  close(progress.bar.l)


      