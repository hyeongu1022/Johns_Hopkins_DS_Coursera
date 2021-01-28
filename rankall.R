rankall <- function(outcome, num="best") {
	setwd("/Users/macintosh/Downloads/rprog_data_ProgAssignment3-data")
	##Read the outcome data
	data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	
	outcome_lst <- c("heart attack", "heart failure", "pneumonia")
	colname_lst <- c(11, 17, 23)
	s <- split(colname_lst, outcome_lst)
	
	##Check that outcome is valid
	if(outcome %in% outcome_lst) {
		col_idx <- as.numeric(s[outcome]) }
	else {
		stop("invalid outcome") }
		
	##Return hospital name in every states with lowest 30-day mortality rate.
	rank_df <- data[, c(2, 7, col_idx)]
	rank_df[,3] <- as.numeric(rank_df[,3]) ## so that NAs can be detected and removed
	rank_df <- rank_df[complete.cases(rank_df), ]
	s <- split(rank_df[,c(1,3)], rank_df[,2])
	states <- names(s) # contains all states

	
	col_hospital <- c()
	col_state <- c()
	for (st in states) {
	  hospitals <- s[[st]][, 1] #
	  rates <- s[[st]][, 2]
	  spl <- split(hospitals, rates)
	  spl <- lapply(spl, sort)
	  spl <- unlist(spl) # make two 13.1 to 13.11 and 13.12
	  
	  if(num == "best") {
	    n <- 1 
	  }
	  else if(num == "worst") {
	    n <- length(spl)
	  }
	  else {
	    n <- num #if num is neither "best" nor "worst"   
	  }
	  
	  col_hospital <-c(col_hospital, as.character(spl[n])) #append hospital name 
	  col_state <- c(col_state, st) #append state
	}
	
	result_df <- data.frame("hospital" = col_hospital, "State" = col_state)
	
	result_df
}