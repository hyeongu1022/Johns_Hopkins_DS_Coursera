best <- function(state, outcome) {
	setwd("/Users/macintosh/Downloads/rprog_data_ProgAssignment3-data")
	##Read the outcome data
	data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	
	outcome_lst <- c("heart attack", "heart failure", "pneumonia")
	colname_lst <- c(11, 17, 23)
	s <- split(colname_lst, outcome_lst)
	
	##Check that state and outcome are valid
	if(!(state %in% as.list(unique(data$State)))) {
		stop("invalid state")
	}
	
	if(outcome %in% outcome_lst) {
		col_idx <- as.numeric(s[outcome])
	}
	else {
		stop("invalid outcome")
	}
	
	
	##Return hospital name in that state with lowest 30-day mortality rate.
	state_df <- subset(data, State == state)  ##filter only the specified state out.
	mortality_rate <- as.numeric(state_df[, col_idx])
	min <- min(mortality_rate, na.rm=TRUE) ##find minimum mortality rate in specified outcome column
	
	best_hospital <- subset(state_df, state_df[, col_idx]==min, na.rm=TRUE)
	best_hospital <- sort(best_hospital[,2]) ##sort the best hospital names.
	best_hospital <- best_hospital[1] ##choose the very first one 
	
	best_hospital
}