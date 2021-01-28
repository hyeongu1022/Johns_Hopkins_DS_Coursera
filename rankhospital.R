rankhospital <- function(state, outcome, num = "best") {
	setwd("/Users/macintosh/Downloads/rprog_data_ProgAssignment3-data")
	##Read the outcome data
	data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	
	outcome_lst <- c("heart attack", "heart failure", "pneumonia")
	colname_lst <- c(11, 17, 23)
	s <- split(colname_lst, outcome_lst)
	
	##Check that state and outcome are valid
	if(!(state %in% as.list(unique(data$State)))) {
		stop("invalid state") }
	if(outcome %in% outcome_lst) {
		col_idx <- as.numeric(s[outcome]) }
	else {
		stop("invalid outcome") }
		
	##Return hospital name in that state with lowest 30-day mortality rate.
	state_df <- subset(data, State == state)  ##filter only the specified state out.
	rank_df <- state_df[, c(2, col_idx)]
	rank_df[,2] <- as.numeric(rank_df[,2]) ## so that NAs can be detected and removed
	rank_df <- rank_df[complete.cases(rank_df), ]
	rank_df <- rank_df[order(rank_df[,2]), ]
	s <- split(rank_df[,1], rank_df[,2])
	s_sort <- sapply(s, sort)
	rank <- unlist(s_sort) ## this makes two equal numbers (8.7) to 8.71 and 8.72
	rank <- as.list(rank)

		
	if(num == "best") {
		num <- 1 
	}
	else if(num == "worst") {
		num <- length(rank_df[,2])
	}
	else if(num > length(rank_df[,2])) {
		return(NA)
	}
	
	result <- rank[[names(rank)[num]]]
	
	
	result
}