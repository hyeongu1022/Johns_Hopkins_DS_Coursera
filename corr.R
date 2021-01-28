corr <- function(directory, threshold=0) {
	setwd(paste("/Users/macintosh/Downloads/", directory,sep=""))
	c <- c()
	com_df <- complete(directory)
	for (i in 1:nrow(com_df)) {
		if(com_df[i, "nobs"] > threshold) {
			id <- com_df[i, "id"]
			if(id < 10) {
				id <- paste("00", id, sep="")	
			}	
			else if(id >= 10 && id < 100) {
				id <- paste("0", id, sep="")
			}
			data <- read.csv(paste(id, ".csv", sep=""))
			c <- append(c, cor(data$sulfate, data$nitrate, use="complete.obs"))
		}
	}
	c	
}

