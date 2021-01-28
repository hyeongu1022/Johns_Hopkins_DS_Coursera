complete <- function(directory, id=1:332) {
	setwd(paste("/Users/macintosh/Downloads/", directory,sep=""))
	i_d <- c()
	nobs <- c()
	for(i in id) {
		if(i < 10) {
			ii <- paste("00", i, sep="")	
		}	
		else if(i >= 10 && i < 100) {
			ii <- paste("0", i, sep="")
		}
		else {
			ii <- i
		}
		data <- read.csv(paste(ii, ".csv", sep=""))
		good <- complete.cases(data)
		i_d <- append(i_d, i)
		nobs <- append(nobs, length(good[good==T]))
	}
	id <- i_d
	df <- cbind(id, nobs)
	rownames(df) <- seq(1:length(id))
	df
}