pollutantmean <- function(directory, pollutant, id=1:332) {
	setwd(paste("/Users/macintosh/Downloads/", directory,sep=""))
	n <- c()
	for(i in id) {
		if(i < 10) {
			i <- paste("00", i, sep="")	
		}	
		else if(i >= 10 && i < 100) {
			i <- paste("0", i, sep="")
		}
		data <- read.csv(paste(i, ".csv", sep=""))
		col <- data[[pollutant]]
		bad <- is.na(col)
		col <- col[!bad]
		n <- append(n, col)
	}
	mean(n)
}

