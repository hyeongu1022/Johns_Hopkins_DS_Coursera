makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
       	set <- function(y) {
                	x <<- y
               	i <<- NULL
        	}
        	get <- function() {x}
        	setInv <- function(inv) {m <<- inv}
        	getInv <- function() {i}
        	list(set = set, get = get,
             	setInv = setInv,
             	getInv = getInv)
}

cacheSolve <- function(x, ...) {
        i <- x$getInv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInv(i)
        i
}
