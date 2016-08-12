## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = numeric()) {
	m <- NULL
	setMat <- function(y) {
		x <<- y
		m <<- NULL
	}
	getMat <- function() x
	setInv <- function(Inv) m <<- Inv
	getInv <- function() m
	list(setMat = setMat, getMat = getMat,
	     setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
	m <- x$getInv()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$getMat()
	m <- solve(data)
	x$setInv(m)
	m
}
