

## makeCacheMatrix creates a list of 4 functions. 
## Function 1: setMat() sets the value of the matrix
## Function 2: getMat() returns the value of the input matrix
## Function 3: setInv() defines the matrix inverse
## Function 4: getInv() returns the value of the matrix inverse


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




## cacheSolve returns the inverse of a given matrix if it already exists. 
## If the inverse doesn't exist, it calculates and caches the inverse using setInv() from makeCacheMatrix.

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
