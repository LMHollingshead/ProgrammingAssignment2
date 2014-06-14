## A set of R functions that calculates a matrix inverse and maintains
## that calculation for future retrieval by caching the matrix and the
## solution.

## makeCacheMatrix when called defines four functions.
## set: prepares the generic value to be used in the makeCacheMatrix environment
## get: gets the value previously prepared
## setinverse: calculates the inverse
## getinverse: gets the previously calculated inverse
makeCacheMatrix <- function(x=numeric()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) m <<- solve
	getinverse <- function() m
	list(set = set, get = get,
	     setinverse = setinverse,
	     getinverse = getinverse)
	
}

## cacheSolve either calculates a matrix inverse or retrieves a previously
## calculated inverse using the functions and environment of makeCacheMatrix
cacheSolve <- function(x, ...) {
	m <- x$getinverse()
	if(!is.null(m)) {
		message("getting cached matrix")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
}
