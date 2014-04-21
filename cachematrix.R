## Cached Matrix tools - Peer Assessment
## Basic usage eample:
## > m <-  makeCacheMatrix(matrix(c(...),ncol=...,nrow=...))
## > cacheSolve(m)
## > cacheSolve(m)
## The first time the inverse matrix is computed; the
## second time the cached result is returned.

makeCacheMatrix <- function(x = matrix(numeric(),,)) {

	## Creates and manages the matrix with cacheable inverse

	if (dim(x)[1]!=dim(x)[2]) {
		message("[makeCacheMatrix] Wrong dimensions.")
		message("Please use a square matrix.")
	}

	x.inv <- NULL		## Start with a NULL value for the 
					## cached inverse.

    	set <- function(y) {	## When the current matrix is changed,
	     	x <<- y		## set the local one to the assigned one.
      	x.inv <<- NULL		## The cached inverse isn't valid any
    	}			## longer, so is set to NULL.
				## Double headed assignment arrows
				## are needed for main environment
				## variables.

    	get <- function() {	## It just returns the value of the
		x		## current matrix.
	}

    	set.inv <- function(computed.inv) {	## When a value for the
		x.inv <<- computed.inv		## computed inverse is
	}					## supplied, store it into
						## the cache variable.

    	get.inv <- function() {	## It just returns the value of the
		x.inv		## cached inverse matrix.
	}

    	list(				## It returns the result of the main
		set = set,		## function: the list of the above
		get = get,		## defined functions.
        	set.inv = set.inv,	## The first name is the name of the
        	get.inv = get.inv	## element in the list; the second is
    	)				## the corresponding function.
}

cacheSolve <- function(x) {

	## Returns the computed or cached inverse matrix.
	## The argument is a matrix-with-cached-inverse object
	## created with the makeCacheMatrix() function.

	x.inv <- x$get.inv()		## Gets the (possible) cached inverse.

    	if(!is.null(x.inv)) {		## Cached inverse found.
      	message("[cacheSolve] Getting cached data.")
        	return(x.inv)		## Cached value returned, function exited.
    	} else {
		message("[cacheSolve] Computing the inverse matrix.")
	}
					## After the msg, the inverse is computed.
					## This version assumes that the given
					## matrix IS invertible. No check is
					## performed on the determinant.
    	matrix.to.solve <- x$get()
    	x.inv <- solve(matrix.to.solve)
    	x$set.inv(x.inv)		## The computed inverse is cached,
    	x.inv				## then returned as the result.
}
