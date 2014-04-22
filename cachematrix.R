# Jon Rogers (GitHub ID is VanFalk)
# Peer Assessment Project

# The functions in this code utilize "solve" and the "<<-" operator to compute
# and return the inverse of a square matrix. The code is nearly identical to the
# examples provided in the documentation as the process is the same with a
# swapping of fucntions


# The "makeCacheMatrix" function creates a special "matrix" object that can cache 
# its inverse by utilizing the "<<-" operator 

makeCacheMatrix <- function(x = numeric()) {
	s <- NULL
	set <- function(y) {
		     x <<- y
		     s <<- NULL
	}
  get <- function() x
	setsolve <- function(solve) s <<- solve
	getsolve <- function() s
	list(set = set, get = get,
			 setsolve = setsolve,
			 getsolve = getsolve)
}

# The "cacheSolve" function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already been calculated
# (and the matrix has not changed), then cachesolve will retrieve the 
# inverse from the cache.

cacheSolve <- function(x, ...) {
	       s <- x$getsolve()
	       if(!is.null(s)) {
		       message("returning cached data")
		       return(s)
	 }
	 data <- x$get()
	 s <- solve(data, ...)
	 x$setsolve(s)
	 s
}