## Calculate the inverse of a matrix and cache its result for future use
## How to use:
##    m <- makeCacheMatrix(<matrix>)
##    result <- cacheSolve(m)



## makeCacheMatrix 
## parameter: matrix or void
## returns a "special" matrix object (really a list)
## This list contains 4 functions: 
##       set: enable to set (modify) the initial matrix afterwards from the special matrix
##       get: return the initial matrix (the one passed by parameters)
##       setreverse: enable to 'register' (cache) data for future use
##       getreverse: return the 'registered' (cached) data

makeCacheMatrix <- function(x = matrix()) {
	##clear the variable containing the cached data
	r <- NULL
	##create the 'set' function which enable to modify the initial matrix
	set <- function(y) {
	x <<- y
	r <<- NULL
	}
	##create the 'get' function which return the initial matrix (here x)
	get <- function() x
	##create the 'setreverse' function which modify the r (containing the cached data) variable
	setreverse <- function(reverse) r <<- reverse
	##creates the 'getreverse' function which return the cached data
	getreverse <- function() r
	##return the result of makeCacheMatrix: a list containing 4 functions
	list(set = set, get = get,
	   setreverse = setreverse,
	   getreverse = getreverse)
}

## cacheSolve 
## parameter: special matrix returned by makeCacheMatrix function
## returns a "special" matrix object (really a list)
## Calculate the inverse of the passed matrix if not already calculated. 
## Otherwise return the previously calculated one.

cacheSolve <- function(x, ...) {
	##Get the cached data (if any)
	m <- x$getreverse()
	##Check if the inverse (cached data) has been already calculated
	if(!is.null(m)) {
	##already calculated return it
			message("getting cached data")
			return(m)
	}
	##retrieve the initial matrix
	data <- x$get()
	##calculate the inverse
	r <- solve(data, ...)
	##cache it for future use
	x$setreverse(r)
	##return the result
	r
}
