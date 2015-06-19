## Our functions were designed to give us the inverse of a matrix without repeated and unnecessary calculus.
## These functions improve computational time by using cache.

## The following function creates a special matrix (in fact it is a list) that can store its inverse in cache,
## in order to avoid costly calculus

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve returns the calculated inverse of a matrix if it exists, in another case, cacheSolve
## does required calculus.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <- x$getinv()
		if(!is.null(inv)) {
			message("getting cached data")
			return(inv)
		}
		data <- x$get()
		inv <- solve(data, ...)
		x$setinv(inv)
		inv
}
