## Create a square matrix and cache its inverse (makeCacheMatrix)
## Check to see if the inverse has been calculated; 
## if it has not, cacheSolve calculates the inverse of the matrix created by makeCacheMatrix, 
## if it has, the inverse is called from the cache and the calculation is skipped. 

## Create matrix and cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	s <- NULL
	set <- function(y){
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

## calculates inverse of matrix or call inverse from cache

cacheSolve <- function(x, ...) {
		s <- x$getsolve()
		if(!is.null(s)) {
			message("getting cached data")
			return(s)
		}
		data <- x$get()
		s <- solve(data, ...)
		x$setsolve(s)
		s
}
