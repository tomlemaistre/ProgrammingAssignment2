##-------------------------------------------------------------------------------------
## cachematrix.R is a set of functions that creates a special matrix that will cache the 
## inverse of the matrix to save on computation after the inverse has been calculated once.
## It contains 2 functions:
##     makeCacheMatrix(x): This function constructs the cached matrix from a matrix x 
##     cacheSolve(x, ...): This function will try to access the cached inverse. If it already
##     exists, the cached value will be returned. Otherwise, it will calculate, cache, and return
##     the inverse of the matrix x.
##-------------------------------------------------------------------------------------


## Summary: This function takes in a matrix and creates an object that can cache the 
##          inverse of the matrix.
## Params: matrix x: This is the matrix to create a cached inverse matrix of.

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y){
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setInverse <- function(solve) i <<- solve
	getInverse <- function() i
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Summary: This function takes in a matrix and checks for a cached inverse value. If 
##          found, it returns that value, otherwise it calculates, caches and returns
## 			the inverse for matrix x.
## Params: matrix x: This is the matrix to create a cached inverse matrix of.

cacheSolve <- function(x, ...) {
        ## Get the inverse
		i <- x$getInverse()
		## If the Inverse is not null, return it.
		if(!is.null(i)){
			message("Getting Cached Data...")
			return(i)
		}
		## Inverse was null, therefore not cached. Calculate and cache the inverse.
		data <- x$get()
		i <- solve(data,...)
		x$setInverse(i)
		## Return a matrix that is the inverse of 'x'
		i
}
