# These functions can be used together to create a "special matrix",
# which store its inverse in cache, rather than compute it repeatedly 

# This function creates a "special matrix", which is in reality a list capable 
# of store the inverse of the matrix passed as parameter. The functions get, 
# set, getInverse and setInverse are available in the returned list. The 
# functions get and set can be used to get and set the matrix itself and the 
# functions getInverse and setInverse can be used to retrieve and store the 
# inverse of the matrix in cache, respectively.
makeCacheMatrix <- function(x = matrix()) {
	# initialize the cache
	inv <- NULL

	# functions get and set of matrix itself (function set clears the cache)
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}	
	get <- function() x
	
	# get and set of the inverse of the matrix in cache
	setInverse <- function(inverse) inv <<- inverse	
	getInverse <- function() inv
	
	# create and return a list with the above created functions
	list(set = set, 
		 get = get,
		 setInverse = setInverse,
		 getInverse = getInverse )	
}


# This function returns the inverse of the "special matrix" created by
# function makeCacheMatrix. If the cache is empty, the inverse is computed and
# stored in the cache, otherwise the inverse from the cache is returned.
cacheSolve <- function(x, ...) {
	# get the inverse stored in cache
    inv <- x$getInverse()
	
	# if the cache is not null, return the inverse from the cache
	if(!is.null(inv)) return(inv)
	
	# get the matrix and compute its inverse
	data <- x$get()
	inv <- solve(data, ...)
	
	# store the inverse in the cache and returns it
	x$setInverse(inv)
	inv
}
