# makeCacheMatrix: user-defined object used for cache inverse
# cacheSolve: calculate cached inverse if didn't find, otherwise just get cached inverse

# This function creates a special "matrix" object that can cache its inverse. 
makeCacheMatrix <- function(x = matrix()) {
		# variable initialization
		inverse <- NULL
		# assign value for key member
		setMatrix <- function(data) {
			# <<- means you want to assign the variable already existed in other environment
			# rather than override the old one and create new one in the current environment
			x <<- data
			inverse <<- NULL
		}
		getMatrix <- function(){
			x
		}
		setInverse <- function(inv) {
			inverse <<- inv
		}
		getInverse <- function() {
			inverse
		}
		# create special object
        list(setMatrix = setMatrix, 
        	getMatrix = getMatrix,
            setInverse = setInverse,
            getInverse = getInverse)		
}

# This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of 'x'
        # check cached inverse exist or not
        inverse <- x$getInverse()
        if(!is.null(inverse)) {
        	message("getting cached inverse of matrix")
        	return(inverse)
        }
        # didn't get the cached
        # omit the else part because it will keep going down due to return(inverse)
        # calculate the inverse matrix using built-in solve()
        data <- x$getMatrix()
        inverse <- solve(data, ...)
        x$setInverse(inverse)
        inverse
}