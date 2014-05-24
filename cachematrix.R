##  the function makeCacheMatrix creates a list containing a function to
##  set the value of the matrix
##  get the value of the matrix
##  set the value of the inverse of the matrix
##  get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
		i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## the function cache solve calculates the inverse of a matrix and cache the result
## If the inverse have been already calculate, the function returns the result from the cache without re-calculating it

cacheSolve <- function(x, ...) {
      
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
		}
