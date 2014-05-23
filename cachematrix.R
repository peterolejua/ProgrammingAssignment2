## A pair of functions that cache the inverse of a matrix.

## This function creates a special object that can cache the inverse.
# the function makeCacheMatrix creates a special "matrix", 
# which is really a list containing a function to
# 
# 1- set the value of the matrix
# 2- get the value of the matrix
# 3- set the value of the inverse
# 4- get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        
        list(set = set, get = get, setinverse = setinverse, 
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
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

# m = matrix(c(1,0,0,0,1,0,0,0,9),3,3)
# n=makeCacheMatrix(m)
# minverted = cacheSolve(n)
# > minverted
# [,1] [,2]      [,3]
# [1,]    1    0 0.0000000
# [2,]    0    1 0.0000000
# [3,]    0    0 0.1111111
# m %*% minverted
# > m %*% minverted
# [,1] [,2] [,3]
# [1,]    1    0    0
# [2,]    0    1    0
# [3,]    0    0    1