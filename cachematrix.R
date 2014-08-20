## Intro ----------------------------------------------------------------------

## The first function is intended to create an object instead of a 
## multi-dimension vector i.e. matrix to store the matrix and its inverse.
## It includes also the methods necessary to manipulate the "object" 

# 1st function ----------------------------------------------------------------

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(cached_matrix = matrix()) {
      # Initially the cached inverse has to be reseted to null whenever a new "matrix" 
      # object is beeing created
      cached_inverse <- NULL
      set <- function(y) {
            cached_matrix <<- y
            cached_inverse <<- NULL
      }
      # The following functions are methods of the "matrix" object
      get <- function() {
            cached_matrix
      }
      setinverse <- function(new_inverse) {
            cached_inverse <<- new_inverse
      }
      getinverse <- function() {
            cached_inverse
      }
      # The next block allows the methods to be used from outside of the function
      # it creates a list of the available methods
      list( set = set, 
            get = get,
            setinverse = setinverse,
            getinverse = getinverse)
} 
# 2nd function ----------------------------------------------------------------
## This function computes the inverse of the "matrix" object returned defined 
## in makeCacheMatrix.
## If the inverse has been calculated already (and the matrix has not changed), 
## then cachesolve should retrieve the inverse from the cache in order to spare
## calculation time.

cacheSolve <- function(object, ...) {
      ## Return a matrix that is the inverse of 'x'
      inverse <- object$getinverse()
      if(!is.null(inverse)) {
            message("getting cached data")
            return(inverse)
      }
      # in case the matrix has changed and thus there is no cached inverse 
      # available an "fresh" inverse needs to get calculated
      data <- object$get()
      inverse <- solve(data, ...)
      object$setinverse(inverse)
      inverse
}

## Test -----------------------------------------------------------------------
# x <- matrix(c(3, 9,11,6,23,22,233,33,23) ,3,3)
# > x
#       [,1] [,2] [,3]
# [1,]    3    6  233
# [2,]    9   23   33
# [3,]   11   22   23

# expected result:
# matrix.object <- makeCacheMatrix(x)
# cacheSolve(matrix.object)
#              [,1] [,2]         [,3]
#[1,]  0.015797915 -0.4  0.413873296
#[2,] -0.012510024  0.2 -0.160224539
#[3,]  0.004410585  0.0 -0.001202887

# cacheSolve(matrix.object)
# getting cached data
#              [,1] [,2]         [,3]
#[1,]  0.015797915 -0.4  0.413873296
#[2,] -0.012510024  0.2 -0.160224539
#[3,]  0.004410585  0.0 -0.001202887