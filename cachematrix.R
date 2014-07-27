# This code create a modified versión de matrix with cache for inversion. 
# There are two functions: 
#  makeCacheMatrix --> generate a modified versión of matrix.
#  cacheSolve --> do inverse calculation of this modified matrix extructure.

# makeCacheMatrix: This function creates a special "matrix" object that can 
# cache its inverse.
# only one parameter: the matrix for wrapper on special "matrix" object 
# (cache enabled). I assume that the matrix supplied is always invertible.
makeCacheMatrix <- function(x = matrix()) {
    inverseOfx <- NULL
    set <- function(y) {
        x <<- y
        inverseOfx <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inverseOfx <<- inverse
    getInverse <- function() inverseOfx
    list(set = set, get = get,setInverse = setInverse,getInverse = getInverse)
}


# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and the 
#matrix has not changed), then the cachesolve should retrieve the inverse from 
#the cache.

cacheSolve <- function(x, ...) {
    inverseOfMatrix <- x$getInverse()
    if(!is.null(inverseOfMatrix)) {
        message("getting cached data") 
    }
    myMatrix <- x$get()
    inverseOfMatrix <- solve(myMatrix, ...)
    x$setInverse(inverseOfMatrix)
    inverseOfMatrix
}
