## This file defines two functions, `makeCacheMatrix` and `cacheSolve`, that
## provide a container type for a matrix that can cache it's inverse
## 
## The container type is created with the function `makeCacheMatrix` and
## is represented as a list with four members: `get`, `set`, `getinverse`
## and `setinverse` for getting and setting the matrix and its inverse, respectively.
##
## The function `cacheSolve` can calculate the inverse of such a `CacheMatrix`.
## It uses the R function `solve` to calculate the solution to a square matrix
## under the hood, caching the result for further calls.


## `makeCacheMatrix` is a contructor function for the type `CacheMatrix`.
## It creates and returns a type with the members `get` and `set` for
## storing a matrix, effectively wrapping the matrix.
## The members `setinverse` and `getinverse` store the inverse of the matrix.
## Setting the wrapped matrix to a new matrix resets the inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## `cacheSolve` returns the inverse of the matrix represented by the
## CacheMatrix type.
## On the first call to `cacheSolve`, the inverse is calculated with
## the R `solve` function and stored in the object for future calls.
## Subsequent calls to `cacheSolve` return the cached inverse.

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
