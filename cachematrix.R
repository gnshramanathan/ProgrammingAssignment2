## Put comments here that give an overall description of what your
## functions do


##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(a = matrix())
 {
    inv <- NULL
    set <- function(b) {
        a <<- b
        inv <<- NULL
    }
    get <- function() a
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(a, ...) 
{
    inv <- a$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- a$get()
    inv <- solve(data)
    a$setinverse(inv)
    inv
}




