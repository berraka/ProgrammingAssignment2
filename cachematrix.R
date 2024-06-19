
## makeCacheMatrix creates a list which contains the matrix (x) that we pass 
    # as its argument itself (x$get()), and sets its inverse to NULL if x's
    # inverse was not calculated and contained by the cacheSolve function 
    # before (x$getinverse()).

makeCacheMatrix <- function(x = matrix()) {
    n <- NULL
    set <- function(y) {
        x <<- y
        n <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) n <<- inv
    getinverse <- function() n
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



##cacheSolve function looks if the passed matrix (x) has a cached inverse 
    # (x$getinverse()) or NULL. If there is a cache, it prints that the inverse
    # of this matrix was caught before, and provides the inverse. If there is no
    # cache, it computes the inverse of the passed matrix (solve(x)) and stores
    # the result in x$getinverse for cache, so that it can be looked for later.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    n <- x$getinverse()
    if(!is.null(n)){
        message("getting cached data")
        return(n)
    }
    data <- x$get()
    n <- solve(data, ...)
    x$setinverse(n)
    n
}
