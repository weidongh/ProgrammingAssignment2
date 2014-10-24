
## make a cached matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(i) {inv <<- i}
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## return inverse matrix of x (x is created by makeCacheMatrix)
cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cache data");
        return(inv)
    }
    m <- x$get()
    inv <- solve(m)
    x$setinv(inv)
    inv
}
