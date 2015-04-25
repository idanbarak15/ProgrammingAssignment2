makeCacheMatrix <- function(x) {
        m <- NULL
        set1 <- function(y) {
                x <<- y
                m <<- NULL
        }
        get1 <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set1 = set1, get1 = get1,
             setinverse = setinverse,
             getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get1()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}