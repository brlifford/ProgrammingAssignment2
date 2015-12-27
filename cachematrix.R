## Creates a function that creates a square invertible matrix object 
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
        ## list is used as input for cacheSolve()
}


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                ## check if inv is cached, skip computation
                message("getting cached data")
                return(inv)
        }
        ## not cached - calculate inverse
        mat.data <- x$get()
        inv <- solve(mat.data, ...)
        
        ## set the value of the cache
        x$setinv(inv)
        
        return(inv)
}
