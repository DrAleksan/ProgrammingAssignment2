

## Create a CacheMatrix from matrix x. Variable r for reverse

makeCacheMatrix <- function(x = matrix()) {
    r <- NULL
    set <- function(y) {
        x <<- y
        r <<- NULL
    }
    get <- function() x
    set_reverse <- function(reverse) {
        r <<- reverse
    }
    get_reverse <- function() r
    list(set = set, get = get,
         set_reverse = set_reverse,
         get_reverse = get_reverse)
}


## Checking if reverse hadn't been solved and return it if it does, otherwise solve and remember it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    r <- x$get_reverse()
    if(is.null(r)){
        res <- solve(x$get())
        x$set_reverse(res)
        return(res)
    }
    x$get_reverse()
}
