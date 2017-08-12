## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    r <- x$get_reverse()
    if(is.null(r)){
        print("remembered")
        res <- solve(x$get())
        x$set_reverse(res)
        return(res)
    }
    print("used")
    x$get_reverse()
}
