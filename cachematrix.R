## The set of functions below takes an invertible matrix from user
## and retun the inverse of matrix. If the inverse is already calculated,
## value is fetched from the cache rather than re-computing

## makeCacheMatrix function takes an invertible matrix as input,
## and return a list with 4 objects (functions)
## The functions returned in the list are used for setting the value of matrx,
## retrieving the matrix, setting the inverse and retrieving the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        set_inv <- function(inverse) inv <<- inverse
        get_inv <- function() inv
        list(set = set, get = get,
             set_inv = set_inv,
             get_inv = get_inv)
}


## cacheSolve function calculates the inverse of input invertible matrix
## if the inverse is already computed and cached, it returns the cached value,
## otherwise it computes the inverse and returns that

cacheSolve <- function(x, ...) {
        inv <- x$get_inv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$set_inv(inv)
        inv
}

