## In this two functions, the first one calculates the inverse of a matrix  
## and then the second one if it is needed again, it gets it from the cache

## This function calculates the inverse of the matrix x

makeCacheMatrix <- function(x = matrix()) {
        tam <- NULL
        inv <- function (y = matrix()) {
                x <<- y
                tam <<- NULL
        }
        mat <- function() x
        matinv <- function (solve) tam <<- solve
        getinv <- function () tam
        list(inv = inv, mat = mat, matinv = matinv, getinv = getinv )
}


## The following function, looks for the inverse of the matrix in the cache and if 
## it is empty, it calculates it

cacheSolve <- function(x, ...) {
        tam <- x$getinv()
        if(!is.null(tam)){
                message("getting cached data")
                return(tam)
        }
        data <- x$mat()
        matinv <- solve(data, ...)
        matinv
}

