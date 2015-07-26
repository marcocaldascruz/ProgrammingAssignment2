
## This function sets the values (matrix and inverse matrix values).
makeCacheMatrix <- function(x = matrix()) {

## Init the value of m, its will be the inverse value of the matrix.

            m <- NULL

## it implements the “set” Function which sets the value of the matrix.
## the m value is NULL (yet).

            set <- function(y) {
                    x <<- y
                    m <<- NULL
            }
## it implements the “get” Function which returns the value of the matrix.

            get <- function() x

## it implements the “setsolve” Function which sets the inverse of the matrix.
            setsolve <- function(solve) m <<- solve

## it implements the “getsolve” Function which returns the inverse of the matrix.
            getsolve <- function() m

## The functions list associates to the makeCacheMatrix function.
            list(set = set, get = get,
                 setsolve = setsolve,
                 getsolve = getsolve)
}


## This function returns the inverse matrix value cached or it computes.
cacheSolve <- function(x, ...) {
## In variable m, the function deposits the value of the inverse of the matrix.
            m <- x$getsolve()

## If the value is defined then the function only shows the variable (any compute).
            if(!is.null(m)) {
                    message("getting cached data")
                    return(m)
            }
## else in “data” gets the value of the matrix.
            data <- x$get()
##      in “m” gets the value of the inverse of the matrix.
            m <- solve(data, ...)
##      it sets in m, the value calculated (inverse of the matrix).
            x$setsolve(m)
##      return m (inverse matrix value).
            m
    
}
