## Below are two functions that are used to create a special object "matrix" that stores a
## matrix vector and cache's its inverse.

## The function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
             z <- NULL
             set <- function(y) {
               x <<- y
               z <<- NULL
             }
             get <- function() x
             setinverse <- function(inverse) z <<- inverse
             getinverse <- function() z
             list(set = set, get = get,
                  setinverse = setinverse,
                  getinverse = getinverse)
}

## The function computes the inverse of the special "matrix" returned by 'makeCacheMatrix' above.
## If the inverse has already been calculated (and the matrix has not changed), then the 'cacheSolve'
## should retrieve the inverse from the cache.
## Computing the inverse of a squre matrix can be done with the 'solve' function in R.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         z <- x$getinverse()
         if(!is.null(z)) {
                message("getting cached data")
                return(z)
         }
         data <- x$get()
         ## Calculate the inverse using matrix multiplication
         z <- inverse(data) %*% data
         x$setinverse(z)
         z
}
