## Caching the inverseof a Matrix avoids the repeated computation of a matrix inverse. The benfits are there for larger number of matrices that would take very complex calculations.

## This fucntion creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
         set <- function(y) {
                 x <<- y
                 inv <<- NULL
         }
         get <- function() x
         setInverse <- function(inverse) inv <<- inverse
         getInverse <- function() inv
         list(set = set,
              get = get,
              setInverse = setInverse,
              getInverse = getInverse)
}


## This function computes the inverse of the special matrix created by makeCacheMatric above. If the inverse has already been calculated without any changes to/ in the matrix, then it should retrieve the inverse from the cache instead of calculating/ computing it again.

cacheSolve <- function(x, ...) {
	inv <- x$getInverse()
         if (!is.null(inv)) {
                 message("getting cached data")
                 return(inv)
         }
         mat <- x$get()
         inv <- solve(mat, ...)
         x$setInverse(inv)
                  inv

        ## Return a matrix that is the inverse of 'x'
}
## The code below is a test case that when entered along with the above code will give the inverse of the matrix. 
m <- matrix (6:9, nrow=2, ncol=2)
M <- makeCacheMatrix(m)
M$get()
cacheSolve(M)
M$getInverse()


