## Functions that calculate the inverse of a matrix, caching the results for future.

## Cached matrix object that stores and retrieves original matrix and cached solution.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Calculates the inverse of a matrix, using cached result if previously calculated.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

# Test Script:
# matrix <- matrix(sample(1:100, size = 9, replace = F), nrow = 3, ncol = 3)
# ob <- makeCacheMatrix(matrix)
# cacheSolve(ob)
# cacheSolve(ob)