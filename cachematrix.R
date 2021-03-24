## 2 functions that use Lexical scoping to cache a matrix and its inverse

## This function sets the input as x in matrix format and assigns NULL to its inverse as it hasn't been computed yet.
## It also contains getters and setters that have the matrix and its potential inverse stored in a list of elements which
## can be accessed.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
      x <<- y
      i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set=set, get=get,
        setinverse = setinverse,
        getinverse = getinverse)
  }


## This function calculates the inverse of the inputted matrix which is assigned to 'setinverse'.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()

  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }

  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
  ## Return a matrix that is the inverse of 'x'
}
