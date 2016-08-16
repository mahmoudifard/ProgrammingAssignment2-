## This function creates a special "matrix" object that can cache its inverse.

## The key assumption here is that the input matrix is convertable

makeCacheMatrix <- function(x = matrix()) {
  invs <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invs <<- inverse
  getinverse <- function() invs
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invs <- x$getinverse()
  if(!is.null(invs)) {
    message("getting cached data.")
    return(invs)
  }
  data <- x$get()
  invs <- solve(data)
  x$setinverse(invs)
  invs
}
