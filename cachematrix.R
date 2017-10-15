## Purpose of these functions is to create a square matrix entry and
## return the inverse of that matrix and store it.
## If the function recognizes the entry in the cache, it will fetch
## the answer instead of calculating the answer.
 
## Creates and records the matrix entry (matrix and inverse matrix)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Checks for previous entry for that matrix.
## If there is no previous entry, it will calculate 
## inverse by solve()

cacheSolve <- function (x,...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m  
}
        ## Return a matrix that is the inverse of 'x'
