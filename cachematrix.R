## Huijie Shen
## In this assignment, we create a function called makeCacheMatrix to create inversible matix
## and a function called cacheSolve to compute the inverse from cache

## This function creates a special "matrix" object that can cache its inverse. which is really a list containing a function to
#set the value of the matrix
#get the value of the matirx
#set the value of the inverse
#get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    # use <<- operator to assign a value to an object in an environment that is different from the current environment
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse 
  getinverse <- function() i
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  return(i)
}

