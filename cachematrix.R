## Put comments here that give an overall description of what your
## functions do


##this function will catche the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ## use solve(x) to inverse the matrix
  get <- function() x
  setinverse <- function() i <<- solve(x)
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## this function will first check whether the matrix is 
## already inversed or not. If yes, then it will return
## the original input(cache). If not, then it will set
## the matrix to inverse 

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
