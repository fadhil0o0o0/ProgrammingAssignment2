## Put comments here that give an overall description of what your
# functions do

## these functions do caching to inverse of a matrix, and do inversion based on these matrix.
# if the matrix has been cached before by the makeCacheMatrix, the cacheSolve will not repeat the inversion process.
# instead, it will use the previously cached inverse matrix, provided the matrix has not changed.

## Write a short comment describing this function
# makeCacheMatrix is the function to create a special kind matrix that will enable caching of its inverse.
makeCacheMatrix <- function (x = matrix())
{
  # initialize the cached inverse first by NULL
  i <- NULL
  
  # set the value to another environment
  set <- function(y)
  {
    x <<- y
    i <<- NULL
  }
  
  # gets the matrix
  get <- function()
  {
    x
  }
  
  # set the inverse of current matrix to another value in the environment
  setinverse <- function(solve) i <<- solve
  
  # gets the inverse. if the value has not been cached previously by calling setinverse(), it will return null
  getinverse <- function() i
  
  # return the list containing the functions
  list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
# cacheSolve is the special kind of solve(x) function that do inverse to the special matrix created by makeCacheMatrix(x) function.
cacheSolve <- function(x, ...) {
  # try to get the inverse from the special matrix created by makeCacheMatrix.
  i <- x$getinverse()
  
  # if the value is available, meaning that it has been cached previously. return this value.
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  else # if the value is not available, get the value from the special matrix, solve it and store it in the cache to be used later.
  {
    message("calculating inverse because there is no cached data")
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
  }
  
  #return the inverse (provided the the matrix is inversible.)
  i
}
