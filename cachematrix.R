
## @x: a square invertible matrix
## return: a list containing functions to
##              1. set the matrix
##              2. get the matrix
##              3. set the inverse
##              4. get the inverse
##         this list is used as the input to cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    # use `<<-` to assign a value to an object in an environment 
    # different from the current environment. 
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = setinverse)
  
}

cacheSolve <- function(x, ...) {
  ## @x: output of makeCacheMatrix()
  ## return: inverse of the original matrix input to makeCacheMatrix()
  
  m <- x$getinverse
  
  # if the inverse has already been calculated
  if(!is.null(m)) {
    message("getting the cached data")
    return(m)
  }
  
  # otherwise, calculates the inverse 
  data <- x$get()
  m <- solve(data, ...)
  
  # sets the value of the inverse in the cache via the setinv function.
  x$setinverse(m)
  m
}