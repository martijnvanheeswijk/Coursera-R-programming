makeCacheMatrix <- function(x = matrix()) {
  ## @x: a matrix, which is square and invertible
  ## return: a list containing functions to
  ##              1. set matrix
  ##              2. get matrix
  ##              3. set inverse of matrix
  ##              4. get inverse of matrix
  ##         list is used as the input to cacheSolve()
  i <- NULL
  set <- function(y) {
    # use `<<-` to assign a value to object in an environment 
    # different from the current environment
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  ## x = output of makeCacheMatrix()
  # return: inverse of original matrix input to makeCacheMatrix()
  inv <- x$getinverse()
  
  #if inverse has been calculated
  if (!is.null(inv)) {
    # get inverse from the cache and skips the computation
    message("getting cached data")
    return(inv)
  }
  # otherwise, calculate inverse
  data <- x$get()
  inv <- solve(data, ...)
  
  # sets the value of the inverse in the cache via the setinv function.
  x$setinverse(inv)
  inv
}
