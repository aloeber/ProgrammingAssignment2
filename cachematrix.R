## Creates a list containing functions to get and set the matrix it has been passed, along with the inverse of that matrix

makeCacheMatrix <- function(x = matrix())
{
  inv <- NULL
  set <- function(y)
  {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Executes the solve command and returns the inverse if never run before, otherwise simply returns the cached inverse

cacheSolve <- function(x, ...)
{
  inv <- x$getinv()
  if(!is.null(inv))
  {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
