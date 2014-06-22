## This two functions create list that stores a matrix and its inverse, so that the inverse does
##    not have to be solved for each time it is needed

## This function creates the list and the matrix. 
### y, the variable that sets the matrix's value, must be a convertible matrix itself

makeCacheMatrix <- function(x = matrix()) 
{
  i <- NULL
  set <- function(y)
  {
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

## This function returns the inverse of 'x' and stores it in the list so that it doesn't to be solved for again

cacheSolve <- function(x) 
{
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
