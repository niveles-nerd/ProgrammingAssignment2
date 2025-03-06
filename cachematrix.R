## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # Initialize the inverse as NULL
  inv <- NULL
  
  # Define the set function to set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset the inverse when the matrix changes
  }
  
  # Define the get function to retrieve the matrix
  get <- function() x
  
  # Define the setinverse function to cache the inverse
  setinverse <- function(inverse) inv <<- inverse
  
  # Define the getinverse function to retrieve the cached inverse
  getinverse <- function() inv
  
  # Return a list of functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  # Retrieve the cached inverse
  inv <- x$getinverse()
  
  # If the inverse is already cached, return it
  if (!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  
  # If not, compute the inverse
  data <- x$get()
  inv <- solve(data, ...)  # Compute the inverse using solve()
  
  # Cache the inverse
  x$setinverse(inv)
  
  # Return the inverse
  inv
}
