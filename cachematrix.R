## Since inverting a matrix is generally computationally intensive, it is in the best interest of the researcher to not redo the calculation if it 
## already exists, these couple functions are designed to do just that

# As requested, makeCacheMatrix creates a list containing a function to set  and get the value of the matrix, then set and get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
    m <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# The following function is the obne that returns the inverse of the matrix. Moreover, it checks if the inverse already exists and displays the message 
## "getting cached data" it in case it exists and skips calculating it again

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}