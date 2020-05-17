## This function creates a special "matrix" object that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  ## Setting the matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  ##Getting the matrix
  get <- function() x
  ##Setting the inverse of the matrix
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  ## Getting the matrix 
  data <- x$get()
  ## Creating the inverse
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

