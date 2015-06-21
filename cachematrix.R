## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {

  #Intialise the inverse
  cachedInverse <-NULL
  
  #sets the matrix and resets the cahced inverse
  set <- function (mtx) {
    x <<- mtx
    cachedInverse <<- NULL
  }
  #returns the matrix
  get <- function() x
  
  #sets the inverted matrix
  setSolved <- function(solvedInverse) cachedInverse <<- solvedInverse
  
  #returns the cached value of the inverted matrix
  getSolved <- function() cachedInverse
  
  ## setters and getters
  list(set = set, get = get,
       setSolved = setSolved,
       getSolved = getSolved)
  
}


## Write a short comment describing this function
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        
  inverted <- x$getSolved()
  
  #checks for a cached value and returns it if finds one
  if(!is.null(inverted)){
    return(inverted)
  }
  
  #calculates the inverted matrix since it didn't find 
  #a cached value
  data <- x$get()
  inverted <- solve(data, ...)
  x$setSolve(inverted)
  inverted
}
