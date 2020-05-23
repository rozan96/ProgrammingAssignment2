## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#This function creates a special matrix which is able to set the value, get the value of the matrix and also to 
#set and get the value of its inverse.


makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  set <- function(y) {
    x <<- y
    I <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) I <<- inverse
  getinverse <- function() I
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

#This function calculates the inverse of the special matrix and checks if the inverse is stored in its cache. If 
#so, it outputs the cached value and if not, it calculates it and sets the inverse in the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  I <- x$getinverse()
  if(!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  data <- x$get()
  I <- solve(data, ...)
  x$setinverse(I)
  I
}
