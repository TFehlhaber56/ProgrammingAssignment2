## makeCacheMatrix is a function that creates a special matrix object.
## This object caches the inverse of the input matrix.  

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
  x <<- y
  inv <- NULL
}
get <- function()x
setInverse <- function(inverse) inv <<- inverse
getInverse <- function() inv
list(set = set, 
     get = get,
     setInverse = setInverse, 
     getInverse = getInverse)
}


## cacheSolve calls the functions stored in the special matrix
## by the above function makeCacheMatrix.  It determines if the 
##inverse has already been calculated and unchanged.  
## If so, it then retrieves the inverse from the cache, else, 
##it calculates the inverse and returns it.
## 
cacheSolve <- function(x, ...) {
   inv <- x$getInverse()
  if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setInverse(inv)
  inv
}  
        

