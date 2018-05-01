#This function creates a special matrix object that can cache the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y)
  {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) 
    inv <<- inverse
  getinverse <- function() inv
  list(set = set, 
       get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


#This function calculates the inverse of the makeCacheMatrix
#First it checks to see if the inverse has already been calculated 
#Then it gets the inverse from the cache and skips the computation

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) 
  {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  
  #Return a matrix that is the inverse of x
  inv
}