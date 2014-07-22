makeCacheMatrix <- function(z = matrix()) {
 
  k<- NULL
  set <- function(y) {
    z <<- y
    k <<- NULL
  }
  get <- function() z
  
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
 
 

cacheSolve <- function(z, ...) {

  k <- z$getinverse()
  
  if(!is.null(k)) {
    message("getting cached data")
    return(k)
  }
  data <- z$get()
  k <- solve(data, ...)
  z$setinverse(k)
  k
}
