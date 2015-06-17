## creates a matrix that can be cached along with its inverse


## creates a matrix in which its inverse can be cached when solved
makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  
  set<- function(y)
  {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)

}


## solves the inverse of a matrix;  if it has already been solved then it will
## retrieve it from the cache in which it has been stored


cacheSolve <- function(x, ...) {

  i<- x$getInverse()
  if(!is.null(i))
  {
    message("getting cached data")
    return(i)
  }
  data<- x$get()
  i <- solve(data)
  x$setInverse(i)
  i
       
}
