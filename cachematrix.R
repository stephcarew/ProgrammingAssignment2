## These functions are useful when one wishes to obtain the inverse of a matrix
## 

## makeCacheMatrix return a list containing a function to 
## a) set the value of the matrix
## b) get the value of the matrix
## c) set the value of the inverse
## d) get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve returns a matrix that is the inverse of 'x' by first checking to see
## if the value is already cached by. If not, it will compute and store the value
## of the inverse

cacheSolve <- function(x) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}


