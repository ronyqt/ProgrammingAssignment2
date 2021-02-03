## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)){
    message("getting cached data \n")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setinverse(inverse)
  inverse
}

test_case <- matrix(c(1, 1, 1, 0, 1, 1, 1, 0, 1), nrow = 3, ncol = 3, byrow=FALSE)
print(test_case)
x <- makeCacheMatrix(test_case)
inverse <- cacheSolve(x)
print(inverse)
print(test_case %*% inverse)

