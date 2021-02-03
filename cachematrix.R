## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#cache the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  #the default value the inverse is NULL
  inverse <- NULL
  #This function is to set the matrix and its inverse
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  #This function is to return the matrix
  get <- function() x
  #This function is to set the inversion
  setinverse <- function(inv) inverse <<- inv
  #This function is to return the inverse
  getinverse <- function() inverse
  #Return a list containing four functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  #Leverage the getinverse function to get the inverse
  inverse <- x$getinverse()
  #If the inverse has already been calculated, then return it
  #If the inverse is NULL, then calcualte
  if(!is.null(inverse)){
    message("getting cached data \n")
    return(inverse)
  }
  #Use the get function to get the matrix
  data <- x$get()
  #Calculate the inverse and use setinverse function to set the inverse
  inverse <- solve(data)
  x$setinverse(inverse)
  inverse
}

#A test case to do the validation
test_case <- matrix(c(1, 1, 1, 0, 1, 1, 1, 0, 1), nrow = 3, ncol = 3, byrow=FALSE)
print(test_case)
x <- makeCacheMatrix(test_case)
inverse <- cacheSolve(x)
print(inverse)
print(test_case %*% inverse)
