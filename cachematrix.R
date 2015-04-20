## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates an object that stores a user defined matrix and it's inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL # initializes the variable inv with NULL value
  set <- function(y) {
    x <<- y # stores the value of the input y in the variable x, in case it isn't defined
    inv <<- NULL # searches for a definition of the variable on the 
                 # parent enviroment and changes it's value to NULL
  }
  get <- function() x # displays the stored matrix
  setinv <- function(solve) inv <<- solve # searches for a definition of the variable inv
                                          # on the parent enviroment and changes it's 
                                          # value to the value of the variable solve
  
  getinv <- function() inv # display the inverted matrix
  list(set = set,get = get, # creates a list of functions implemented
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve computes the inverse matrix and stores it in cache, 
## so it only has to be computed once

cacheSolve <- function(x, ...) {
  inv <- x$getinv() # gets the value of the inverse matrix cached in x$inv
  if(!is.null(inv)) { # if a inverse matrix exists in cache it will be returned
    message("getting cached data")
    return(inv)
  }
  data <- x$get() # loads the matrix stored in x$get
  inv <- solve(data, ...) # computes the inverse of the matrix
  x$setinv(inv) # saves the inverse matrix to x$setinv
  inv # returns inverted matrix
}
