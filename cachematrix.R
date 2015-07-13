## These functions calculate the inverse of a matrix and saves it
## into cache. When the user attempts to calculate the matrix inverse,
## the previous value is returned instead of compute it repeatedly.

#The makeCacheMatrix function, creates a special "matrix", which is really a list containing a function to

# 1. set the value of the vector
# 2. get the value of the vector
# 3. set the value of the mean
# 4. get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
  ## create a matrix object x and associated sub-functions
  
  ## to define cache m
  m <- NULL
  set <- function(y) {
    
    # to assign the input of matrix y to the variable x in parent environment
    x <<- y  
    ## to re-initialize m to null in parent environment 
    m <<- NULL 
  }
  
  ## return the matrix x
  get <- function() x  
  
  ## set the cache m equal to inverse of the matrix x
  setinverse <- function(inverse) m <<- inverse 
  
  ## return the cached inverse of x
  getinverse <- function() m 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function calculates the inverse of the special "matrix" that created
## with the above function. However, it first checks to see if the inverse
## has already been calculated. If so, it get's the inverse from the cache
## and skips the computation. Otherwise, it calculates the matrix inverse
## and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        
  ## Return a matrix that is the inverse of 'x'
  
  ## to get's the inverse from the cache
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
