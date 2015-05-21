## Put comments here that give an overall description of what your
## functions do

## The following function creates a vector that contains 4 functions: set the matrix, get the matrix,
##  set inverse of the matrix, get inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
 m <- NULL                # initializing m
  set <- function(y) {      # set matrix
    x <<- y                   
    m <<- NULL
  }
  get <- function() x       # get  matrix
  setinverse <- function(inverse) m <<- inverse   # set  inverse of matrix
  getinverse <- function() m    # get inverse of matrix 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##The next function calculates the inverse of the matrix created with maheCacheMatrix by first checking 
## whether the inverse has been calculated in which case it get it from the get inverse which is in the cache memory
## if it hasn´t been calculated it solves the matrix to get the inverse and put it the cache 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()       # assign inverse of matrix to m
  if(!is.null(m)) {       # get cached inverse if it was cached
    message("getting cached data")
    return(m)
  }
  data <- x$get()      # else assign the original matrix to data
  m <- solve(data, ...) # get inverse of data
  x$setinverse(m)     # cache the inverse
  m                 # output the inverse
}
