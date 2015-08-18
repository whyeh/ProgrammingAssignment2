## This script contains two functions: 1) for users to set and get matrices, 
## 2. for users to obtain the inverse of the input matrix (this function will
## first test to see if there is an inverse matrix stored in cache before
## calculating one)


## This function takes matrix in and store it in the global environment to be used as Cache
## 4 nested functions: read input matrix and set it to global x matrix, get the stored x matrix,
## set global inv variable to the calculated inverse matrix, and get the stored inverse matrix. 

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

## Firstly, this function tests whether there is an inverse matrix for x stored in cache. 
## If not, it will calculate the inverse of x. Then, it will call the setinverse 
## function in the makeCacheMatrix to store result in cache. Finally, it will return the
## inverse matrix of x

cacheSolve <- function(x, ...) {
     inv <- x$getinverse()
     if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
     }
     ## Return a matrix that is the inverse of 'x'
     data <- x$get()
     inv <- solve(data, ...)
     x$setinverse(inv) ##call the setinverse function in the makeCacheMatrix 
     ##function to store the calculated inverse matrix to inv 
     ##(located in the global environment)
     inv
}