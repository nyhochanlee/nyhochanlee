# nyhochanlee
R Programming Assignment
# For this assignment, we assume that the matrix supplied is always invertible.
makeCacheMatrix <- function(x = matrix()) { # we take a function with a matrix input
  inv <- NULL # Set up inv variable
  set <- function(y) { # set the value of matrix
    x <<- y
    inv <<- NULL
  }
  get <- function() x # get the value of matrix
  setinverse <- function(inverse) inv <<- inverse # set the value of inverse
  getinverse <- function() inv # get the value of inverse
  list (set = set, get = get, setinverse = setinverse, getinverse = getinverse) # Return a list of set, get, setinverse, getinverse
}


# For this assignment, we assume that the matrix supplied is always invertible.
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) { # if the inverse value exists
                message("getting cached data") # we print this message
                return(inv) # and return the existing value
        }
        data <- x$get()
        inv <- solve(data, ...) # When the second argument is missing, the function will return the inverse of data
        x$setinverse(inv) # store the inverse value to "setinverse"
        inv # return the value of inverse
}
