## These two functions are used to create a special object that stores a matrix and cache's its inverse.

## The first function (makeCacheMatrix) creates a special "matrix", which is really a list containing a function to
## set the value of the matrix (set function)
## get the value of the matrix (get function)
## set the value of the inverse (set_inv function)
## get the value of the inverse (get_inv function)
## It returns a list

makeCacheMatrix <- function(x = matrix()) {
      mat_inv <- NULL
      set <- function(y) {
            x <<- y
            mat_inv <<- NULL
      }
      get <- function() x
      set_inv <- function(solve) mat_inv <<- solve
      get_inv <- function() mat_inv
      list(set=set, get=get, set_inv=set_inv, get_inv=get_inv)
}

## The second function (cacheSolve) calculates the inverse of the special "matrix" created with the 
## makeCacheMatrix function. It first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation.
## If not, it calculates the inverse of the data and sets the value of the inverse in the cache via the set_inv function.

cacheSolve <- function(x, ...) {
mat_inv <- x$get_inv()
      if(!is.null(mat_inv)) {
            message("getting cached data.")
            return(mat_inv)
      }
      data <- x$get()
      mat_inv <- solve(data)
      x$set_inv(mat_inv)
      mat_inv
}
