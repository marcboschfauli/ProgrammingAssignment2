# Matrix inversion is usually a costly computation and their 
# may be some benefit to caching the inverse of a matrix 
# rather than compute it repeatedly. The following functions
# are used to cache the matrix inverse 


# makeCacheMatrix This function creates a special "matrix" 
# object that can cache its inverse creating a list 
# containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y) {
    x<<-y
    inv<<-NULL
  }
  get<-function() x 
  setMatrixInverse<-function(MatrixInverse) inv <<-MatrixInverse
  getMatrixInverse<-function() inv
  list(set=set, get=get, setMatrixInverse=setMatrixInverse,getMatrixInverse=getMatrixInverse)
}


# This function returns the inverse of a matrix, assuming it is invertible.
# It first checks if the inverse has been computed. If that's the case, it
# gets the result with the getMatrixInverse function ommiting the computation.
# If not,the inverse is is computed and the value is set in the cache with 
# the setMatrixInverse function

cacheSolve <- function(x, ...) {
  inv <- x$getMatrixInverse()
  if(!is.null(inv)){  # if inverse has been computed
    return(inv)       # return chache matrix inverse
  }
  inv<-solve(x$get())     # otherwise get the inverse
  x$setMatrixInverse(inv) # and save it to the cache
  return(inv)             # return matrix inverse
}

