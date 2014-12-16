## Matrix inversion is usually a costly computation 
## and their may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly.

## makeCacheMatrix is a function that creates a special "matrix" object 
## that can cache the input matrix and its inverse.

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the matrix inverse
## 4. get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL ## sets the value inv to NULL
  set <- function (y) { ## set the value of the matrix
    x <<- y ## For cacheSolve check whether the matrix has changed
    inv <<- NULL ## If cacheSolve is used, this sets the value for the matrix inverse
  }
  get <- function() x
  setinverse <- function (inverse) inv <<- inverse
  getinverse <- function() inv
  list (set=set, get=get,setinverse=setinverse, getinverse = getinverse) ## Creates a list for all the four funtions
}


## cacheSolve is a function that calls the functions stored 
## in the special "matrix" returned by makeCacheMatrix, 
## If the inverse has already been calculated, it returns the inverse from the cache.
## If the input is new, it calculates the inverse of the data and
## sets the inverse in the cache via the setinverse function.

## This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
  ## Comparison to the matrix obtained with the above function
  inv <- x$getinverse() ## getting the matrix inverse already calculated
  if(!is.null(inv)) { ## to check if this function has been run in the past
    message("getting cached data!") ## Message returned if the matrix has not been changed
    return (inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv) ## set the inverse matrix
  inv ## return the matrix
}







