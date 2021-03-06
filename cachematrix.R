## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix



makeCacheMatrix <- function(x = matrix()) {
  t <- NULL
  set <- function(y) {
    x <<- y
    t <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) t <<- inverse
  getinverse <- function() t
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


#if the inverse has already been calculated. If so, it gets the inverse  from the cache and skips the computation. 
#Otherwise, it calculates theinverse  of the data and sets the value of the inverse  in the cache via the setinverse  function.


cacheSolve <- function(x, ...) {
  t <- x$getinverse()
  if(!is.null(t)) {
    message("getting cached data")
    return(t)
  }
  data <- x$get()
  t <- solve(data)
  x$setinverse(t)
  t
}

#Example
#> a <- makeCacheMatrix(matrix(c(1,2,3,4),nrow=2,ncol=2))
#> a$get()
#[,1] [,2]
#[1,]    1    3
#[2,]    2    4
#> a$getinverse()
#NULL
#> cacheSolve(a)
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#> a$getinverse()
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#> cacheSolve(a)
#getting cached data
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5