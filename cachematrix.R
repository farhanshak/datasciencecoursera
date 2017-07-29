## Put comments here that give an overall description of what your
## functions do. Kept original comments and formulas for direction

## Write a short comment describing this function (see below)

## makeCacheMatrix <- function(x = matrix()) {}

## Caching the Inverse of a Matrix:
## As a Matrix inversion can be a costly computation, there is benefit 
## to caching the inverse of a matrix rather than compute it repeatedly to make
## it less costly. Below are a pair of functions that are used to create a special 
## object that can store a matrix as well as caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Write a short comment describing this function (see below)

##cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'}

## This function computes the inverse of the special matrix created by 
## makeCacheMatrix item above. In cases where the inverse has already been calculated (and the 
## matrix didn't change), it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}