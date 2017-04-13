## call library(testthat) if you want to run tests by command: source('cachematrix.R')

## it is an R function is able to cache potentially time-consuming computations

## This function creates a special "matrix" object that can cache its inverse.
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function()
    x
  setinverse <- function(inverse)
    i <<- inverse
  getinverse <- function()
    i
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

# tests

check_testthat <- function() {
  any(devtools::loaded_packages()$package == 'testthat')
}

testGetInverseAfter <- function(x, x1) {
  cacheSolve(x)
  x$set(x1)
}

if (check_testthat()) {
  test_that("cacheSolve is working as expected", {
    m <- matrix(c(3, 3.2, 3.5, 3.6), 2, 2)
    i <- matrix(c(-9, 8, 8.75,-7.50), 2, 2)
    expect_equal(cacheSolve(makeCacheMatrix(m)), i)
    expect_equal(testGetInverseAfter(makeCacheMatrix(m), i), NULL)
  })
}
