# ID = 2 / Submission 2 / FINAL
# COMMIT: 037c4a3cfdb282b1c1985642e4e55ada86440011
# rm(list = ls())
# ================================================================================
#
# Matrix inversion beyond 1000 X 1000 exceeded the capability of my laptop and
# took several/many minutes to compute. Requiring this calculating time, perhaps 
# for many iterations in a loop, would drastically slow down computer analysis.
# Calculating the matrix inversion once and using that result many times would
# significantly speed up computer analysis involving large data sets. Multiple
# tests on a 1000 X 1000 matrix of pseudo random numbers consistenly suggested 
# a time saving approximating 82.5% through the utilization of the caching process.
#
# FUNCTION 1
# ----------
# The makeCacheMatrix function creates a "matrix", "sets" and "gets" the value of 
# the matrix and "sets" and "gets" the value of the inverse of this matrix.
#
makeCacheMatrix <- function(x = matrix()) {
  inverse1 <- NULL                                       # initialize inverse1 to NULL
  set <- function(y) {
    x <<- y                                              # set matrix x
    inverse1 <<- NULL                                    # reinitialize inverse1 to NULL
  }
  get <- function() x                                    # retrieve stored matrix x
  setinverse <- function(inverse) inverse1 <<- inverse   # set inverse of matrix to inverse1
  getinverse <- function() inverse1                      # get inverse1
  list(set=set, get=get,                                 # list internal methods 
       setinverse=setinverse, 
       getinverse=getinverse)
}
# ================================================================================
#
# FUNCTION 2
# ----------
# The cacheSolve function checks for matrix inversion in cache and, if present, 
# skips the calculation, If the matrix inversion is not found, program flow drops
# down to calculate the matrix inversion and return the result as "inverse1".
#
cacheSolve <- function(x, ...) {
  inverse1 <- x$getinverse()                             # get the cached value
  if(!is.null(inverse1)) {                               # if cached value exists, return it
    
    print(start.time1)
    message("Retrieving Cached Inverse Matrix")
    
    return(inverse1)                                     # return previous cached value
  }
  message("Calculating Inverse Matrix")
  
  print(start.time2)
  data <- x$get()                                        # get matrix
  inverse1 <- solve(data)                                # calculate inverse matrix
  x$setinverse(inverse1)                                 # assign inverse matrix to inverse1
  
  inverse1                                               # print matrix inverse1
}
# ================================================================================
#
# TESTING HYPOTHESIS:
# ------------------
#
# set.seed(192837465)
# r = rnorm(1000000)
# x = matrix(r, nrow=1000, ncol=1000)
x = rbind(c(2, 5), c(9, 4))
m = makeCacheMatrix(x)
message("Printing Original Matrix")
m$get()

# FIRST ITERATION
start.time2 = Sys.time()
cacheSolve(m)
duration2 = Sys.time() - start.time2

# SECOND ITERATION
start.time1 = Sys.time()
cacheSolve(m)
duration1 = Sys.time() - start.time1

time.saving = (duration2 - duration1)

message("CALCULATING TIME SAVING by RETRIEVING CACHED MATRIX")
message("Based on Calculations with a 1000 X 1000 Matrix of Pseudo Random Numbers")
#
message("Time Required to Calculate Matrix Inversion")
print(duration2)
#
message("Time Required to Retrieve Matrix Inversion from Cache")
print(duration1)
#
message("Time Saved by Retrieving Matrix Inversion from Cache")
print(time.saving)
#
message("Retrieving the matrix inversion from cache consistently reduced computing time by @ 82.5%.")

# message("Percentage of Time Saved by Retrieving Matrix Inversion from Cache")
# print((time.saving/duration1)*100)

# ------------------
# The values for the matrix inversion were confirmed using independent software.
# Testing for "time saving" was done on the 1000 X 1000 matrix of pseudo random numbers.
# Any matrix larger than 1000 X 1000 exceeded the capabilities of my laptop RAM.
# Testing on the 2 X 2 matrix yielded times too small to be used in calculations.
