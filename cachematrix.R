# Matrix inversion beyond 1000 X 1000 exceeded the capability of my laptop and
# took several/many minutes to compute. Requiring this calculating time, perhaps 
# for many iterations in a loop, would drastically slow down computer analysis.
# Calculating the matrix inversion once and using that result many times would
# significantly speed up computer analysis involving large data sets.

# FUNCTION 1
# ----------
# to create and retrieve both the original matrix and the matrix inversion

makeCacheMatrix <- function(x = matrix()) {
  inverse1 <- NULL
  set <- function(y) {
    x <<- y
    inverse1 <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inverse1 <<- inverse
  getinverse <- function() inverse1
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# FUNCTION 2
# ----------
# checks for matrix inversion in cache and, if present, skips the calculation
# if matrix inversion is not found, program flow drops down to calculate inversion
# and return the result

cacheSolve <- function(x, ...) {
  inverse1 <- x$getinverse()
  if(!is.null(inverse1)) {
    start.time1 = Sys.time()
    print(start.time1)
    message("Retrieving Cached Inverse Matrix")
    # dur1 = Sys.time() - start.time1
    return(inverse1)
  }
  message("Calculating Inverse Matrix")
  start.time2 = Sys.time()
  print(start.time2)
  data <- x$get()
  inverse1 <- solve(data)
  x$setinverse(inverse1)
  # dur2 = Sys.time() - start.time2
  inverse1
}
# ====================================================================

# TESTING HYPOTHESIS:
# ------------------
# testing with pseudo random numbers using both 1000 X 1000 and 10000 X 10000
# matrices produced more authentic results but exceeded capabiliy of my laptop
# this demo uses much smaller 2 X 2 matrix with independently validated results

# set.seed(192837465)
# r = rnorm(1000000)
# x = matrix(r, nrow=1000, ncol=1000)
x = matrix(r, nrow=2, ncol=2)
x = rbind(c(2, 5), c(9, 4))
m = makeCacheMatrix(x)
message("Printing Original Matrix")
m$get()

# FIRST ITERATION
cacheSolve(m)

# SECOND ITERATION
cacheSolve(m)

# CALCULATING TIME SAVING by RETRIEVING CACHED MATRIX
# message("Time Saved by Retrieving Cached Inverse Matrix")
# ------------------
# message("Currently I am working to integrate a routine to")
# message("calculate the time saved by accessing the cached")
# message("matrix, as I had in an earlier version.")
# message("Any help would be appreciated!   :-)")
# ------------------
# values for inverse matrix were confirmed using independent software
# matrix beyond 1000 X 1000 exceeded capabilities of my laptop
