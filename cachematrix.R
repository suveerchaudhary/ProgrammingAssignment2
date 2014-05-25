##  This function will accept a matrix to be processes by second function #
###  Below comment lines are to test the code
# create matrix option 1            :  a <- makeCacheMatrix(matrix(1:4, 2,2))        #
# create matrix option 2            :  a <- makeCacheMatrix(matrix(c(7, 12, 16 , 2, 45, 3, 1, 5, 7), nrow=3, ncol=3)) #
# Assign to a variable to test later : b <- a$get()                                                                    #
# Get Inverse of matrix and assign  :  e<- cacheSolve(a)                                                               #
# Test your result (it should be Identity matrix) :   e%*%b                                                                           #
#
makeCacheMatrix <- function(x = numeric()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setSolve <- function(Solve) m <<- Solve
      getSolve <- function() m
      list(set = set, get = get,
           setSolve = setSolve,
           getSolve = getSolve)
}

##  This function will calculate Inverse of a matrix 
# If same Matrix was already calculated, it will get the cached matrix value instead of calculating again

cacheSolve <- function(x, ...) {
      m <- x$getSolve()
      if(!is.null(m)) {
            message("getting cached matrix")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...) #calculates Inverse of Matrix#
      x$setSolve(m)
      m
}
