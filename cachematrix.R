## This function creates a special "matrix" object that can cache its inverse. 
makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL # resets the inverse to NULL
      }
      get <- function() x ## This will return original matrix
      setmatrix <- function(solve) m <<- solve ## here we set solve to outside environment
      getmatrix <- function() m         ## This returns cached values
}
## next we list all objects
list(set = set, get = get,
     setmatrix = setmatrix,
     getmatrix = getmatrix)
}

## This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above
cacheSolve <- function(x, ...) {
      ## Returns a matrix that is the inverse of 'x'
      m <- x$getmatrix() ##get the value of the inverse
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      } ## use if inverse is already cached
      data <- x$get()
      m <- solve(data, ...) ##make the inverse
      x$setmatrix(m) ## store the inverse
      m ## return matrix inverse
}
