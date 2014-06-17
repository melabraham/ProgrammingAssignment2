## makeCacheMatrix function creates a special matrix to 
## set the value of the matrix
## get the value of the matrix
## set the value of the matrix inverse
## get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {

  mat <- NULL
  set <- function(y) {
     x<<-y
     mat<<-NULL
   }
  get <- function()x

  setmatrix <- function(solve) mat <<- solve
  getmatrix <- function() mat
  list(set=set, get=get,
   setmatrix=setmatrix,
   getmatrix=getmatrix)
}


## cacheSolve function calculates the inverse of the matrix
## function first checks to see if the matrix inverse has been calculated
## if available, it gets it from the cache and skips the computation
## otherwise, it calculates the inverse and sets the value in the cache

cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'

    mat <- x$getmatrix()
    if(!is.null(mat)) {
            message("getting cached data")
            return(mat)
    }

    matrix <- x$get()
    mat <- solve(matrix, ...)
    x$setmatrix(mat)
    mat

}
