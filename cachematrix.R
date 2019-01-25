## Matrix computation is an costly operation. This function will allow matrix inverse
## to be cached. It it is needed multiple times in the program, then this function
## will return inverse from cache rather than recalculating it.

## This function will create operations like get, set, getinverse, setinverse that
## can be performed on a matrix.

makeCacheMatrix <- function(x = matrix()) {
	sol <- NULL
	set <- function(y) {
		if (identical(class(y), "matrix")){
			ydim<-dim(y)
			if (ydim[1] == ydim[2]){
				x <<- y
                sol <<- NULL
			}
		} 
	}
	get <- function() { x }
	setSolve <- function(s) { sol <<- s } 
	getSolve <- function() { sol }
	list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}


## This function will compute the inverse of a special matrix. If matrix has 
## not changed then this function will retrieve the inverse from cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
	sol <- x$getSolve()
  if(!is.null(sol)) {
    message("getting cached data")
    return(sol)
  }
  #assume data is always solvable
  data <- x$get()
	sol <- solve(data) 
	x$setSolve(sol)
	sol
}
