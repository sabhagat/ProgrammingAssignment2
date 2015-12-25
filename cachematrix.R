## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
