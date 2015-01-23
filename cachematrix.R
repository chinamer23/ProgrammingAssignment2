## makeCacheMatrix is a function that creates a special 'matrix' object that can cache its inverse. 
## cacheSolve is a function that computes the inverse of the special 'matrix' returned by makeCacheMatrix.  
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve 
## should retrieve the inverse from the cache.

makeCacheMatrix <- function(x = matrix()) {
	invx <- NULL
	set <-function(y){
		x<<-y
		invx<<-NULL
		}
		get<-function()x
		setinv<-function(inv) invx<<-inverse
		getinv<-function()invx
		list(set=set,get=get,
			setinv=setinv,
			getinv=getinv)
}


## cacheSolve is a function that computes the inverse of the special 'matrix' returned by makeCacheMatrix.  
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invx <- x$getinverse()
        if(!is.null(invx)){
        	message("getting cached inverse matrix")
        	return(invx)
        } else {
        invx <- solve(x$get())
        x$setinverse(invx)
        return(invx)
        }
}
$ git push github master

