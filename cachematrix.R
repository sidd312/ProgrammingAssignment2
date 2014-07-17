## Put comments here that give an overall description of what your
## functions do
#makeCacheMatrix : This function creates a special "matrix" object
#that can cache its inverse.
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	m<-NULL
	##set the value of the vector
	set<-function(y){
		x<<-y
		m<<-NULL
	}
	##get the value of the vector
	get <-function() x
	##set the value of the mean
	setsolve<-function(solve) m<<-solve
	##get the value of the mean
	getsolve <-function() m
	list(set=set, get=get,
	setmean=setmean,getmean=getmean)
}


## Write a short comment describing this function
##This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. If the inverse has
##already been calculated (and the matrix has not changed), then
##`cacheSolve` should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
		m<-x$getsolve()
		if(!is.null(m)){
			message("getting cashed data")
			return(m)
		}
		data <-x$get()
		m<-solve(data, ...)
		x$setsolve(m)
		m
        ## Return a matrix that is the inverse of 'x'
}
