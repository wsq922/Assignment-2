###makeCacheMatrix
makeCacheMatrix<-function(x=matrix()){
	m<-Null
	set<-function(y){
		x<<-y
		m<<-y
	}
	get<-function() x
	setCacheMatrix<-function(inverse) m<<-inverse
	getCacheMatrix<-function() m
	list(set=set, get=get,
	setCacheMatrix=setCacheMatrix,
	getCacheMatrix=getCacheMatrix)
}
###cachesSolve
cacheSolve<-function(x,...) {
	m<-x$getCacheMatrix()
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}
	data<-x$get()
	m<-solve(data)
	x$setCacheMatrix(m)
	m
}
