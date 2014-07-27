## the specific purpose here is to create an object that both holds a matrix and its inverse.
## it takes one argument, which is a square invertible matrix and it holds this value
## in whatever object this function is assigned to, this matrix will be accesable by four functions, which are stored as a list
## set, get, setmatrixinverse, getmatrixinverse
## the latter function returns the inverse
## but it is not necesary for these to be caclulated repetitively. if it has already been done, it will cache it.
## this works in conjunction with the function cacheSolve, which checks if it is cached or needs to be solved

makeCacheMatrix <- function (m = matrix()){
	i <- NULL ## if you are creating the instance for the 1st time, there can be nothing cached so i is null
	set <- function(y) { ## if you want to change the original argument/matrix
		m <<- y ## pass new matrix argument up to the parent
		i <<- NULL ## clears cache, because you have a new matrix
	}
	get <- function() m ## returns m, because maybe you forgot what the original matrix is
	setmatrixinverse <- function (matrixinverse) i<<- matrixinverse ##caches the inversematrix up an environment
	getmatrixinverse <- function () i ## returns i, because maybe you want to know what the inverse is
	list(set = set, get = get, setmatrixinverse = setmatrixinverse, getmatrixinverse = getmatrixinverse) ## returns a list of functions
}


## first checks if inverse is cached, if not calculates and returns it

cacheSolve <- function (instancename, ...) { 
	i <- instancename$getmatrixinverse()  ##first things first, gets the cache
	if(!is.null(i)) return (i) ## tests if the cache is null, if not returns the inverse matrix
	##everything after this is assuming the cache was empty
	matrixdata <- instancename$get() ##retrieves the original matrix
	i <- solve(matrixdata, ...) ## solve for the inverse matrix, the ... is for other arguments specific to the solve function
	instancename$setmatrixinverse(i) ## sets the cached inverse matrix
	i ## give the people what they want
}
