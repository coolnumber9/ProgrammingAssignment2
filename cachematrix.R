## Below are functions completed as part of the Coursera Programming 
## Assignment 2 (Peer-graded) : Lexical Scoping. 

## The two functions below revolves around the concept of caching
## in order to make time-consuming computations faster instead of
## repeating the computation.

## makeCacheMatrix function creates a special matrix object that can
## cache its inverse.

makeCacheMatrix <- function(origMat = matrix()) {

	invMat <- NULL ## this is also to identify if it's cached already or not...

	## Function to save the original matrix to another matrix variable
	makeMatrix <- function(matrix) {
		origMat <<- matrix
		invMat <<- NULL ## make sure theres no random value
	}

	## Function that returns the original matrix passed as an argument
	retMat <- function() origMat

	## Funtion that sets the inverse of the matrix
	doInverseMat <- function(inverse) invMat <<- inverse

	## Function that returs the inverse of the matrix
	retInverseMat <- function() invMat

	## Last execution to return the following lists...
	list (makeMatrix = makeMatrix, 
		retMat = retMat, 
		doInverseMat = doInverseMat,
		retInverseMat = retInverseMat)
}


## cacheSolve function computes the inverse of the special matrix returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(origMat, ...) {
    ## Return a matrix that is the inverse of 'x'
    sMat <- origMat$retInverseMat()

    if(!is.null(origMat)) {
    	return(origMat)
    }

    dat_to_be_solved <- origMat$retMat()

    sMat <- solve(dat_to_be_solved, ...)

    origMat$doInverseMat(sMat)

    sMat
}
