## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# This function returns a set of functions applicable to the passed-in matrix for
# use in storing and retrieving the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
	#Start by declaring the inverse as NULL
	i <- NULL
	
	# A funtion to set the matrix to a provided value and also nullify the inverse
	set <- function(y){
		x <<-y
		i<<- NULL
	}
	
	# A function to get the matrix
	get<- function() x
	
	# A function to set the inverse of the matrix
	setinverse<- function(inverse) i<<-inverse
	
	# A function to get the inverse of the matrix
	getinverse<- function() i
	
	# Return the methods for use on the matrix
	list(set = set, get=get,
			setinverse=setinverse,
			getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        #Retrieve inverse from x
        i <- x$getinverse()
        
        #Check if x's inverse has been calculated
        if(!is.null(i)){
        	
        	# If so, return cached inverse
        	message("Getting cached data...")
        	return(i)
        }
        
        #Otherwise get the matrix data from x
        data <- x$get()
        
        # Get the inverse of matrix x using the <solve> function
        i <- solve(data,...)
        
        #Store solved x in x's cache
        x$setinverse(i)
        
        #Return the inverse of x
        i
}
