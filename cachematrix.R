## Assignment 2 of R Programming course
#######################################

## Function responsible to 'augment' a matrix by providing a way of storing
## its value and provides means of setting, getting an associated cached 
## object (in this exercise we store the inverse of a matrix but other
## results of expensive calculations could be stored).
## Returns:
## List of functions to get set the matrix and get set the cached object.
## How it works:
## The function makeCacheMatrix is defined in the global frame but the list 
## that it returns is defined in the local frame of the function call and
## therefore inherits the environment of this call.
## The variable cache.obj and m are also defined within the function call 
## frame and therefore also inherit the environment of the call. 
## The functions bound to the list returned by makeCacheMatrix all reference 
## to free variables, these are variables not defined within theses functions'
## body but, due to the Lexical scoping of R, will be searched in the parent
## environment, that is the environment where the functions are defined. 
makeCacheMatrix <- function(m = matrix()) {
	   
        
        # Initialising variable to NULL, this is done in the environment created
        # when makeCacheMatrix is called
        cached.obj <- NULL
	
        # Stores some cache data into a variable that is defined in the frame
        # first call of makeCacheMatrix.
	setCachedObj <- function(o) {
                # cached.obj is a free variable (not defined within this function)
                # Using the <<- operator causes a search to be made through parent
                # environments for an existing definition of the variable being
                # assigned.
	        cached.obj <<- o
	}
	
        # Function that returns the free variable cached.obj, defined in the
        # parent environment of getCachedObj's call environment.
	getCachedObj <- function() cached.obj
	
        # Function that returns the initial matrix passed in to makeCacheMatrix
        # as an argument.
        # m within the body of get is a free variable
	get <- function() m
        
        # Function that resets the value of matrix and clears the cached object.
        # This is done only if the new matrix is different from the previously
        # stored matrix.
        set <- function(y) {
                
                if( ! identical( m, y ) ){ 
                        m <<- y
                        cached.obj <<- NULL
                }
        }
        
        # Returning a list that points to internal functions used to set get
        # the initial matrix and the stored cache data.
        list(get = get, set = set, setCachedObj = setCachedObj, getCachedObj = getCachedObj)
}


## Function to calculate the inverse of a matrix and cache the result.
## Returns either a newly calculated inverse or a previously cached result if
## available.
## Takes as arguments an 'augmented' matrix (the result of makeCacheMatrix's call)
## and a variable length of paramters ('...' or ellipses), these latter are passed 
## to the inverse function (solve).
cacheSolve <- function(x, ...) {
        
        # Checking if the 'augmented' matrix's cached object is not null, in that case 
        # skip any calculation and further assignment and return the cached object.
	inv <- x$getCachedObj()
        
 	if(is.null(inv)) {
                # If we are here we have to calculate the inverse, let's pull the original
                # matrix out the 'augmented' matrix object, calculate its inverse and store
                # it into the 'augmented' matrix                
                data <- x$get()
                inv <- solve(data, ...)
 		x$setCachedObj(inv)
 	}
	# returning the cached object or the freshly calculated inverse.
	inv
}
