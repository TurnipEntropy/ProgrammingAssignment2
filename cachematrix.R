#Description of makeCacheMatrix:
# makeCacheMatrix operates as a class by compiling functions into a list. It
# also serves to store the value of x and inv in the parent level, allowing
# those variables to be changed as necessary, and allowing inv, the inverse
# of x, to be cached. Descriptions of the functions provided by makeCacheMatrix
# are provided in the source code.

#Usage:
# makeCacheMatrix(x)
# Usage of functions contained in makeCacheMatrix:
#  var.name <- makeCacheMatrix(data)
#  var.name$functionName() //where functionName is a function in the list

#Arguments:
#  x      Any R Object can be accepted as input. Not all functions contained
#         in makeCacheMatrix will work on all R objects. Numeric matrices or
#         dataframes are the anticipated objects.

#Return value: 
# A list of functions contained in the makeCacheMatrix function. 
# While the returned type is list, the list taken as a whole can be considered 
# an object of type makeCacheMatrix at a conceptual level.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    #the set function redefines x to equal y and sets inv back to NULL, 
    #since the inverse has not been calculated for the new data. It does so 
    #at the parent level (<<-), which is still enclosed in the function 
    #makeCacheMatrix.
    set <- function (y){
        x <<- y
        inv <<- NULL
    }
    
    #the get function returns x
    get <- function() {x}
    
    #sets inv to the value of inverse. Once again, this is done at the parent
    #level (<<-), which allows for access at a later time through getInverse(). 
    #This is intended to be used to cache the inverse of x, but may be used 
    #to cache any desired value.
    setInverse <- function(inverse) {
        inv <<- inverse
    }
    
    #returns the value of inv. 
    getInverse <- function() {inv}
    
    #-------------------------------------------------------------------
    #THE FOLLOWING FUNCTIONS ARE NOT REQUIRED FOR THE ASSIGNMENT:
    
    #checks if x is a singular matrix. The det() function finds the
    #determinant of the matrix, which is zero if the matrix is singular. 
    #For a matrix to be invertible, it must be nonsingular.
    is.Singular <- function(){
        x.correctType <- as.matrix(x)
        if(det(x.correctType) == 0){TRUE}
        else {FALSE}
    }
    
    #checks if x is 2 dimensional. This is a requirement for 
    #inversion.  
    is.2D <- function() {
        if (length(dim(x)) == 2) {TRUE}
        else {FALSE}
    }
    
    #checks if x is square. For a matrix to be invertible, it must be
    #square.
    is.Square <- function (){    
        if (is.2D()) {
            if(dim(x)[1] == dim(x)[2]){TRUE}
            else{FALSE}
        } else {FALSE}
    }
    #------------------------------------------------------------------------
    #THE FOLLOWING IS ONCE AGAIN REQUIRED FOR THE ASSIGNMENT:
    
    #Creates the list of functions that is returned by the function 
    #makeCacheMatrix.
    list (set = set, get = get, setInverse = setInverse, 
          getInverse = getInverse,is.Singular = is.Singular,
          is.2D = is.2D, is.Square = is.Square)
}




#Description of cacheSolve:
# The cacheSolve function takes data supplied from a list of functions,
# such as makeCacheMatrix, and either pulls the stored value of its inverse,
# or calculates the inverse if it has not yet been calculated.

#Usage: 
# cacheSolve(x, ...)

#Arguments:
#  x     A list of functions, such as makeCacheMatrix. This list must contain
#        the following functions:  getInverse(),is.Square(), is.Singular(), 
#        is.2D(), get(),and setInverse().
#
# ...    Other arguments that can be accepted by the solve() function.


#Return value:
#   m    cacheSolve returns the inverse of a matrix, accesible by funList, 
#        if the matrix is invertible. Otherwise returns null.

cacheSolve <- function(funList, ...) {
    m <- funList$getInverse()
    
    #if the inverse of the matrix has already been taken, then m will 
    #not equal NULL, and no further operations will be necessary.
    #m is returned
    if(!is.null(m)){
        message("cached data found and returned")
        return(m)
    }
    
    #if m is null, then the inverse has not been taken and will be here. 
    #The if statement below checks to see if the matrix is invertible*. 
    #If it is invertible, then data is pulled from funList and inverted.
    #The variable m stores the inverted data. If it is not invertible, a 
    #warning message is sent to the console and the user is warned that the 
    #value of m has been set to NULL.
    #* An invertible matrix is square, nonsingular, and 2-dimensional. This
    #check was not required by the assignment, but also was not disallowed.
    if (funList$is.Square() && !funList$is.Singular() && funList$is.2D()) {
        
        data <- funList$get()
        m <- solve(data, ...)
        funList$setInverse(m)
        return(m)
        
    } else { 
        warning("Matrix is non-invertible. m has been set to NULL.")
        m <- NULL
        funList$setInverse(m)
        return(m)
    }
}
