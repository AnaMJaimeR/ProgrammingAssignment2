#These functions are used in order to cache the inverse of a matrix. The objective
#is to reduce computational efforts. Hence, if the inverse of a matrix needs to be used
#repeatedly during a code, calling the cached version would be a better option rather than 
#recalculating it. In this way, more efficiency may be achieved.

#In order to do this, I wrote down 2 different functions "makeCacheMatrix" and "cacheSolve".


##Firstly, I created the "makeCacheMatrix()" which has only 1 argument: a matrix "x" (its inverse will be 
##calculated). The default is set to be an empty matrix in order to avoid R errors in case any matrix
##is specified by the user.
##This "makeCacheMatrix()" function will generate a new environment with 4 different functions
##and 2 objects: "x" and "m" that will be used by the "cacheSolve()" (to be explained later). 
##The functions are:

##1. set() that allows to mutate the "x" matrix specified at a first place as the "makeCacheMatrix()"
##argument, into a new "x" matrix specified as the "set()" argument. The "x" matrix is stored in the 
##parent environment instead of the current environment by using the "<<-" operator. Also, it sets 
##"m" to NULL so no old cached information is wrongly returned.
##2. get() allows to call the "x" matrix that is currently stored.
##3. setsolve() allows to cache the inverse matrix in an object "m". This object is stored in the 
##parent environment instead of the current environment by using the "<<-" operator.
##4. getsolve() allows to call the "m" object which contains the currently cached inverse matrix.


makeCacheMatrix <- function(x = matrix()) {
  m<- NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setsolve<-function(solve) m<<-solve
  getsolve<-function() m
  list(set = set, get = get,
       setsolve = setsolve, getsolve = getsolve)
}

##Therefore, in general, what the "makeCacheMatrix()" does is to create a list with the 4 aforementioned
##functions. Also, the "m" object created by this function will cache the inverse of the "x" matrix.


##Secondly, I created the "cacheSolve()" which has only 1 argument: an object of the type "makeCacheMatrix".
##This function calls the "m" object by using the "getsolve()" function. After this, it
##verifies that "m" contains the cached inverse of matrix "x" ("m" is not NULL). If this is TRUE ("m" is
##not NULL) and the "x" matrix has not changed, then it will return the cached data; 
##otherwise ("m" is NULL) it will call the "x" matrix by using the "get()" function and it will 
##compute the inverse matrix via "solve()" function.
##Once the calculation is completed, the function will store it in the cache by using the "setsolve()"
##function for future use.
##The "x" matrix can be modified by using the "set()" function.

cacheSolve <- function(x, ...) {
  m<-x$getsolve()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  else{
    data<-x$get()
    if (ncol(data)==nrow(data)){
      m<-solve(data,...)
      if(!is.na(sum(m))){
        x$setsolve(m)
        m
      }
      else{
        message("data=NA in the matrix")
      }
    }
    else{
      message("The matrix must be square")

         }
  }
}

##While doing this process, the "cacheSolve()" function verifies that the "x" matrix is square and that 
#it does not cointain NAs nor is an empty matrix. If any of these conditions is FALSE, then the 
#function will return a message specifying what the problem is.