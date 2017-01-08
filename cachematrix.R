##Week 3 Lexical Scoping Assignment
##Caching the inverse of a Matrix

##makeCacheMatrix creating a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set<-function(y){
                x<<-y
                inv<<-NULL
                }
        get<-function()x
        setInverse<-function(inverse) inv<<-inverse
        getInverse<-function() inv
        list(set=set, get=get, set=setInverse, get=getInverse)
}


##cacheSolve computing the inverse of matrix created above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getinverse(
                if(!is.null(inv)) {
                        message("Getting Cached Data")
                        return(inv)
                        }
                data<-x$getInverse
                inv<-solve(data,...)
                x$setinverse(inv)
                inv
}
