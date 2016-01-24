## Two independent functions listed in this script; the first will take some
## input and return a matrix of the input, also while finding and storing the
## inverse of said matrix in "z". The second function will look for the inverse
## of the created matrix and if none is found will compute and return the 
## initially sought after inverse.

## Creates function from input and saves the inverse in "z"

makeCacheMatrix <- function(x = matrix()) {
        slv<- NULL
        set<-function(y){
                x<<-y
                slv<<-NULL
        }
        get<-function()x
        setslv<-function(solv)slv<<-solv
        getslv<-function()slv
        list(set=set,get=get,setslv=setslv,getslv=getslv)
}


## Attempts to return a saved value of "z" being the inverse of a matrix.
## If no value of "z" is found, this function will compute and return the
## inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        slv<-x$getslv()
        if(!is.null(slv)){
                print("Abra Kadabra")
                return(slv)
        }
        m<-x$get()
        slv<-solve(m,...)
        x$setslv(slv)
        slv
}
