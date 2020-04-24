common.Wcheckformat2 <- function(W)
{
    time.use <- Sys.time()
    #### Check W is a matrix of the correct dimension
    if(!is.matrix(W)) stop("W is not a matrix.", call.=FALSE)
    n <- nrow(W)
    if(ncol(W)!= n) stop("W is not a square matrix.", call.=FALSE)    
    print( paste( 'Check W section takes', Sys.time() - time.use))
    
    #### Check validity of inputed W matrix
    if(sum(is.na(W))>0) stop("W has missing 'NA' values.", call.=FALSE)
    if(!is.numeric(W)) stop("W has non-numeric values.", call.=FALSE)
    if(min(W)<0) stop("W has negative elements.", call.=FALSE)
    if(sum(W!=t(W))>0) stop("W is not symmetric.", call.=FALSE)
    if(min(apply(W, 1, sum))==0) stop("W has some areas with no neighbours (one of the row sums equals zero).", call.=FALSE)    
    print( paste( 'Check validity section takes', Sys.time() - time.use))
    
    
    #### Create the triplet form
    W.triplet <- c(NA, NA, NA)
    for(i in 1:n)
    {
        for(j in 1:n)
        {
            if(W[i,j]>0)
            {
                W.triplet <- rbind(W.triplet, c(i,j, W[i,j]))     
            }else{}
        }
    }
    print( paste( ' triplet for loops section takes', Sys.time() - time.use))
    W.triplet <- W.triplet[-1, ]     
    n.triplet <- nrow(W.triplet) 
    W.triplet.sum <- tapply(W.triplet[ ,3], W.triplet[ ,1], sum)
    n.neighbours <- tapply(W.triplet[ ,3], W.triplet[ ,1], length)
    print( paste( 'Create triplet form section takes', Sys.time() - time.use))
    
    
    #### Create the start and finish points for W updating
    W.begfin <- array(NA, c(n, 2))     
    temp <- 1
    for(i in 1:n)
    {
        W.begfin[i, ] <- c(temp, (temp + n.neighbours[i]-1))
        temp <- temp + n.neighbours[i]
    }
    print( paste( 'Check start and finish points section takes', Sys.time() - time.use))
    
    
    #### Return the critical quantities
    results <- list(W=W, W.triplet=W.triplet, n.triplet=n.triplet, W.triplet.sum=W.triplet.sum, n.neighbours=n.neighbours, W.begfin=W.begfin, n=n)
    return(results)   
}
