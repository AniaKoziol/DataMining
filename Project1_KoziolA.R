# szybki

if(!require(infotheo)){
  install.packages("infotheo")
  library(infotheo) 
}

# rozwiÄ…zanie

CMIMselection <- function(X, y, kmax){
  
  stopifnot(is.numeric(kmax), kmax>0, floor(kmax)==kmax, kmax <= ncol(X), is.data.frame(X), ncol(X)>1)
  p<- ncol(X)
  ind<- numeric(kmax)
  score <- numeric(kmax)
  m <- rep(0, p)
  pom <- 0
  part_score <- numeric(p)
  
  
  part_score <- unname(unlist(lapply(X, function(x){mutinformation(y,x)})))
  
  
  for(k in 1:kmax)
  {
    pom <- 0
    for (i in 1:p)
    {
      
      while ((part_score[i] > pom) & (m[i]<(k-1)))
      {
        m[i] <- m[i]+1 
        part_score[i] <- min(part_score[i], condinformation(y, X[,i], X[,ind[m[i]]]))  
      }
      
      if (part_score[i] > pom) 
      {
        pom <- part_score[i] 
        ind[k] <-  i   
        score[k] <- part_score[ind[k]]
      }
    }
  }
  
  return(list("S" = ind, "score" = score))
} 
 






















