##################### myDCT #########################
myDCT <- function(v){
  n=length(v)
  c=replicate(n,0)
  for(k in 0:(n-1)){
    ak=sqrt(2/n)
    if(k==0){
      ak=1/sqrt(n)
    }
    for(j in 0:(n-1)){
      c[k+1]=c[k+1]+
        (cos(pi*k*((2*j)+1)/(2*n))*v[j+1])
    }
    c[k+1]=c[k+1]*ak
  }
  return(c)
}
##################### myDCT2 #########################
myDCT2 <- function(mat){
  n=nrow(mat)
  m=ncol(mat)
  c=matrix(0,ncol=m, nrow=n)
  for(k in 0:(n-1)){
    for(l in 0:(m-1)){
      akl=2/sqrt(n*m)
      if(k==0 && l==0){
        akl=1/sqrt(n*m)
      }else if(k==0 || l==0){
        akl=sqrt(2/(n*m))
      }
      for(i in 0:(n-1)){
        for(j in 0:(m-1)){
          c[k+1,l+1]=c[k+1,l+1]+
            (cos(pi*k*((2*i)+1)/(2*n))*
               cos(pi*l*((2*j)+1)/(2*m))*
               (mat[i+1,j+1]))
        }
      }
      c[k+1,l+1]=c[k+1,l+1]*akl
    }
  }
  return(c)
}
##################### myIDCT #########################
myIDCT <- function(c){
  n=length(c)
  v=replicate(n,0)
  for(j in 0:(n-1)){
    for(k in 0:(n-1)){
      ak=sqrt(2/n)
      if(k==0){
        ak=1/sqrt(n)
      }
      v[j+1]=v[j+1]+(ak*cos(pi*k*((2*j)+1)/(2*n))*c[k+1])
    }
  }
  return(v)
}
##################### myIDCT2 #########################
myIDCT2 <- function(cmat){
  n=nrow(cmat)
  m=ncol(cmat)
  mat=matrix(0,ncol=m, nrow=n)
  for(i in 0:(n-1)){
    for(j in 0:(m-1)){
      for(k in 0:(n-1)){
        for(l in 0:(m-1)){
          akl=2/sqrt(n*m)
          if(k==0 && l==0){
            akl=1/sqrt(n*m)
          }else if(k==0 || l==0){
            akl=sqrt(2/(n*m))
          }
          mat[i+1,j+1]=mat[i+1,j+1]+
            (akl*cos(pi*k*((2*i)+1)/(2*n))*
               cos(pi*l*((2*j)+1)/(2*m))*
               (cmat[k+1,l+1]))
        }
      }
    }
  }
  return(mat)
}