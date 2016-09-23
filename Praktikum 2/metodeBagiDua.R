fungsi <- function(x){
  return (x^2 + 7*x + 10)
}

ma <- matrix(data=0,nrow=1,ncol=0,byrow=FALSE,dimnames=NULL)
mb <- matrix(data=0,nrow=1,ncol=0,byrow=FALSE,dimnames=NULL)
mc <- matrix(data=0,nrow=1,ncol=0,byrow=FALSE,dimnames=NULL)
md <- matrix(data=0,nrow=1,ncol=0,byrow=FALSE,dimnames=NULL)
me <- matrix(data=0,nrow=1,ncol=0,byrow=FALSE,dimnames=NULL)

a <- -10
b <- -3

iterasi <-1
while(abs(a-b) > 0.0000001){
  c <- (a+b)/2
  ma <- matrix(c(ma,iterasi),nrow=1,byrow=FALSE,dimnames=NULL)
  mb <- matrix(c(mb,a),nrow=1,byrow=FALSE,dimnames=NULL)
  mc <- matrix(c(mc,b),nrow=1,byrow=FALSE,dimnames=NULL)
  md <- matrix(c(md,c),nrow=1,byrow=FALSE,dimnames=NULL)
  me <- matrix(c(me,fungsi(c)),nrow=1,byrow=FALSE,dimnames=NULL)
  if(fungsi(a)*fungsi(c) < 0){
    b <- c
  }
  else{
    a <- c
  }
  iterasi <- iterasi+1
}
tabel <- data.frame(t(ma),t(mb),t(mc),t(md),t(me))
names(tabel) <- c("iterasi","nilai a","nilai b","nilai c","nilai f(c)")
tabel