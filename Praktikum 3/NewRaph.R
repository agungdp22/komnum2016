fungsi <- function(x){
  return(x^2 - 2*x)
}

turunan <- function(x){
  return(2*x - 2)
}
#deklarasi kolom dalam tabel
ma <- matrix(data=0,nrow=1,ncol=0,byrow=FALSE,dimnames=NULL)
mb <- matrix(data=0,nrow=1,ncol=0,byrow=FALSE,dimnames=NULL)
mc <- matrix(data=0,nrow=1,ncol=0,byrow=FALSE,dimnames=NULL)

#tebakan awal bebas
x1 <- 58

for (i in 1:100) {
  ma <- matrix(c(ma,x1),nrow=1,byrow=FALSE,dimnames=NULL)
  xn <- x1 -(fungsi(x1)/turunan(x1))
  temp <- x1
  x1 <- xn
  mb <- matrix(c(mb,xn),nrow=1,byrow=FALSE,dimnames=NULL)
  mc <- matrix(c(mc,fungsi(xn)),nrow=1,byrow=FALSE,dimnames=NULL)
  if(abs(xn - temp) < 0.0001){
    break
  }
}
tabel <- data.frame(t(ma),t(mb),t(mc))
names(tabel) <- c("Nilai X1","Nilai Xn","Nilai Fungsi Xn")
tabel