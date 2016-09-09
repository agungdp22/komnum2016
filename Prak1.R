error_absolute<-function(asli,pendekatan){
  return (abs(asli-pendekatan))  
}

error_relative<-function(asli,pendekatan){
  return (abs((asli-pendekatan)/asli)*100)
}

#fungsi pembulatan
fungsi_compute<-function(x){
  return (x*signif((sqrt(x+1)-sqrt(x)),6))
}

#fungsi tanpa pembulatan
fungsi_true<-function(x){
  return (x*(sqrt(x+1)-sqrt(x)))
}

#deklarasi kolom tabel
ma <- matrix(data=0,nrow=1,ncol=0,byrow=FALSE,dimnames=NULL)
mb <- matrix(data=0,nrow=1,ncol=0,byrow=FALSE,dimnames=NULL)
mc <- matrix(data=0,nrow=1,ncol=0,byrow=FALSE,dimnames=NULL)
md <- matrix(data=0,nrow=1,ncol=0,byrow=FALSE,dimnames=NULL)
me <- matrix(data=0,nrow=1,ncol=0,byrow=FALSE,dimnames=NULL)

x<-1
for(i in 1:6){
  true<- fungsi_true(x)
  compute<- fungsi_compute(x)
  ma <- matrix(c(ma,x),nrow=1,byrow=FALSE,dimnames=NULL)
  mb <- matrix(c(mb,compute),nrow=1,byrow=FALSE,dimnames=NULL)
  mc <- matrix(c(mc,true),nrow=1,byrow=FALSE,dimnames=NULL)
  md <- matrix(c(md,error_absolute(true,compute)),nrow=1,byrow=FALSE,dimnames=NULL)
  me <- matrix(c(me,error_relative(true,compute)),nrow=1,byrow=FALSE,dimnames=NULL)
  x<-x*10
}
tabel <- data.frame(t(ma),t(mb),t(mc),t(md),t(me))
names(tabel) <- c("X","Computed","True","Error Absolute","Error Relative")
tabel