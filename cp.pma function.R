cp.pma<-function(data,level,type,effsize,m=1,sample.size,tau.n=0,a.level=0.05,alternative.ES,p.c){
  
  fa1<-function(data,level,type,effsize){
    
    D=metaD=NULL
    
    if (level=="arm" & type=="binary" & effsize=="OR"){
      D=data
      colnames(D) <- c("study","year","r1","n1","r2","n2")
      metaD <- metabin(c(D$r1), c(D$n1), c(D$r2), c(D$n2), sm="OR", method="I",allstudies=T) #pairwise meta-analysis
    }
    if (level=="arm" & type=="binary" & effsize=="RR"){
      D=data
      colnames(D) <- c("study","year","r1","n1","r2","n2")
      metaD <- metabin(c(D$r1), c(D$n1), c(D$r2), c(D$n2), sm="RR", method="I",allstudies=T) #pairwise meta-analysis
    }
    if (level=="arm" & type=="continuous" & effsize=="MD"){
      D=data
      colnames(D) <- c("study","year","y1","sd1","n1","y2","sd2","n2")
      metaD <- metacont(c(D$n1),c(D$y1),c(D$sd1),c(D$n2),c(D$y2),c(D$sd2),sm="MD") #pairwise meta-analysis
    }
    if (level=="arm" & type=="continuous" & effsize=="SMD"){
      D=data
      colnames(D) <- c("study","year","y1","sd1","n1","y2","sd2","n2")
      metaD <- metacont(c(D$n1),c(D$y1),c(D$sd1),c(D$n2),c(D$y2),c(D$sd2),sm="SMD") #pairwise meta-analysis
    }
    if (level=="study" & type=="binary" & effsize=="OR"){
      D=data
      colnames(D) <- c("study","year","effect","se")
      metaD<-metagen(D$effect, D$se,sm="OR", tau.common=TRUE,byvar=comp)
    }
    if (level=="study" & type=="binary" & effsize=="RR"){
      D=data
      colnames(D) <- c("study","year","effect","se")
      metaD<-metagen(D$effect, D$se,sm="RR", tau.common=TRUE)
    }
    if (level=="study" & type=="continuous" & effsize=="MD"){
      D=data
      colnames(D) <- c("study","year","effect","se")
      metaD<-metagen(D$effect, D$se,sm="MD", tau.common=TRUE)
    }
    if (level=="study" & type=="continuous" & effsize=="SMD"){
      D=data
      colnames(D) <- c("study","year","effect","se")
      metaD<-metagen(D$effect, D$se,sm="SMD", tau.common=TRUE)
    }
    if (level=="study" & type=="timetoevent" & effsize=="HR"){
      D=data
      colnames(D) <- c("study","year","effect","se")
      metaD<-metagen(D$effect, D$se,sm="HR", tau.common=TRUE)
    }
    
    y=metaD$TE #estimated treatment effect of individual studies
    se=metaD$seTE #estimated standard errors of individual studies
    #tau.old=metaD$tau
    tau.old=0
    
    n=metaD$k
    #mu=metaD$TE.random
    mu=metaD$TE.fixed
    
    list(D=D, level=level, type=type, effsize=effsize, y=round(y,2),
               se=round(se,2),tau.old=tau.old,n=n,mu=mu)
  }
  
  fa1impl<-fa1(data<-data,level=level,type=type,effsize=effsize)
  
  fa2<-function(D,m=1,sample.size,level,type,effsize,alternative.ES,p.c){

      delta=log(1/alternative.ES)
   
    
    w.new=NULL
    

      if (level=="arm" & type=="binary" & effsize=="OR"){
    
       # p.t=0.5750499 
       # p.c=0.49 
        odds.t=p.c/(alternative.ES*(1-p.c))
        p.t=odds.t/(1+odds.t)
        w.new=1/(2/(sample.size/m)*((1/p.c)+(1/(1-p.c))+(1/p.t)+(1/(1-p.t))))
      }
      
    invisible(list(w.new=w.new,delta=delta))
  }
  
  fa2impl<-fa2(D=fa1impl$D,m=m, sample.size=sample.size,alternative.ES=alternative.ES,p.c=p.c,
               level=level,type=type,effsize=effsize)
  
  fb<-function(delta,a.level,y,se,tau.o,tau.n,w.new,m,n){
    
    if(tau.n=="tau.o"){
      tau.n=tau.o
    }
    
    w.old=1/(se^2+tau.o^2)
    tau.a=sqrt((n/(n+m))*tau.o^2+(m/(n+m))*tau.n^2)
    w.rev=1/((1/w.old)+tau.a^2) 
    c.a=qnorm(1-a.level/2,0,1)
    
    phi1=pnorm(-sqrt((1+w.new*tau.a^2)/(m*w.new))
               *(c.a*sqrt(sum(w.rev)+((m*w.new)/(1+w.new*tau.a^2)))-(sum(w.rev*y)))
               +((m*delta)/(sqrt((m/w.new)+m*tau.a^2))))
    phi2=pnorm(-sqrt((1+w.new*tau.a^2)/(m*w.new))
               *(c.a*sqrt(sum(w.rev)+((m*w.new)/(1+w.new*tau.a^2)))-(sum(w.rev*y)))
               -((m*delta)/(sqrt((m/w.new)+m*tau.a^2))))
    cp=phi1+phi2
    invisible(list(cp=cp))
  }
  
  fbimpl<-fb(delta=fa2impl$delta,a.level=a.level,y=fa1impl$y,se=fa1impl$se,
             tau.o=fa1impl$tau.old,tau.n=tau.n,w.new=fa2impl$w.new,m=m,n=fa1impl$n)
  
  list(fa1impl,w.new=fa2impl$w.new, cp=fbimpl$cp)
}