cp.nma<-function(x,design,sample.size.range=seq(10,1000,1),tau.n=0,delta=NA,a.level=0.05,p.c, alternative.ES){
  
  #define design variables
  if(!is.character(design)){stop("You need to define the comparison you want to evaluate in a new study")}
  mm<-match(x$comparisons,design,nomatch = 0)
  if(sum(mm)==0) {stop("You need to define an existing comparison")}
  m<-rbind(mm)
  comp=design
  m1=m[1,]
  
  #get variables from the x cp.nma.setup object
  level=x$level
  type=x$type
  effsize=x$effsize
  
  sample.size.matrix=matrix(0,nrow=length(sample.size.range),ncol=length(x$comparisons))
  
  for(i in 1:length(sample.size.range)){
    sample.size.matrix[i,]=sample.size.range[i]
  }
  
  for (j in 1:length(x$comparisons)){
    if (m1[j]==0){sample.size.matrix[,j]=0}
  }
  
  cp.nma1<-function(x,m,sample.size,tau.n=0,delta=NA,level,type,effsize,a.level,comp,p.c, alternative.ES){
    
    a2fun<-function(x,m,sample.size,tau.n=0,delta=NA,level,type,effsize, p.c, alternative.ES){
      tau.o=x$tau.o
      #tau.o=0
      
      if(tau.n=="tau.o"){
        #tau.n=x$tau.o
        tau.n=0
      }
      if (is.na(delta)){
        delta=x$TE.net
      }
      if (level=="arm" & type=="binary"){
        
        t.1=t.2=p.1=p.2=w.new=n=rep(0,length(x$comparisons))
        fort<-unlist(strsplit(x$comparisons, ":"))
        fort1<-seq(1, 2*length(x$comparisons)-1,2)
        fort2<-seq(2, 2*length(x$comparisons),2)
        t.1<-fort[fort1]
        t.2<-fort[fort2]
        input<-data.frame(x$comparisons,n,m,sample.size,delta,t.1,t.2,p.1,p.2,w.new)
        D=x$D
        #D$p1=D$r1/D$n1
        #D$p1=0.581427
        #D$p2=D$r2/D$n2
        #D$p2=(0.77*0.581427*(1-0.581427))/(1+(0.77*0.581427*(1-0.581427)))
        odds.t=p.c/(alternative.ES*(1-p.c))
        p.t=odds.t/(1+odds.t)
        D$p2=p.c
        D$p1=p.t
        
        #p.t=(exp(delta)*p.c*(1-p.c))/(1+(exp(delta)*p.c*(1-p.c)))
        treatments=data.frame(x$trts)
        p1fortr<-p2fortr<-pfortr<-rep(0,length(x$trts))
        for (i in 1:length(p1fortr)){
          p1fortr[i]=mean(subset(D$p1,as.character(D$treat1)==treatments[i,1]))
          p2fortr[i]=mean(subset(D$p2,as.character(D$treat2)==treatments[i,1]))
          pfortr[i]=mean(c(p1fortr[i],p2fortr[i]),na.rm=TRUE)
        }
        treatments$p1=p1fortr
        treatments$p2=p2fortr
        treatments$p=pfortr
        tc=min(which((treatments$x.trts==as.character(input$t.1[1]))== TRUE))
        input$p.1[1]=as.numeric(treatments$p[tc])
        for (i in 1:length(input$p.1)){
          tc=min(which((treatments$x.trts==as.character(input$t.1[i]))== TRUE))
          input$p.1[i]=as.numeric(treatments$p[tc])
        }
        for (i in 1:length(input$p.2)){
          tc=min(which((treatments$x.trts==as.character(input$t.2[i]))== TRUE))
          input$p.2[i]=as.numeric(treatments$p[tc])
        }
        if (effsize=="OR"){
          input$w.new=1/(2/(input$sample.size/input$m)*
                           ((1/as.numeric(input$p.1))+(1/(1-as.numeric(input$p.1)))
                            +(1/as.numeric(input$p.2))+(1/(1-as.numeric(input$p.2)))))
        }
        
        
      }
      for(i in 1:length(input$w.new)){
        if (is.na(input$w.new[i])==TRUE){input$w.new[i]=0}
      }
      studies=x$studies
      for (i in 1:length(input$n)){
        if (length(which((as.character(studies$comparison)==as.character(input$x.comparisons[i]))== TRUE))>0){
          a=min(which((as.character(studies$comparison)==as.character(input$x.comparisons[i]))== TRUE))
          input$n[i]=studies$freq[a]
        }
      }
      
      tau.a=sqrt((sum(input$n)/(sum(input$n)+sum(input$m)))*tau.o^2
                 +(sum(input$m)/(sum(input$n)+sum(input$m)))*tau.n^2)
      
      invisible(list(input=input,tau.a=tau.a))
      
    }
    
    a2funimpl<-a2fun(x=x,m=m,sample.size=sample.size,tau.n=tau.n,delta=delta,level=level,type=type,effsize=effsize, p.c=p.c, alternative.ES=alternative.ES)
    
    cfun<-function(comp, X.full=x$X.full, X.obs=x$X.obs, V=x$V, TE.dir=x$TE.dir, tau.a=a2funimpl$tau.a, a.level){
      
      Y.obs=rbind(X.obs,X.obs)
      
      Vn=matrix(0,nrow=nrow(V),ncol=ncol(V))
      diag(Vn)=1000
      
      input=a2funimpl$input
      
      u<-which(input$m!=0)
      v<-rep(0,length(u))
      
      for (i in 1:v){
        u[i]=u
        v[i]=  which(rownames(V)==input$x.comparisons[u])
        Vn[v,v]=(1+input$w.new[u]*tau.a^2)/(input$m[u]*input$w.new[u])
      }
      
      rownames(Vn)=rownames(V)
      colnames(Vn)=colnames(V)
      Va=adiag(V,Vn)
      
      Mn=matrix(0,nrow(as.matrix(TE.dir)),ncol(as.matrix(TE.dir)))
      Mstar=rbind(as.matrix(TE.dir),as.matrix(TE.dir)) #fix this!!!
      
      Ha=X.full %*% solve(t(Y.obs) %*% solve(Va) %*% Y.obs) %*% t(Y.obs) %*% solve(Va)
      C=X.full %*% solve(t(Y.obs) %*% solve(Va) %*% Y.obs) %*% t(X.full)
      
      muA=Ha %*% Mstar
      
      #data.frame(x$TE.net, muA) #just checking
      
      c.a=qnorm(1-a.level/2,0,1)
      
      tcomp=which(rownames(Ha)==comp)
      
      den=sqrt(t(Ha[tcomp,(ncol(Ha)/2+1):(ncol(Ha))]) %*% solve(Vn) %*% (Ha[tcomp,(ncol(Ha)/2+1):(ncol(Ha))]))
      
      phi1=pnorm((-c.a*sqrt(C[tcomp,tcomp])-t(Ha[tcomp,]) %*% Mstar)/den)
      phi2=pnorm((-c.a*sqrt(C[tcomp,tcomp])+t(Ha[tcomp,]) %*% Mstar)/den)
      
      cp=phi1+phi2
      list(cp=cp)
    }
    
    cfunimpl=cfun(comp=comp, X.full=x$X.full, X.obs=x$X.obs, V=x$V, TE.dir=x$TE.dir,
                  tau.a=a2funimpl$tau.a, a.level=a.level)
    
    invisible(list(cp=cfunimpl$cp))
    
  }
  
  cp.nma.matrix=matrix(0,nrow=(length(sample.size.range)),ncol=nrow(m))
  
  for (i in 1:length(sample.size.range)){
    for (j in 1:nrow(m)){
      cp.nma.impl<-cp.nma1(x,m=m[j,],sample.size=sample.size.matrix[i,],tau.n=tau.n,delta=delta,
                          level=level,type=type,effsize=effsize,a.level=a.level,comp=comp, p.c=p.c, alternative.ES=alternative.ES)
      cp.nma.matrix[i,j]=cp.nma.impl$cp
    }
  }
  
  
  list(sample.size=sample.size.range, adding.study.comparing=comp, conditional.power=cp.nma.matrix)
}
