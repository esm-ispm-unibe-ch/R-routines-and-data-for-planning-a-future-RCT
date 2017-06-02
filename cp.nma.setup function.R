
test=function(){print("labla")}
cp.nma.setup<-function(data,level,type,effsize){
  
  DataInput<-function(data,level,type,effsize){
    if (level=="arm" & type=="binary"){
      
      data<- data[order(data$id),] #sort by id
      data$arm<-sequence(tabulate(data$id)) #generate new variable: treatment arm
      dataw=reshape(data,
                    idvar=c("year","study","id"),
                    v.names=c("t","n","r"),
                    timevar=c("arm"),
                    direction="wide"
      ) #reshape from long to wide format
      
      dataw<- dataw[order(dataw$year),] #sort by year
      dataw$idyear<-seq(1:max(dataw$id)) #generate new variable: idyear
      x=c(dataw$idyear[dataw$t.3!="NA"]) #indicator of three-arm studies
      y=x[!is.na(x)]
      D=NULL
      
      if(length(dataw$t.3)==0 & length(dataw$t.4)==0 & length(dataw$t.5)==0){
        D=data.frame(as.vector(c(dataw$year),mode="numeric"),
                     c(c(as.character(dataw$study))),
                     as.vector(c(dataw$id),mode="numeric"),
                     c(c(as.character(dataw$t.1))),
                     as.vector(c(dataw$n.1),mode="numeric"),
                     as.vector(c(dataw$r.1),mode="numeric"),
                     c(c(as.character(dataw$t.2))),
                     as.vector(c(dataw$n.2),mode="numeric"),
                     as.vector(c(dataw$r.2),mode="numeric"),
                     as.vector(c(dataw$idyear),mode="numeric")
        )
      }
      
      if(length(dataw$t.3)>0 & length(dataw$t.4)==0 & length(dataw$t.5)==0){
        aa=array(0,c(2,10,length(y)))
        a=matrix(0,length(y)*2,10)
        for (i in 1:length(y)){
          aa[1,,i]=as.matrix(subset(dataw, idyear==y[i], select=c(year,study,id,t.1,n.1,r.1,t.3,n.3,r.3,idyear)))
          aa[2,,i]=as.matrix(subset(dataw, idyear==y[i], select=c(year,study,id,t.2,n.2,r.2,t.3,n.3,r.3,idyear)))
        }
        for (i in 1:length(y)){
          a[i,]=aa[1,,i]
          a[i+length(y),]=aa[2,,i]
        }
        x1=c(dataw$idyear[dataw$t.4!="NA"]) #indicator of four-arm studies
        y1=x1[!is.na(x1)]
        D=data.frame(as.vector(c(dataw$year,a[,1]),mode="numeric"),
                     c(c(as.character(dataw$study)),c(as.character(a[,2]))),
                     as.vector(c(dataw$id,a[,3]),mode="numeric"),
                     c(c(as.character(dataw$t.1)),c(as.character(a[,4]))),
                     as.vector(c(dataw$n.1,a[,5]),mode="numeric"),
                     as.vector(c(dataw$r.1,a[,6]),mode="numeric"),
                     c(c(as.character(dataw$t.2)),c(as.character(a[,7]))),
                     as.vector(c(dataw$n.2,a[,8]),mode="numeric"),
                     as.vector(c(dataw$r.2,a[,9]),mode="numeric"),
                     as.vector(c(dataw$idyear,a[,10]),mode="numeric")
        )
      }
      if(length(dataw$t.3)>0 & length(dataw$t.4)>0 & length(dataw$t.5)==0){
        x=c(dataw$idyear[dataw$t.3!="NA"]) #indicator of three-arm studies
        y=x[!is.na(x)]
        D=NULL
        aa=array(0,c(2,10,length(y)))
        a=matrix(0,length(y)*2,10)
        
        for (i in 1:length(y)){
          aa[1,,i]=as.matrix(subset(dataw, idyear==y[i], select=c(year,study,id,t.1,n.1,r.1,t.3,n.3,r.3,idyear)))
          aa[2,,i]=as.matrix(subset(dataw, idyear==y[i], select=c(year,study,id,t.2,n.2,r.2,t.3,n.3,r.3,idyear)))
        }
        for (i in 1:length(y)){
          a[i,]=aa[1,,i]
          a[i+length(y),]=aa[2,,i]
        }
        x1=c(dataw$idyear[dataw$t.4!="NA"]) #indicator of four-arm studies
        y1=x1[!is.na(x1)]
        D=NULL
        aa1=array(0,c(3,10,length(y1)))
        a1=matrix(0,length(y1)*3,10)
        for (i in 1:length(y1)){
          aa1[1,,i]=as.matrix(subset(dataw, idyear==y1[i], select=c(year,study,id,t.1,n.1,r.1,t.4,n.4,r.4,idyear)))
          aa1[2,,i]=as.matrix(subset(dataw, idyear==y1[i], select=c(year,study,id,t.2,n.2,r.2,t.4,n.4,r.4,idyear)))
          aa1[3,,i]=as.matrix(subset(dataw, idyear==y1[i], select=c(year,study,id,t.3,n.3,r.3,t.4,n.4,r.4,idyear)))
        }
        for (i in 1:length(y1)){
          a1[i,]=aa1[1,,i]
          a1[i+length(y1),]=aa1[2,,i]
          a1[i+length(y1)+length(y1),]=aa1[3,,i]
        }
        
        x5=c(dataw$idyear[dataw$t.5!="NA"]) #indicator of five-arm studies
        y5=x1[!is.na(x5)]
        
        D=data.frame(as.vector(c(dataw$year,a[,1],a1[,1]),mode="numeric"),
                     c(c(as.character(dataw$study)),c(as.character(a[,2])),c(as.character(a1[,2]))),
                     as.vector(c(dataw$id,a[,3],a1[,3]),mode="numeric"),
                     c(c(as.character(dataw$t.1)),c(as.character(a[,4])),c(as.character(a1[,4]))),
                     as.vector(c(dataw$n.1,a[,5],a1[,5]),mode="numeric"),
                     as.vector(c(dataw$r.1,a[,6],a1[,6]),mode="numeric"),
                     c(c(as.character(dataw$t.2)),c(as.character(a[,7])),c(as.character(a1[,7]))),
                     as.vector(c(dataw$n.2,a[,8],a1[,8]),mode="numeric"),
                     as.vector(c(dataw$r.2,a[,9],a1[,9]),mode="numeric"),
                     as.vector(c(dataw$idyear,a[,10],a1[,10]),mode="numeric")
        )
      }
      if(length(dataw$t.3)>0 & length(dataw$t.4)>0 & length(dataw$t.5)>0){
        x=c(dataw$idyear[dataw$t.3!="NA"]) #indicator of three-arm studies
        y=x[!is.na(x)]
        D=NULL
        aa=array(0,c(2,10,length(y)))
        a=matrix(0,length(y)*2,10)
        
        for (i in 1:length(y)){
          aa[1,,i]=as.matrix(subset(dataw, idyear==y[i], select=c(year,study,id,t.1,n.1,r.1,t.3,n.3,r.3,idyear)))
          aa[2,,i]=as.matrix(subset(dataw, idyear==y[i], select=c(year,study,id,t.2,n.2,r.2,t.3,n.3,r.3,idyear)))
        }
        
        for (i in 1:length(y)){
          a[i,]=aa[1,,i]
          a[i+length(y),]=aa[2,,i]
        }
        x1=c(dataw$idyear[dataw$t.4!="NA"]) #indicator of four-arm studies
        y1=x1[!is.na(x1)]
        aa1=array(0,c(3,10,length(y1)))
        a1=matrix(0,length(y1)*3,10)
        
        for (i in 1:length(y1)){
          aa1[1,,i]=as.matrix(subset(dataw, idyear==y1[i], select=c(year,study,id,t.1,n.1,r.1,t.4,n.4,r.4,idyear)))
          aa1[2,,i]=as.matrix(subset(dataw, idyear==y1[i], select=c(year,study,id,t.2,n.2,r.2,t.4,n.4,r.4,idyear)))
          aa1[3,,i]=as.matrix(subset(dataw, idyear==y1[i], select=c(year,study,id,t.3,n.3,r.3,t.4,n.4,r.4,idyear)))
        }
        
        for (i in 1:length(y1)){
          a1[i,]=aa1[1,,i]
          a1[i+length(y1),]=aa1[2,,i]
          a1[i+length(y1)+length(y1),]=aa1[3,,i]
        }
        
        x5=c(dataw$idyear[dataw$t.5!="NA"]) #indicator of five-arm studies
        y5=x1[!is.na(x5)]
        
        aa5=array(0,c(4,10,length(y5)))
        a5=matrix(0,length(y5)*4,10)
        
        for (i in 1:length(y5)){
          aa5[1,,i]=as.matrix(subset(dataw, idyear==y5[i], select=c(year,study,id,t.1,n.1,r.1,t.5,n.5,r.5,idyear)))
          aa5[2,,i]=as.matrix(subset(dataw, idyear==y5[i], select=c(year,study,id,t.2,n.2,r.2,t.5,n.5,r.5,idyear)))
          aa5[3,,i]=as.matrix(subset(dataw, idyear==y5[i], select=c(year,study,id,t.3,n.3,r.3,t.5,n.5,r.5,idyear)))
          aa5[4,,i]=as.matrix(subset(dataw, idyear==y5[i], select=c(year,study,id,t.4,n.4,r.4,t.5,n.5,r.5,idyear)))
        }
        
        for (i in 1:length(y5)){
          a5[i,]=aa5[1,,i]
          a5[i+length(y5),]=aa5[2,,i]
          a5[i+length(y5)+length(y5),]=aa5[3,,i]
          a5[i+length(y5)+length(y5)+length(y5),]=aa5[4,,i]
        }
        
        D=data.frame(as.vector(c(dataw$year,a[,1],a1[,1],a5[,1]),mode="numeric"),
                     c(c(as.character(dataw$study)),c(as.character(a[,2])),c(as.character(a1[,2])),c(as.character(a5[,2]))),
                     as.vector(c(dataw$id,a[,3],a1[,3],a5[,3]),mode="numeric"),
                     c(c(as.character(dataw$t.1)),c(as.character(a[,4])),c(as.character(a1[,4])),c(as.character(a5[,4]))),
                     as.vector(c(dataw$n.1,a[,5],a1[,5],a5[,5]),mode="numeric"),
                     as.vector(c(dataw$r.1,a[,6],a1[,6],a5[,6]),mode="numeric"),
                     c(c(as.character(dataw$t.2)),c(as.character(a[,7])),c(as.character(a1[,7])),c(as.character(a5[,7]))),
                     as.vector(c(dataw$n.2,a[,8],a1[,8],a5[,8]),mode="numeric"),
                     as.vector(c(dataw$r.2,a[,9],a1[,9],a5[,9]),mode="numeric"),
                     as.vector(c(dataw$idyear,a[,10],a1[,10],a5[,10]),mode="numeric")
        )
      }
      colnames(D) <- c("year","study","id","treat1","n1","r1","treat2","n2","r2","idyear")
    }
    if (level=="arm" & type=="continuous"){
      data<- data[order(data$id),] #sort by id
      data$arm<-sequence(tabulate(data$id)) #generate new variable: treatment arm
      dataw=reshape(data,
                    idvar=c("study","id"),
                    v.names=c("t","y","sd","n"),
                    timevar=c("arm"),
                    direction="wide"
      ) #reshape from long to wide format
      
      x=c(dataw$id[dataw$t.3!="NA"]) #indicator of three-arm studies
      y=x[!is.na(x)]
      D=NULL
      
      if(length(dataw$t.3)==0 & length(dataw$t.4)==0 & length(dataw$t.5)==0){
        D=data.frame(c(c(as.character(dataw$study))),
                     as.vector(c(dataw$id),mode="numeric"),
                     c(c(as.character(dataw$t.1))),
                     as.vector(c(dataw$y.1),mode="numeric"),
                     as.vector(c(dataw$sd.1),mode="numeric"),
                     as.vector(c(dataw$n.1),mode="numeric"),
                     c(c(as.character(dataw$t.2))),
                     as.vector(c(dataw$y.2),mode="numeric"),
                     as.vector(c(dataw$sd.2),mode="numeric"),
                     as.vector(c(dataw$n.2),mode="numeric")
        )
      }
      
      if(length(dataw$t.3)>0 & length(dataw$t.4)==0 & length(dataw$t.5)==0){
        aa=array(0,c(2,12,length(y)))
        a=matrix(0,length(y)*2,12)
        for (i in 1:length(y)){
          aa[1,,i]=as.matrix(subset(dataw, idyear==y[i], select=c(year,study,id,t.1,y.1,sd.1,n.1,t.3,y.3,sd.3,n.3,idyear)))
          aa[2,,i]=as.matrix(subset(dataw, idyear==y[i], select=c(year,study,id,t.2,y.2,sd.2,n.2,t.3,y.3,sd.3,n.3,idyear)))
        }
        for (i in 1:length(y)){
          a[i,]=aa[1,,i]
          a[i+length(y),]=aa[2,,i]
        }
        x1=c(dataw$idyear[dataw$t.4!="NA"]) #indicator of four-arm studies
        y1=x1[!is.na(x1)]
        D=data.frame(as.vector(c(dataw$year,a[,1]),mode="numeric"),
                     c(c(as.character(dataw$study)),c(as.character(a[,2]))),
                     as.vector(c(dataw$id,a[,3]),mode="numeric"),
                     c(c(as.character(dataw$t.1)),c(as.character(a[,4]))),
                     as.vector(c(dataw$y.1,a[,5]),mode="numeric"),
                     as.vector(c(dataw$sd.1,a[,6]),mode="numeric"),
                     as.vector(c(dataw$n.1,a[,7]),mode="numeric"),
                     c(c(as.character(dataw$t.2)),c(as.character(a[,8]))),
                     as.vector(c(dataw$y.2,a[,9]),mode="numeric"),
                     as.vector(c(dataw$sd.2,a[,10]),mode="numeric"),
                     as.vector(c(dataw$n.2,a[,11]),mode="numeric"),
                     as.vector(c(dataw$idyear,a[,12]),mode="numeric")
        )
      }
      if(length(dataw$t.3)>0 & length(dataw$t.4)>0 & length(dataw$t.5)==0){
        aa=array(0,c(2,12,length(y)))
        a=matrix(0,length(y)*2,12)
        for (i in 1:length(y)){
          aa[1,,i]=as.matrix(subset(dataw, idyear==y[i], select=c(year,study,id,t.1,y.1,sd.1,n.1,t.3,y.3,sd.3,n.3,idyear)))
          aa[2,,i]=as.matrix(subset(dataw, idyear==y[i], select=c(year,study,id,t.2,y.2,sd.2,n.2,t.3,y.3,sd.3,n.3,idyear)))
        }
        for (i in 1:length(y)){
          a[i,]=aa[1,,i]
          a[i+length(y),]=aa[2,,i]
        }
        x1=c(dataw$idyear[dataw$t.4!="NA"]) #indicator of four-arm studies
        y1=x1[!is.na(x1)]
        aa1=array(0,c(3,12,length(y1)))
        a1=matrix(0,length(y1)*3,12)
        for (i in 1:length(y1)){
          aa1[1,,i]=as.matrix(subset(dataw, idyear==y1[i], select=c(year,study,id,t.1,y.1,sd.1,n.1,t.4,y.4,sd.4,n.4,idyear)))
          aa1[2,,i]=as.matrix(subset(dataw, idyear==y1[i], select=c(year,study,id,t.2,y.2,sd.2,n.2,t.4,y.4,sd.4,n.4,idyear)))
          aa1[3,,i]=as.matrix(subset(dataw, idyear==y1[i], select=c(year,study,id,t.3,y.3,sd.3,n.3,t.4,y.4,sd.4,n.4,idyear)))
        }
        for (i in 1:length(y1)){
          a1[i,]=aa1[1,,i]
          a1[i+length(y1),]=aa1[2,,i]
          a1[i+length(y1)+length(y1),]=aa1[3,,i]
        }
        x5=c(dataw$idyear[dataw$t.5!="NA"]) #indicator of five-arm studies
        y5=x1[!is.na(x5)]
        D=data.frame(as.vector(c(dataw$year,a[,1],a1[,1]),mode="numeric"),
                     c(c(as.character(dataw$study)),c(as.character(a[,2])),c(as.character(a1[,2]))),
                     as.vector(c(dataw$id,a[,3],a1[,3]),mode="numeric"),
                     c(c(as.character(dataw$t.1)),c(as.character(a[,4])),c(as.character(a1[,4]))),
                     as.vector(c(dataw$y.1,a[,5],a1[,5]),mode="numeric"),
                     as.vector(c(dataw$sd.1,a[,6],a1[,6]),mode="numeric"),
                     as.vector(c(dataw$n.1,a[,7],a1[,7]),mode="numeric"),
                     c(c(as.character(dataw$t.2)),c(as.character(a[,8])),c(as.character(a1[,8]))),
                     as.vector(c(dataw$y.2,a[,9],a1[,9]),mode="numeric"),
                     as.vector(c(dataw$sd.2,a[,10],a1[,10]),mode="numeric"),
                     as.vector(c(dataw$n.2,a[,11],a1[,11]),mode="numeric"),
                     as.vector(c(dataw$idyear,a[,12],a1[,12]),mode="numeric")
        )
      }
      if(length(dataw$t.3)>0 & length(dataw$t.4)>0 & length(dataw$t.5)>0){
        aa=array(0,c(2,12,length(y)))
        a=matrix(0,length(y)*2,12)
        for (i in 1:length(y)){
          aa[1,,i]=as.matrix(subset(dataw, idyear==y[i], select=c(year,study,id,t.1,y.1,sd.1,n.1,t.3,y.3,sd.3,n.3,idyear)))
          aa[2,,i]=as.matrix(subset(dataw, idyear==y[i], select=c(year,study,id,t.2,y.2,sd.2,n.2,t.3,y.3,sd.3,n.3,idyear)))
        }
        for (i in 1:length(y)){
          a[i,]=aa[1,,i]
          a[i+length(y),]=aa[2,,i]
        }
        x1=c(dataw$idyear[dataw$t.4!="NA"]) #indicator of four-arm studies
        y1=x1[!is.na(x1)]
        aa1=array(0,c(3,12,length(y1)))
        a1=matrix(0,length(y1)*3,12)
        for (i in 1:length(y1)){
          aa1[1,,i]=as.matrix(subset(dataw, idyear==y1[i], select=c(year,study,id,t.1,y.1,sd.1,n.1,t.4,y.4,sd.4,n.4,idyear)))
          aa1[2,,i]=as.matrix(subset(dataw, idyear==y1[i], select=c(year,study,id,t.2,y.2,sd.2,n.2,t.4,y.4,sd.4,n.4,idyear)))
          aa1[3,,i]=as.matrix(subset(dataw, idyear==y1[i], select=c(year,study,id,t.3,y.3,sd.3,n.3,t.4,y.4,sd.4,n.4,idyear)))
        }
        for (i in 1:length(y1)){
          a1[i,]=aa1[1,,i]
          a1[i+length(y1),]=aa1[2,,i]
          a1[i+length(y1)+length(y1),]=aa1[3,,i]
        }
        x5=c(dataw$idyear[dataw$t.5!="NA"]) #indicator of five-arm studies
        y5=x1[!is.na(x5)]
        aa5=array(0,c(4,12,length(y5)))
        a5=matrix(0,length(y5)*4,12)
        for (i in 1:length(y5)){
          aa5[1,,i]=as.matrix(subset(dataw, idyear==y5[i], select=c(year,study,id,t.1,y.1,sd.1,n.1,t.5,y.5,sd.5,n.5,idyear)))
          aa5[2,,i]=as.matrix(subset(dataw, idyear==y5[i], select=c(year,study,id,t.2,y.2,sd.2,n.2,t.5,y.5,sd.5,n.5,idyear)))
          aa5[3,,i]=as.matrix(subset(dataw, idyear==y5[i], select=c(year,study,id,t.3,y.3,sd.3,n.3,t.5,y.5,sd.5,n.5,idyear)))
          aa5[4,,i]=as.matrix(subset(dataw, idyear==y5[i], select=c(year,study,id,t.4,y.4,sd.4,n.4,t.5,y.5,sd.5,n.5,idyear)))
        }
        for (i in 1:length(y5)){
          a5[i,]=aa5[1,,i]
          a5[i+length(y5),]=aa5[2,,i]
          a5[i+length(y5)+length(y5),]=aa5[3,,i]
          a5[i+length(y5)+length(y5)+length(y5),]=aa5[4,,i]
        }
        D=data.frame(as.vector(c(dataw$year,a[,1],a1[,1],a5[,1]),mode="numeric"),
                     c(c(as.character(dataw$study)),c(as.character(a[,2])),c(as.character(a1[,2])),c(as.character(a5[,2]))),
                     as.vector(c(dataw$id,a[,3],a1[,3],a5[,3]),mode="numeric"),
                     c(c(as.character(dataw$t.1)),c(as.character(a[,4])),c(as.character(a1[,4])),c(as.character(a5[,4]))),
                     as.vector(c(dataw$y.1,a[,5],a1[,5],a5[,5]),mode="numeric"),
                     as.vector(c(dataw$sd.1,a[,6],a1[,6],a5[,6]),mode="numeric"),
                     as.vector(c(dataw$n.1,a[,7],a1[,7],a5[,7]),mode="numeric"),
                     c(c(as.character(dataw$t.2)),c(as.character(a[,8])),c(as.character(a1[,8])),c(as.character(a5[,8]))),
                     as.vector(c(dataw$y.2,a[,9],a1[,9],a5[,9]),mode="numeric"),
                     as.vector(c(dataw$sd.2,a[,10],a1[,10],a5[,10]),mode="numeric"),
                     as.vector(c(dataw$n.2,a[,11],a1[,11],a5[,11]),mode="numeric"),
                     as.vector(c(dataw$idyear,a[,12],a1[,12],a5[,12]),mode="numeric")
        )
      }
      colnames(D) <- c("year","study","id","treat1","y1","sd1","n1","treat2","y2","sd2","n2","idyear")
    }
    if (level=="study" & type=="binary"){
      data<- data[order(data$year),] #sort by year
      for (i in 1:length(data$year)){
        data$idyear[i]=which((data$id==data$id[i])== TRUE)
      }
      D=data
      colnames(D) <- c("year","study","id","treat1","treat2","effect","se","idyear")
    }
    if (level=="study" & type=="continuous"){
      data<- data[order(data$year),] #sort by year
      for (i in 1:length(data$year)){
        data$idyear[i]=which((data$id==data$id[i])== TRUE)
      }
      D=data
      colnames(D) <- c("year","study","id","treat1","treat2","effect","se","idyear")
    }
    if (level=="study" & type=="timetoevent"){
      data<- data[order(data$year),] #sort by year
      for (i in 1:length(data$year)){
        data$idyear[i]=which((data$id==data$id[i])== TRUE)
      }
      D=data
      colnames(D) <- c("year","study","id","treat1","treat2","effect","se","idyear")
    }
    list(D=D, data=data, level=level, type=type, effsize=effsize)
  }
  d<-DataInput(data,level,type, effsize)
  anma<-function(D,type,level,effsize){
    
    ####################pairwise meta-analysis###########################################
    
    comp<-paste(D$treat1,rep("vs",length(D$id)),D$treat2)
    metaNetw=NULL
    
    if (level=="arm" & type=="binary" & effsize=="OR"){
      metaD <- metabin(c(D$r1), c(D$n1), c(D$r2), c(D$n2), sm="OR", method="I",allstudies=T) #pairwise meta-analysis
      metaPairw<-metagen(metaD$TE, metaD$seTE, sm="OR",
                         tau.common=TRUE,byvar=comp)
      CompPairw=metaPairw$bylevs
      metaNetw<- netmeta(metaD$TE,metaD$seTE,D$treat1,D$treat2,sm="OR",comb.random=TRUE,
                         studlab=D$id)
    }
    if (level=="arm" & type=="binary" & effsize=="RR"){
      metaD <- metabin(c(D$r1), c(D$n1), c(D$r2), c(D$n2), sm="RR", method="I",allstudies=T) #pairwise meta-analysis
      metaPairw<-metagen(metaD$TE, metaD$seTE, sm="RR",
                         tau.common=TRUE,byvar=comp)
      CompPairw=metaPairw$bylevs
      metaNetw<- netmeta(metaD$TE,metaD$seTE,D$treat1,D$treat2,sm="RR",comb.random=TRUE,
                         studlab=D$id)
    }
    if (level=="arm" & type=="continuous" & effsize=="MD"){
      metaD <- metacont(c(D$n1),c(D$y1),c(D$sd1),c(D$n2),c(D$y2),c(D$sd2),sm="MD") #pairwise meta-analysis
      metaPairw<-metagen(metaD$TE, metaD$seTE, sm="MD",
                         tau.common=TRUE,byvar=comp)
      CompPairw=metaPairw$bylevs
      metaNetw<- netmeta(metaD$TE,metaD$seTE,D$treat1,D$treat2,sm="MD",comb.random=TRUE,
                         studlab=D$id)
    }
    if (level=="arm" & type=="continuous" & effsize=="SMD"){
      metaD <- metacont(c(D$n1),c(D$y1),c(D$sd1),c(D$n2),c(D$y2),c(D$sd2),sm="SMD") #pairwise meta-analysis
      metaPairw<-metagen(metaD$TE, metaD$seTE, sm="SMD",
                         tau.common=TRUE,byvar=comp)
      CompPairw=metaPairw$bylevs
      metaNetw<- netmeta(metaD$TE,metaD$seTE,D$treat1,D$treat2,sm="SMD",comb.random=TRUE,
                         studlab=D$id)
    }
    if (level=="study" & type=="binary" & effsize=="OR"){
      metaPairw<-metagen(D$effect, D$se,sm="OR",
                         tau.common=TRUE,byvar=comp)
      CompPairw=metaPairw$bylevs
      metaNetw<- netmeta(metaPairw$TE,metaPairw$seTE,D$treat1,D$treat2,sm="OR",comb.random=TRUE,
                         studlab=D$id)
    }
    if (level=="study" & type=="binary" & effsize=="RR"){
      metaPairw<-metagen(D$effect, D$se,sm="RR",
                         tau.common=TRUE,byvar=comp)
      CompPairw=metaPairw$bylevs
      metaNetw<- netmeta(metaPairw$TE,metaPairw$seTE,D$treat1,D$treat2,sm="RR",comb.random=TRUE,
                         studlab=D$id)
    }
    if (level=="study" & type=="continuous" & effsize=="MD"){
      metaPairw<-metagen(D$effect, D$se,sm="MD",
                         tau.common=TRUE,byvar=comp)
      CompPairw=metaPairw$bylevs
      metaNetw<- netmeta(metaPairw$TE,metaPairw$seTE,D$treat1,D$treat2,sm="MD",comb.random=TRUE,
                         studlab=D$id)
    }
    if (level=="study" & type=="continuous" & effsize=="SMD"){
      metaPairw<-metagen(D$effect, D$se,sm="SMD",
                         tau.common=TRUE,byvar=comp)
      CompPairw=metaPairw$bylevs
      metaNetw<- netmeta(metaPairw$TE,metaPairw$seTE,D$treat1,D$treat2,sm="SMD",comb.random=TRUE,
                         studlab=D$id)
    }
    if (level=="study" & type=="timetoevent" & effsize=="HR"){
      metaPairw<-metagen(D$effect, D$se,sm="HR",
                         tau.common=TRUE,byvar=comp)
      CompPairw=metaPairw$bylevs
      metaNetw<- netmeta(metaPairw$TE,metaPairw$seTE,D$treat1,D$treat2,sm="HR",comb.random=TRUE,
                         studlab=D$id)
    }
    
    x=metaNetw
    list(x=x)
    
  }
  anmaimpl<-anma(D=d$D,level=d$level,type=d$type,effsize =d$effsize)
  nma.krahn <- function(x, tau.preset=0){
    
    
    if (!inherits(x, "netmeta"))
      stop("Argument 'x' must be an object of class \"netmeta\"")
    
    
    n <- x$n
    
    
    if (x$reference.group=="")
      trts <- colnames(x$A.matrix)
    else
      trts <- c(x$reference.group,
                colnames(x$A.matrix)[colnames(x$A.matrix)!=x$reference.group])
    
    
    studies.pre <- data.frame(studlab=x$studlab,
                              treat1=x$treat1, treat2=x$treat2,
                              TE=-x$TE, seTE=sqrt(x$seTE^2+tau.preset^2),
                              narms=x$narms[match(x$studlab, x$studies)],
                              stringsAsFactors=FALSE)
    ##
    studies <- studies.pre <- studies.pre[order(studies.pre$studlab),]
    
    
    twoarm   <- any(studies$narms==2)
    multiarm <- any(studies$narms>2)
    selmulti <- studies$narms>2
    
    
    sel <- studies.pre$treat2==x$reference.group
    ##
    studies$treat1[sel] <- studies.pre$treat2[sel]
    studies$treat2[sel] <- studies.pre$treat1[sel]
    studies$TE[sel] <- -studies.pre$TE[sel]
    studies <- data.frame(studies,
                          comparison=paste(studies$treat1, studies$treat2, sep=":"))
    
    
    comparison.num.poss <- n*(n-1)/2
    comparisons <- levels(factor(as.character(studies$comparison)))
    comparison.num <- length(comparisons)
    
    
    trts.poss <- rep(NA, comparison.num.poss)
    k <- 1
    for (i in 1:(n-1))
      for (j in (i+1):n){
        trts.poss[k] <- paste(trts[i], trts[j], sep=":")
        k <- k+1
      }
    
    
    direct <- matrix(NA, nrow=comparison.num, ncol=6)
    ##
    colnames(direct) <- c("comparison", "TE", "seTE",
                          "TE.2arm", "seTE.2arm", "n.2arm")
    ##
    direct <- data.frame(direct)
    j <- 0
    ##
    for (i in names(table(studies$comparison))){
      j <- j+1
      ##
      TE.i <- studies$TE[studies$comparison==i]
      seTE.i <- studies$seTE[studies$comparison==i]
      m1 <- metagen(TE.i, seTE.i, sm=x$sm)
      ##
      direct$comparison[j] <- i
      direct$TE[j] <- m1$TE.fixed
      direct$seTE[j] <- m1$seTE.fixed
      ##
      if (sum(studies$comparison==i & !selmulti) > 0) {
        TE.i <- studies$TE[studies$comparison==i&studies$narms==2]
        seTE.i <- studies$seTE[studies$comparison==i&studies$narms==2]
        m2 <- metagen(TE.i, seTE.i, sm=x$sm)
        ##
        direct$TE.2arm[j] <- m2$TE.fixed
        direct$seTE.2arm[j] <- m2$seTE.fixed
        direct$n.2arm[j] <- m2$k
      }
    }
    
    
    if (multiarm){
      multistudies <- split(studies[selmulti,], as.character(studies$studlab[selmulti]))
      multistudies <- lapply(multistudies,
                             function(x) x[which(x$treat1==names(which.max(table(x$treat1)))),])
      multistudies <- lapply(multistudies,
                             function(x) x[order(x$treat2),])
      ##
      des <- lapply(multistudies, function(x) paste(c(x$treat1[1], x$treat2), collapse=":"))
      multistudies <- data.frame(unsplit(multistudies,
                                         rep(names(multistudies),
                                             unlist(lapply(multistudies, function(x) nrow(x))))),
                                 design=unsplit(des, rep(names(multistudies),
                                                         unlist(lapply(multistudies, function(x) nrow(x))))))
      ##
      multistudies <- data.frame(multistudies,
                                 des=paste(multistudies$comparison, multistudies$design, sep="_"))
      ##
      row.names(studies) <- NULL
      multistudies2 <- split(studies[selmulti,], as.character(studies$studlab[selmulti]))
      multistudies2 <- lapply(multistudies2, function(x) x[do.call(order,x[,c("treat1","treat2")]),])
      multistudies2 <- lapply(multistudies2,
                              function(x)
                                rbind(
                                  x[x$treat1==names(which.max(table(x$treat1))),],
                                  x[x$treat1 %in% names(table(x$treat1)[-which.max(table(x$treat1))]),]))
      multistudies2 <- unsplit(multistudies2,
                               rep(names(multistudies2),
                                   unlist(lapply(multistudies2, function(x) nrow(x)))))
    }
    
    studies <- data.frame(studies, design=studies$comparison)
    if (multiarm & sum(is.na(direct$seTE.2arm))>0)
      direct2 <- data.frame(direct[!is.na(direct$seTE.2arm),])
    else
      direct2 <- direct
    ##
    direct2 <- data.frame(direct2)
    
    
    V.design <- diag(direct2$seTE.2arm^2,
                     nrow=length(direct2$seTE.2arm),
                     ncol=length(direct2$seTE.2arm))
    
    
    if (multiarm){
      sp <- split(multistudies2, multistudies2$studlab)
      armM <- unlist(lapply(split(multistudies2$narms, multistudies2$studlab), function(x) x[1]))
      ##
      covs <- lapply(sp,
                     function(x){
                       n <- x$narms[1]
                       k <- 0
                       m <- matrix(NA, nrow=n-1, ncol=n-1)
                       for (i in 1:(n-2)){
                         for (j in (i+1):(n-1)){
                           m[i, j] <- (x$seTE[i]^2 + x$seTE[j]^2 - x$seTE[n+k]^2)/2
                           m[j, i] <- (x$seTE[i]^2 + x$seTE[j]^2 - x$seTE[n+k]^2)/2
                           k <- k+1
                         }
                       }
                       diag(m) <- x$seTE[1:(n-1)]^2
                       m
                     })
      ##
      V3 <- NA
      ##
      for (i in 1:length(covs))
        V3 <- magic::adiag(V3, covs[[i]])
      ##
      V3 <- V3[-1, -1]
      ##
      V.studies <- magic::adiag(diag(studies$seTE[!selmulti]^2), V3)
      colnames(V.studies) <- c(as.character(studies$design[!selmulti]),
                               as.character(multistudies$design))
      rownames(V.studies) <- c(as.character(studies$design[!selmulti]),
                               as.character(multistudies$design))
      ##
      multicomp <- names(which(table(multistudies$design)>0))
      V3.agg <- NA
      TE.agg <- NA
      ##
      for (i in 1:length(multicomp)){
        studlabM <- unique(multistudies$studlab[multistudies$design==multicomp[i]])
        ncovs <- covs[names(covs) %in% studlabM]
        l <- sapply(ncovs, solve)
        dim <- multistudies$narms[multistudies$studlab==studlabM[1]][1]-1
        covs3 <- solve(matrix(apply(l, 1, sum), nrow=dim))
        V3.agg <- magic::adiag(V3.agg, covs3)
        m <- matrix(NA, nrow=dim, ncol=length(studlabM))
        for (j in 1:length(studlabM))
          m[,j] <- matrix(l[, j], nrow=dim) %*%
          multistudies$TE[multistudies$studlab==studlabM[j]]
        ##
        TE.agg <- c(TE.agg,covs3 %*% apply(m, 1, sum))
      }
      
      V3.agg <- V3.agg[-1,-1]
      TE.agg <- TE.agg[-1]
      
      V <- magic::adiag(V.design, V3.agg)
      ##
      nam <- rep(multicomp, unlist(lapply(split(multistudies, multistudies$design),
                                          function(x) x$narms[1]))-1)
      ##
      if (any(twoarm))
        rownames(V) <- colnames(V) <- c(direct2$comparison, nam)
      else
        rownames(V) <- colnames(V) <- nam
      ##
      TE.dir <- c(direct2$TE.2arm, TE.agg)
    }
    else{
      V <- magic::adiag(V.design)
      rownames(V) <- direct2$comparison
      colnames(V) <- direct2$comparison
      TE.dir <- direct2$TE.2arm
      V.studies <- diag(studies$seTE[!selmulti]^2)
      colnames(V.studies) <- rownames(V.studies) <- as.character(studies$comparison[!selmulti])
    }
    ##
    if (min(eigen(V, only.values = TRUE)$values)<0)
      stop("Covariance matrix is not nnd")
    
    
    fX <- function(n){
      possK <- n*(n-1)/2
      X <- matrix(0, nrow=possK, ncol=n-1)
      X[1:(n-1), 1:(n-1)] <- diag(rep(-1, n-1))
      X[n*(n-1)/2, (n-2):(n-1)] <- cbind(1, -1)
      if (n*(n-1)/2-(n-1)>1){
        l <- n
        j <- n-2
        u <- n+j-1
        for (k in 1:(n-3)){
          X[l:u, k:(n-1)] <- cbind(1, diag(rep(-1, n-k-1)))
          j <- j-1
          l <- u+1
          u <- l+j-1
        }
      }
      X
    }
    ##
    X.full <- fX(n)
    rownames(X.full) <- trts.poss
    colnames(X.full) <- trts.poss[1:n-1]
    ##
    X.obs2.design <- X.full[direct2$comparison,, drop=FALSE]
    
    
    if (multiarm){
      num.basics.design <- unlist(lapply(split(multistudies,multistudies$design),
                                         function(x) x$narms[1]))-1
      ##
      basics <- lapply(split(multistudies, multistudies$design),
                       function(x) split(x, x$studlab)[[1]]$comparison)
      basics <- unsplit(basics, rep(1:length(multicomp), num.basics.design))
      ##
      X.obs3.design <- X.full[as.character(basics),]
      rownames(X.obs3.design) <- rep(multicomp, num.basics.design)
      X.obs <- rbind(X.obs2.design, X.obs3.design)
    }
    else
      X.obs <- X.obs2.design
    ##
    H <- X.full %*% solve(t(X.obs) %*% solve(V) %*% X.obs) %*% t(X.obs) %*% solve(V)
    TE.net <- H %*% TE.dir
    
    
    covTE.net.base <- solve(t(X.obs) %*% solve(V) %*% X.obs)
    co <- NA
    for (i in 1:(n-2)){
      for (j in 2:(n-1)){
        if (i != j && i<j){
          co <- c(co,
                  diag(covTE.net.base)[i] +
                    diag(covTE.net.base)[j] -
                    2*covTE.net.base[i,j])
        }
      }
    }
    ##
    covTE.net <- c(diag(covTE.net.base), co[-1])
    
    
    comps <- as.character(studies$comparison[!selmulti])
    studlabs <- as.character(studies$studlab[!selmulti])
    ##
    if (multiarm){
      comps <- c(comps, as.character(multistudies$comparison))
      studlabs <- c(studlabs, as.character(multistudies$studlab))
    }
    
    
    X.obs.studies <- X.full[comps,]
    
    
    H.studies <- X.full %*%
      solve(t(X.obs.studies) %*% solve(V.studies) %*% X.obs.studies) %*%
      t(X.obs.studies) %*% solve(V.studies)
    ##
    colnames(H.studies) <- studlabs
    
    
    network <- data.frame(TE=TE.net, seTE=sqrt(covTE.net))
    
    
    if (multiarm){
      len.designs <- c(rep(1, length(direct2$comparison)),
                       unlist(lapply(strsplit(multicomp, ":"),
                                     function(x){length(x)-1})))
      freq <- rep(c(direct2$n.2arm,
                    unlist(lapply(split(multistudies, multistudies$design),
                                  function(x) length(names(table(x$studlab)))))),
                  len.designs)
      narms <- rep(c(rep(2, nrow(direct2)),
                     unlist(lapply(strsplit(multicomp, ":"), function(x) length(x)))),
                   len.designs)
      ##
      design <- data.frame(design=c(direct2$comparison,
                                    rep(multicomp, unlist(lapply(strsplit(multicomp,":"),
                                                                 function(x) length(x)))-1)),
                           comparison=c(direct2$comparison,
                                        as.character(unlist(lapply(split(multistudies,
                                                                         multistudies$design),
                                                                   function(x) as.character(unlist(
                                                                     split(x,x$studlab)[[1]]["comparison"])))))),
                           narms=narms,
                           freq=freq,
                           TE.dir=TE.dir,
                           seTE.dir=sqrt(diag(V)))
    }
    else{
      len.designs <- c(rep(1, length(direct2$comparison)))
      freq <- rep(c(direct2$n.2arm), len.designs)
      narms <- rep(c(rep(2, nrow(direct2))), len.designs)
      ##
      design <- data.frame(design=colnames(V),
                           comparison=colnames(V),
                           narms=rep(2, length(direct2$comparison)),
                           freq=direct2$n.2arm,
                           TE.dir=TE.dir,
                           seTE.dir=sqrt(diag(V)))
    }
    ##
    rownames(design) <- NULL
    ##
    design <- data.frame(design,
                         TE.net=network[as.character(design$comparison), "TE"],
                         seTE.net=network[as.character(design$comparison), "seTE"])
    
    
    if (multiarm)
      studies <- rbind(studies[!selmulti,], multistudies[, 1:8])
    ##
    studies <- studies[, c("studlab", "treat1", "treat2",
                           "TE", "seTE", "narms",
                           "design", "comparison")]
    ##
    studies <- merge(studies, design[, names(design) != "narms"],
                     by=c("design", "comparison"))
    ##
    studies <- studies[, c("studlab", "design", "comparison", "treat1", "treat2",
                           "narms", "freq", "TE", "seTE",
                           "TE.dir", "seTE.dir", "TE.net", "seTE.net")]
    ##
    studies_lim <- studies[which(studies$narms==2),]
    studies_mult <- studies[which(studies$narms>2),]
    studies <- rbind(studies_lim[order(studies_lim$studlab),],
                     studies_mult[do.call(order, studies_mult[,c("studlab","treat1","treat2")]),])
    
    res <- list(n=n,
                k=x$k,
                d=length(unique(design$design)),
                trts=trts,
                comparisons=comparisons,
                studies=studies,
                direct=direct,
                network=network,
                design=design,
                multicomp=if (multiarm) multicomp else NULL,
                X.obs=X.obs,
                X.full=X.full,
                V=V,
                V.studies=V.studies,
                H=H,
                H.studies=H.studies)
    
    class(res) <- "nma.krahn"
    
    res
  }
  x=anmaimpl$x
  x2<-nma.krahn(x=x,tau.preset=x$tau)
  design=x2$design
  TE.dir=design$TE.dir
  TE.net=(x2$H)%*%(TE.dir)
  X.obs=x2$X.obs
  V=x2$V
  X.full=x2$X.full
  studies=x2$studies
  result<-(list(level=level,type=type,effsize=effsize,studies=studies, trts=x2$trts,tau.o=x$tau,D=d$D,TE.dir=TE.dir,TE.net=TE.net,X.obs=X.obs,V=V,X.full=X.full,comparisons=rownames(X.full)))
  class(result) <- "cp.nma.setup"
 result
}
