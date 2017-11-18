kitchk<-function(pa,pb,pc){
  if (length(pa)==1){            #length of sides as value a,b,c
    a=pa
    b=pb
    c=pc
  }else{                        #position(xa,ya),(xb,yb),...
    a=sqrt(sum((pc-pb)^2))
    b=sqrt(sum((pa-pc)^2))
    c=sqrt(sum((pa-pb)^2))
  }
  if ((a+b)<=c | abs(a-b)>=c){
    Return("Not a Triangle")
  }
  pe=a+b+c                          #!!!perimetre
  p=pe/2
  area=sqrt(p*(p-a)*(p-b)*(p-c))       #!!!area
  if(pe>7.9){
    pchk="perimeter too large"
  }else if (pe<4.0){
    pchk="perimeter too small"
  }else{
    pchk="perimeter OK"
  }
  si=c(a,b,c)
  name=c("a","b","c")
  schk=NULL
  for (i in (1:3)){
    if (si[i]>(2.7)){
      schk[i]="too long"
    }else if(si[i]<1.2){
      schk[i]="too short"
    }else{
      schk[i]="OK"
    }
    schk[i]=paste("side",name[i],schk[i])
  }
  ds=1.95-si#Indicator of difference from the average equilateral
  est=sqrt(sum(ds)^2)#Deviation
  trilist=list("Side"=si,"Side.Check"=schk,"Perimeter"=pe,
               "Perimeter.Check"=pchk)
  geolist=list("Indicator"=ds,"Deviation"=est,"Working.Area"=area)
  return(list("SidesInfo"=trilist,"OtherInfo"=geolist))
}

