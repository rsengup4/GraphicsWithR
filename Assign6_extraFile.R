

###############
###  round  ###
###############

round2 = function(x, n=0) {
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5 + 1e-10
  z = trunc(z)
  z = z/10^n
  z*posneg
}

col.tran <- function(coli, percent) {
  rgb.val <- col2rgb(coli)
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100-percent)*255/100)
  t.col
}


# outputFF <- function(out1, title1, tfoot1) {
#   temi=colnames(out1)
#   out1=as.data.frame(lapply(out1,as.character),stringsAsFactors=F)
#   colnames(out1)=temi
#   
#   tmp=out1
#   tmp[is.na(tmp)]=""
#   tmp[tmp=="0 (0.0%)"]="0"
#   tmp[tmp=="0 (0%)"]="0"
#   tmp[tmp=="0 (0.00%)"]="0"
#   tmp[tmp=="0/0 (NaN%)"]="0"
#   
#   
#   outputi=tmp
#   
#   if (any(grepl(".delete",colnames(outputi)))) {
#     indexi=which(outputi$iB.delete==1)
#     outputi[indexi,1]=paste0("<b>",outputi[indexi,1],"</b>")
#     indexi=which(outputi$iI.delete>0)
#     if (length(indexi)>0) {
#       for (j in 1:length(indexi)) {
#         jj=outputi$iI.delete[indexi[j]]
#         outputi[indexi[j],1]=paste0(paste0(rep("&nbsp;&nbsp;&nbsp;",jj),collapse=""),outputi[indexi[j],1])
#       }
#     }
#     indexi=grepl(".delete",colnames(outputi))
#     outputi=outputi[,-which(indexi)]
#   }
#   
#   
#   colnames(outputi)=paste0("&nbsp;",colnames(outputi),"&nbsp;")
#   outputi[,1]=paste0(outputi[,1],"&nbsp;&nbsp;")
#   for (j in 2:ncol(outputi)) {
#     outputi[,j]=paste0("&nbsp;",outputi[,j],"&nbsp;")
#   }
#   
#   cgroupi=colnames(outputi)[-1]
#   n.cgroupi=rep(1,length(cgroupi))
#   colnames(outputi)=NULL
#   if (!is.null(tfoot1) & !is.null(title1)) {
#     tmp=htmlTable(outputi, align=c("l",rep("c",ncol(outputi)-1)), css.cgroup = "font-weight: 0;", rnames=FALSE,cgroup=c("",cgroupi), n.cgroup=c(1,n.cgroupi), caption=title1, tfoot=tfoot1)
#   } else if (!is.null(title1)) {
#     tmp=htmlTable(outputi, align=c("l",rep("c",ncol(outputi)-1)), css.cgroup = "font-weight: 0;", rnames=FALSE,cgroup=c("",cgroupi), n.cgroup=c(1,n.cgroupi), caption=title1)
#   } else {
#     tmp=htmlTable(outputi, align=c("l",rep("c",ncol(outputi)-1)), css.cgroup = "font-weight: 0;", rnames=FALSE,cgroup=c("",cgroupi), n.cgroup=c(1,n.cgroupi))
#   }
#   
#   
#   tmp=gsub("border-bottom: 2px solid grey", "border-bottom: 1px solid black",tmp)
#   tmp=gsub("border-top: 2px solid grey", "border-top: 1px solid black",tmp)
#   tmp=gsub("solid grey", "solid black",tmp)
#   # tmp=sub("<td ","<td style='font-size: 10pt;'",tmp)
#   tmp=gsub(">&nbsp;<", ">&nbsp;&nbsp;<",tmp)
#   return(tmp)
# }



outputFF <- function(out1, title1, tfoot1, cgroup1=NULL, n.cgroup1=NULL) {
  temi=colnames(out1)
  out1=as.data.frame(lapply(out1,as.character),stringsAsFactors=F)
  colnames(out1)=temi
  
  tmp=out1
  tmp[is.na(tmp)]=""
  tmp[tmp=="0 (0.0%)"]="0"
  tmp[tmp=="0 (0%)"]="0"
  tmp[tmp=="0 (0.00%)"]="0"
  tmp[tmp=="0/0 (NaN%)"]="0"
  
  
  outputi=tmp
  tmpn=colnames(outputi)
  
  if (any(grepl(".delete",colnames(outputi)))) {
    indexi=which(outputi$iB.delete==1)
    outputi[indexi,1]=paste0("<b>",outputi[indexi,1],"</b>")
    indexi=which(outputi$iI.delete>0)
    if (length(indexi)>0) {
      for (j in 1:length(indexi)) {
        jj=outputi$iI.delete[indexi[j]]
        outputi[indexi[j],1]=paste0(paste0(rep("&nbsp;&nbsp;&nbsp;",jj),collapse=""),outputi[indexi[j],1])
      }
    }
    indexi=grepl(".delete",colnames(outputi))
    outputi=outputi[,-which(indexi)]
    colnames(outputi)=tmpn[-which(indexi)]
  }
  
  
  colnames(outputi)=paste0("&nbsp;",colnames(outputi),"&nbsp;")
  outputi[,1]=paste0(outputi[,1],"&nbsp;&nbsp;")
  for (j in 2:ncol(outputi)) {
    outputi[,j]=paste0("&nbsp;",outputi[,j],"&nbsp;")
  }
  
  cgroupi=colnames(outputi)[-1]
  n.cgroupi=rep(1,length(cgroupi))
  colnames(outputi)=NULL
  
  cgroupi=c("",cgroupi)
  n.cgroupi=c(1,n.cgroupi)
  
  if (!is.null(cgroup1) & !is.matrix(cgroup1)) {
    cgroupi=rbind(rep(NA,length(cgroupi)), cgroupi)
    cgroupi[1,1:length(cgroup1)]=cgroup1
    
    n.cgroupi=rbind(rep(NA,length(n.cgroupi)), n.cgroupi)
    n.cgroupi[1,1:length(n.cgroup1)]=n.cgroup1
  } else if (!is.null(cgroup1) & is.matrix(cgroup1)) {
    
    cgroupi=matrix(cgroupi,nrow=1)
    n.cgroupi=matrix(n.cgroupi,nrow=1)
    
    for (j in 1:nrow(cgroup1)) {
      cgroupi=rbind(rep(NA,ncol(cgroupi)), cgroupi)
      cgroupi[1,1:ncol(cgroup1)]=cgroup1[j,]
      
      n.cgroupi=rbind(rep(NA,ncol(n.cgroupi)), n.cgroupi)
      n.cgroupi[1,1:ncol(n.cgroup1)]=n.cgroup1[j,]
    }

  }
  
  
  if (!is.null(tfoot1) & !is.null(title1)) {
    tmp=htmlTable(outputi, align=c("l",rep("c",ncol(outputi)-1)), css.cgroup = "font-weight: 0;", rnames=FALSE,cgroup=cgroupi, n.cgroup=n.cgroupi, caption=title1, tfoot=tfoot1)
  } else if (!is.null(title1)) {
    tmp=htmlTable(outputi, align=c("l",rep("c",ncol(outputi)-1)), css.cgroup = "font-weight: 0;", rnames=FALSE,cgroup=cgroupi, n.cgroup=n.cgroupi, caption=title1)
  } else {
    tmp=htmlTable(outputi, align=c("l",rep("c",ncol(outputi)-1)), css.cgroup = "font-weight: 0;", rnames=FALSE,cgroup=cgroupi, n.cgroup=n.cgroupi)
  }
  
  
  tmp=gsub("border-bottom: 2px solid grey", "border-bottom: 1px solid black",tmp)
  tmp=gsub("border-top: 2px solid grey", "border-top: 1px solid black",tmp)
  tmp=gsub("solid grey", "solid black",tmp)
  # tmp=sub("<td ","<td style='font-size: 10pt;'",tmp)
  tmp=gsub(">&nbsp;<", ">&nbsp;&nbsp;<",tmp)
  
  # tmp=gsub('<td', '<td nowrap="nowrap"; ', tmp)
  return(tmp)
}




