
#cette fontion prends un fichier
#et remet son contenu sous forme de matrice
getvector=function(datat,sep="",nr=2,nc=2){
  
  ve=as.numeric(read.table(datat$datapath,sep = sep,header = FALSE,dec='.'))
  mt=matrix(data=as.numeric(ve),nrow =nr,ncol =nc)
 
  return (mt)
}
toNum=function(nb){
  return(as.numeric(nb))
}
