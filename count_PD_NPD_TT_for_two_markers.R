zip14D<-read.csv(file.choose(), stringsAsFactors=F)  #read a file containing data
parental<-cbind(c('-','+'),c('+','-'))  #setup parental genotypes
parental<-as.data.frame(parental, stringsAsFactors=F)
colnames(parental)<-c('ADE2','LEU2')  #change names to proper gene names
colnames(zip14D)<-c('ADE2','LEU2')

deadspore<-c()  #record tetrad/spore location where there is a dead spore

for (t in 1:772) {
   for (s in (4*(t-1)+1):(4*(t-1)+4)) {
        if (zip14D[s,1]=='x'|zip14D[s,2]=='x') {
                deadspore<-append(deadspore, (4*(t-1)+1):(4*(t-1)+4))
                break                            }
                                       }
                 }

zip1_4D<-zip14D[-c(deadspore),]  #delete rows containing dead spores
rownames(zip1_4D)<-NULL

tetrad_count<-length(zip1_4D$ADE2)/4   #initialize the count for each type
PD<-0
NPD<-0
TT<-0

#analyze one tetrad at a time and get cumulative counts

for (x in 1:tetrad_count) {
  tetrad_temp<-zip1_4D[(4*(x-1)+1):(4*(x-1)+4),]
  pd1<-length(which(tetrad_temp$ADE2==parental[1,1] & tetrad_temp$LEU2==parental[1,2]))
  pd2<-length(which(tetrad_temp$ADE2==parental[2,1] & tetrad_temp$LEU2==parental[2,2]))
  npd1<-length(which(tetrad_temp$ADE2==parental[1,1] & tetrad_temp$LEU2==parental[2,2]))
  npd2<-length(which(tetrad_temp$ADE2==parental[2,1] & tetrad_temp$LEU2==parental[1,2]))
  if (pd1==2 & pd2==2) {
    PD<-PD+1
    }
  else if (npd1==2 & npd2==2) {
    NPD<-NPD+1
    }
  else if (pd1==1 & pd2==1 & npd1==1 & npd2==1) {
    TT<-TT+1
    }
  else {next}
}

  
sprintf('PD=%s', PD)
sprintf('NPD=%s', NPD)
sprintf('TT=%s', TT)


        
    