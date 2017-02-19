library(caTools)                                               #package for generating marker pair combinations
all_tetrad_data<-read.csv(file.choose(), stringsAsFactors=F)   #read all tetrad data into a dataframe
Parental<-read.csv(file.choose(), stringsAsFactors=F)          #read a file containg parental genotypes
Gene_names<-colnames(all_tetrad_data)                          #get all gene/marker names into a vector
No_of_genes<-length(Gene_names)                                #get the number of markers to be analyzed
No_of_tetrads<-nrow(all_tetrad_data)/4                    #get total number of tetrads
gene_pair<-as.data.frame(combs(Gene_names, 2), stringsAsFactors=F)                 #generate marker pairs to analyze
colnames(gene_pair)<-c('gene_1', 'gene_2')
gene_pair$markerPair<-paste(gene_pair$gene_1, gene_pair$gene_2, sep='/')
No_of_pairs<-nrow(gene_pair)                              #get the number of gene pairs

deadspore<-c()                                                 #record tetrad/spore location where there is a dead spore

for (t in 1:No_of_tetrads) {
   for (s in (4*(t-1)+1):(4*(t-1)+4)) {
        if (all_tetrad_data[s,1]=='x' | all_tetrad_data[s,1]=='') {
                deadspore<-append(deadspore, (4*(t-1)+1):(4*(t-1)+4))
                break                  }
                                      }
                           }

all_viable_tetrads<-all_tetrad_data[-c(deadspore),]  #delete rows containing dead spores
rownames(all_viable_tetrads)<-NULL                   #reset the row numbering

tetrad_count<-nrow(all_viable_tetrads)/4        #get the total viable tetrad count
PD_count<-c()
NPD_count<-c()
TT_count<-c()                                              #initialize different counts and empty vectors to store them

for (g in 1:No_of_pairs)                        #iterate through each gene pair first
{	
	PD<-0                                         #reset PD, NPD, TT to 0 for each iteration so that every gene pair can be counted differently
	NPD<-0
	TT<-0
	gene_1<-gene_pair[g,1]                        #get gene names in each iteration
	gene_2<-gene_pair[g,2]
	for (x in 1:tetrad_count)                     #for each gene pair, iterate through all tetrads
{
  		tetrad_temp<-all_viable_tetrads[(4*(x-1)+1):(4*(x-1)+4),]
  		pd1<-length(which(tetrad_temp[, get('gene_1')]==Parental[1, get('gene_1')] & tetrad_temp[, get('gene_2')]==Parental[1, get('gene_2')]))
  		pd2<-length(which(tetrad_temp[, get('gene_1')]==Parental[2, get('gene_1')] & tetrad_temp[, get('gene_2')]==Parental[2, get('gene_2')]))
  		npd1<-length(which(tetrad_temp[, get('gene_1')]==Parental[1, get('gene_1')] & tetrad_temp[, get('gene_2')]==Parental[2, get('gene_2')]))
  		npd2<-length(which(tetrad_temp[, get('gene_1')]==Parental[2, get('gene_1')] & tetrad_temp[, get('gene_2')]==Parental[1, get('gene_2')]))
  		if (pd1==2 & pd2==2) 
    	{
    		PD<-PD+1
    	}
  		else if (npd1==2 & npd2==2) 
    	{
    		NPD<-NPD+1
    	}
  		else if (pd1==1 & pd2==1 & npd1==1 & npd2==1) 
    	{
    		TT<-TT+1
    	}
  		else {next}
}
	PD_count<-append(PD_count, PD)            #populate a vector containing counts for each gene pair
	NPD_count<-append(NPD_count, NPD)
	TT_count<-append(TT_count, TT)
}

Summary_table<-rbind(PD_count, NPD_count, TT_count)
Summary_table<-as.data.frame(Summary_table)
colnames(Summary_table)<-gene_pair$markerPair
Summary_table