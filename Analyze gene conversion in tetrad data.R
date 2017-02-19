all_gene_conv<-c()

for (g in 1:No_of_genes)                        #iterate through each gene pair first
{                                         #reset PD, NPD, TT to 0 for each iteration so that every gene pair can be counted differently
  gene_conv<-0 
  gene_name<-Gene_names[g]                        #get gene names in each iteration
  for (x in 1:tetrad_count)                     #for each gene pair, iterate through all tetrads
{
  		tetrad_temp<-all_viable_tetrads[(4*(x-1)+1):(4*(x-1)+4),]
  		pd1<-length(which(tetrad_temp[, get('gene_name')]==Parental[1, get('gene_name')]))
  		pd2<-length(which(tetrad_temp[, get('gene_name')]==Parental[2, get('gene_name')]))
  		
  		if (pd1==3 | pd1==4 | pd2==3 | pd2==4) 
    	{
    		gene_conv<-gene_conv+1
    	}
  		else {next}
   all_gene_conv<-c(all_gene_conv, gene_conv)
}

all_gene_conv<-as.data.frame(all_gene_conv)
colnames(all_gene_conv)<-Gene_names
rownames(all_gene_conv)<-c('Gene_conversion')
all_gene_conv