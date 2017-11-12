library(shiny)			 #R web framework
library(shinyjs)		 #javascript for R
library(shinythemes)	 #bootstrap CSS styles
library(caTools)         #package for generating marker pair (interval) combinations
library(rhandsontable)	 #for handsontable.js access by R
library(data.table)      #for fread() function
library(scales)			 #for percent() function to show percentages

ui<-fluidPage(theme=shinytheme('cerulean'),
	useShinyjs(),    #initiate shinyjs package
	#define a busy class to show an animated bar when app is busy doing calculations
	tagList(
	    tags$head(
	      tags$link(rel="stylesheet", type="text/css",href="style.css"),
	      tags$script(type="text/javascript", src = "busy.js")
	    )
	  ),
    div(class = "busy",  
      p("Shiny is working on it...")),
	#Navigation bar to separate new data entry, uploda existing data and results summary
	navbarPage('Tetrad Go! ', id='allPanel', theme=shinytheme('cerulean'),
		tabPanel('New tetrad data entry', value='dataTab1',
			sidebarLayout(
				sidebarPanel(id='sidebar1',
					
					HTML('<div id="new_intr"><strong>The Tetrad Go application is used to score tetrad dissection 
						  data from sporulated yeast cells that can then be used to calculate map distances 
						  and gene conversion frequencies and determine the interference frequency between 
						  intervals. To set up a new data sheet, indicate how many tetrads you plan to analyze 
						  (this number can be increased later) and enter the gene names as indicated below.  
						  Click on "Submit" to generate the data sheet. Do NOT use the back/forward button or Backspace
						  to try to navigate between tabs. Always click on the tabs to navigate the app. Chrome is 
						  recommended for this app.</strong></div>'),
							 
					hr(),
					numericInput('tetrad_count', 'Tetrad count', 100, min=1, max=5000, width=100),
					textInput('genes', 'Enter gene names to be analyzed separated by comma (use all caps)'),
					actionButton('submit', 'Submit', class='btn-primary'),
					hr(),
					conditionalPanel(
						condition="input.submit!=0",
						downloadButton('downloadData1', 'Save file to disk', class='btn-primary')
						),
					br(),
					wellPanel('This application was developed by Dr. Xiangyu Chen with the support of National Institutes 
							   of Health grant, R01 GM50717 to Nancy M. Hollingsworth.  The application is continually 
							   evolving and feedback and questions should be sent to Dr. Chen at xchen509@gmail.com'),
					conditionalPanel(
						condition='input.submit!=0',
						h4('Hit "Home" key on your keyboard to return to the top of the page')
						)
					),

				mainPanel(

					conditionalPanel(
       					condition ="input.submit != 0",
       					tags$div(selectInput('slt1', 'Select genes from dropdown menu below to calculate map 
       										  distances and percent gene conversion', 
       									      c('Select all', 'gene1', 'gene2'), width='100%', multiple=TRUE)),      		
       					div(style='display:inline-block', actionButton('analyze1', 'Map distance and NPD ratio', class='btn-primary')),
       					div(style='display:inline-block', actionButton('analyze2', 'Gene conversion frequencies', class='btn-primary')),
       					div(style='display:inline-block', actionButton('analyze6', 'Interference (Malkova)', class='btn-primary'))
       					#div(style='display:inline-block', downloadButton('downloadData1', 'Save file to disk', class='btn-primary'))
       				),
					br(),
					rHandsontableOutput('hot1')
						)
					)
				),
	
			tabPanel('Upload existing tetrad data', value='dataTab2',
				sidebarLayout(
					sidebarPanel(id='sidebar2',
						
						fileInput('file1', 'Upload existing tetrad data', accept=c('text/csv', 'text/
								   comma-separated-values, text/plain', '.csv')),						
						checkboxInput('header', 'Header', TRUE),
						hr(),
						conditionalPanel(
							condition='output.fileUploaded==true',
			    			numericInput('new_row', 'Add more tetrads', 100, min=50, max=2000),
			    			actionButton('submit1', 'Submit', class='btn-primary')
			   			),
			   			hr(),
			   			conditionalPanel(
			   				condition='output.fileUploaded==true',
			   				wellPanel(
			   					selectInput('table_type', 'Select tetrad type of reference interval to extract tetrad data, select the 
			   						         two genes for reference interval using the selection above the tetrad table', c('NPD+T', 'PD')),
			   					actionButton('extract', 'Extract table', class='btn-primary')
			   					),
			   				downloadButton('downloadData2', 'Save tetrad file to disk', class='btn-primary'),
			   				br(),
			   				h4('Hit "Home" key on your keyboard to return to the top of the page')
			   				)
					),

					mainPanel(id='data2',
						conditionalPanel(
       						condition ="output.fileUploaded==true",
       						tags$div(selectInput('slt2', 'Select genes from dropdown menu below to calculate map 
       											  distances, percent gene conversion and interference 
       										      (for interference, please select the genes in the order as they appear on the same chromosome 
       										   	  from left to right!)', c('Select all', 'gene1', 'gene2'), width='100%', multiple=TRUE)),    						
       						div(style='display:inline-block', actionButton('analyze3', 'Map distance and NPD ratio', class='btn-primary')),
       						div(style='display:inline-block', actionButton('analyze4', 'Gene conversion frequencies', class='btn-primary')),
       						div(style='display:inline-block', actionButton('analyze5', 'Interference (Malkova)', class='btn-primary'))
       						#div(style='display:inline-block', downloadButton('downloadData2', 'Save file to disk', class='btn-primary'))
       						),
						br(),
						rHandsontableOutput('hot2')
							)
						)
					),
			
			navbarMenu('Analysis',
				tabPanel(title='Map distance results', value='mapDis',
					sidebarLayout(
						sidebarPanel(
							h3('Map distance results'),
							br(), br(),
							wellPanel(HTML(
								    '<p>Map distance (cM) calculated using the formula of Perkins (1947): 
								     <br>(6NPD+<sup>T</sup>&frasl;<sub>2</sub>)&frasl;<sub>(PD+NPD+T)</sub>*100<br>
									 <br>PD=parental ditype<br>
									 <br>NPD=non-parental ditype<br>
									 <br>T=tetratype<br> 
									 <br>Only four viable spore asci are used for the calculations<br>
									 Perkins, D.D. (1947). Biochemical mutants in the fungus <i>Ustilago maydis</i>. Genetics 34: 607-626</p>')),
							conditionalPanel(
								condition='input.npd_r!=0',
								wellPanel(id='npd_ratio_ref',
									HTML(
								    '<p>NPD freq expected is calculated using the Papazian equation (1952): 
								     <br>&#189[1-T-(1-3<sup>T</sup>&frasl;<sub>2</sub>)<sup><sup>2</sup>&frasl;<sub>3</sub></sup>]<br>
									 <br>Here T refers to observed tetratype freq<br>
									 <br>The NPD ratio is NPD freq observed divided by NPD freq expected<br>
									 <br>Only four viable spore asci are used for the calculations<br>
									 Papazian, HP (1952). The Analysis of Tetrad Data. Genetics 37(2): 175-88</p>'))
								)
							),

						mainPanel(
							conditionalPanel(
		       					condition ="input.analyze1 != 0 | input.analyze3!=0 | input.analyze7!=0 | input.analyze8!=0",			       					       					
		       					div(style='display:inline-block', downloadButton('downloadData3', 'Download Results', class='btn-primary')),
								div(style='display:inline-block', actionButton('back1', 'Back to data table', class='btn-primary')),
								div(style='display:inline-block',
									conditionalPanel(
										condition ="output.mapD_table==true",
										actionButton('npd_r', 'Show NPD ratio', class='btn-primary')
										))
									),
								br(),															
								rHandsontableOutput('hot3')
							)
						)
					),

				tabPanel(title='Gene conversion frequency results', value='GCperc',
					sidebarLayout(
						sidebarPanel(
							h3('Gene conversion frequency results sorted'),
							br(), 
							wellPanel('The percent gene conversion (GC) is calculated by adding up four viable spore 
									   tetrads exhibiting non-Mendelian segregation for a given marker (4+:0-, 3+:1-, 
									   1+:3-, 0+:4-), dividing by the total number of four viable spore tetrads and 
							 		   multiplying by 100.')
							),

						mainPanel(
							conditionalPanel(
	       						condition ="input.analyze2 != 0 | input.analyze4!=0",
	       						div(style='display:inline-block', downloadButton('downloadData4', 'Download Results', class='btn-primary')),
								div(style='display:inline-block', actionButton('back2', 'Back to data table', class='btn-primary'))	
									),
							br(),
							rHandsontableOutput('hot4')
						)
					)
				),

				tabPanel(title='Interference results', value='interference1',
					sidebarLayout(
						sidebarPanel(
							h3('Interference results'),
							br(),
							wellPanel(HTML(
								    '<p>Interference calculated using the formula of Malkova (2004): 
								     <hr>
								     <br>Map distance is first calculated by counting all PDs, NPDs, and Ts in the test interval when 
									 the reference interval is all NPDs and Ts. Map distance is then calculated again in the test interval 
									 when the reference interval is all PDs<br>
									 <br>Interference is the ratio of the two map distances<br>
									 <br>Only four viable spore asci are used for the calculations<br>
									 <hr>
									 Malkova A. 2004 Gene conversion and crossing over along the 405-kb left arm of Saccharomyces 
									 cerevisiae chromosome VII. Genetics 168(1):49-63</p>'))
							),

						mainPanel(
							conditionalPanel(
	       					condition ="input.analyze5 !=0 | input.analyze6!=0",       					
	       					div(style='display:inline-block', downloadButton('downloadData5', 'Download Results', class='btn-primary')),
	       					div(style='display:inline-block', actionButton('back3', 'Back to data table', class='btn-primary'))
								),
							br(),
							rHandsontableOutput('hot5')
							)
						)
					),
				

				tabPanel(title='Extracted NPD+T table', value='data3',
					sidebarLayout(
						sidebarPanel(
							
							h3('Extracted tetrad data when reference interval has only NPDs and Ts'),
							br(),
							wellPanel('The reference interval is the first two columns of the table. NPDs are colored yellow and Ts are
								       colored tan')
							),

						mainPanel(id='ext_table_npd_tt',
							conditionalPanel(
								condition='input.extract !=0',
								div(style='display:inline-block', actionButton('analyze7', 'Map distance of all test intervals', class='btn-primary')),
								div(style='display:inline-block', downloadButton('downloadData6', 'Download extracted table', class='btn-primary')),
								div(style='display:inline-block', actionButton('back4', 'Back to tetrad data', class='btn-primary')),
								div(style='display:inline-block',
									conditionalPanel(
										condition ="output.pd_table==true",
										actionButton('to_pd', 'To PD table', class='btn-primary')
										))
								),
							br(),
							rHandsontableOutput('hot6')
							)
						)
					),
				tabPanel(title='Extracted PD table', value='data4',
					sidebarLayout(
						sidebarPanel(
							
							h3('Extracted tetrad data when reference interval has only PDs'),
							br(),
							
							wellPanel('The reference interval is highlighted in pink')
							),

						mainPanel(id='ext_table_pd',
							conditionalPanel(
								condition='input.extract !=0',
								div(style='display:inline-block', actionButton('analyze8', 'Map distance of all test intervals', class='btn-primary')),
								div(style='display:inline-block', downloadButton('downloadData7', 'Download extracted table', class='btn-primary')),
								div(style='display:inline-block', actionButton('back5', 'Back to tetrad data', class='btn-primary')),
								div(style='display:inline-block',
									conditionalPanel(
										condition ="output.npd_tt_table==true",
										actionButton('to_npd_tt', 'To NPD+T table', class='btn-primary')
										))
								),
							br(),
							rHandsontableOutput('hot7')
							)
						)
					)
				),
				
			tabPanel('Frequently asked questions',
			
					wellPanel(includeHTML('FAQ.html'))
				)
	)
)
				

server<-function(input, output, session) {

	  #This is a function to calculate map distance and gene conversion using user entered data

	  results<-function(TetradDF) {

		new_analysis <- TetradDF[-c(1,2), ]						    #get data for all tetrads (without parental)
		Parental <-TetradDF[c(1,2), ]           			        #get data for parental genotypes

		parental_1<-Parental[1,]
		parental_2<-Parental[2,]

		Gene_names<-colnames(new_analysis)                          #get all gene/marker names into a vector
  		No_of_genes<-length(Gene_names)                             #get the number of markers to be analyzed
		No_of_tetrads<-nrow(new_analysis)/4                         #get total number of tetrads

		gene_pair<-as.data.frame(combs(Gene_names, 2), stringsAsFactors=F)                  #generate marker pairs to analyze
		colnames(gene_pair)<-c('gene_1', 'gene_2')
		gene_pair$markerPair<-paste(gene_pair$gene_1, gene_pair$gene_2, sep='/')
		No_of_pairs<-nrow(gene_pair)     													#get the number of gene pairs
			              													
		if (length(which(parental_1==''))!=0 | length(which(parental_2==''))!=0) {
			showModal(
				modalDialog(
        		title = "Data Error",
        		div(h3("Please make sure you have entered all parental genotypes !
        		        Click outside this window to go back", style="color:blue")),
        		easyClose = TRUE,
        		footer = NULL
      		))
		}

		else if ((nrow(new_analysis)%%4)!=0) {
			showModal(
				modalDialog(
        		title = "Data Error",
        		div(h3("Please check your data and make sure your tetrad rows are the multiple of 4 !
        		        Click outside this window to go back", style="color:blue")),
        		easyClose = TRUE,
        		footer = NULL
      		))
		}

		else {

			deadspore<-integer()                                        							#record tetrad/spore location where there is a dead spore

			for (t in 1:No_of_tetrads) {
	 		for (s in (4*(t-1)+1):(4*(t-1)+4)) {
	    		if (new_analysis[s,1]=='x' | new_analysis[s,1]=='' | new_analysis[s,1]=='X') {
	      		deadspore<-c(deadspore, 4*(t-1)+1, 4*(t-1)+2, 4*(t-1)+3, 4*(t-1)+4)
	      		break                  }
	 		 }
			}

			if (length(deadspore)==nrow(new_analysis)) {
				showModal(
					modalDialog(
	        		title = "Data Error",
	        		div(h3("You can not use empty data table to calculate map distance !
	        		        Click outside this window to go back", style="color:blue")),
	        		easyClose = TRUE,
	        		footer = NULL
	      		))
	      		deadspore<-deadspore[-c(1,2,3,4)]
	      		all_viable_tetrads<-new_analysis[-c(deadspore), ]
	      		rownames(all_viable_tetrads)<-NULL
			}
			else if(length(deadspore)>0) {
				all_viable_tetrads<-new_analysis[-c(deadspore), ]          	#delete rows containing dead spores
				rownames(all_viable_tetrads)<-NULL                        	#reset the row numbering
			}
			else {
				all_viable_tetrads<-new_analysis
			}

	        viable_tetrad_count<-nrow(all_viable_tetrads)/4           	#get the total viable tetrad count
	        PD_count<-integer()
	        NPD_count<-integer()
	        TT_count<-integer()
	        gene_conv_4_0<-integer()									#initialize different counts and empty vectors to store them
	        gene_conv_3_1<-integer()
	        gene_conv_2_2<-integer()
	        gene_conv_1_3<-integer()
	        gene_conv_0_4<-integer()
	        								
	        for (mk in 1:No_of_genes) {

	        	Gene_conv_4_0<-0
	        	Gene_conv_3_1<-0
	        	Gene_conv_2_2<-0
	        	Gene_conv_1_3<-0
	        	Gene_conv_0_4<-0
	        	gene_x<-Gene_names[mk]
	        	for (y in 1:viable_tetrad_count) {
	        		tetrad_temp<-all_viable_tetrads[(4*(y-1)+1):(4*(y-1)+4),]
	        		tetrad_temp<-as.data.frame(tetrad_temp, stringsAsFactors=F)
	        		tetrad_temp[tetrad_temp=='a']<-'+'
	        		tetrad_temp[tetrad_temp=='f']<-'-'
	        		
	        			if (length(which(tetrad_temp[gene_x]=='+'))==4) {
	        				Gene_conv_4_0<-Gene_conv_4_0+1  }
	        			else if (length(which(tetrad_temp[gene_x]=='+'))==3) {
	        				Gene_conv_3_1<-Gene_conv_3_1+1  }
	        			else if (length(which(tetrad_temp[gene_x]=='+'))==2) {
	        				Gene_conv_2_2<-Gene_conv_2_2+1  }
	        			else if (length(which(tetrad_temp[gene_x]=='+'))==1) {
	        				Gene_conv_1_3<-Gene_conv_1_3+1  }
	        			else if (length(which(tetrad_temp[gene_x]=='+'))==0) {
	        				Gene_conv_0_4<-Gene_conv_0_4+1  }
	        			else {next}
	        		}
	        		gene_conv_4_0<-c(gene_conv_4_0, Gene_conv_4_0)
	        		gene_conv_3_1<-c(gene_conv_3_1, Gene_conv_3_1)
	        		gene_conv_2_2<-c(gene_conv_2_2, Gene_conv_2_2)
	        		gene_conv_1_3<-c(gene_conv_1_3, Gene_conv_1_3)
	        		gene_conv_0_4<-c(gene_conv_0_4, Gene_conv_0_4)
	        	}

	     	for (g in 1:No_of_pairs) {	
	  
	  			PD<-0
	  			NPD<-0
	  			TT<-0
	  			gene_1<-gene_pair[g,1]
	  			gene_2<-gene_pair[g,2]
	  			for (x in 1:viable_tetrad_count) {
	    
	    			tetrad_temp<-all_viable_tetrads[(4*(x-1)+1):(4*(x-1)+4),]
	    			pd1<-length(which(tetrad_temp[, get('gene_1')]==Parental[1, get('gene_1')] & tetrad_temp[, get('gene_2')]==Parental[1, get('gene_2')]))
	    			pd2<-length(which(tetrad_temp[, get('gene_1')]==Parental[2, get('gene_1')] & tetrad_temp[, get('gene_2')]==Parental[2, get('gene_2')]))
	   			 	npd1<-length(which(tetrad_temp[, get('gene_1')]==Parental[1, get('gene_1')] & tetrad_temp[, get('gene_2')]==Parental[2, get('gene_2')]))
	    			npd2<-length(which(tetrad_temp[, get('gene_1')]==Parental[2, get('gene_1')] & tetrad_temp[, get('gene_2')]==Parental[1, get('gene_2')]))
	  
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
	  
	  			PD_count<-c(PD_count, PD)
	  			NPD_count<-c(NPD_count, NPD)
	  			TT_count<-c(TT_count, TT)
			}

			gene_conv_total<-gene_conv_4_0+gene_conv_3_1+gene_conv_2_2+gene_conv_1_3+gene_conv_0_4
			Gene_conv_table<-as.data.frame(cbind(Gene_names, gene_conv_4_0, gene_conv_3_1, gene_conv_2_2, gene_conv_1_3, gene_conv_0_4, gene_conv_total))
			perc_GC<-percent(((gene_conv_4_0+gene_conv_3_1+gene_conv_1_3+gene_conv_0_4)/gene_conv_total))
			Gene_conv_table<-cbind(Gene_conv_table, perc_GC)
			colnames(Gene_conv_table)<-c('Gene', '4+:0-', '3+:1-', '2+:2-', '1+:3-', '0+:4-', 'Total', '% GC')
			Map_dist<-round(((6*NPD_count+TT_count/2)/(PD_count+NPD_count+TT_count))*100)
			total_recomb<-PD_count+NPD_count+TT_count
			TT_obs_freq<-TT_count/total_recomb
			NPD_obs_freq<-NPD_count/total_recomb
			NPD_exp_freq<-(1-TT_obs_freq-((1-3*TT_obs_freq/2)^2)^(1/3))/2      #Papazian equation for calculating expcted NPD proportion
			NPD_ratio<-NPD_obs_freq/NPD_exp_freq
			NPD_ratio_table<-as.data.frame(cbind(NPD_obs_freq, NPD_exp_freq, NPD_ratio))
			colnames(NPD_ratio_table)<-c('NPD freq observed', 'NPD freq expected', 'NPD ratio (obs/exp)')
			Intervals<-gene_pair$markerPair
			Summary_table<-cbind(Intervals, PD_count, NPD_count, TT_count, total_recomb, Map_dist)
			Summary_table<-as.data.frame(Summary_table)
			colnames(Summary_table)<-c('Interval', 'PD', 'NPD', 'T', 'Total', 'Map Distance (cM)')
			#rownames(Summary_table)<-NULL
			return(list(Summary_table, Gene_conv_table, NPD_ratio_table))
	}
}

	#function to calculate interference using Malkova method
	interference<-function(tetrads_interf) {
                                             
	    new_analysis <- tetrads_interf[-c(1,2), ]                   #get data for all tetrads
	    rownames(new_analysis)<-NULL
	    #new_analysis[new_analysis=='']<-NA                         #may be unnecessary
	    #new_analysis<-na.omit(new_analysis)                        #may be unnecessary
	    Parental <-tetrads_interf[c(1,2), ]                         #get data for parental genotypes

	    Gene_names<-colnames(new_analysis)                          #get all gene/marker names into a vector
	    No_of_genes<-length(Gene_names)                             #get the number of markers to be analyzed
	    No_of_tetrads<-nrow(new_analysis)/4                         #get total number of tetrads

	    marker1<-character()
	    marker2<-character()
	  
	    for (j in 1:(No_of_genes-1)) {
	      marker1<-c(marker1, Gene_names[j])
	      marker2<-c(marker2, Gene_names[j+1])
	      }

	    all_intervals<-as.data.frame(cbind(marker1, marker2), stringsAsFactors=F)
	    all_intervals$interval_for<-paste(all_intervals$marker1, all_intervals$marker2, sep='/')
	    index_of_interval<-c(1:nrow(all_intervals))
	    No_of_pairs<-nrow(all_intervals)

	    deadspore<-integer()                                                 #record tetrad/spore location where there is a dead spore

	    for (t in 1:No_of_tetrads) {
	      for (s in (4*(t-1)+1):(4*(t-1)+4)) {
	        if (new_analysis[s,1]=='x' | new_analysis[s,1]=='' | new_analysis[s,1]=='X') {
	          deadspore<-c(deadspore, 4*(t-1)+1, 4*(t-1)+2, 4*(t-1)+3, 4*(t-1)+4)
	          }
	      }
	    }

	    if(length(deadspore)>0) {
			all_viable_tetrads<-new_analysis[-c(deadspore), ]          	#delete rows containing dead spores
			rownames(all_viable_tetrads)<-NULL                        	#reset the row numbering
		}
		else {
			all_viable_tetrads<-new_analysis
		}

	    tetrad_count<-nrow(all_viable_tetrads)/4              #get the total viable tetrad count
	                                      
	    all_pd_loc<-list()									  #initialize different counts and empty vectors to store them
	    all_npd_loc<-list()
	    all_tt_loc<-list()

	    for (g in 1:No_of_pairs)
	    {	
	      pd_loc<-integer()
	      npd_loc<-integer()
	      tt_loc<-integer()
	      gene_1<-all_intervals[g,1]
	      gene_2<-all_intervals[g,2]
	      for (x in 1:tetrad_count) 
	      {
	        tetrad_temp<-all_viable_tetrads[(4*(x-1)+1):(4*(x-1)+4),]
	        pd1<-length(which(tetrad_temp[, get('gene_1')]==Parental[1, get('gene_1')] & tetrad_temp[, get('gene_2')]==Parental[1, get('gene_2')]))
	        pd2<-length(which(tetrad_temp[, get('gene_1')]==Parental[2, get('gene_1')] & tetrad_temp[, get('gene_2')]==Parental[2, get('gene_2')]))
	        npd1<-length(which(tetrad_temp[, get('gene_1')]==Parental[1, get('gene_1')] & tetrad_temp[, get('gene_2')]==Parental[2, get('gene_2')]))
	        npd2<-length(which(tetrad_temp[, get('gene_1')]==Parental[2, get('gene_1')] & tetrad_temp[, get('gene_2')]==Parental[1, get('gene_2')]))
	        if (pd1==2 & pd2==2) 
	        {
	          pd_loc<-c(pd_loc, x)
	        }
	        else if (npd1==2 & npd2==2) 
	        {
	          npd_loc<-c(npd_loc, x)
	        }
	        else if (pd1==1 & pd2==1 & npd1==1 & npd2==1) 
	        {
	          tt_loc<-c(tt_loc, x)
	        }
	        else {next}
	      }
	      all_pd_loc<-c(all_pd_loc, list(pd_loc))
	      all_npd_loc<-c(all_npd_loc, list(npd_loc))
	      all_tt_loc<-c(all_tt_loc, list(tt_loc))
	    }

	    pd_plus_all_interval<-integer()
	    pd_minus_all_interval<-integer()
	    npd_plus_all_interval<-integer()
	    npd_minus_all_interval<-integer()
	    tt_plus_all_interval<-integer()
	    tt_minus_all_interval<-integer()
	    Interv<-list(); Names<-list(); Ref_pd<-list(); Map_dis1<-list()
	    Ref_npd_tt<-list(); Map_dis2<-list(); Ratios<-list()

	    for (k in index_of_interval) {
	      interv<-character(); names<-character(); ref_pd<-character(); map_dis1<-character()
	      ref_npd_tt<-character(); map_dis2<-character(); ratios<-character()
	      interv<-c(interv, paste('reference interval', as.character(k), sep=' '))
	      names<-c(names, all_intervals$interval_for[k])
	      ref_pd<-c(ref_pd, paste('PD:', as.character(length(all_pd_loc[[k]]))))
	      map_dis1<-c(map_dis1, ' ')
	      ref_npd_tt<-c(ref_npd_tt, paste('NPD+T:', as.character(length(all_npd_loc[[k]])+length(all_tt_loc[[k]]))))
	      map_dis2<-c(map_dis2, ' ')
	      ratios<-c(ratios, ' ')
	      
	      for (l in index_of_interval[-k]) {
	      	test_pd_plus<-   (length(Reduce(intersect, list(all_npd_loc[[k]], all_pd_loc[[l]])))
	                        +length(Reduce(intersect, list(all_tt_loc[[k]], all_pd_loc[[l]]))))
	        test_pd_minus<-  length(Reduce(intersect, list(all_pd_loc[[k]], all_pd_loc[[l]])))
	        test_npd_plus<-  (length(Reduce(intersect, list(all_npd_loc[[k]], all_npd_loc[[l]])))                            
	                        +length(Reduce(intersect, list(all_tt_loc[[k]], all_npd_loc[[l]]))))
	        test_npd_minus<- length(Reduce(intersect, list(all_pd_loc[[k]], all_npd_loc[[l]])))
	        test_tt_plus<-   (length(Reduce(intersect, list(all_npd_loc[[k]], all_tt_loc[[l]])))                        
	                        +length(Reduce(intersect, list(all_tt_loc[[k]], all_tt_loc[[l]]))))
	        test_tt_minus<-  length(Reduce(intersect, list(all_pd_loc[[k]], all_tt_loc[[l]])))

	        minus_total<-test_pd_minus+test_npd_minus+test_tt_minus
	        plus_total<-test_pd_plus+test_npd_plus+test_tt_plus
	        map_dist_minus<-round(((6*test_npd_minus+test_tt_minus/2)/minus_total)*100, digits=1)
	        map_dist_plus<-round(((6*test_npd_plus+test_tt_plus/2)/plus_total)*100, digits=1)
	        ratio1<-round(map_dist_plus/map_dist_minus, digits=2)

	        interv<-c(interv, paste('test interval', as.character(k), sep=' '))
	      	names<-c(names, all_intervals$interval_for[l])
	      	ref_pd<-c(ref_pd, paste('PD:NPD:T|', as.character(test_pd_minus), ':', as.character(test_npd_minus), ':',
	      		      as.character(test_tt_minus)))
	      	map_dis1<-c(map_dis1, paste('Map distance(cM):', as.character(map_dist_minus)))
	      	ref_npd_tt<-c(ref_npd_tt, paste('PD:NPD:T|', as.character(test_pd_plus), ':', as.character(test_npd_plus), ':',
	      		          as.character(test_tt_plus)))
	      	map_dis2<-c(map_dis2, paste('Map distance(cM):', as.character(map_dist_plus)))
	      	ratios<-c(ratios, paste('Ratio:', as.character(ratio1)))                                                                     
	      }
	      	Interv<-c(Interv, list(interv))
	      	Names<-c(Names, list(names))
	      	Ref_pd<-c(Ref_pd, list(ref_pd))
	      	Map_dis1<-c(Map_dis1, list(map_dis1))
	      	Ref_npd_tt<-c(Ref_npd_tt, list(ref_npd_tt))
	      	Map_dis2<-c(Map_dis2, list(map_dis2))
	      	Ratios<-c(Ratios, list(ratios))
	    }

	    #This one-liner is a very good way to rbind multiple lists item by item given that all items have the same length
	    interf_table<-Reduce(rbind, Map(rbind, Interv, Names, Ref_pd, Map_dis1, Ref_npd_tt, Map_dis2, Ratios))
	    interf_table<-as.data.frame(interf_table)
	    rownames(interf_table)<-NULL
	    colnames(interf_table)<-NULL
	    return(interf_table)
	  }

	  #function to extract tetrad data based on a reference interval selected (either when it's
	  #NPD+T or when it's all PD
	  extract_table<-function(allTetrads, oneInterval) {

		oneInterval_data<-subset(allTetrads, select=oneInterval)
		new_analysis <- oneInterval_data[-c(1,2), ]						#get data for all tetrads
		Parental <-oneInterval_data[c(1,2), ]           			    #get data for parental genotypes                          
		No_of_tetrads<-nrow(new_analysis)/4                         	#get total number of tetrads

		if ((nrow(new_analysis)%%4)!=0) {
			showModal(
				modalDialog(
        		title = "Data Error",
        		div(h3("Please check your data and make sure your tetrad rows are the multiple of 4 !
        		        Click outside this window to go back", style="color:blue")),
        		easyClose = TRUE,
        		footer = NULL
      		))
		}

		else {

			deadspore<-integer()                                        	#record tetrad/spore location where there is a dead spore

				for (t in 1:No_of_tetrads) {
		 			for (s in (4*(t-1)+1):(4*(t-1)+4)) {
		    			if (new_analysis[s,1]=='x' | new_analysis[s,1]=='' | new_analysis[s,1]=='X') {
		      				deadspore<-c(deadspore, 4*(t-1)+1, 4*(t-1)+2, 4*(t-1)+3, 4*(t-1)+4)
		      					break }
		 		 }
				}

			if(length(deadspore)>0) {
				all_viable_tetrads<-allTetrads[-(deadspore+2),]
				oneInterval_viable_tetrads<-new_analysis[-(deadspore),]          	#delete rows containing dead spores
		    	rownames(all_viable_tetrads)<-NULL                        			#reset the row numbering
		    	rownames(oneInterval_viable_tetrads)<-NULL
			}
			else {
				all_viable_tetrads<-allTetrads
				oneInterval_viable_tetrads<-new_analysis
			}
			
		    viable_tetrad_count<-nrow(oneInterval_viable_tetrads)/4           	#get the total viable tetrad count

		    PD_loc<-integer()
		    NPD_loc<-integer()
		    TT_loc<-integer()

			for (i in 1:viable_tetrad_count) {

				tetrad_temp<-oneInterval_viable_tetrads[(4*(i-1)+1):(4*(i-1)+4),]
				pd1<-length(which(tetrad_temp[, 1]==Parental[1, 1] & tetrad_temp[, 2]==Parental[1, 2]))
				pd2<-length(which(tetrad_temp[, 1]==Parental[2, 1] & tetrad_temp[, 2]==Parental[2, 2]))
				npd1<-length(which(tetrad_temp[, 1]==Parental[1, 1] & tetrad_temp[, 2]==Parental[2, 2]))
				npd2<-length(which(tetrad_temp[, 1]==Parental[2, 1] & tetrad_temp[, 2]==Parental[1, 2]))

				if (pd1==2 & pd2==2) {

					PD_loc<-c(PD_loc, (4*(i-1)+1), (4*(i-1)+2), (4*(i-1)+3), (4*(i-1)+4))
				}
				else if (npd1==2 & npd2==2) {

						NPD_loc<-c(NPD_loc, (4*(i-1)+1), (4*(i-1)+2), (4*(i-1)+3), (4*(i-1)+4))
					}
				else if (pd1==1 & pd2==1 & npd1==1 & npd2==1) {
						TT_loc<-c(TT_loc, (4*(i-1)+1), (4*(i-1)+2), (4*(i-1)+3), (4*(i-1)+4))
					}
				else {next}
				}

			PD_loc<-PD_loc+2
			NPD_loc<-NPD_loc+2
			TT_loc<-TT_loc+2
			NPD_TT<-c(NPD_loc, TT_loc)
			NPD_TT<-sort(NPD_TT)
			PD_loc<-c(1,2,PD_loc)
			NPD_TT<-c(1,2,NPD_TT)
			NPD_TT_table<-all_viable_tetrads[NPD_TT, ]
			PD_table<-all_viable_tetrads[PD_loc, ]
			first_name<-colnames(allTetrads)[1]
			remain<-setdiff(colnames(allTetrads), c(first_name, oneInterval))
			new_names<-c(first_name, oneInterval, remain)
			NPD_TT_table<-subset(NPD_TT_table, select=new_names)
			PD_table<-subset(PD_table, select=new_names)
			rownames(NPD_TT_table)<-NULL
			rownames(PD_table)<-NULL
			return(list(NPD_TT_table, PD_table))
		}
	}

	#function to get NPD and TT position in the tetrad data table for highlighting
	npd_tt_pos<-function(extTetrads) {

		new_analysis <- extTetrads[-c(1,2), ]						#get data for all tetrads
		Parental <-extTetrads[c(1,2), ]           			    #get data for parental genotypes
	                                   
		No_of_tetrads<-nrow(new_analysis)/4                         	#get total number of tetrads
		total_loc<-c(1:nrow(new_analysis))
	    NPD_loc<-integer()
	    TT_loc<-integer()

		for (i in 1:No_of_tetrads) {

			tetrad_temp<-new_analysis[(4*(i-1)+1):(4*(i-1)+4),]
			npd1<-length(which(tetrad_temp[, 1]==Parental[1, 1] & tetrad_temp[, 2]==Parental[2, 2]))
			npd2<-length(which(tetrad_temp[, 1]==Parental[2, 1] & tetrad_temp[, 2]==Parental[1, 2]))

			if (npd1==2 & npd2==2) {

					NPD_loc<-c(NPD_loc, (4*(i-1)+1), (4*(i-1)+2), (4*(i-1)+3), (4*(i-1)+4))
				}
			}

		TT_loc<-setdiff(total_loc, NPD_loc)
		return(list(NPD_loc, TT_loc))
	}

	  #This block of code will set the condition to hide the buttons above uploaded table until the table
	  #has been uploaded correctly

	  getData <-reactive({
	  	if(is.null(input$file1)) {
	  		return(NULL)
	  	}
	  	else {
	  		data_temp<-input$file1
	  	}
	  	})

	  output$fileUploaded <- reactive({
	  	return(!is.null(getData()))
	  	})
	  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)

	  #the following code will show a button above the extrated NPD+T or PD table when the other table has already been extracted
	  getData_1<-reactive({
	  	if(is.null(input$hot6)) {
	  		return(NULL)
	  	}
	  	else {
	  		data_temp_1<-input$hot6
	  	}
	  	})

	  output$npd_tt_table<-reactive({
	  	return(!is.null(getData_1()))
	  	})
	  outputOptions(output, 'npd_tt_table', suspendWhenHidden=FALSE)

	  getData_2<-reactive({
	  	if(is.null(input$hot7)) {
	  		return(NULL)
	  	}
	  	else {
	  		data_temp_2<-input$hot7
	  	}
	  	})

	  output$pd_table<-reactive({
	  	return(!is.null(getData_2()))
	  	})
	  outputOptions(output, 'pd_table', suspendWhenHidden=FALSE)

	  getData_3<-reactive({
	  	if(is.null(input$hot3)) {
	  		return(NULL)
	  	}
	  	else {
	  		data_temp_3<-input$hot3
	  	}
	  	})

	  output$mapD_table<-reactive({
	  	return(!is.null(getData_3()))
	  	})
	  outputOptions(output, 'mapD_table', suspendWhenHidden=FALSE)
  
	  #These two download handlers are for either new data download or existing
	  #but modified data download

      output$downloadData1<-downloadHandler(

        filename=function() {
           paste('Tetrad data', Sys.Date(), '.csv', sep=' ')
        },
        content=function(file) {
          write.csv(hot_to_r(input$hot1), file, row.names=F)
        }
      )

      output$downloadData2<-downloadHandler(

        filename=function() {
           paste('Tetrad data', Sys.Date(), '.csv', sep=' ')
        },
        content=function(file) {
          write.csv(hot_to_r(input$hot2), file, row.names=F)
        }
      )

      output$downloadData3<-downloadHandler(

        filename=function() {
           paste('Map distance', Sys.Date(), '.csv', sep=' ')
        },
        content=function(file) {
          write.csv(hot_to_r(input$hot3), file, row.names=T)
        }
      )

      output$downloadData4<-downloadHandler(

        filename=function() {
           paste('Gene conversion', Sys.Date(), '.csv', sep=' ')
        },
        content=function(file) {
          write.csv(hot_to_r(input$hot4), file, row.names=T)
        }
      )

      output$downloadData5<-downloadHandler(

        filename=function() {
           paste('Interference', Sys.Date(), '.csv', sep=' ')
        },
        content=function(file) {
          write.csv(hot_to_r(input$hot5), file, row.names=T)
        }
      )

      output$downloadData6<-downloadHandler(

        filename=function() {
           paste('Extracted', input$table_type, input$slt2[1], input$slt2[2], Sys.Date(), '.csv', sep=' ')
        },
        content=function(file) {
          write.csv(hot_to_r(input$hot6), file, row.names=F)
        }
      )

      output$downloadData7<-downloadHandler(

        filename=function() {
           paste('Extracted', input$table_type, input$slt2[1], input$slt2[2], Sys.Date(), '.csv', sep=' ')
        },
        content=function(file) {
          write.csv(hot_to_r(input$hot7), file, row.names=F)
        }
      )
   
      #Display uploaded tetrad data file in the same formatting
	  output$hot2<-renderRHandsontable({
      	tetrad_file<-input$file1
      	
      	if(is.null(tetrad_file)) {
      		return(NULL)
      	}
      	else {
      		
      		in_tetrads_temp<-fread(tetrad_file$datapath, header=input$header, stringsAsFactors=F,
      			sep=',')
      	
      		in_tetrads<-in_tetrads_temp
      		number_of_tetrads<-(nrow(in_tetrads)-2)/4
      		markers<-colnames(in_tetrads[,-1])
      		number_of_genes<-length(markers)
      		spore_loc<-c(0: (number_of_tetrads-1))
      		even_spores_old<-spore_loc[which(spore_loc%%2==0)]
      		row_highLT<-integer()
      		for (l in even_spores_old) {
				row_highLT<-c(row_highLT, 4*l+2, 4*l+3, 4*l+4, 4*l+5)
			}
			in_tetrads[is.na(in_tetrads)]<-''

			DF=in_tetrads
			row_hi<-row_highLT
			rhandsontable(DF, row_hi=row_hi, width=1000, height=1000) %>%
			    # Highlight the four spores of every other tetrad to show clarity
			    hot_cols(renderer = "
           			 function(instance, td, row, col, prop, value, cellProperties) {
           			 Handsontable.TextCell.renderer.apply(this, arguments);
           
                     if (instance.params && instance.params.row_hi.includes(row)) {
                     	td.style.background='#BBDEFB';
                     }
                     }")
			}
		})

	#The following code will setup a select input menu for user to select genes to analyze. A 'Select all'
	#option is also included
	fileIn<-reactive({
		if(is.null(input$file1)) {
			return(NULL)
		}
		else {
			input$file1
		}
		})
	myfile<-reactive({
		if(is.null(fileIn())) {
			return(NULL)
		}
		else {
			read.csv(fileIn()$datapath, header=input$header, stringsAsFactors=F, sep=',')
		}
		})
	
	observe({
		selects2<-c('Select all', colnames(myfile()[,-1]))
		updateSelectInput(session, 'slt2', 'Select genes from the dropdown menu below to calculate map distances, percent gene conversion and interference 
       							 (for interference, please select the genes in the order as they appear on the same chromosome from 
       							 left to right!)', choices=selects2)
		observe({
			if('Select all' %in% input$slt2) {
				selected_choices2<-setdiff(selects2, 'Select all')
				updateSelectInput(session, 'slt2', 'Select genes from the dropdown menu below to calculate map distances, percent 
								  gene conversion and interference (for interference, please select the genes in the order as they 
								  appear on the same chromosome from left to right!)', selected=selected_choices2)
			}
			})
		})

	observeEvent(input$back1, {
		if (last_btn$counter==1) {
		updateNavbarPage(session, 'allPanel',
					selected='dataTab1')
			}
	    else if (last_btn$counter==3) {
	    	updateNavbarPage(session, 'allPanel',
					selected='dataTab2')
			}
		else if (last_btn$counter==7) {
		updateNavbarPage(session, 'allPanel',
					selected='data3')
		}
		else if (last_btn$counter==8) {
		updateNavbarPage(session, 'allPanel',
					selected='data4')
		}
	})

	observeEvent(input$back2, {
		if (last_btn$counter==2) {
		updateNavbarPage(session, 'allPanel',
					selected='dataTab1')
			}
	    else if (last_btn$counter==4) {
		updateNavbarPage(session, 'allPanel',
					selected='dataTab2')
		}
	})

	observeEvent(input$back3, {
		if (last_btn$counter==6) {
		updateNavbarPage(session, 'allPanel',
					selected='dataTab1')
			}
	    else if (last_btn$counter==5) {
		updateNavbarPage(session, 'allPanel',
					selected='dataTab2')
		}
	})

	observeEvent(input$back4, {
		updateNavbarPage(session, 'allPanel',
					selected='dataTab2')
	})
	
	observeEvent(input$back5, {
		updateNavbarPage(session, 'allPanel',
					selected='dataTab2')
	})

	observeEvent(input$to_npd_tt, {
		updateNavbarPage(session, 'allPanel',
					selected='data3')
	})

	observeEvent(input$to_pd, {
		updateNavbarPage(session, 'allPanel',
					selected='data4')
	})

	#The following code will set up a new table for data entry
	observeEvent(input$submit, {

		if (input$submit==1) {
			removeUI(
				selector='#new_intr'
				)
			insertUI(
				selector='#sidebar1',
				where='afterBegin',				
				ui=HTML('<div><strong>Indicate the linkage relationships in the haploid parents using "+" for growth 
					     and "-" for no growth.  For the <i>MAT</i> locus use "a" for <i>MAT</i>a and "f" for <i>MAT</i>alpha.  
					     When scoring tetrads, put an "x" in each box containing a dead spore. Tetrads containing dead spores 
					     will not be included in the map distance or gene conversion calculations. When data entry is complete, 
					     the Excel csv file can be saved onto a hard drive by clicking on "Save file to disk". This file can 
					     subsequently be uploaded using the "Upload existing tetrad data" tab and new data can be added.  Any 
					     Excel csv file formatted in the same way can be uploaded and used for analysis. To determine map 
					     distances, gene conversion frequencies and interference, click on the corresponding buttons "Map distances", 
					     "Gene conversion frequencies" or "Interference". Please see the "Frequently asked questions" tab for 
					     additional information.</strong></div>')
					)
		}

		spore<-c('Parent-1', 'Parent-2')
		genes<-c('Spores')
		
		user_genes<-unlist(strsplit(input$genes, ','))
		user_genes<-toupper(user_genes)
		genes<-c(genes, user_genes)
		genes<-trimws(genes) 									#remove trailing and leading spaces which could have display error
		no_of_col<-length(genes)

		if (input$tetrad_count<1 | (round(input$tetrad_count)!=input$tetrad_count)) {
			showModal(
				modalDialog(
        		title = "Input Error",
        		div(h3("Please enter a tetrad number as a whole number greater than 1 !
        		        Click outside this window to go back", style="color:blue")),
        		easyClose = TRUE,
        		footer = NULL
      		))
		}
		
		else if (no_of_col<3) {
			showModal(
			modalDialog(
    		title = "Input Error",
    		div(h3("Please enter at least two genes (perhaps you forgot to put comma in between) !
    		        Click outside this window to go back", style="color:blue")),
    		easyClose = TRUE,
    		footer = NULL
  		))
	}

		else {
			selects1<-c('Select all', genes[-1])
			all_choices<-c(1:length(selects1))
			if (!is.null(selects1)) {
				updateSelectInput(session, 'slt1', 'Select genes from the dropdown menu below to calculate map distances and percent gene conversion', 
										 choices=selects1)
			}
			observe({
				if('Select all' %in% input$slt1) {
					selected_choices1<-setdiff(selects1, 'Select all')
					updateSelectInput(session, 'slt1', 'Select genes from the dropdown menu below to calculate map distances and percent gene conversion',
						selected=selected_choices1)
				}
				})

			for (i in 1:input$tetrad_count) {
				ix<-as.character(i)
				spore<-c(spore, c(paste('T_', ix, '-1', sep=''), paste('T_', ix, '-2', sep=''),
	                          	paste('T_', ix, '-3', sep=''), paste('T_', ix, '-4', sep='')))
			}

			spore_location<-c(0:(input$tetrad_count-1))
			even_spores<-spore_location[which(spore_location%%2==0)]

			row_highlight<-integer()
			for (k in even_spores) {
				row_highlight<-c(row_highlight, 4*k+2, 4*k+3, 4*k+4, 4*k+5)
			}

			all_tetrads<-as.data.frame(matrix(ncol=no_of_col, nrow=input$tetrad_count*4+2))
			all_tetrads<-data.frame(lapply(all_tetrads, as.character), stringsAsFactors=F)
			colnames(all_tetrads)<-genes
			all_tetrads$Spores<-spore

			output$hot1<-renderRHandsontable({
				all_tetrads[is.na(all_tetrads)]<-''    # Set NA to blank for clarity
				DF=all_tetrads
				row_HT=row_highlight
				rhandsontable(DF, row_HT=row_HT, width=1000, height=1000) %>%
				    # Highlight the four spores of every other tetrad to show clarity
				    hot_cols(renderer = "
	           			 function(instance, td, row, col, prop, value, cellProperties) {
	           			 Handsontable.TextCell.renderer.apply(this, arguments);
	           
	                     if (instance.params && instance.params.row_HT.includes(row)) {
	                     	td.style.background='#BBDEFB';
	                     }
	                     }")
				})
	  }
	})
	
	#record last analysis button clicked for back to data table button to work correctly. Each analysis button 
	#is assigned a different number when clicked
	last_btn<-reactiveValues(counter=0)
	table_1<-reactiveValues(table_1_pre=data.frame(num=1:10), table_1_cur=data.frame(num=2:11))
	table_2<-reactiveValues(table_2_pre=data.frame(num=1:10), table_2_cur=data.frame(num=2:11))
	table_3<-reactiveValues(table_3_pre=data.frame(num=1:10), table_3_cur=data.frame(num=2:11))
	table_4<-reactiveValues(table_4_pre=data.frame(num=1:10), table_4_cur=data.frame(num=2:11))
	table_5<-reactiveValues(table_5_pre=0, table_5_cur=0)
	npd_r_table<-reactiveValues(ratio_table=0)

	observe({
		if(!is.null(input$hot1)) {
			table_1$table_1_cur<-as.data.frame(hot_to_r(input$hot1))
		}
		})

	observe({
		if(!is.null(input$hot2)) {
			table_2$table_2_cur<-as.data.frame(hot_to_r(input$hot2))
		}
		})

	observe({
		if(!is.null(input$hot3)) {
			table_5$table_5_cur<-as.data.frame(hot_to_r(input$hot3))
		}
		})

	observe({
		if(!is.null(input$hot6)) {
			table_3$table_3_cur<-as.data.frame(hot_to_r(input$hot6))
		}
		})

	observe({
		if(!is.null(input$hot7)) {
			table_4$table_4_cur<-as.data.frame(hot_to_r(input$hot7))
		}
		})

	#record whether a calculation needs to be redone or not. If the selected genes haven't changed then clicking on any of
	#the analysis button will just quickly bring you back to the results table without recalculating everything
	selection<-reactiveValues(last_slt1=c(1), curr_slt1=c(2), last_slt2=c(3), curr_slt2=c(4),
							  last_slt3=c(5), curr_slt3=c(6), last_slt4=c(7), curr_slt4=c(8),
							  last_slt5=c(9), curr_slt5=c(10), last_slt6=c(11), curr_slt6=c(12))
	
	observeEvent(input$slt1,{selection$curr_slt1<-input$slt1;selection$curr_slt3<-input$slt1;selection$curr_slt5<-input$slt1})
	observeEvent(input$slt2,{selection$curr_slt2<-input$slt2;selection$curr_slt4<-input$slt2;selection$curr_slt6<-input$slt2})
	
	observeEvent(input$analyze1, {

		last_btn$counter=1

		new_analysis_temp <- hot_to_r(input$hot1)
		new_analysis_temp <- as.data.frame(new_analysis_temp, stringsAsFactors=F)
		valid1<-length(input$slt1)
		if (valid1<2) {
			showModal(
				modalDialog(
        		title = "Input Error",
        		div(h3("Please select at least two genes!
        		        Click outside this window to go back", style="color:blue")),
        		easyClose = TRUE,
        		footer = NULL
      		))
			}
		else if (setequal(selection$last_slt1, selection$curr_slt1) & identical(table_1$table_1_pre, table_1$table_1_cur) 
						  & last_btn$counter!=3 & last_btn$counter!=7 & last_btn$counter!=8) {
			updateNavbarPage(session, 'allPanel', selected='mapDis')
		}
		else {
			hide('npd_ratio_ref')
			selection$last_slt1<-input$slt1
			table_1$table_1_pre<-as.data.frame(hot_to_r(input$hot1))
			updateNavbarPage(session, 'allPanel', selected='mapDis')
			
			new_analysis_temp <- subset(new_analysis_temp, select=input$slt1)
			summary_map_dist<-results(new_analysis_temp)[[1]]
			npd_r_table$ratio_table<-results(new_analysis_temp)[[3]]
			colnumber1<-length(colnames(summary_map_dist))
			
			output$hot3 <- renderRHandsontable({
				
				DF1=summary_map_dist
				
				rhandsontable(DF1, readOnly=TRUE, rowHeaderWidth=50) %>%
				    
				    hot_cols(columnSorting=TRUE) %>%
				    hot_col(c(1), colWidths=150) %>%
				    hot_col('Map Distance (cM)', colWidths=150) %>%				    
				    hot_table(highlightCol=TRUE, highlightRow=TRUE)
				})

	  }
	})

	observeEvent(input$analyze2, {

		last_btn$counter=2
		new_analysis_temp <- hot_to_r(input$hot1)
		new_analysis_temp <- as.data.frame(new_analysis_temp, stringsAsFactors=F)
		valid1<-length(input$slt1)
		if (valid1<1) {
			showModal(
				modalDialog(
        		title = "Input Error",
        		div(h3("Please select at least one gene!
        		        Click outside this window to go back", style="color:blue")),
        		easyClose = TRUE,
        		footer = NULL
      		))
			}
		else if (setequal(selection$last_slt3, selection$curr_slt3) & identical(table_1$table_1_pre, table_1$table_1_cur)
				 & last_btn$counter!=4) {
			updateNavbarPage(session, 'allPanel', selected='GCperc')
		}
		else {
			selection$last_slt3<-input$slt1
			table_1$table_1_pre<-as.data.frame(hot_to_r(input$hot1))
			updateNavbarPage(session, 'allPanel', selected='GCperc')
			new_analysis_temp <- subset(new_analysis_temp, select=input$slt1)
			summary_gene_conv<-results(new_analysis_temp)[[2]]
			colnumber2<-length(colnames(summary_gene_conv))

			output$hot4 <-renderRHandsontable({

				DF2=summary_gene_conv
				
				rhandsontable(DF2, readOnly=TRUE, rowHeaderWidth=50) %>%
				    
				    hot_cols(columnSorting=TRUE) %>%
				    hot_col(c(1), colWidths=150) %>%
				    hot_col(c(colnumber2), colWidths=70) %>%
				    hot_cols(colWidths=50) %>%
				    #hot_col(c(1:colnumbers), format='0,0') %>%
				    hot_table(highlightCol=TRUE, highlightRow=TRUE)
		})
	  }
	})

	observeEvent(input$analyze3, {

		last_btn$counter=3
		old_analysis_temp<-as.data.frame(hot_to_r(input$hot2), stringsAsFactors=F)
		valid2<-length(input$slt2)
		if (valid2<2) {
			showModal(
				modalDialog(
        		title = "Input Error",
        		div(h3("Please select at least two genes!
        		        Click outside this window to go back", style="color:blue")),
        		easyClose = TRUE,
        		footer = NULL
      		))
			}
		else if (setequal(selection$last_slt2, selection$curr_slt2) & identical(table_2$table_2_pre, table_2$table_2_cur)
						  & last_btn$counter!=1 & last_btn$counter!=7 & last_btn$counter!=8) {
			updateNavbarPage(session, 'allPanel', selected='mapDis')
		}
		else {		
			updateNavbarPage(session, 'allPanel', selected='mapDis')
			hide('npd_ratio_ref')
			selection$last_slt2<-input$slt2
			table_2$table_2_pre<-as.data.frame(hot_to_r(input$hot2))
			old_analysis_temp <- subset(old_analysis_temp, select=input$slt2)
			
						
			summary_map_dist<-results(old_analysis_temp)[[1]]
			npd_r_table$ratio_table<-results(old_analysis_temp)[[3]]
			colnumber1<-length(colnames(summary_map_dist))
			
			output$hot3 <- renderRHandsontable({
				
				DF1=summary_map_dist
				
				rhandsontable(DF1, readOnly=TRUE, rowHeaderWidth=50) %>%
				    
				    hot_cols(columnSorting=TRUE) %>%
				    hot_col(c(1), colWidths=150) %>%
				    hot_col('Map Distance (cM)', colWidths=150) %>%
				    #hot_col(c(2:colnumbers), format='0,0') %>%
				    hot_table(highlightCol=TRUE, highlightRow=TRUE)
				})		
				
									
	  }
	})

	observeEvent(input$analyze4, {

		last_btn$counter=4
		old_analysis_temp<-as.data.frame(hot_to_r(input$hot2), stringsAsFactors=F)
		valid2<-length(input$slt2)
		if (valid2<1) {
			showModal(
				modalDialog(
        		title = "Input Error",
        		div(h3("Please select at least one gene!
        		        Click outside this window to go back", style="color:blue")),
        		easyClose = TRUE,
        		footer = NULL
      		))
			}
		else if (setequal(selection$last_slt4, selection$curr_slt4) & identical(table_2$table_2_pre, table_2$table_2_cur)
						  & last_btn$counter!=2) {
			updateNavbarPage(session, 'allPanel', selected='GCperc')
		}
		else {
			selection$last_slt4<-input$slt2
			table_2$table_2_pre<-as.data.frame(hot_to_r(input$hot2))
			updateNavbarPage(session, 'allPanel', selected='GCperc')
			old_analysis_temp <- subset(old_analysis_temp, select=input$slt2)
			summary_gene_conv<-results(old_analysis_temp)[[2]]
			colnumber2<-length(colnames(summary_gene_conv))

			output$hot4 <-renderRHandsontable({

				DF2=summary_gene_conv
				
				rhandsontable(DF2, readOnly=TRUE, rowHeaderWidth=50) %>%
				    
				    hot_cols(columnSorting=TRUE) %>%
				    hot_col(c(1), colWidths=150) %>%
				    hot_col(c(colnumber2), colWidths=70) %>%
				    hot_cols(colWidths=50) %>%
				    #hot_col(c(1:colnumbers), format='0,0') %>%
				    hot_table(highlightCol=TRUE, highlightRow=TRUE)
		})
	  }
	})

	observeEvent(input$analyze5, {

		last_btn$counter=5
		old_analysis_temp<-as.data.frame(hot_to_r(input$hot2))
		valid3<-length(input$slt2)
		if (valid3<3) {
			showModal(
				modalDialog(
        		title = "Input Error",
        		div(h3("Please select at least three genes to form at least two intervals for interference!
        		        Click outside this window to go back", style="color:blue")),
        		easyClose = TRUE,
        		footer = NULL
      		))
			}
		else if (setequal(selection$last_slt6, selection$curr_slt6) & identical(table_2$table_2_pre, table_2$table_2_cur)
					      & last_btn$counter!=6) {
			updateNavbarPage(session, 'allPanel', selected='interference1')
		}
		else {
			selection$last_slt6<-input$slt2
			table_2$table_2_pre<-as.data.frame(hot_to_r(input$hot2))
			updateNavbarPage(session, 'allPanel', selected='interference1')
			genes_interf<-input$slt2
			genes_interf<-trimws(genes_interf)									#remove trailing and leading spaces which could have display error
			inter_tetrads<-subset(old_analysis_temp, select=genes_interf)
			interference_table<-interference(inter_tetrads)

			row_location<-c(0:(nrow(interference_table)/7-1))
			odd_row<-row_location[which(row_location%%2==1)]

			row_highlight<-integer()
			for (k in odd_row) {
				row_highlight<-c(row_highlight, 7*k, 7*k+1, 7*k+2, 7*k+3, 7*k+4, 7*k+5, 7*k+6)
			}
			#colnumber1<-length(colnames(interf_table))

			output$hot5 <-renderRHandsontable({

				DF<-interference_table
				row_HT<-row_highlight
				
				rhandsontable(DF, readOnly=TRUE, rowHeaderWidth=50, row_HT=row_HT) %>%
				    
				    hot_cols(columnSorting=TRUE) %>%
				    hot_cols(colWidths=170) %>%
				    hot_col(c(1), colWidths=150) %>%
				    #hot_col(c(colnumber1-1), colWidths=200) %>%
				    #hot_col(c(colnumber1-1), format='0,0') %>%
				    hot_table(highlightCol=TRUE, highlightRow=TRUE) %>%
				    hot_cols(renderer = "
       			 		function(instance, td, row, col, prop, value, cellProperties) {
       			 		Handsontable.TextCell.renderer.apply(this, arguments);
       
                 		if (instance.params && instance.params.row_HT.includes(row)) {
                 			td.style.background='#BBDEFB';
                 		}
                 		}")
		})
	  }
	})

	observeEvent(input$analyze6, {

		last_btn$counter=6
		old_analysis_temp<-as.data.frame(hot_to_r(input$hot1))
		valid3<-length(input$slt1)
		if (valid3<3) {
			showModal(
				modalDialog(
        		title = "Input Error",
        		div(h3("Please select at least three genes to form at least two intervals for interference!
        		        Click outside this window to go back", style="color:blue")),
        		easyClose = TRUE,
        		footer = NULL
      		))
			}
		else if (setequal(selection$last_slt5, selection$curr_slt5) & identical(table_1$table_1_pre, table_1$table_1_cur)
						  & last_btn$counter!=5) {
			updateNavbarPage(session, 'allPanel', selected='interference1')
		}
		else {
			selection$slt5<-input$slt1
			table_1$table_1_pre<-as.data.frame(hot_to_r(input$hot1))
			updateNavbarPage(session, 'allPanel', selected='interference1')
			genes_interf<-input$slt1
			genes_interf<-trimws(genes_interf)									#remove trailing and leading spaces which could have display error
			inter_tetrads<-subset(old_analysis_temp, select=genes_interf)
			interference_table<-interference(inter_tetrads)

			row_location<-c(0:(nrow(interference_table)/7-1))
			odd_row<-row_location[which(row_location%%2==1)]

			row_highlight<-integer()
			for (k in odd_row) {
				row_highlight<-c(row_highlight, 7*k, 7*k+1, 7*k+2, 7*k+3, 7*k+4, 7*k+5, 7*k+6)
			}
			#colnumber1<-length(colnames(interf_table))

			output$hot5 <-renderRHandsontable({

				DF<-interference_table
				row_HT<-row_highlight
				
				rhandsontable(DF, readOnly=TRUE, rowHeaderWidth=50, row_HT=row_HT) %>%
				    
				    hot_cols(columnSorting=TRUE) %>%
				    hot_cols(colWidths=170) %>%
				    hot_col(c(1), colWidths=150) %>%
				    #hot_col(c(colnumber1-1), colWidths=200) %>%
				    #hot_col(c(colnumber1-1), format='0,0') %>%
				    hot_table(highlightCol=TRUE, highlightRow=TRUE) %>%
				    hot_cols(renderer = "
       			 		function(instance, td, row, col, prop, value, cellProperties) {
       			 		Handsontable.TextCell.renderer.apply(this, arguments);
       
                 		if (instance.params && instance.params.row_HT.includes(row)) {
                 			td.style.background='#BBDEFB';
                 		}
                 		}")
		})
	  }
	})

	observeEvent(input$analyze7, {

		last_btn$counter=7
		new_analysis_temp <- as.data.frame(hot_to_r(input$hot6), stringsAsFactors=F)
		
		if (identical(table_3$table_3_pre, table_3$table_3_cur) &last_btn$counter!=1 &last_btn$counter!=3 &last_btn$counter!=8) {
			updateNavbarPage(session, 'allPanel', selected='mapDis')
		}
		
		else {

			hide('npd_ratio_ref')
			table_3$table_3_pre<-as.data.frame(hot_to_r(input$hot6))
			updateNavbarPage(session, 'allPanel', selected='mapDis')
			new_analysis_temp[new_analysis_temp=='']<-NA
			new_analysis_temp<-new_analysis_temp[sapply(new_analysis_temp, function(x) !any(is.na(x)))]
			test_intervals<-colnames(new_analysis_temp)[-c(1,2,3)]
			new_analysis_temp <- subset(new_analysis_temp, select=test_intervals)
			#new_analysis_temp[new_analysis_temp=='']<-NA
			#this one liner deletes any columns containg NA (empty) values
			#new_nalaysis_temp<-new_analysis_temp[sapply(new_analysis_temp, function(new_analysis_temp)!any(is.na(new_analysis_temp)))]
			summary_map_dist<-results(new_analysis_temp)[[1]]
			colnumber1<-length(colnames(summary_map_dist))
			
			output$hot3 <- renderRHandsontable({
				
				DF1=summary_map_dist
				
				rhandsontable(DF1, readOnly=TRUE, rowHeaderWidth=50) %>%
				    
				    hot_cols(columnSorting=TRUE) %>%
				    hot_col(c(1), colWidths=180) %>%
				    hot_table(highlightCol=TRUE, highlightRow=TRUE)
			})
	}
})

	observeEvent(input$analyze8, {

		last_btn$counter=8
		new_analysis_temp <- as.data.frame(hot_to_r(input$hot7))
		
		if (identical(table_4$table_4_pre, table_4$table_4_cur) &last_btn$counter!=1 &last_btn$counter!=3 &last_btn$counter!=7) {
			updateNavbarPage(session, 'allPanel', selected='mapDis')
		}
		
		else {

			hide('npd_ratio_ref')
			table_4$table_4_pre<-as.data.frame(hot_to_r(input$hot7))
			updateNavbarPage(session, 'allPanel', selected='mapDis')
			new_analysis_temp[new_analysis_temp=='']<-NA
			new_analysis_temp<-new_analysis_temp[sapply(new_analysis_temp, function(x) !any(is.na(x)))]
			test_intervals<-colnames(new_analysis_temp)[-c(1,2,3)]
			new_analysis_temp <- subset(new_analysis_temp, select=test_intervals)
			#new_analysis_temp[new_analysis_temp=='']<-NA
			#this one liner deletes any columns containg NA (empty) values
			#new_nalaysis_temp<-new_analysis_temp[sapply(new_analysis_temp, function(new_analysis_temp)!any(is.na(new_analysis_temp)))]
			summary_map_dist<-results(new_analysis_temp)[[1]]
			colnumber1<-length(colnames(summary_map_dist))
			
			output$hot3 <- renderRHandsontable({
				
				DF1=summary_map_dist
				
				rhandsontable(DF1, readOnly=TRUE, rowHeaderWidth=50) %>%
				    
				    hot_cols(columnSorting=TRUE) %>%
				    hot_col(c(1), colWidths=180) %>%
				    hot_table(highlightCol=TRUE, highlightRow=TRUE)
			})
	}
})
	
	observeEvent(input$npd_r, {
		
		if (!identical(table_5$table_5_pre, table_5$table_5_cur)) {
			show('npd_ratio_ref')
			map_data<-as.data.frame(hot_to_r(input$hot3))
			map_data<-as.data.frame(cbind(map_data, npd_r_table$ratio_table))
			table_5$table_5_pre<-map_data

			output$hot3 <- renderRHandsontable({
				
				DF=map_data
				
				rhandsontable(DF, readOnly=TRUE, rowHeaderWidth=50) %>%
				    
				    hot_cols(columnSorting=TRUE) %>%
				    hot_col('NPD freq observed', colWidths=150) %>%
				    hot_col('NPD freq expected', colWidths=150) %>%
				    hot_col(c(1), colWidths=150) %>%
				    hot_col('Map Distance (cM)', colWidths=150) %>%
				    hot_col('NPD freq observed', format='0.000000') %>%
				    hot_col('NPD freq expected', format='0.000000') %>%
				    hot_col('NPD ratio (obs/exp)', format='0.000000') %>%
				    hot_table(highlightCol=TRUE, highlightRow=TRUE)
				})
			}
		})


	observeEvent(input$extract, {

		last_btn$counter=9
		old_analysis_temp<-as.data.frame(hot_to_r(input$hot2))
		valid4<-length(input$slt2)
		if (valid4!=2) {
			showModal(
				modalDialog(
        		title = "Input Error",
        		div(h3("Please select just two genes to form one reference interval for extraction!
        		        Click outside this window to go back", style="color:blue")),
        		easyClose = TRUE,
        		footer = NULL
      		))
			}
		else if (is.null(input$table_type)) {
			showModal(
				modalDialog(
        		title = "Input Error",
        		div(h3("Please select tetrad type of reference interval (NPD+T or PD) for extraction!
        		        Click outside this window to go back", style="color:blue")),
        		easyClose = TRUE,
        		footer = NULL
      		))
			}
		 else if (input$table_type=='NPD+T') {
				updateNavbarPage(session, 'allPanel',
					selected='data3')
				refInterval<-input$slt2
				refInterval<-trimws(refInterval)									
				exted_table<-extract_table(old_analysis_temp, refInterval)[[1]]
				new_interval_table<-subset(exted_table, select=refInterval)
				row_highlight_npd<-npd_tt_pos(new_interval_table)[[1]]
				row_highlight_tt<-npd_tt_pos(new_interval_table)[[2]]
				tetrad_count<-(nrow(exted_table)-2)/4
				spore_location<-c(0:(tetrad_count-1))
				even_spores<-spore_location[which(spore_location%%2==0)]
				
				row_highlight<-integer()
				for (k in even_spores) {
					row_highlight<-c(row_highlight, 4*k+2, 4*k+3, 4*k+4, 4*k+5)
				}

				row_highlight_npd<-row_highlight_npd+1
				row_highlight_tt<-row_highlight_tt+1
				col_highlight<-c(1,2)

				output$hot6 <-renderRHandsontable({

					DF<-exted_table
					row_HT<-row_highlight
					
					rhandsontable(DF, readOnly=TRUE, rowHeaderWidth=50, row_HT=row_HT, col_highlight=col_highlight,
						 		  row_highlight_npd=row_highlight_npd, row_highlight_tt=row_highlight_tt, width=1000, height=1000) %>%
					    
					    hot_col(c(1), colWidths=100) %>%
					    hot_table(highlightCol=TRUE, highlightRow=TRUE) %>%
					    hot_cols(renderer = "
           			 		function(instance, td, row, col, prop, value, cellProperties) {
           			 		Handsontable.TextCell.renderer.apply(this, arguments);
           
                     		if (instance.params && instance.params.row_highlight_npd.includes(row)
                     			     && instance.params.col_highlight.includes(col)) {
					    		td.style.background='tan';
					    	}
					    	else if (instance.params && instance.params.row_highlight_tt.includes(row)
                     			     && instance.params.col_highlight.includes(col)) {
					    		td.style.background='yellow';
					    	}
					    	else if (instance.params && instance.params.row_HT.includes(row)) {
                     			td.style.background='#BBDEFB';
                     		}
                     		}")
			})
		  }
		  else if (input$table_type=='PD') {
				updateNavbarPage(session, 'allPanel',
					selected='data4')
				refInterval1<-input$slt2
				refInterval1<-trimws(refInterval1)									
				exted_table1<-extract_table(old_analysis_temp, refInterval1)[[2]]
				tetrad_count1<-(nrow(exted_table1)-2)/4
				spore_location1<-c(0:(tetrad_count1-1))
				even_spores1<-spore_location1[which(spore_location1%%2==0)]

				row_highlight<-integer()
				for (k in even_spores1) {
					row_highlight<-c(row_highlight, 4*k+2, 4*k+3, 4*k+4, 4*k+5)
				}
				col_highlight<-c(1,2)

				output$hot7 <-renderRHandsontable({

					DF<-exted_table1
					row_HT<-row_highlight
					
					rhandsontable(DF, readOnly=TRUE, rowHeaderWidth=50, row_HT=row_HT, col_highlight=col_highlight,
						          width=1000, height=1000) %>%
					    
					    hot_table(highlightCol=TRUE, highlightRow=TRUE) %>%
					    hot_cols(renderer = "
           			 		function(instance, td, row, col, prop, value, cellProperties) {
           			 		Handsontable.TextCell.renderer.apply(this, arguments);
           
           					if (instance.params && instance.params.col_highlight.includes(col)) {
                     			td.style.background='pink';
                     		}
                     		else if (instance.params && instance.params.row_HT.includes(row)) {
                     			td.style.background='#BBDEFB';
                     		}
                     	}")
			})
		  }
		})

	observeEvent(input$submit1, {

		if (!is.null(input$hot2)) {
			DF_temp<-hot_to_r(input$hot2)
      		number_of_tetrads<-(nrow(DF_temp)-2)/4
      		markers<-colnames(DF_temp[,-1])
      		number_of_genes<-length(markers)
      		spore_new<-character()

      		if (input$new_row<50 | (round(input$new_row)!=input$new_row) | input$new_row>2000) {
				showModal(
					modalDialog(
	        		title = "Input Error",
	        		div(h3("Please enter a tetrad number as a whole number between 50 and 2000 !
	        		        Click outside this window to go back", style="color:blue")),
	        		easyClose = TRUE,
	        		footer = NULL
	      		))
			}

			else {

				for (n in (number_of_tetrads+1):(number_of_tetrads+input$new_row)) {
					iz<-as.character(n)
					spore_new<-c(spore_new, c(paste('T_', iz, '-1', sep=''), paste('T_', iz, '-2', sep=''),
	                          	paste('T_', iz, '-3', sep=''), paste('T_', iz, '-4', sep='')))
				}

				DF_new<-as.data.frame(matrix(ncol=length(colnames(DF_temp)), nrow=input$new_row*4))
				#DF_new<-data.frame(lapply(DF_new, as.character), stringsAsFactors=F)
				colnames(DF_new)<-colnames(DF_temp)
				DF_new[1]<-spore_new
				updated_tetrad_data<-rbind(DF_temp, DF_new)

	      		spore_loc<-c(0: ((number_of_tetrads+input$new_row)-1))
	      		even_spores_old<-spore_loc[which(spore_loc%%2==0)]
	      		row_highLT<-integer()
	      		for (l in even_spores_old) {
					row_highLT<-c(row_highLT, 4*l+2, 4*l+3, 4*l+4, 4*l+5)
				}
				updated_tetrad_data[is.na(updated_tetrad_data)]<-''
				rownames(updated_tetrad_data)<-NULL

				DF=updated_tetrad_data
				row_hi<-row_highLT
				
				output$hot2<-renderRHandsontable({
				rhandsontable(DF, row_hi=row_hi, width=1000, height=1000) %>%
				    # Highlight the four spores of every other tetrad to show clarity
				    hot_cols(renderer = "
	           			 function(instance, td, row, col, prop, value, cellProperties) {
	           			 Handsontable.TextCell.renderer.apply(this, arguments);
	           
	                     if (instance.params && instance.params.row_hi.includes(row)) {
	                     	td.style.background='#BBDEFB';
	                     }
	                     }")
				    })
			}
		}
	})
}

shinyApp(ui=ui, server=server)