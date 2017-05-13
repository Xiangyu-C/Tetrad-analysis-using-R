library(shiny)
library(rhandsontable)

ui<-fluidPage(
	titlePanel('Tetrad Go!'),

	#Sidebar to initialize table for data entry
	sidebarLayout(
		sidebarPanel(
			numericInput('tetrad_count', 'Tetrad count', 100, min=1, max=5000, width=100),
			numericInput('gene_count', 'Genes to be analyzed', 2, min=2, max=50, width=100),
			textInput('genes', 'Enter gene names separated by comma'),
			actionButton('submit', 'Submit')
			

			),

		mainPanel(
			conditionalPanel(
       			condition = ("input.submit != 0"),
       			downloadButton('downloadData', 'Save file to disk')),
			br(),
       
			rHandsontableOutput('hot')
			)
		)
	)

server<-function(input, output, session) {
  
  
    
      output$downloadData<-downloadHandler(
   
    
      
        filename=function() {
           paste('Data', Sys.Date(), '.csv', sep='')
        },
        content=function(file) {
          write.csv(hot_to_r(input$hot), file)
        }
    
      )

    
  
  

	#observeEvent(input$save, {
	#	hot=isolate(input$hot)
	#	if(!is.null(input$hot)) {
	#		write.table(hot_to_r(input$hot), fname)
	#	  print(fname)
	#	}
	#	})

	observeEvent(input$submit, {

	    spore<-c('Parent-1', 'Parent-2')
		genes<-c('Spores')

		for (i in 1:input$tetrad_count) {
			ix<-as.character(i)
			spore<-c(spore, c(paste('tetrad', ix, '-1', sep=''), paste('tetrad', ix, '-2', sep=''),
                          	paste('tetrad', ix, '-3', sep=''), paste('tetrad', ix, '-4', sep='')))
		}

		#for (j in 1:input$gene_count) {
		#	iy<-as.character(j)
		#	genes<-c(genes, c(paste('Gene', iy, sep=' ')))
		#}

		genes<-c(genes, unlist(strsplit(input$genes, ',')))

		spore_location<-c(0:(input$tetrad_count-1))
		even_spores<-spore_location[which(spore_location%%2==0)]

		row_highlight<-integer()
		for (k in even_spores) {
			row_highlight<-c(row_highlight, 4*k+2, 4*k+3, 4*k+4, 4*k+5)
		}

		all_tetrads<-as.data.frame(matrix(ncol=input$gene_count+1, nrow=input$tetrad_count*4+2))
		all_tetrads<-data.frame(lapply(all_tetrads, as.character), stringsAsFactors=F)
		colnames(all_tetrads)<-genes
		all_tetrads$Spores<-spore

		output$hot<-renderRHandsontable({
			all_tetrads[is.na(all_tetrads)]<-''    # Set NA to blank for clarity
			DF=all_tetrads
			row_HT=row_highlight
			rhandsontable(DF, row_HT=row_HT) %>%
			    # Highlight the four spores of every other tetrad to show clarity
			    hot_cols(renderer = "
           			 function(instance, td, row, col, prop, value, cellProperties) {
           			 Handsontable.TextCell.renderer.apply(this, arguments);
           
                     if (instance.params && instance.params.row_HT.includes(row)) {
                     	td.style.background='lightgreen';
                     }
                     }")
			})

	})
	
}

shinyApp(ui=ui, server=server)