# Tetrad-analysis-using-R (app url: https://xiangyu.shinyapps.io/tetradgo/)
Build a tetrad analysis app using R/Shiny as the back end language.
The app was built to handle yeast tetrad data for chromosome segregation/crossover/gene conversion/interference analysis. The url for this app on shiny server is: xiangyu.shinyapps.io/tetradgo. Please feel free to visit that site for tetrad data analysis and you are welcome to download the code and run as a local app.
The app is called 'tetradgo' and it's a fully functional app that can replace an old app using Excel/Macro. Also, since it's written in R, many things are calculated faster. The Shiny package gives the app an easy and modern look. Users should have no problem navigating the web app. There is an FAQ page as a reference in case there is a question regarding how to use the app. 
The features of this app includes:
1. Create new tetrad data table for data entry with highlighting of every other tetrad (four rows)
2. Users can upload their own data using the upload function of the app after they format their data according to the instructions
3. Both newly created and uploaded the data can be downloaded as .csv files
4. Users can specify which gene pairs to analyze map distance, gene conversion and interference or select all of the markers
5. By clicking each analysis button, the app will automatically redirect to the result page where the result table can then be downloaded as .csv file
6. Users can also output tables based on a reference interval/gene pair and then download
7. Whenever the caculations take more than 1 sec, there will be an animated bar on top of the page to remind the users that the server is doing the calulcations
8. From the results page, there is a button for users to return to the data table
9. On the side panel of each page, there is information to help explain the app
There is more to list.
I hope you find this app helpful in your research. Please feel free to contact me if you have any questions or suggestions.
