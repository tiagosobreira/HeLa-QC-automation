# HeLa-QC-automation
The app would consolidate all the necessary steps to automate the data process of a standard sample (e.g. HeLa cells), allowing researchers to assess the quality and reliability of the mass spectrometer. 
Below is a summary of how the app pipeline works:
1.	Copy the raw files from the instrument computer to the central data storage 
2.	Process the data using Proteome Discoverer
3.	Retrieve the number of proteins, peptides, PSMs, and MS/MS and send the data to the shiny server
4.	The shiny app will have the plots with all the data above organized by date
