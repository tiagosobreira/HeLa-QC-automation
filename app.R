library(DT)
library(shinyTime)
library(dplyr)
library(shinyjs)
library(excelR)
library(plotly)


tables = list()

create.table <- function(name){


    return(
        tabsetPanel(
            id = paste0(name,"_mainTabs"), type = "tabs",
            tabPanel(
                title = "Summary", id = paste0(name,"_summary"),
                fluidRow(
                    column(8,plotlyOutput(paste0(name,"_Proteins"))),
                    column(3, DT::dataTableOutput(paste0(name,"_Proteins_last5")))
                ),
                tags$hr(style="border-color: black;"),
                
                fluidRow(
                    column(8,plotlyOutput(paste0(name,"_Peptides"))),
                    column(3, DT::dataTableOutput(paste0(name,"_Peptides_last5")))
                ),
                tags$hr(style="border-color: black;"),
                
                fluidRow(
                    column(8,plotlyOutput(paste0(name,"_PSMs"))),
                    column(3, DT::dataTableOutput(paste0(name,"_PSMs_last5")))
                ),
                tags$hr(style="border-color: black;"),
                
                fluidRow(
                    column(8,plotlyOutput(paste0(name,"_MSMS"))),
                    column(3, DT::dataTableOutput(paste0(name,"_MSMS_last5")))
                )
            ),
            tabPanel(
                title = "Proteome Discoverer", id = paste0(name,"_MQ"),
                tags$style(tables[[paste0(name,"_MQ_table")]] <<- 0),
                div(style = 'overflow-x: scroll', excelOutput(paste0(name,"_MQ_table")), width = "100%"),
                
                plotOutput(paste0(name,"_MQ_plot"))
            )
        )
                 
    )
}



ui <- fluidPage(
    shinyjs::useShinyjs(),
    
    inlineCSS(list(.jexcel_content = "max-height:550px !important")),
    titlePanel("HeLa QC"), 
    fluidRow(
        column(12,
               img(src="logo.svg",height='100',width='100'),
               navbarPage( "HeLa MS Quality Control",

                          tabPanel("Eclipse_1",
                                   create.table("Eclipse_1")
                                   ),
                          tabPanel("Eclipse_2",
                                   create.table("Eclipse_2")
                                   )
                          
                          
                          )
               
               )
    )
    
)



server <- function(input, output, session) {

    MSs = c("Eclipse_1","Eclipse_2")
    

    lapply(1:2, function(i) {
        
        ms = as.character(MSs[i])
        print(ms)
        #assign(paste0(ms,"_MQdata"), read.csv(paste0("/home/tiago.sobreira/share/HeLa_QC/HeLa_",ms,"_data.csv"),sep=","), envir = .GlobalEnv)
        assign(paste0(ms,"_MQdata"), read.csv(paste0("/mnt/c/Users/tiago.sobreira/ubuntu/HeLa_QC/HeLa_",ms,"_data.csv"),sep=","), envir = .GlobalEnv)
        
        output[[paste0(ms,"_MQ_table")]] <- renderExcel(
            excelTable(data=get(paste0(ms,"_MQdata")), search=TRUE, getSelectedData = TRUE, allowInsertColumn = FALSE, allowDeleteColumn = FALSE, allowRenameColumn = FALSE)
        )

        dateaux = get(paste0(ms,"_MQdata"))
        dateauxall = dateaux
        
        dataFAIMS = dateaux %>% filter(grepl('_FAIMS',Sample.name))

        dateaux = dateaux %>% filter(!grepl('_FAIMS',Sample.name))

        
        ##
        ##Protein plot
        ##
        n = nrow(dateaux)
        protdata = (dateaux %>% select(Date,Proteins))[(n-4):n,]
        
        n = nrow(dataFAIMS)
        protdataFAIMS = (dataFAIMS %>% select(Date,Proteins))[(n-4):n,]


        
        
        if (nrow(protdataFAIMS)>0){
        
            protdata = cbind(protdata,protdataFAIMS)
            names(protdata) = c("Date","Proteins","Date FAIMS","Proteins FAIMS")
            
            avg = data.frame("Median", round(median(dateaux$Proteins)))
            names(avg) = c("Date","Proteins")
            
            avgFAIMS = data.frame("Median", round(median(dataFAIMS$Proteins)))
            names(avgFAIMS) = c("Date FAIMS","Proteins FAIMS")
            avg1 = cbind(avg,avgFAIMS)
            
            protdata = rbind(protdata,avg1)
            
            output[[paste0(ms,"_Proteins_last5")]] = DT::renderDataTable(protdata, server = FALSE,rownames = FALSE, width = "10px" ,options = list(dom = 't',ordering=F))

            output[[paste0(ms,"_Proteins")]] <- renderPlotly({
              plot_ly() %>% layout(title="Proteins") %>% 
              add_markers(data=dateaux, name="noFAIMS", x = as.Date(dateaux$Date, format="%m/%d/%Y"), y = dateaux$Proteins, text = dateaux$Sample.name, marker = list(color = 'rgb(0, 210, 0)')) %>% 
              add_trace(dateauxall, x = as.Date(dateauxall$Date, format="%m/%d/%Y"), y = median(dateaux$Proteins), colors="green", type = "scatter", mode = "lines", name="Median noFAIMS", line = list(color = 'rgb(0, 255, 0)',dash = 'dash')) %>%
              add_trace(dateauxall, x = as.Date(dateauxall$Date, format="%m/%d/%Y"), y = median(dateaux$Proteins)*0.9, colors="red", type = "scatter", mode = "lines", name="90% Median noFAIMS", line = list(color = 'rgb(0, 255, 0)')) %>%
              add_markers(data=dataFAIMS, name="FAIMS", x = as.Date(dataFAIMS$Date, format="%m/%d/%Y"), y = dataFAIMS$Proteins, text = dataFAIMS$Sample.name, marker = list(color = 'rgb(210, 0, 0)')) %>% 
              add_trace(dataFAIMS, x = as.Date(dataFAIMS$Date, format="%m/%d/%Y"), y = median(dataFAIMS$Proteins), colors="green", type = "scatter", mode = "lines",  name="Median FAIMS", line = list(color = 'rgb(255, 0, 0)',dash = 'dash')) %>%
              add_trace(dataFAIMS, x = as.Date(dataFAIMS$Date, format="%m/%d/%Y"), y = median(dataFAIMS$Proteins)*0.9, colors="red", type = "scatter", mode = "lines", name="90% Median FAIMS", line = list(color = 'rgb(255, 0, 0)')) 
            })

        }
        else{

            blanc = data.frame("","")
            names(blanc) = c("Date","Proteins")

            protdata = rbind(protdata,blanc)
            names(protdata) = c("Date","Proteins")
            
            avg = data.frame("Median", round(median(dateaux$Proteins)))
            names(avg) = c("Date","Proteins")
            
            protdata = rbind(protdata,avg)
            
            output[[paste0(ms,"_Proteins_last5")]] = DT::renderDataTable(protdata, server = FALSE,rownames = FALSE, width = "10px" ,options = list(dom = 't',ordering=F))

            output[[paste0(ms,"_Proteins")]] <- renderPlotly({
              plot_ly() %>% layout(title="Proteins") %>%
              add_markers(data=dateaux, name="Proteins", x = as.Date(dateaux$Date, format="%m/%d/%Y"), y = dateaux$Proteins, text = dateaux$Sample.name,  marker = list(color = 'rgb(0,210,0)')) %>%
              add_trace(dateaux, x = as.Date(dateaux$Date, format="%m/%d/%Y"), y = median(dateaux$Proteins), colors="green", type = "scatter", mode = "lines", name="Median", line = list(color = 'rgb(0, 255, 0)',dash = 'dash')) %>%
              add_trace(dateaux, x = as.Date(dateaux$Date, format="%m/%d/%Y"), y = median(dateaux$Proteins)*0.9, colors="red", type = "scatter", mode = "lines", name="90% Median", line = list(color = 'rgb(0, 255, 0)')) 
            })

        }

        ##
        ##Peptide plot
        ##
        n = nrow(dateaux)
        pepdata = (dateaux %>% select(Date,Peptides))[(n-4):n,]
        
        n = nrow(dataFAIMS)
        pepdataFAIMS = (dataFAIMS %>% select(Date,Peptides))[(n-4):n,]
        
        
        
        
        if (nrow(pepdataFAIMS)>0){
          
          pepdata = cbind(pepdata,pepdataFAIMS)
          names(pepdata) = c("Date","Peptides","Date FAIMS","Peptides FAIMS")
          
          avg = data.frame("Median", round(median(dateaux$Peptides)))
          names(avg) = c("Date","Peptides")
          
          avgFAIMS = data.frame("Median", round(median(dataFAIMS$Peptides)))
          names(avgFAIMS) = c("Date FAIMS","Peptides FAIMS")
          avg1 = cbind(avg,avgFAIMS)
          
          pepdata = rbind(pepdata,avg1)
          
          output[[paste0(ms,"_Peptides_last5")]] = DT::renderDataTable(pepdata, server = FALSE,rownames = FALSE, width = "10px" ,options = list(dom = 't',ordering=F))
          
          output[[paste0(ms,"_Peptides")]] <- renderPlotly({
            plot_ly() %>% layout(title="Peptides") %>% 
              add_markers(data=dateaux, name="noFAIMS", x = as.Date(dateaux$Date, format="%m/%d/%Y"), y = dateaux$Peptides, text = dateaux$Sample.name, marker = list(color = 'rgb(0, 210, 0)')) %>% 
              add_trace(dateauxall, x = as.Date(dateauxall$Date, format="%m/%d/%Y"), y = median(dateaux$Peptides), colors="green", type = "scatter", mode = "lines", name="Median noFAIMS", line = list(color = 'rgb(0, 255, 0)',dash = 'dash')) %>%
              add_trace(dateauxall, x = as.Date(dateauxall$Date, format="%m/%d/%Y"), y = median(dateaux$Peptides)*0.9, colors="red", type = "scatter", mode = "lines", name="90% Median noFAIMS", line = list(color = 'rgb(0, 255, 0)')) %>%
              add_markers(data=dataFAIMS, name="FAIMS", x = as.Date(dataFAIMS$Date, format="%m/%d/%Y"), y = dataFAIMS$Peptides, text = dataFAIMS$Sample.name, marker = list(color = 'rgb(210, 0, 0)')) %>% 
              add_trace(dataFAIMS, x = as.Date(dataFAIMS$Date, format="%m/%d/%Y"), y = median(dataFAIMS$Peptides), colors="green", type = "scatter", mode = "lines",  name="Median FAIMS", line = list(color = 'rgb(255, 0, 0)',dash = 'dash')) %>%
              add_trace(dataFAIMS, x = as.Date(dataFAIMS$Date, format="%m/%d/%Y"), y = median(dataFAIMS$Peptides)*0.9, colors="red", type = "scatter", mode = "lines", name="90% Median FAIMS", line = list(color = 'rgb(255, 0, 0)')) 
          })
          
        }
        else{
          
          blanc = data.frame("","")
          names(blanc) = c("Date","Peptides")
          
          pepdata = rbind(pepdata,blanc)
          names(pepdata) = c("Date","Peptides")
          
          avg = data.frame("Median", round(median(dateaux$Peptides)))
          names(avg) = c("Date","Peptides")
          
          pepdata = rbind(pepdata,avg)
          
          output[[paste0(ms,"_Peptides_last5")]] = DT::renderDataTable(pepdata, server = FALSE,rownames = FALSE, width = "10px" ,options = list(dom = 't',ordering=F))
          
          output[[paste0(ms,"_Peptides")]] <- renderPlotly({
            plot_ly() %>% layout(title="Peptides") %>%
              add_markers(data=dateaux, name="Peptides", x = as.Date(dateaux$Date, format="%m/%d/%Y"), y = dateaux$Peptides, text = dateaux$Sample.name,  marker = list(color = 'rgb(0,210,0)')) %>%
              add_trace(dateaux, x = as.Date(dateaux$Date, format="%m/%d/%Y"), y = median(dateaux$Peptides), colors="green", type = "scatter", mode = "lines", name="Median", line = list(color = 'rgb(0, 255, 0)',dash = 'dash')) %>%
              add_trace(dateaux, x = as.Date(dateaux$Date, format="%m/%d/%Y"), y = median(dateaux$Peptides)*0.9, colors="red", type = "scatter", mode = "lines", name="90% Median", line = list(color = 'rgb(0, 255, 0)')) 
          })
          
        }
        

        ##
        ##PSMs plot
        ##
        n = nrow(dateaux)
        MSdata = (dateaux %>% select(Date,PSMs))[(n-4):n,]
        
        n = nrow(dataFAIMS)
        MSdataFAIMS = (dataFAIMS %>% select(Date,PSMs))[(n-4):n,]
        
        
        if (nrow(MSdataFAIMS)>0){
          
          MSdata = cbind(MSdata,MSdataFAIMS)
          names(MSdata) = c("Date","PSMs","Date FAIMS","PSMs FAIMS")
          
          avg = data.frame("Median", round(median(dateaux$PSMs)))
          names(avg) = c("Date","PSMs")
          
          avgFAIMS = data.frame("Median", round(median(dataFAIMS$PSMs)))
          names(avgFAIMS) = c("Date FAIMS","PSMs FAIMS")
          avg1 = cbind(avg,avgFAIMS)
          
          MSdata = rbind(MSdata,avg1)
          
          output[[paste0(ms,"_PSMs_last5")]] = DT::renderDataTable(MSdata, server = FALSE,rownames = FALSE, width = "10px" ,options = list(dom = 't',ordering=F))
          
          output[[paste0(ms,"_PSMs")]] <- renderPlotly({
            plot_ly() %>% layout(title="PSMs") %>% 
              add_markers(data=dateaux, name="noFAIMS", x = as.Date(dateaux$Date, format="%m/%d/%Y"), y = dateaux$PSMs, text = dateaux$Sample.name, marker = list(color = 'rgb(0, 210, 0)')) %>% 
              add_trace(dateauxall, x = as.Date(dateauxall$Date, format="%m/%d/%Y"), y = median(dateaux$PSMs), colors="green", type = "scatter", mode = "lines", name="Median noFAIMS", line = list(color = 'rgb(0, 255, 0)',dash = 'dash')) %>%
              add_trace(dateauxall, x = as.Date(dateauxall$Date, format="%m/%d/%Y"), y = median(dateaux$PSMs)*0.9, colors="red", type = "scatter", mode = "lines", name="90% Median noFAIMS", line = list(color = 'rgb(0, 255, 0)')) %>%
              add_markers(data=dataFAIMS, name="FAIMS", x = as.Date(dataFAIMS$Date, format="%m/%d/%Y"), y = dataFAIMS$PSMs, text = dataFAIMS$Sample.name, marker = list(color = 'rgb(210, 0, 0)')) %>% 
              add_trace(dataFAIMS, x = as.Date(dataFAIMS$Date, format="%m/%d/%Y"), y = median(dataFAIMS$PSMs), colors="green", type = "scatter", mode = "lines",  name="Median FAIMS", line = list(color = 'rgb(255, 0, 0)',dash = 'dash')) %>%
              add_trace(dataFAIMS, x = as.Date(dataFAIMS$Date, format="%m/%d/%Y"), y = median(dataFAIMS$PSMs)*0.9, colors="red", type = "scatter", mode = "lines", name="90% Median FAIMS", line = list(color = 'rgb(255, 0, 0)')) 
          })
          
        }
        else{
          
          blanc = data.frame("","")
          names(blanc) = c("Date","PSMs")
          
          MSdata = rbind(MSdata,blanc)
          names(MSdata) = c("Date","PSMs")
          
          avg = data.frame("Median", round(median(dateaux$PSMs)))
          names(avg) = c("Date","PSMs")
          
          MSdata = rbind(MSdata,avg)
          
          output[[paste0(ms,"_PSMs_last5")]] = DT::renderDataTable(MSdata, server = FALSE,rownames = FALSE, width = "10px" ,options = list(dom = 't',ordering=F))
          
          output[[paste0(ms,"_PSMs")]] <- renderPlotly({
            plot_ly() %>% layout(title="PSMs") %>%
              add_markers(data=dateaux, name="PSMs", x = as.Date(dateaux$Date, format="%m/%d/%Y"), y = dateaux$PSMs, text = dateaux$Sample.name,  marker = list(color = 'rgb(0,210,0)')) %>%
              add_trace(dateaux, x = as.Date(dateaux$Date, format="%m/%d/%Y"), y = median(dateaux$PSMs), colors="green", type = "scatter", mode = "lines", name="Median", line = list(color = 'rgb(0, 255, 0)',dash = 'dash')) %>%
              add_trace(dateaux, x = as.Date(dateaux$Date, format="%m/%d/%Y"), y = median(dateaux$PSMs)*0.9, colors="red", type = "scatter", mode = "lines", name="90% Median", line = list(color = 'rgb(0, 255, 0)')) 
          })
          
        }
        



        ##
        ##MSMS
        ##
        n = nrow(dateaux)
        MSdata = (dateaux %>% select(Date,MSMS))[(n-4):n,]
        
        n = nrow(dataFAIMS)
        MSdataFAIMS = (dataFAIMS %>% select(Date,MSMS))[(n-4):n,]
        
        
        if (nrow(MSdataFAIMS)>0){
          
          MSdata = cbind(MSdata,MSdataFAIMS)
          names(MSdata) = c("Date","MSMS","Date FAIMS","MSMS FAIMS")
          
          avg = data.frame("Median", round(median(dateaux$MSMS)))
          names(avg) = c("Date","MSMS")
          
          avgFAIMS = data.frame("Median", round(median(dataFAIMS$MSMS)))
          names(avgFAIMS) = c("Date FAIMS","MSMS FAIMS")
          avg1 = cbind(avg,avgFAIMS)
          
          MSdata = rbind(MSdata,avg1)
          
          output[[paste0(ms,"_MSMS_last5")]] = DT::renderDataTable(MSdata, server = FALSE,rownames = FALSE, width = "10px" ,options = list(dom = 't',ordering=F))
          
          output[[paste0(ms,"_MSMS")]] <- renderPlotly({
            plot_ly() %>% layout(title="MSMS") %>% 
              add_markers(data=dateaux, name="noFAIMS", x = as.Date(dateaux$Date, format="%m/%d/%Y"), y = dateaux$MSMS, text = dateaux$Sample.name, marker = list(color = 'rgb(0, 210, 0)')) %>% 
              add_trace(dateauxall, x = as.Date(dateauxall$Date, format="%m/%d/%Y"), y = median(dateaux$MSMS), colors="green", type = "scatter", mode = "lines", name="Median noFAIMS", line = list(color = 'rgb(0, 255, 0)',dash = 'dash')) %>%
              add_trace(dateauxall, x = as.Date(dateauxall$Date, format="%m/%d/%Y"), y = median(dateaux$MSMS)*0.9, colors="red", type = "scatter", mode = "lines", name="90% Median noFAIMS", line = list(color = 'rgb(0, 255, 0)')) %>%
              add_markers(data=dataFAIMS, name="FAIMS", x = as.Date(dataFAIMS$Date, format="%m/%d/%Y"), y = dataFAIMS$MSMS, text = dataFAIMS$Sample.name, marker = list(color = 'rgb(210, 0, 0)')) %>% 
              add_trace(dataFAIMS, x = as.Date(dataFAIMS$Date, format="%m/%d/%Y"), y = median(dataFAIMS$MSMS), colors="green", type = "scatter", mode = "lines",  name="Median FAIMS", line = list(color = 'rgb(255, 0, 0)',dash = 'dash')) %>%
              add_trace(dataFAIMS, x = as.Date(dataFAIMS$Date, format="%m/%d/%Y"), y = median(dataFAIMS$MSMS)*0.9, colors="red", type = "scatter", mode = "lines", name="90% Median FAIMS", line = list(color = 'rgb(255, 0, 0)')) 
          })
          
        }
        else{
          
          blanc = data.frame("","")
          names(blanc) = c("Date","MSMS")
          
          MSdata = rbind(MSdata,blanc)
          names(MSdata) = c("Date","MSMS")
          
          avg = data.frame("Median", round(median(dateaux$MSMS)))
          names(avg) = c("Date","MSMS")
          
          MSdata = rbind(MSdata,avg)
          
          output[[paste0(ms,"_MSMS_last5")]] = DT::renderDataTable(MSdata, server = FALSE,rownames = FALSE, width = "10px" ,options = list(dom = 't',ordering=F))
          
          output[[paste0(ms,"_MSMS")]] <- renderPlotly({
            plot_ly() %>% layout(title="MSMS") %>%
              add_markers(data=dateaux, name="MSMS", x = as.Date(dateaux$Date, format="%m/%d/%Y"), y = dateaux$MSMS, text = dateaux$Sample.name,  marker = list(color = 'rgb(0,210,0)')) %>%
              add_trace(dateaux, x = as.Date(dateaux$Date, format="%m/%d/%Y"), y = median(dateaux$MSMS), colors="green", type = "scatter", mode = "lines", name="Median", line = list(color = 'rgb(0, 255, 0)',dash = 'dash')) %>%
              add_trace(dateaux, x = as.Date(dateaux$Date, format="%m/%d/%Y"), y = median(dateaux$MSMS)*0.9, colors="red", type = "scatter", mode = "lines", name="90% Median", line = list(color = 'rgb(0, 255, 0)')) 
          })
          
        }
        
    })

  
    observeEvent(reactiveValuesToList(input),{
        
        for (i in names(reactiveValuesToList(input))){

            if (grepl ("MQ_table",i) ){
                if (! tables[[i]] == as.character(input[[i]][-1][2])){
                    
                    if (!is.null(input[[i]][-1][2]$selectedDataBoundary$borderTop)){
                        tables[[i]] <<- as.character(input[[i]][-1][2])
                        
                        ms = gsub("_MQ_table","",i)
                        col = as.character((input[[paste0(ms,"_MQ_table")]]$selectedDataBoundary$borderLeft)+1)
                        if (as.numeric(col)>2){

                            dateaux = get(paste0(ms,"_MQdata"))
        
                            output[[paste0(ms,"_MQ_plot")]] <- renderPlot({
                                plot(sort(as.Date(dateaux$Date, format="%m/%d/%Y")), dateaux[,as.numeric(col)], type="p")
                            })
                        }
                    }
                }
            }

            
        }
    })
    
}

shinyApp(ui, server)
