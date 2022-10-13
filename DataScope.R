# %%%%%%%%%%%%%%%%%%% Two lines of code for less pain %%%%%%%%%%%%%%%%%%%

rm(list=ls()) # Clears the environment so that you do not inadvertently rely on a previously loaded dataset
set.seed(888) # Sets the random seed so that processes such as kmeans and t-SNE run the same way each time

########################### INSTALL PACKAGES ##############################
#######%%%%%%%%%%%######## DOWNLOAD LIBRARIES #######%%%%%%%%%%%###########

library(jpeg)
library(LS2W)
#library(LS2Wstat)
library(tidyverse)
library(ggplot2)
library(imager)
library(rsconnect)
library(plyr)
library(Rtsne) # t-SNE dimensionality reduction
library(cowplot)
library(magick)
library(DT)
library(digest)
library(shinyjs)
#library(gtools)
library(reshape)
library(RColorBrewer)
library(ggforce)
library(umap)
library(signal)
library(OpenImageR)
library(shinythemes)


## Create a function to save a the labels to data files locally:
save_data_flatfile <- function(data) {
    data <- t(data)
    file_name <- paste0(paste(get_time_human(), digest(data, 
                                                       algo = "md5"), sep = "_"), ".csv")
    write.csv(x = data, file = file.path(results_dir, file_name), 
              row.names = FALSE, quote = TRUE)
}

## Create a standardization 0-1 function:
std01 <- function(x){(x-min(x))/(max(x)-min(x))}

# Create a function that converts the computer time to huimans time
humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")


### Create a function to update the list of frames
update_frames <- function(labelled_frames, frames_list){
    
    l  = rbind(labelled_frames, frames_list)
    return(l)
}

labelled_frames =list( frame = vector("numeric", 0))
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
# %%%%%%%%%%%%%%%%%You will need to change directories in this section%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5



# Create a function to save the data 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%% CAutION: THIS SECTION IS HARD CODED %%%%%%%%%%%%%%%%%%%%%%%5
# You will need to change it for each video
# create a file to record the responses
response.temp = data.frame(frame=as.integer(),
                           valance=as.integer(),
                           arousal=as.integer(),
                           label = factor(levels(c(1:15))),
                           comment=character(), 
                           timestamp=numeric())

############## %%%%%%%%%%%%%% *% CHANGE HERE %* %%%%%%%%%%%%%%% ###############
saveData <- function(data) {
  ##change
    data1 = as.data.frame(read.csv("inseer csv file lication.csv"))
    data = rbind(data1, data )
    write.csv(x = data, file = (fileName),
              row.names = FALSE, quote = TRUE)
}


##change
fileName = ("name your output.csv")
write.csv(x = response.temp, file = (fileName),
          row.names = FALSE, quote = TRUE)



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
## Set the directory and load the data

## change
setwd("where pictures are saved")

#change
full.data=read_csv("output from OpenFace data.txt") #this can be other data as well
data.of.AU = full.data [,c(1,2,511:527)] # these changes for OpenFace output to keep action untis only
data.of.AU = as.data.frame(data.of.AU)

#create list of all .bmp files in folder 
temp_files <- (list.files(pattern="*.bmp",full.names = T))
files = gsub("^(?:[^_]+_){3}([^_]+).*", "\\1", temp_files)
files =  sub("\\..*", "", files)
files = as.numeric(files)
files = as.data.frame(files)
names(files)[1] = "frame"

#files = 1:length(temp_files)

files$file <- temp_files
files$frame = 1:length(temp_files)

#data.of.AU$file = files$file
data.of.AU = right_join(data.of.AU, files)

#data.of.AU = data.of.AU %>% dplyr:: filter(success == 1)
data.of.AU = data.of.AU[,-2]

data.of.AU$label = NA
data.of.AU = data.of.AU %>% mutate(label = factor(label,levels = c(1:15)))

## cretae a group for down sampling
#uncomment the next line to downsample
#data.of.AU = data.of.AU[seq(1,nrow(data.of.AU),5),]
data.of.AU2=data.of.AU
#data.umap = readRDS("data.umap.rds") 
data.umap = as.data.frame(uwot::umap(data.of.AU[,c(2:18)], y = data.of.AU[,20]))
data.umap$frame = data.of.AU$frame


meanFace = full.data [,c(1,2,511:527)]
meanFace = right_join(meanFace,files)
#meanFace = meanFace %>% dplyr::filter(success == 1)
meanFace = data.frame(frame=meanFace[,1], file =(meanFace[,20]),  mean.AU=rowMeans(meanFace[,3:19], na.rm = T))
meanFace = as.data.frame(meanFace[which.min(meanFace$mean.AU),])
meanFace$file = as.character(meanFace$file)


long.AU.df = data.of.AU[,c(1, 2:18)] %>% group_by(frame)

colClean <- function(x){ colnames(x) <- gsub("AU", "", colnames(x)); x } 
long.AU.df=colClean(long.AU.df)
colClean <- function(x){ colnames(x) <- gsub("_r", "", colnames(x)); x } 
long.AU.df=colClean(long.AU.df)

long.AU.df=long.AU.df%>% group_by(frame) %>% gather(variable, value,2:18)
long.AU.df$value = as.numeric(long.AU.df$value)
mean.AU.df = long.AU.df %>% dplyr::group_by(variable) %>% dplyr::summarise(m = mean(value, na.rm=T))


detach(package:plyr)

#######%%%%%%%%%%%######## BEGINNING OF THE INTERACTIVE COMPONENT #######%%%%%%%%%%%########


# Define UI for application 
ui <- 
    
    navbarPage(theme = shinytheme("flatly"), collapsible = FALSE,
               "The DataScope", 
  
    #tags$style(includeCSS("styles.css")),
    # titlePanel("The DataScope",
    #             div(class="outer",
                     tags$head(includeCSS("styles.css")),
   #hr(),
    #tags$div(tags$img(src='~/Google Drive/Exp/Reinforcement_App/datascope.png', width = 108, height = 108, style="float:left; margin-left: 5px; margin-right: 5px; margin-top: -15px")), 
    #br(),
    fluidRow(
        column(3,  h6("  Select the size of the cluster in the UMAP space using the slider"),
               br(),
               sliderInput("brushed_points",
                                          "Cluster Size",
                                          min = -5,
                                          max = 5,
                                          step = 0.1,
                                          value = 0)),
        
        column (6,offset = 3,  h6("Select the variable from the drop down menu and use the slider to control the zooming window"),
                br(),
                
                column(6, sliderInput("steps",
                              "Zooming Window",
                              min = 0,
                              max = 5 * nrow(data.of.AU),
                              step = 5,
                              value = c(0,0))),
                column(4, offset = 1, varSelectInput("AU", "Action Unit",(data.of.AU[,c(2:18)]))))),
    hr(),
    fluidRow( 
        column(6, 
                  
                   wellPanel(style = "background-color: #fff; border-color: #2c3e50; height: 460px;",
                         plotOutput("plotSub2",brush = "plot_brush"))),
       
        
        column(6,
               
               fluidRow( 
                   wellPanel(style = "background-color: #fff; border-color: #2c3e50; height: 220px;",
                             plotOutput("OpenFace",  height=180))),
              
       
              fluidRow(wellPanel(style = "background-color: #fff; border-color: #2c3e50; height: 220px;",
                        plotOutput("PCP",  height=180))))),
    
   hr(),
  #fluidRow(verbatimTextOutput("plot_brushinfo")),
   
   fluidRow(
       column(6,
              fluidRow(column(8,
                              fluidRow(
                                  column(6, sliderInput("arousal",
                                                           "Arousal",
                                                           min = -5,
                                                           max = 5,
                                                           step = 1,
                                                           value = 0)),
                                  column(6,selectInput("label", "Label", choices=list("Happy" = "1", "Sad" = "2", "Surprised" = "3", "Scared" = "4", "Angry" = "5", "Unimpressed" = "6",
                                                "Dissatisfied" = "7", "Bored" = "8", "Concerned" = "9", "Focused" = "10", "Interested"= "11", "Yawning"= "12", "Neutral" = "13","Nervous" = "14","NA" = "15")))),
                              br(),
                              fluidRow  (column(6,sliderInput("valance","Valance",
                                                                                    min = -5,
                                                                                    max = 5,
                                                                                    step = 1,
                                                                                    value = 0)),
                                         column(6,textInput("comment", "Comments")))),
                       column(3,  wellPanel(style = "background-color: #fff; border-color: #2c3e50; height: 180px;",
                                                    plotOutput(outputId = "meanFace", height=150)))),
       
       fluidRow(column(3, offdset =3, 
                       
                       column(2,actionButton("submit", "Submit", class = "btn-primary"))))),
                           
            
            
                column ( 6,   wellPanel(style = "background-color: #fff; border-color: #2c3e50; height: 420px;",
                                       plotOutput(outputId = "plotSub"))))
   
  
    
    
)


fieldsAll <- c( "arousal","valance","label", "comment")
epochTime <- function() {
    as.integer(Sys.time())
}



# Define server logic 
server <- function(input, output, session) {
    
   
    row = reactive({
        as.numeric(input$steps[2])})
    steps = reactive({
        as.numeric(input$steps[1])})
    formData = reactive({
        data <- sapply(fieldsAll, function(x) input[[x]])
        data <- c(data, timestamp = epochTime())
        data <- t(data)
        data <- as.data.frame(data)
        data <- data[rep(seq_len(nrow(data)), length(frames()$frame_no)),]
        data
    })
    
    
    
    frames =  reactive({
        
        temp = as.data.frame(brushedPoints(values4$data.umap, input$plot_brush))
        temp = temp %>%  dplyr::filter(!frame %in% values$labelled_frames$frame)
        frame_no = temp$frame
        frame_no = as.data.frame(frame_no)
        
        frame_no })
    
    
    ## Save the labelled frames to turn them into grey in the UMAP plot 
    values <- reactiveValues(labelled_frames = data.frame(frame=0))
    values2 <- reactiveValues(labels = data.frame(label =as.factor(1)))
    values3 <- reactiveValues(data.of.AU2 = data.frame())
    values4 <- reactiveValues(data.umap = data.umap)
    
   
    # This is to check what is being selected in the brush
    output$plot_brushinfo <- renderPrint({
        cat("Brush (debounced):\n")
      t=(cbind(frames(),formData()))
      t
     
    })
    
    
    
    output$OpenFace <- renderPlot({
        data.test = values4$data.umap %>% dplyr::filter(V1 > (input$plot_brush$xmin - input$brushed_points) & V1 < (input$plot_brush$xmax + input$brushed_points) &V2 > (input$plot_brush$ymin - input$brushed_points) & V2 < (input$plot_brush$ymax + input$brushed_points)) 
    
        
        ggplot( )+
            geom_line(data = data.of.AU,aes(x = frame, y = !!input$AU), alpha = .7)+
            geom_point(data = (data.of.AU %>% dplyr::filter (frame %in% data.test$frame)),aes(x = frame, y = !!input$AU), size = .4,  alpha = .7,  color="red")+
            #geom_line(aes(x = frame, y = AU04_r), alpha = .7)+
            
            #geom_line(aes(x = frame, y = AU02_r))+
            theme_bw()+
            xlab('Frame Number')+
            ylab(paste("Value of", input$AU))+
            theme(legend.position = "none")+
            facet_zoom(xlim = c(steps(), row()), horizontal = FALSE,zoom.size=1)
        
        
    })
    
    
    output$plotSub <- renderPlot ({
        
        if (is.null(input$plot_brush)){

            im.data2 = (data.umap %>% dplyr::filter(frame==5))}
        else {   
            im.data2 = as.data.frame(brushedPoints(values4$data.umap, input$plot_brush))
            im.data2 = im.data2 %>% dplyr::filter(V1 > (input$plot_brush$xmin - input$brushed_points) & V1 < (input$plot_brush$xmax + input$brushed_points) &V2 > (input$plot_brush$ymin - input$brushed_points) & V2 < (input$plot_brush$ymax + input$brushed_points))
         
            
        }
        
        test_im_data = merge (data.of.AU , im.data2, by = "frame")
        test_im_data = test_im_data %>% dplyr::filter(!frame %in% values$labelled_frames$frame)
        par(mar = c(0, 0, 0, 0))
        par(mfrow = c(3,6))
        
        if (nrow(test_im_data) > 18){
            test_im_data = test_im_data[sample(nrow(test_im_data), 18), ]  
        }
        
        for(i in 1:nrow(test_im_data)){
            plot(load.image(test_im_data[i,19]), axes=FALSE)
        }
    })
    
    
    output$meanFace <- renderPlot ({
        
        #im.data2 = as.data.frame(brushedPoints(data.umap, input$plot_brush))
        #meanFacePlot= data.of.AU %>% filter(frame == meanFace$frame) 
        
        
        par(mar = c(0, 0, 0, 0))
        #plot(load.image(data.of.AU[10,20]), axes=FALSE)
        
        plot(load.image(meanFace[1,2]), axes=FALSE)
        
    })
    
    output$plotSub2 <- renderPlot ({
        if (is.null(input$plot_brush)){
        
        ggplot()+
            geom_point(data = (values4$data.umap %>% dplyr::filter(frame %in% values$labelled_frames$frame)), aes(x = V1, y = V2),  alpha = 1,  color="green")+
            geom_point(data = (values4$data.umap%>% dplyr::filter(!frame %in% values$labelled_frames$frame)), aes(x = V1, y = V2),  alpha = .5, shape = 21)+
            geom_point(data = (values4$data.umap %>% dplyr::filter(frame >  steps() & frame < row())), aes(x = V1, y = V2), alpha=.75,  color = "blue")+
            theme_void()
            # xlab('Component 1')+
            # ylab('Component 2')
            }
            
        else {
            ggplot()+
                geom_point(data = (values4$data.umap %>% dplyr::filter(frame %in% values$labelled_frames$frame)), aes(x = V1, y = V2),  alpha = 1,  color="green")+
                geom_point(data = (values4$data.umap%>% dplyr::filter(!frame %in% values$labelled_frames$frame)), aes(x = V1, y = V2),  alpha = .5, shape = 21)+
                geom_point(data = (values4$data.umap %>% dplyr::filter(frame > steps()  & frame < row() )), aes(x = V1, y = V2), alpha=.75,  color = "blue")+
                geom_point(data = (values4$data.umap %>% dplyr::filter(V1 > (input$plot_brush$xmin - input$brushed_points) & V1 < (input$plot_brush$xmax + input$brushed_points) &V2 > (input$plot_brush$ymin - input$brushed_points) & V2 < (input$plot_brush$ymax + input$brushed_points)) ),
                           aes(x = V1, y = V2),  alpha = 1,  color="red")+
                
                #geom_point(data = data.meanFace[10,], aes(x = V1, y = V2),  alpha = 1, color ="green", size =3)+
                theme_void()
                # xlab('Component 1')+
                # ylab('Component 2')
        }
        
    })
    
    output$PCP <- renderPlot ({
     
        ggplot()+
        geom_line(data = (long.AU.df%>% dplyr::filter((frame %in% frames()$frame_no) & (!frame %in% values$labelled_frames$frame))), aes(x = variable, y = value, group = frame), alpha=.4)+
          geom_point(data = mean.AU.df , aes(x = variable, y = m), size = 7, color = "blue", alpha=.4)+
          ylim(0, 5)+
          ylab("Action Unit Intensity")+
          xlab("Action Unit")+
          theme_bw()
      
    })
    
    
    
    # action to take when submit button is pressed
    observeEvent(input$submit, {
        saveData(cbind(frames(),formData()))
        # labeled.frames = data.umap %>% filter(frame %in% labeled$frame_no)
    })
    
    newEntry <- observeEvent(input$submit,{

      label_new <- c(as.character(values2$labels$label),as.character(rep(input$label,length(frames()$frame_no))))
      values2$labels <- data.frame(label = label_new)

      frame_new <- c(values$labelled_frames$frame,as.numeric(frames()$frame_no))
      values$labelled_frames <- data.frame(frame=frame_new)
      values$labelled_frames$label = values2$labels$label

      # values3$data.of.AU2 = join( data.of.AU,values$labelled_frames, by  =("frame"))
      # 
      # values4$data.umap = as.data.frame(uwot::umap(data.of.AU[,c(2:18)], y =as.factor(values3$data.of.AU2[,21]) ))
      # values4$data.umap$frame = data.of.AU[,1]

    })

    
}



# Run the application 


shinyApp(ui = ui, server = server)