#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(tidyverse)
dat<-read.csv("apps.csv", encoding="UTF-8")

dat2<-dat[,c(3,6)] %>% unique()
icons<-dat2$icon[1:100]
names(icons)<-sapply(1:length(icons), function(x) paste("id",x, sep=""))

titles<-dat2$title[1:100]
names(titles)<-names(icons)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    output$image1<- renderUI({
        
        imgurl <- icons
        
        
        imgfr <- lapply(imgurl, function(file){
            tags$div(id=names(file),
                tags$img(src=file, width=168, height=168),
               tags$figcaption(titles[names(file)], style="display: table-caption; font: bold;")
            )
            
        })
        
        imgfr$cellArgs <- list(
            style = "
        width: auto;
        height: auto;
        margin: 8px;
        ")
        
        do.call(flowLayout, imgfr)
    })
    
    
    lapply(X = names(icons), FUN = function(id) {
        onclick(id, {
            print(id)
        })
    })
    
 
})
