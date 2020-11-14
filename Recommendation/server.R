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

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    ##the app_icon file have been saved
    app_icons<-read.csv("app_icon.csv", stringsAsFactors = TRUE)
    
    ##the icon url for displaying 
    icons<-app_icons$icon
    ##assign name for easy reference
    names(icons)<-sapply(1:length(icons), function(x) paste("id",x, sep=""))
    
    ##titles will include the ratings
    titles<-app_icons$titles
    ##assign name for easy reference
    names(titles)<-names(icons)
    
    ##reactive to input user id
    index<-reactive(
        if (input$userid=="newuser")
            order(app_icons$rating, decreasing = TRUE)[1:100]
        else 
            order(app_icons$rating, decreasing = TRUE)[1:100]
            #order(app_icons[,input$userid], decreasing = TRUE)[1:100]
    )
    output$image1<- renderUI({
        
        imgurl <- icons[index()]
        
        imgfr <- lapply(imgurl, function(file){
            tags$div(id=names(file),
                tags$img(src=file, width=168, height=168),
               tags$figcaption(titles[names(file)], style="display: table-caption; word-wrap: normal;")
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
            showModal(modalDialog(
                title="detail",
                HTML(paste("<img src=", icons[id], ">", sep="")),
                HTML("<br>"),
                titles[id],
                HTML("<br>"),
                HTML("Description: "),
                app_icons[app_icons$titles==titles[id],"tagline"],
                size="l",
                easyClose = TRUE
            ))
        })
    })
    
 
})
