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

# Define server logic for main panel
shinyServer(function(input, output, session) {
    ##the app_icon file have been saved, it have the url for icon, title, rating and tagline for app display
    load("app_icons.Rdata")
    
    ##the icon url for displaying 
    icons<-app_icons$icon
    
    ##assign names for easy reference
    names(icons)<-sapply(1:length(icons), function(x) paste("id",x, sep=""))
    
    ##titles will include the ratings
    titles<-app_icons$titles
    
    ##assign names for easy reference
    names(titles)<-names(icons)
    
    ##the abbr_id
    abbre_id<-names(icons)
    
    ##
    names(abbre_id)<-app_icons$id    
        
    ##create new user recommendation list
    load("recom_newuser.Rdata")
    
    ##reactive to user id input for recommend reason
    reason<-reactive(
        ## input id is newuser
        if (input$userid=="newuser")
            "The recommendation is based on rating and review count"
        ## input id is active user
        else
            "The recommendation is based on user rating history"
    )
    
    load("top32.Rdata")
    ##reactive to user id input for default display
    index<-reactive(
        ## input id is newuser
        if (input$userid=="newuser"){
            order(app_icons$rating, app_icons$reviews_count, decreasing = TRUE)[1:32]
            #recom_list<-newuser_recom_list
        }
        ## input id is active user
        else {
            abbre_id_names<<-top32$app_id[top32$user_id==input$userid]
            abbre_id[abbre_id_names]
            
        }
    )
    
    ##reactive to user_id for app similarity display
    recom_list<-reactive(
        ## input id is newuser
        if (input$userid=="newuser"){
            newuser_recom_list
        }
        ## input id is active user
        else {
            #order(app_icons[,input$userid], decreasing = TRUE)[1:100]
            #activeuser_recom_list[[input$userid]]
            newuser_recom_list
        }
    )
    
    output$text1<-renderText(
        reason()
    )
    
    output$image1<- renderUI({
        ## obtain the list of url
        imgurl <- icons[index()]
        
        ## create image and caption(title) for each url
        imgfr <- lapply(imgurl, function(file){
            tags$div(id=names(file),
                tags$img(src=file, width=168, height=168),
               tags$figcaption(titles[names(file)], style="display: table-caption; font-weight:bold;")
            )
        })
        
        ## arguments for the images
        imgfr$cellArgs <- list(
            style = "
        width: auto;
        height: auto;
        margin: 8px;
        ")
        
        do.call(flowLayout, imgfr)
    })
    
    ## make images clickable to pop up dialog window
    lapply(X = names(icons), FUN = function(id) {
        onclick(id, {
            showModal(modalDialog(
                ##formating dialog window
                title="Detail",
                HTML(paste("<img src=", icons[id], "><br>", sep="")),
                titles[id],
                HTML("<br>Description: "),
                HTML("<br>"),
                app_icons[app_icons$titles==titles[id],"tagline"],
                HTML("<br><br><br>"),
                HTML('<p style="font-size:15px;font-weight:bold"> Similar apps to the one above: </p>' ),
                HTML(paste("<img src=", icons[recom_list()[[id]][1]], ">", sep="")),
                titles[recom_list()[[id]][1]],
                HTML("<br><br>"),
                HTML(paste("<img src=", icons[recom_list()[[id]][2]], ">", sep="")),
                titles[recom_list()[[id]][2]],
                HTML("<br><br>"),
                HTML(paste("<img src=", icons[recom_list()[[id]][3]], ">", sep="")),
                titles[recom_list()[[id]][3]],
                HTML("<br><br>"),
                HTML(paste("<img src=", icons[recom_list()[[id]][4]], ">", sep="")),
                titles[recom_list()[[id]][4]],
                HTML("<br><br>"),
                HTML(paste("<img src=", icons[recom_list()[[id]][5]], ">", sep="")),
                titles[recom_list()[[id]][5]],
                HTML("<br><br>"),
                size="l",
                easyClose = TRUE
            ))
        })
    })
    
    output$text2<-renderText(
        "History:"
    )
    
    ##load history_table for user
    load("history_table.Rdata")
    
    
    index2<-reactive(
        ## input id is newuser
        if (input$userid=="newuser")
            "id1"
        ## input id is active user
        else {
            abbre_id_names<-history_table$app_id[history_table$author==input$userid]
            abbre_id[abbre_id_names]
        }
    )
    
    ##Display History of user
    output$image2<- renderUI({
        ## obtain the list of url
        imgurl <- icons[index2()]
        
        ## create image and caption(title) for each url
        imgfr <- lapply(imgurl, function(file){
            tags$div(id2=names(file),
                     tags$img(src=file, width=84, height=84),
                     tags$figcaption(titles[names(file)], style="display: table-caption; font-weight:bold;")
            )
        })
        
        ## arguments for the images
        imgfr$cellArgs <- list(
            style = "
        width: auto;
        height: auto;
        margin: 8px;
        ")
        
        do.call(flowLayout, imgfr)
    })
    
    
 
})
