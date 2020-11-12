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
dat<-read.csv("apps.csv")
icons<-unique(dat$icon)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    output$image1<- renderUI({
        
        imgurl <- icons[1:4750]
        
        imgfr <- lapply(imgurl, function(file){
            tags$div(id=file,
                tags$img(src=file, width=84, height=84)
               # tags$script(src="titlescript.js")
            )
            
        })
        
        imgfr$cellArgs <- list(
            style = "
        width: auto;
        height: auto;
        margin: 5px;
        ")
        
        do.call(flowLayout, imgfr)
    #    do.call(tagList, imgfr)
        #div(id = "myImage",
         #   tags$img(src = imgurl, width = 84, height = 84)
        #)
    })
    
    lapply(X = dat$icon[1:10], FUN = function(id) {
        onclick(id, {
            print(id)
        })
    })
    
    # onclick(
    #     "file", 
    #     { 
    #         # Do whatever you want!
    #         print("Hey there")
    #     }
    # )
   
})
