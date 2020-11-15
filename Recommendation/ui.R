#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)

shinyUI(fluidPage(
    useShinyjs(),
    
    titlePanel("App recommendation"),
    # selectInput("search", "Select category", choices = c("all","Store design", "Customer support", "Sales and conversion optimization", "Marketing",                        
    #                                                      "Orders and shipping", "Inventory management", "Productivity", "Finding and adding products",      
    #                                                      "Places to sell", "Finances", "Reporting", "Trust and security"               
    #                                                      )),
    
    #selectInput("search", "Select category", choices =c("all",categories$app_category)),
    
    #define the dropdown list of user
    selectInput("userid", "Recommendation for ", choices =c("newuser1","newuser2")),
    #selectInput("userid", "Recommendation for ", choices =c("newuser", rownames(users_matrix))),
                
    uiOutput("image1", click = "myImage")
    
    # fluidRow(
    #   column(15,uiOutput("image1", click = "myImage"))
    # )
    
)
)
