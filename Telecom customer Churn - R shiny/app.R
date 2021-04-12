library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyanimate)
library(shinyBS)
library(shinyjs)
library(plotly)
library(readxl)
library(shinyWidgets)
library(DT)
library(magrittr)
library(anomalize) #tidy anomaly detectiom
library(tidyverse) #tidyverse packages like dplyr, ggplot, tidyr
library(coindeskr) #bitcoin price extraction from coindesk
library(ggthemes) 
library(ggpubr)
library(ggplot2)
library(forecast)
library(tseries)
library(shinyBS)
library(shiny)
library(shinydashboard)
library(randomForest)
library(DT)
library(plyr)
library(randomForest)
library(DT)
set.seed(1000000)
load("rfm.rda")
library(shiny)
library(neuralnet)
library(nnet)
library(DT)



gm= tags$h3(strong("Good Morning",style="color:#446e9b"))
ga= tags$h3(strong("Good Afternoon",style="color:#446e9b"))
ge= tags$h3(strong("Good Evening",style="color:#446e9b"))

#===========
## FUNCTIONS
#===========
## SIMPLE GREETING
good_time <- function(){
    
    ## LOAD PACKAGE
    require(lubridate, quietly = T)
    
    ## ISOLATE currHour
    currhour = hour(now())
    
    
    ## RUN LOGIC
    if(currhour < 12){
        return(gm)
    } 
    else if(currhour >= 12 & currhour < 17){
        return(ga)
    }
    else if( currhour >= 17){
        return(ge)  
    }
}



## STARTING LOGGED VALUE; LET'S CHANGE THAT!
Logged = FALSE;

#tags$div(img(src="wel.png",height='50%',width='50%'),align="center"),
#====
# UI
#====
## make login screen
ui1 <- function(){
    
    tagList(tags$style(HTML('body {font-family:"Verdana",Georgia,Serif; background-color:#446e9b}')),
        div(id="container",align="center",
            div(id = "login",
                # make login panel
                wellPanel(id="well",style = "overflow-y: ;width:100%;height:100%",
                          
                          HTML(paste0('
                                <h2>
                                Hello, ', 
                                      good_time(),
                                      '</h2>',
                                      '<h3>
                                <br>You are in Admin page.</br>
                                </h3>')
                          ),
                          br(),
                          br(),
                          tags$div(textInput("userName", "Username",width = "100%"),align="left"),
                          br(),
                          tags$div(passwordInput("passwd", "Password",width = "100%"),align="left"),
                          br(),
                          # button
                          tags$div(actionButton("Login", "Log in"),align="center"),
                          # login error message
                          tags$div(uiOutput("message"),align="center")
                )
                
            )
        ),
        # css for container
        tags$style(type = "text/css", 
                   "#container{
                   display: flex;
                   justify-content: center;
                   margin-top: 150px;
                   }"),
        # css for login well panel
        tags$style(type="text/css", "
                   #login,{
                   font-size:14px; 
                   width: 360px;}"),
        # well panel
        tags$style(type="text/css",
                   "#well{
                    padding: 50px;
                    background: white;
                   border: 1px;
                   box-shadow: ;}"),
        # welcome text css
        tags$style(type = 'text/css',
                   "h2, h3{
                   color: #525252;}"),
        # input fields
        tags$style(type="text/css",
                   "#userName, #passwd{
                        box-shadow: none;
                        outline:none;
                        border: none;
                        padding-left: 0;
                        border-bottom: 2px solid #446e9b;
                        border-radius: 0;
                   }
                   #userName:focus, #passwd:focus{
                   box-shadow: 0px 10px 10px -5px lightgray;
                   }"),
        # button css
        tags$style(type='text/css',
                   "#Login{
                    outline: none;
                   margin-left: 0px;
                   width: 100px;
                   font-size: 12pt;
                   background: transparent;
                   border: 2px solid #446e9b;
                   color: #446e9b;
                   border-radius: 10px;
                   transition: 0.8s ease-in-out;
                   }
                   #Login:hover{
                   background: #446e9b;
                   color: white;}"),
        # error box - fadeOut animation
        tags$style(type="text/css",
                   "@-webkit-keyframes fadeOut {
                        from {
                            opacity: 1;
                        }
                        to {
                            opacity: 0;
                        }
                    }
                    @keyframes fadeOut {
                        from {
                            opacity: 1;
                        }
                        to {
                            opacity: 0;
                        }
                    }"),
        tags$style(type="text/css",
                   "#error-box{
                   margin-top: 20px;
                   margin-left: 0px;
                   padding: 5px 10px 5px 10px;
                   text-align: center;
                   width: 325px;
                   color: white;
                   background: #ef3b2c;
                   border: 1px solid #ef3b2c;
                   border-radius: 5px;
                   -webkit-animation: fadeOut;
                   animation: fadeOut;
                   opacity: 0;
                   animation-duration: 15s;}")
    )
}

#=========
# PRINT UI
#=========
ui = (uiOutput("page"))

#========
# SERVER
#========

server = shinyServer(function(input, output,session){
  
  options(shiny.maxRequestSize=30*1024^2) 
  
    
   users <- data.frame(User="churn",Password="customer")
    ## BEGIN BUILD LOG IN SCREEN
    USER <- reactiveValues(Logged = Logged)
    
    ## ERROR CHECKING
    observeEvent(input$Login,{
        
        ## output error message
        output$message <- renderUI({
            if(!is.null(input$Login)){
                my_username <- length(users$User[grep(pattern = input$userName, x = users$User)])
                my_password <- length(users$User[grep(pattern = input$passwd, x = users$Password)])
                if(input$Login > 0){
                    if(my_username < 1 ||  my_password < 1){
                        HTML("<div id='error-box'>
                             Sorry, that's not the right username or password. Please, 
                             try again. If you continue to have problems,
                             <a href='https://github.com/sailogeshh'>
                             <u>Contact Us..</u></a>
                             </div>")
                    }
                }
            }
        })
        
        ## CHECK INPUT
        if (USER$Logged == FALSE) {
            if (!is.null(input$Login)) {
                if (input$Login > 0) {
                    Username <- isolate(input$userName)
                    Password <- isolate(input$passwd)
                    Id.username <- which(users$User == Username)
                    Id.password <- which(users$Password == Password)
                    if (length(Id.username) > 0 & length(Id.password) > 0) {
                        if (Id.username %in% Id.password) {
                            USER$Logged <- TRUE
                        }
                    }
                }
            }
        }
    })
 
    ## Make UI
    observe({
        # What to do when logged = F
        if (USER$Logged == FALSE) {
            output$page <- renderUI({
                div(class="outer",do.call(bootstrapPage,c("",ui1())))
            })
        }
        
        # Render UI when logged = T
        if (USER$Logged == TRUE) 
        {
            ## get the current user's authorization level 
            user_log <- toupper(input$userName)
            
            # if admin ("input.SELECT == 1 || input.FED == 2" )
            if(user_log == "CHURN" ){
                output$page <- renderUI({
           ###################################################### ADMIN UI PAGE ###################################################################################################################
                  fluidPage(
                    tags$style(HTML('body {font-family:"Verdana",Georgia,Serif; background-color:white}')),
                    theme = shinytheme("united"),
                    withAnim(),
                    #setBackgroundImage(src = "w.jpg"),
                    tags$head(
                      tags$style(type = 'text/css', 
                                 HTML('
                                      .navbar-default .navbar-brand{color: ;}
                                      .tab-panel{ background-color: #; color: #}
                                      .navbar-default .navbar-nav > .active > a, 
                                      .navbar-default .navbar-nav > .active > a:focus, 
                                      .navbar-default .navbar-nav > .active > a:hover {
                                      color: #e6e6e6;
                                      background-color: #;
                                      
                                      }')
                                                )
                                 ),
                    
                    tags$style(HTML(".navbar  {
                                    background-color:#105da2; }
                                    
                                    .navbar .navbar-nav {float: right; margin-right: 35px;
                                    margin-top: 26px;
                                    color: #; 
                                    font-size: 18px; 
                                    background-color: #; }
                                    
                                    .navbar.navbar-default.navbar-static-top{ 
                                    color: #; 
                                    font-size: 23px; 
                                    background-color: # ;}
                                    
                                    .navbar .navbar-header {
                                    float: left;
                                    background-color: # ;}
                                    
                                    .navbar-default .navbar-brand { color: #e6e6e6; 
                                    margin-top: 10px;
                                    font-size: 24px; 
                                    background-color: # ;} 
                                    
                                    ")),
                    tags$style(type="text/css",
                               "#well0{
                               padding: 100px;
                               background: white;
                               border: 1px;
                               box-shadow:2px 2px;}"),
                    tags$style(type="text/css",
                               "#well2{
                               padding: 100px;
                               background: #;
                               border: 1px;
                               box-shadow:2px 2px;}"),
                    tags$style(type="text/css",
                               "#well8{
                               padding: 100px;
                               background: #;
                               border: 1px;
                               box-shadow: 2px 2px;}"),
                    tags$style(type="text/css",
                               "#rrr{
                               padding: 100px;
                               background: #;
                               border: 0px;
                               box-shadow: 0px 0px;}"),
                    tags$head(
                      tags$style(HTML("
                                      input[type=\"number\"] {
                                      font-size: 20px;height:50px;
                                      }
                                      
                                      "))
                      ),
                    #tags$style(type='text/css','#qq {background-color: #d2d3d4; color: #004264;font-size:120%}'),
                    #tags$style(type='text/css','#qqq {background-color: #d2d3d4; color: #004264;font-size:120%}'),
                    #tags$style(type='text/css','#qqqq {background-color: #d2d3d4; color: #004264;font-size:120%}'),
                    #tags$style(type='text/css','#qqqqq {background-color: #d2d3d4; color: #004264;font-size:120%}'),
                    
                    tags$head(HTML("<title>Predictive Analytics</title> <link rel='icon' type='image/gif/png' href='t.png'>")),
                    navbarPage(id="tabset",tags$li(class = "dropdown",
                                                   tags$style(".navbar {min-height:100px }")
                    ),
                    #title = ,position = "fixed-top",selected = "Upload",inverse = TRUE,
                    title = tags$div(img(src="log.png","Customer churn - Telecom", style="margin-top: -4px;margin-left: 30px;", height = 60)),position = "fixed-top",selected = "Upload",inverse = F,
                    tabPanel(title = "Upload",icon = icon("upload"),
                             
                             fluidPage(
                               
                               tags$style(" #modal1 .modal-header {background-color:#; border-top-left-radius: 0px; border-top-right-radius: 0px}"),
                               
                               tags$style(type="text/css",
                                          ".shiny-output-error { visibility: hidden; }",
                                          ".shiny-output-error:before { visibility: hidden; }"
                               ),
                               tags$head(tags$style("#pppp{color:black; font-size:35px; font-style:italic; text-align=center;
                                                    overflow-y:scroll; max-height: 300px; background: ghostwhite;}")),
                               tags$head(tags$style("#roi{color:black; font-size:35px; font-style:italic; text-align=center;
                                                    overflow-y:scroll; max-height: 300px; background: ghostwhite;}")),
                               
                               
                               br(),
                               br(),
                               
                               
                               column(7,
                                      
                                      # tags$h3(strong(em("Aim of this Analysi(s):")),style="text-align:center;color:#004264;font-size:180%"),br(),
                                      # tags$div(h4("The identification of rare items, events or observations which raise suspicions",style="text-align:center;color:dimgrey"),align="center"),
                                      # tags$div(h4("by differing significantly from the majority of the data.",style="text-align:center;color:dimgrey"),align="center"),
                                      br(),br(),br(),br(),br(),
                                      tags$div(id = 'logo1',img(src="eee.png",height='70%',width='70%'),align="center")
                               ),
                               
                               br(),
                               br(),
                               
                               column(5,
                                      
                                      
                                      bootstrapPage(useShinyjs(),
                                                    br(),
                                                    
                                                    tags$h3(strong(em("Predictive Analytics (Customer Churn)")),style="text-align:center;color:#034e91;font-size:190%"),
                                                   
                                                    tags$div(id = 'logo2',img(src="ee.png",height='50%',width='50%'),align="center"),
                                                    
                                                    withAnim(),
                                                    
                                                    uiOutput('fileupload'), 
                                                    uiOutput('checkbox'),
                                                    uiOutput("button"),
                                                    uiOutput("helptext"),
                                                    br(),
                                                    
                                                    bsPopover(id="check",title = "",content = "Note: I accept the Terms & Conditions.. Show the Analyse button",placement = "right"),
                                                    tags$div(bsButton("reset", label = "Reset ?", icon =   icon("repeat",lib = "glyphicon"),block = F, style="danger",size = "small"),align="center"),
                                                    
                                                    
                                                    #tags$h1(actionButton("myuser","Logout",icon=icon("user")),style="text-align:center"),
                                                   
                                                    
                                                    tags$div(class = "header", checked = NA,style="text-align:center;color:#929292;font-size:100%",
                                                             tags$tbody("Need Help ?"),
                                                             tags$a(href = "https://github.com/sailogeshh", "Contact Us...")
                                                    )
                                      )
                               )
                               
                               
                               
                               )),
                    
                    
                    
                    
                    tabPanel(title = strong("|")),         
                    
                    
                    navbarMenu("More",icon = icon("plus-square"),
                               tabPanel(
                                 tags$div(tags$a(href="javascript:history.go(0)",bsButton("logoutadmin", label = "Logout", icon =   icon("repeat",lib = "glyphicon"),block = F, style="success"),style="text-align:center"),align="center"),
                                 br()
                               )
                    ))
                    )
                  
                  
                    #########################################################################################################################################################################
                      
                       
                
                })
            }
            
            # if standard user
            else{
                output$page <- renderUI({
                    
                   
                })
            }
        }
    })
    
    ####################################################### server #############################################################################################
 
    
    
    out<-reactive({
      file1 <- input$file
      if(is.null(file1)) {return(NULL)}
      data <- read.csv(file1$datapath,header=TRUE)
      withProgress(message='Loading table',value=30,{
        n<-10
        
        for(i in 1:n){
          incProgress(1/n,detail=paste("Doing Part", i, "out of", n))
          Sys.sleep(0.1)
        }
      })
      #data = read.csv("Telco-Customer-Churn- original data - Copy.csv",header = T)
      data$Gender_1<- ifelse(data$gender=="Male",1,0)
      data$partner_1<- ifelse(data$Partner=="Yes",1,0)
      data$Dependents_1 <- ifelse (data$Dependents=="Yes",1,0)
      data$PhoneService_1 <- ifelse(data$PhoneService=="Yes",1,0)
      data$MultipleLines_1 <- revalue(data$MultipleLines, c("Yes"=1, "No"=0, "No phone service"=2))
      data$InternetService_1 <- revalue(data$InternetService, c("DSL"=1, "Fiber optic"=2, "No"=0))
      data$OnlineSecurity_1 <- revalue(data$OnlineSecurity, c("Yes"=1, "No"=0, "No internet service"=2))
      data$OnlineBackup_1 <- revalue(data$OnlineBackup, c("Yes"=1, "No"=0, "No internet service"=2))
      data$DeviceProtection_1 <- revalue(data$DeviceProtection, c("Yes"=1, "No"=0, "No internet service"=2))   
      data$TechSupport_1 <- revalue(data$TechSupport, c("Yes"=1, "No"=0, "No internet service"=2))
      data$StreamingTV_1 <- revalue(data$StreamingTV, c("Yes"=1, "No"=0, "No internet service"=2))
      data$StreamingMovies_1 <- revalue(data$StreamingMovies, c("Yes"=1, "No"=0, "No internet service"=2))
      data$PaperlessBilling_1 <- revalue(data$PaperlessBilling, c("Yes"=1, "No"=0))
      data$Churn_1 <- revalue(data$Churn, c("Yes"=1, "No"=0))
      final_data <- data[-c(1,2,4,5,7,8,9,10,11,12,13,14,15,17,21)]
      
      set.seed(1000000)
      pred<- data.frame(predict(rf,data=final_data ,type = "prob"))
      Prediction <- ifelse(pred[,2] < 0.26,"Not_Churn","Churn")
      probability <- round(pred[,2],3)
      final_tab<- data.frame(data[1],Prediction)
      final_tab
      
    })
    

    
    observeEvent(input$reset,{
      reset(id = "file")
    })
    
    output[["fileupload"]] <- renderUI({
      input$reset
      tags$div(fileInput("file",label = tags$h4(strong(em("Upload data..")),style="color:#034e91;font-size:160%"),accept=c('csv','comma-seperated-values','.csv')),align="center")
      
    })
    
    output[["checkbox"]] <- renderUI({
      input$reset
      tags$div(checkboxInput("check",tags$a(href = "https://github.com/sailogeshh", "Terms & Conditions",style="color:green;"),value = TRUE),align="center")
      
    })
    
    output[["button"]] <- renderUI({
      if(input$check==TRUE){
        tags$div(bsButton("analyse",strong("Lets Go..!"),icon = icon("refresh"),style = "primary",size="medium"),
                 style="color:white;font-weight:100%;",align="center")
      }
    })
    
    
    output[["helptext"]] <- renderUI({
      if(input$check==TRUE){
        tags$div(helpText("To get results, click the 'Lets go!' button...",style="text-align:center"),align="center")
      }
    })
    
    
    observe(addHoverAnim(session, 'logo1', 'pulse'))
    observe(addHoverAnim(session, 'logo2', 'pulse'))
    observe(addHoverAnim(session, 'analyse', 'shake'))
    observe(addHoverAnim(session, 'reset', 'shake'))
    
    
    observeEvent(input$analyse, {
      confirmSweetAlert(
        session = session,
        inputId = "confirmation",
        type = "warning",
        title = "Are you sure the data was uploaded ?",
        btn_labels = c("Nope", "Yep"),
        danger_mode = TRUE
      )
    })
    
    observeEvent(input$confirmation, {
      if(input$confirmation==TRUE){
        showModal(tags$div(id="modal1", modalDialog(
          inputId = 'Dialog1', 
          title = HTML('<span style="color:#034e91; font-size: 20px; font-weight:bold; font-family:sans-serif ">Output<span>
                       <button type = "button" class="close" data-dismiss="modal" ">
                       <span style="color:white; ">x <span>
                       </button> '),
          footer = modalButton("Close"),
          size = "l",
          dataTableOutput("outdata"),
          uiOutput("down"),
          easyClose = F
          )))
      }
    })
    
    output[["down"]]<-renderUI({
      tags$div(downloadButton("downloadData","Download..!"),align="center")
    })
    
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("Customer Churn Prediction", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(out(), file, row.names = FALSE)
      }
    )
    
    observeEvent(input$confirmation, {
      if(input$confirmation==TRUE){
        output[["outdata"]]<- renderDataTable({
          datatable(out(),extensions = c('Scroller'),
                    options = list(
                      dom = 'Bfrtip',
                      deferRender = TRUE,
                      scrollY = 500,
                      scroller = TRUE
                    ),filter = "top")
        })
      }
    })
    
    
    
    
    #########################################################################################################################################################################
    
    
    
    
}) # END SHINYAPP

#======
# RUN
#======
shinyApp(ui = ui, server = server)
