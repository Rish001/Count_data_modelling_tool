library(shiny)
library(DT)
library(tidyverse)
library(dplyr)
library(MASS)
library(shinythemes)




server <- function(input, output) {
  data <- reactive({
    req(input$filedata)
    read.csv(input$filedata$datapath)
    })
  
  output$checkbox <- renderUI({
    checkboxGroupInput(inputId = "select_var", 
                       label = "Select variables to display", 
                       choices = names(data()))
  })
  
  df_sel <- reactive({
    req(input$select_var)
    df_sel <- data()  %>% dplyr::select(input$select_var)
  })
  
  observe(print(df_sel()))
  output$table <- renderDT(df_sel())
  
  output$checkbox_for_predictors <- renderUI({
    selectInput(inputId = "select_var_p",
                       label = "Select predictors(choose only one)",
                       choices = names(df_sel()))
  })

  x <- reactive({
    req(input$select_var_p)
    x <- df_sel() %>% dplyr::select(input$select_var_p)
  })


  output$checkbox_for_response <-renderUI({
    selectInput(inputId = "select_var_r",
                       label = "Select response(choose only one)",
                       choices = names(df_sel()))
  })

  y <- reactive({
    req(input$select_var_r)
    y <- df_sel() %>% dplyr::select(input$select_var_r)
  })

  
  


    output$dqc <- renderUI({
    if(anyNA(x()) | nrow(x()) < 2 | anyNA(y()) | nrow(y()) < 2){
      "Invalid input or not enough observations"
    }else if (nrow(x()) != nrow(y())) {
      "Number of observations must be equal for x and y"
    }else {
      "good to go!"
    }
    })


    input_matrix <- reactive({
      r <- data.frame(y = y(), x = x())
    })
    
    tab <- reactive({input$CI})
    
    v <- reactive({
      
      
      t <- list()
      for (i in 1:as.numeric(tab())){
        
        
        t[[i]] <- sliderInput(paste0('cl_slider','_',i),label =  "Coverage percent (higher values first)", min = 0, max = 1, value =0.9)
      
      }
      return(t)
    })
    
    observe({print(input$cl_slider_1)})
    output$CI_sliders <- renderUI({v()})
    
    
    
    observe({print(input$model)})
    observeEvent(input$do,{



          cmod <- reactive({
            if (input$model == 'poisson'){
              modl <- glm(input_matrix()[,input$select_var_r]~poly(input_matrix()[,input$select_var_p],input$order),family = input$model,data = input_matrix())
            }
            else {
              modl <- glm.nb(input_matrix()[,input$select_var_r]~poly(input_matrix()[,input$select_var_p],input$order),data = input_matrix())
            }
                        
          })


          output$summary <- renderPrint({
            summary(cmod())
          })
          
          preds <- reactive({predict(cmod(),data = x(),type = "link", se.fit = TRUE)})
          
          
          dt <- reactive({
            r <- data.frame(row.names = 1:nrow(df_sel()))
            columns <- c()
            linked_fitted <- reactive({preds()$fit})
            fitted_values <- reactive({cmod()$family$linkinv(linked_fitted())})
            
            for (i in 1:tab()){
              critval <- qnorm(as.numeric(input[[paste0('cl_slider_',i)]]))
              upr <- reactive({preds()$fit + (critval * preds()$se.fit)})
              lwr <- reactive({preds()$fit - (critval * preds()$se.fit)})
              
              
              
              upr2 <- reactive({cmod()$family$linkinv(upr())})
              lwr2 <- reactive({cmod()$family$linkinv(lwr())})
              names <- c(paste0('upr',input[[paste0('cl_slider_',i)]]),paste0('lwr',input[[paste0('cl_slider_',i)]]))
              columns <- c(columns,names)
              
              r <- cbind(r,upr2(),lwr2())
            }
            colnames(r) <- columns
            r <- cbind(fitted = fitted_values(),r)
            return(r)
          })
          
          
          output$fitted <- renderDT(dt())
          
          

          
          
          output$plot <- renderPlot({
            b <- ggplot(data = df_sel())+
              geom_line(aes(x = df_sel()[,input$select_var_p],y = fitted(cmod())),color = 'red', size = 1)+
              geom_point(aes(x = df_sel()[,input$select_var_p],y = df_sel()[,input$select_var_r]))+
              xlab(input$select_var_p)+
              ylab(input$select_var_r)
            for (i in 1:tab()){
              b <- b + geom_line(aes_(x = df_sel()[,input$select_var_p],y = dt()[,paste0('upr',input[[paste0('cl_slider_',i)]])]),linetype = i)+
                geom_line(aes_(x = df_sel()[,input$select_var_p],y = dt()[,paste0('lwr',input[[paste0('cl_slider_',i)]])]),linetype = i)

            }
            print(b) 
          })
          
          point <- reactive({input$prediction})
          
          
          output$downloadData <- downloadHandler(
            filename = function() {
              paste("data-", Sys.Date(), ".csv", sep="")
            },
            content = function(file) {
              write.csv(dt(), file)
            }
          )
          
          

    })

  
}