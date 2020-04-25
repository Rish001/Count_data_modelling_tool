library(shiny)
library(DT)
library(tidyverse)
library(dplyr)
library(MASS)
library(shinythemes)

ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("COUNT DATA MODELLING TOOL", windowTitle = "Count data"),
  sidebarLayout(
    sidebarPanel(fileInput(inputId = "filedata",
                           label = "Upload data. Choose csv file",
                           accept = c(".csv")),
                 uiOutput("checkbox"),
                 uiOutput("checkbox_for_predictors"),
                 uiOutput("checkbox_for_response"),
                 uiOutput("dqc"),
                 selectInput("model","choose a model",
                             choices = c('poisson', 'Negative Binomial')),
                 selectInput("CI","Choose number of CI bands",
                             choices = c('1','2','3')),
                 uiOutput("CI_sliders"),
                 sliderInput("order","Order of polynomial",1,8,5),
                 actionButton("do","Submit")
                 ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data",DTOutput(outputId = "table")),
        tabPanel("Plot",plotOutput("plot")),
        tabPanel("Results",downloadButton("downloadData", "Download"),
                 DTOutput('fitted')),
        tabPanel("summary",verbatimTextOutput(outputId = "summary"))
        # tabPanel("Prediction",
        #          textInput("prediction","put a non negative value"),
        #          verbatimTextOutput("predicted"))
      )
    )
  )
)