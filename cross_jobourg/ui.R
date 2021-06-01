library(shiny)
library(xml2)
library(shinythemes)
library(stringr)
library(rvest)
library(DT)
library(shinybusy)


shinyUI(navbarPage("CROSS Jobourg",
                   tabPanel("",fluidPage(
                     tags$style("
              body {
    -moz-transform: scale(0.55, 0.55); /* Moz-browsers */
    zoom: 0.55; /* Other non-webkit browsers */
    zoom: 55%; /* Webkit browsers */
}
              "),
                            
                                            fluidRow(
                                              column(width = 2,wellPanel(
                                                actionButton("update","Actualiser"),
                                                p(""),
                                                radioButtons("SNS_259","SNS 259",choices = list("Au bassin"=1, "Dans l'avant-port"=2),inline = T),
                                                radioButtons("SNS_267","SNS 267",choices = list("Au bassin"=1, "Dans l'avant-port"=2),inline = T),
                                                div(style = "margin-top: -20px"),
                                                p("Si bassin = 3H avant / après PM"),
                                                radioButtons("SNS_210","SNS 210",choices = list("Au port"=1, "Au mouillage"=2),inline = T),
                                                radioButtons("garde_littoral","Garde littoral",choices = list("A Chausey"=1, "A Granville"=2),inline = T),
                                                p("M. CHEVALLIER : 06 33 04 31 47"),
                                                div(style = "margin-top: -10px"),
                                                p("M. GIRARD : 06 16 45 22 44")
                                                )),
                                              
                                              column(width = 10,wellPanel(
                                                div(style="display: inline-block;vertical-align:top",h2(textOutput("currentTime"), style="color:red")),
                                                div(style="display: inline-block;vertical-align:top",h2(textOutput("coeff"))),
                                                div(style="display: inline-block;vertical-align:top",h2(textOutput("temp"))),
                                                div(style="display: inline-block;vertical-align:top;width: 20%;",selectizeInput("vigipirate","Niveau Vigipirate",choices = list("-","Vigilance","Sécurité renforcée - Risque attentat","Urgence attentat"),selected = "Urgence attentat")),
                                                div(style="display: inline-block;vertical-align:top;width: 15%;",numericInput("isps","Niveau sûreté ISPS",value=1)),
                                                div(style="display: inline-block;vertical-align:top;font-weight: bold",DTOutput("portes")),
                                                div(style="display: inline-block;vertical-align:top;font-weight: bold",DTOutput("marees")),
                                                div(style="display: inline-block;vertical-align:top;font-weight: bold",DTOutput("bms"))
                                                ))),
                                              
                                            fluidRow(
                                              column(width = 3,wellPanel(
                                                h4(p("Données physiologiques")),
                                                radioButtons("sexe","Sexe",choices=list("M"=1,"F"=2),inline = T,selected = 1),
                                                div(style="display: inline-block;vertical-align:bottom;width: 45%;",numericInput("age","Age",30)),
                                                div(style="display: inline-block;vertical-align:bottom;width: 45%;",numericInput("taille","Taille cm",175)),
                                                div(style="display: inline-block;vertical-align:bottom;width: 45%;",numericInput("poids","Poids kg",77)),
                                                div(style="display: inline-block;vertical-align:bottom;width: 45%;",numericInput("graisse","Masse adipeuse %",25)),
                                                div(style="display: inline-block;vertical-align:top;font-weight: bold",DTOutput("survie1"))
                                                
                                                )),
                                              
                                              column(width = 3,wellPanel(
                                                div(style="display: inline-block;vertical-align:top;font-weight: bold",DTOutput("meteo")),
                                                div(style="display: inline-block;vertical-align:top;font-weight: bold",DTOutput("meteo1"))
                                                
                                                
                                              )),
                                              column(width = 4,wellPanel(
                                                div(style="display: inline-block;vertical-align:top;font-weight: bold",DTOutput("col1")),
                                                div(style="display: inline-block;vertical-align:top;font-weight: bold",DTOutput("moyens"))
                                                
                                                
                                              )),
                                              column(width = 2,wellPanel(
                                                fileInput('barfleur_upload','', 
                                                          accept = c(
                                                            'text/csv',
                                                            'text/comma-separated-values',
                                                            '.csv'
                                                          ),buttonLabel='Importer', placeholder='Barfleur'),
                                                div(style = "margin-top: -20px"),
                                                fileInput('deauville_upload','', 
                                                          accept = c(
                                                            'text/csv',
                                                            'text/comma-separated-values',
                                                            '.csv'
                                                          ),buttonLabel='Importer', placeholder='Deauville'),
                                                div(style = "margin-top: -20px"),
                                                fileInput('bms_upload','', 
                                                          accept = c(
                                                            'text/csv',
                                                            'text/comma-separated-values',
                                                            '.csv'
                                                          ),buttonLabel='Importer', placeholder='BMS'),
                                                div(style = "margin-top: -20px"),
                                                fileInput('meteo_upload','', 
                                                          accept = c(
                                                            'text/csv',
                                                            'text/comma-separated-values',
                                                            '.csv'
                                                          ),buttonLabel='Importer', placeholder='Météo'),
                                                div(style = "margin-top: -20px"),
                                                fileInput('col1_upload','', 
                                                          accept = c(
                                                            'text/csv',
                                                            'text/comma-separated-values',
                                                            '.csv'
                                                          ),buttonLabel='Importer', placeholder='Divers'),
                                                div(style = "margin-top: -20px"),
                                                fileInput('moyens_upload','', 
                                                          accept = c(
                                                            'text/csv',
                                                            'text/comma-separated-values',
                                                            '.csv'
                                                          ),buttonLabel='Importer', placeholder='Moyens')
                                                
                                                
                                              ))
                                            )
                                        )
                            )
                   )
        )