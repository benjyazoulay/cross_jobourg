library(shiny)
library(xml2)
library(shinythemes)
library(stringr)
library(rvest)
library(DT)

shinyUI(navbarPage("CROSS Jobourg",
                   tabPanel("",fluidPage(
                            
                                            fluidRow(
                                              column(width = 3,wellPanel(
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
                                              
                                              column(width = 9,wellPanel(
                                                div(style="display: inline-block;vertical-align:top",h2(textOutput("currentTime"), style="color:red")),
                                                div(style="display: inline-block;vertical-align:top",h2(textOutput("coeff"))),
                                                div(style="display: inline-block;vertical-align:top",h2(textOutput("temp"))),
                                                div(style="display: inline-block;vertical-align:top;width: 20%;",selectizeInput("vigipirate","Niveau Vigipirate",choices = list("-","Vigilance","Sécurité renforcée - Risque attentat","Urgence attentat"),selected = "Urgence attentat")),
                                                div(style="display: inline-block;vertical-align:top;width: 15%;",numericInput("isps","Niveau sûreté ISPS",value=1)),
                                                div(style="display: inline-block;vertical-align:top",DTOutput("portes"))
                                                ))),
                                              
                                            fluidRow(
                                              column(width = 4,wellPanel(
                                                h4(p("Données physiologiques")),
                                                radioButtons("sexe","Sexe",choices=list("M"=1,"F"=2),inline = T,selected = 1),
                                                div(style="display: inline-block;vertical-align:bottom;width: 45%;",numericInput("age","Age",30)),
                                                div(style="display: inline-block;vertical-align:bottom;width: 45%;",numericInput("taille","Taille cm",175)),
                                                div(style="display: inline-block;vertical-align:bottom;width: 45%;",numericInput("poids","Poids kg",77)),
                                                div(style="display: inline-block;vertical-align:bottom;width: 45%;",numericInput("graisse","Masse adipeuse %",25)),
                                                div(style="display: inline-block;vertical-align:top;font-size:80%",DTOutput("survie1")),
                                                p(""),
                                                div(style="display: inline-block;vertical-align:top",DTOutput("moyens"))
                                                )),
                                              
                                              column(width = 8,wellPanel(
                                                div(style="display: inline-block;vertical-align:top",DTOutput("meteo")),
                                                div(style="display: inline-block;vertical-align:top;font-size:80%",DTOutput("col1")),
                                                div(style="display: inline-block;vertical-align:top",DTOutput("meteo1"))
                                                
                                                
                                              ))
                                            ),
                                            fluidRow(
                                              column(width = 12,wellPanel(
                                                div(style="display: inline-block;vertical-align:top",DTOutput("bms")),
                                                div(style="display: inline-block;vertical-align:top",DTOutput("marees"))
                                              ))
                                              )
                                        )
                            )
                   )
        )