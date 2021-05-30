library(shiny)
library(xml2)
library(shinythemes)
library(stringr)
library(rvest)
library(DT)
library(lubridate)
library(httr)
library(dplyr)



shinyServer(function(input, output,session){
  delta_time<-as.integer(as.POSIXct("10:00",tz="Europe/Paris",tryFormats="%H:%M")-as.POSIXct("10:00",tz="UTC",tryFormats="%H:%M"))
  if (delta_time==-1){delta_time=hm("01:00")}
  if (delta_time==-2){delta_time=hm("02:00")}
  
  output$currentTime <- renderText({
    invalidateLater(1000, session)
    paste(Sys.time())
  })
  
  portification<-function(page){
    df <- data.frame(DATE=character(),
                     HEURE=character(), 
                     STATUT=character(), 
                     stringsAsFactors=FALSE) 
    for (i in 1:length(page$DATE)) {
      a<-page$OUVERTURE_1[i]
      b<-page$FERMETURE_1[i]
      if(length(page)==5){
        c<-page$OUVERTURE_2[i]
        d<-page$FERMETURE_2[i]
      }
      
      e<-as.data.frame(cbind(NA,a,"OUVERTURE"))
      f<-as.data.frame(cbind(NA,b,"FERMETURE"))
      if(length(page)==5){
        g<-as.data.frame(cbind(NA,c,"OUVERTURE"))
        h<-as.data.frame(cbind(NA,d,"FERMETURE"))
      }
      
      colnames(e)<-c("DATE","HEURE","STATUT")
      colnames(f)<-c("DATE","HEURE","STATUT")
      if(length(page)==5){
        colnames(g)<-c("DATE","HEURE","STATUT")
        colnames(h)<-c("DATE","HEURE","STATUT")
      }
      
      df<-bind_rows(df,e)
      df<-bind_rows(df,f)
      if(length(page)==5){
        df<-bind_rows(df,g)
        df<-bind_rows(df,h)
      }
    }
    
    df<-df[df$HEURE!="",]
    rownames(df)<-NULL
    for (i in 1:length(df$HEURE)) {
      if(str_length(df$HEURE[i])==4){df$HEURE[i]<-str_c("0",df$HEURE[i])}
    }
    
    a<-as.Date.character(page$DATE[1])
    df$DATE[1]<-as.character(a)
    for (i in 2:length(df$HEURE)) {
      if(df$HEURE[i-1]>df$HEURE[i]){
        a<-a+1
      }
      df$DATE[i]<-as.character(a)
      
    }
    
    df$HEURE<-str_replace(df$HEURE,"h",":")
    
    if(str_length(df$HEURE[1])==5){
      df$HEURE<-str_c(df$DATE," ",df$HEURE,":00")
    }
    if(str_length(df$HEURE[1])==8){
      df$HEURE<-str_c(df$DATE," ",df$HEURE)
    }
    
    a<-as.POSIXct(ymd_hms(df$HEURE,tz="Europe/Paris"))
    attr(a, "tzone") <- "UTC" 
    df$HEURE<-a
    df$DATE<-ymd(str_remove(df$HEURE," .+"))
    
    return(df)
  }
  
  
  
  
  tablification<-function(df){
    heure<-Sys.time()
    #attr(heure, "tzone") <- "UTC"
    
    df1<-df[df$HEURE<heure,]
    a<-df1[length(df1$DATE),]
    df1<-df[df$HEURE>heure,]
    df1<-df1[is.na(df1$HEURE)==FALSE,]
    b<-df1[1,]
    numb_row<-as.integer(rownames(b[1,]))
    c<-df[numb_row+1,]
    d<-df[numb_row+2,]
    e<-bind_rows(a,b,c,d)
    if(e$STATUT[1]=="FERMETURE"){
      e<-e[-1,]
      e<-bind_rows(e,df[numb_row+3,])
    }
    return(e)
  }
  
  hybride<-function(df){
    df<-df[is.na(df$HEURE)==FALSE,]
    df$HEURE<-str_c(as.character(df$DATE)," ",as.character(df$HEURE))
    a<-as.POSIXct(ymd_hms(df$HEURE,tz="Europe/Paris"))
    attr(a, "tzone") <- "UTC" 
    df$HEURE<-a

    df$DATE<-ymd(str_remove(df$HEURE," .+"))
    heure<-Sys.time()
    #attr(heure, "tzone") <- "UTC"
    
    df1<-df[df$HEURE<heure,]
    a<-df1[length(df1$DATE),]
    df1<-df[df$HEURE>heure,]
    df1<-df1[is.na(df1$HEURE)==FALSE,]
    b<-df1[1,]
    numb_row<-as.integer(rownames(b[1,]))
    c<-df[numb_row+1,]
    d<-df[numb_row+2,]
    e<-bind_rows(a,b,c,d)
    e<-e[is.na(e$HEURE)==FALSE,]
    rownames(e)<-NULL
    if(e$STATUT[1]=="FERMETURE"){
      e<-e[-1,]
      e<-bind_rows(e,df[numb_row+3,])
    }
    return(e)
  }
  
  
  observeEvent(input$sexe,{
    if(input$sexe==1){
      updateNumericInput(session,"age",value=30)
      updateNumericInput(session,"taille",value=175)
      updateNumericInput(session,"poids",value=77)
      updateNumericInput(session,"graisse",value=25)
    }
    else if (input$sexe==2){
      updateNumericInput(session,"age",value=30)
      updateNumericInput(session,"taille",value=163)
      updateNumericInput(session,"poids",value=63)
      updateNumericInput(session,"graisse",value=31)
    }
    
  })
  
  meteo<-read.csv("meteo.csv",encoding = "UTF-8")
  meteo1<-read.csv("meteo1.csv",header=T,encoding="UTF-8")
  gates<-read.csv("portes.csv",encoding="UTF-8")
  col1<-read.csv("col1.csv",header=T,encoding="UTF-8")
  survie1<-read.csv("survie1.csv",encoding = "UTF-8")
  survie_tab<-read.csv("survie.csv",encoding = "UTF-8")
  temp_eau<<-11
  bms<-read.csv("BMS.csv",encoding = "UTF-8")
  moyens<-read.csv("moyens.csv",encoding = "UTF-8")
  
  
  
  
  observeEvent(input$update,{
    
    if (!is.null(input$bms_upload)){
      inFile<-input$bms_upload
      bms<- read.csv(inFile$datapath , encoding = "UTF-8")
    }
    if (!is.null(input$meteo_upload)){
      inFile<-input$meteo_upload
      meteo<- read.csv(inFile$datapath , encoding = "UTF-8")
    }
    if (!is.null(input$col1_upload)){
      inFile<-input$col1_upload
      col1<- read.csv(inFile$datapath , encoding = "UTF-8")
    }
    if (!is.null(input$moyens_upload)){
      inFile<-input$moyens_upload
      moyens<- read.csv(inFile$datapath , encoding = "UTF-8")
    }
    
    gates$DEBUT_1<-as.POSIXct(NA)
    gates$FIN_1<-as.POSIXct(NA)
    gates$DEBUT_2<-as.POSIXct(NA)
    gates$FIN_2<-as.POSIXct(NA)
    
    page<-read.csv("granville.csv",encoding = "UTF-8")
    df<-portification(page)
    df<-tablification(df)
    gates$DEBUT_1[1]<-df$HEURE[1]
    gates$FIN_1[1]<-df$HEURE[2]
    gates$DEBUT_2[1]<-df$HEURE[3]
    gates$FIN_2[1]<-df$HEURE[4]
    
    
    page<-read.csv("carteret.csv",encoding = "UTF-8")
    df<-portification(page)
    df<-tablification(df)
    gates$DEBUT_1[2]<-df$HEURE[1]
    gates$FIN_1[2]<-df$HEURE[2]
    gates$DEBUT_2[2]<-df$HEURE[3]
    gates$FIN_2[2]<-df$HEURE[4]
    
    page<-read.csv("saint-vaast.csv",encoding = "UTF-8")
    df<-portification(page)
    df<-tablification(df)
    gates$DEBUT_1[4]<-df$HEURE[1]
    gates$FIN_1[4]<-df$HEURE[2]
    gates$DEBUT_2[4]<-df$HEURE[3]
    gates$FIN_2[4]<-df$HEURE[4]
    
    page<-read.csv("courseulles.csv",encoding = "UTF-8")
    if(input$SNS_259==1){
      page<-page[,-6]
      page<-page[,-6]
      page<-page[,-6]
      page<-page[,-6]
      df<-portification(page)
      df<-tablification(df)
      gates$DEBUT_1[7]<-df$HEURE[1]
      gates$FIN_1[7]<-df$HEURE[2]
      gates$DEBUT_2[7]<-df$HEURE[3]
      gates$FIN_2[7]<-df$HEURE[4]
    }
    if(input$SNS_259==2){
      page<-page[,-2]
      page<-page[,-2]
      page<-page[,-2]
      page<-page[,-2]
      colnames(page)<-c("DATE","OUVERTURE_1","FERMETURE_1","OUVERTURE_2","FERMETURE_2")
      df<-portification(page)
      df<-tablification(df)
      gates$DEBUT_1[7]<-df$HEURE[1]
      gates$FIN_1[7]<-df$HEURE[2]
      gates$DEBUT_2[7]<-df$HEURE[3]
      gates$FIN_2[7]<-df$HEURE[4]
    }
    
    
    
    
    
    
    page<-read.csv("dives.csv",encoding = "UTF-8")
    page$OUVERTURE_1<-str_replace_all(page$OUVERTURE_1,"[*]","")
    page$FERMETURE_1<-str_replace_all(page$FERMETURE_1,"[*]","")
    page$OUVERTURE_2<-str_replace_all(page$OUVERTURE_2,"[*]","")
    page$FERMETURE_2<-str_replace_all(page$FERMETURE_2,"[*]","")
    df<-portification(page)
    df<-tablification(df)
    gates$DEBUT_1[8]<-df$HEURE[1]
    gates$FIN_1[8]<-df$HEURE[2]
    gates$DEBUT_2[8]<-df$HEURE[3]
    gates$FIN_2[8]<-df$HEURE[4]
    
    
    
    
    
    
    
    page<-read.csv("grandcamp.csv",encoding = "UTF-8")
    df<-portification(page)
    df<-tablification(df)
    gates$DEBUT_1[5]<-df$HEURE[1]
    gates$FIN_1[5]<-df$HEURE[2]
    gates$DEBUT_2[5]<-df$HEURE[3]
    gates$FIN_2[5]<-df$HEURE[4]
    
    page<-read.csv("port_en_bessin.csv",encoding = "UTF-8")
    df<-portification(page)
    df<-tablification(df)
    gates$DEBUT_1[6]<-df$HEURE[1]
    gates$FIN_1[6]<-df$HEURE[2]
    gates$DEBUT_2[6]<-df$HEURE[3]
    gates$FIN_2[6]<-df$HEURE[4]
    
    
    ###à revoir
    if(input$SNS_267==2){
      url="https://marine.meteoconsult.fr/meteo-marine/bulletin-detaille/port-264/previsions-meteo-port-en-bessin-aujourdhui"
      page=read_html(url)
      a<-html_text(html_node(page,"div.tide-line:nth-child(1)"))
      a<-str_remove_all(a,"[:space:]")
      a<-str_extract(a,"Maréehaute.....")
      a<-str_remove(a,"Maréehaute")
      a<-str_replace(a,"h",":")
      date<-Sys.Date()+delta_time
      date<-str_remove(date," .+")
      a<-str_c(date," ",a,":00")
      a<-as.POSIXct(ymd_hms(a,tz="Europe/Paris"))
      attr(a, "tzone") <- "UTC" 
      gates$DEBUT_1[6]<-a
      gates$FIN_1[6]<-a
      gates$DEBUT_1[6]<-gates$DEBUT_1[6]-hm("03:00")
      gates$FIN_1[6]<-gates$FIN_1[6]+hm("03:00")
      
      a<-html_text(html_node(page,"div.tide-line:nth-child(3)"))
      a<-str_remove_all(a,"[:space:]")
      a<-str_extract(a,"Maréehaute.....")
      a<-str_remove(a,"Maréehaute")
      a<-str_replace(a,"h",":")
      a<-str_c(date," ",a,":00")
      a<-as.POSIXct(ymd_hms(a,tz="Europe/Paris"))
      attr(a, "tzone") <- "UTC"
      gates$DEBUT_2[6]<-a
      gates$FIN_2[6]<-a
      gates$DEBUT_2[6]<-gates$DEBUT_2[6]-hm("03:00")
      gates$FIN_2[6]<-gates$FIN_2[6]+hm("03:00")
      
      url="https://marine.meteoconsult.fr/meteo-marine/bulletin-detaille/port-264/previsions-meteo-port-en-bessin-demain"
      page=read_html(url)
      a<-html_text(html_node(page,"div.tide-line:nth-child(1)"))
      a<-str_remove_all(a,"[:space:]")
      a<-str_extract(a,"Maréehaute.....")
      a<-str_remove(a,"Maréehaute")
      a<-str_replace(a,"h",":")
      date<-Sys.Date()+1
      date<-date+delta_time
      date<-str_remove(date," .+")
      a<-str_c(date," ",a,":00")
      a<-as.POSIXct(ymd_hms(a,tz="Europe/Paris"))
      attr(a, "tzone") <- "UTC" 
      
      if(Sys.time()>gates$FIN_1[6]){
        gates$DEBUT_1[6]<-gates$DEBUT_2[6]
        gates$FIN_1[6]<-gates$FIN_2[6]
        gates$DEBUT_2[6]<-a
        gates$FIN_2[6]<-a
        gates$DEBUT_2[6]<-gates$DEBUT_2[6]-hm("03:00")
        gates$FIN_2[6]<-gates$FIN_2[6]+hm("03:00")
      }
    }
    
    page<-read.csv("barfleur.csv",encoding = "UTF-8")
    if (!is.null(input$barfleur_upload)){
      inFile<-input$barfleur_upload
      page<- read.csv(inFile$datapath , encoding = "UTF-8")
    }
    df<-hybride(page)
    gates$DEBUT_1[3]<-df$HEURE[1]
    gates$FIN_1[3]<-df$HEURE[2]
    gates$DEBUT_2[3]<-df$HEURE[3]
    gates$FIN_2[3]<-df$HEURE[4]
    
    page<-read.csv("deauville.csv",encoding = "UTF-8")
    if (!is.null(input$deauville_upload)){
      inFile<-input$deauville_upload
      page<- read.csv(inFile$datapath , encoding = "UTF-8")
    }
    df<-hybride(page)
    gates$DEBUT_1[9]<-df$HEURE[1]
    gates$FIN_1[9]<-df$HEURE[2]
    gates$DEBUT_2[9]<-df$HEURE[3]
    gates$FIN_2[9]<-df$HEURE[4]
    
    gates$DEBUT_1<-as.character(gates$DEBUT_1)
    gates$FIN_1<-as.character(gates$FIN_1)
    gates$DEBUT_2<-as.character(gates$DEBUT_2)
    gates$FIN_2<-as.character(gates$FIN_2)

    
    
    heure<-Sys.time()
    #attr(heure, "tzone") <- "UTC"

    for (i in 1:length(gates$PORT)) {
      if(is.na(gates$DEBUT_1[i])){}
      else if (heure<=ymd_hms(gates$FIN_1[i]) & heure>=ymd_hms(gates$DEBUT_1[i])){gates$STATUT[i]<-"OUVERT"}
      else{gates$STATUT[i]<-"FERMÉ"}
      
      if(is.na(gates$DEBUT_2[i])){}
      else if(heure<=ymd_hms(gates$FIN_2[i]) & heure>=ymd_hms(gates$DEBUT_2[i])){gates$STATUT[i]<-"OUVERT"}
    }
    
    if(input$SNS_210==2){gates$STATUT[4]<-"OUVERT"}
    
    gates$DEBUT_1<-as.character(gates$DEBUT_1)
    gates$DEBUT_1<-str_remove(gates$DEBUT_1,as.character(Sys.Date()))
    gates$DEBUT_1<-str_remove(gates$DEBUT_1,"(:00)$")

    
    gates$DEBUT_2<-as.character(gates$DEBUT_2)
    gates$DEBUT_2<-str_remove(gates$DEBUT_2,as.character(Sys.Date()))
    gates$DEBUT_2<-str_remove(gates$DEBUT_2,"(:00)$")
    

    gates$FIN_1<-as.character(gates$FIN_1)
    gates$FIN_1<-str_remove(gates$FIN_1,as.character(Sys.Date()))
    gates$FIN_1<-str_remove(gates$FIN_1,"(:00)$")


    gates$FIN_2<-as.character(gates$FIN_2)
    gates$FIN_2<-str_remove(gates$FIN_2,as.character(Sys.Date()))
    gates$FIN_2<-str_remove(gates$FIN_2,"(:00)$")
    


    
    gates<-datatable(gates, selection = 'none', editable = T, options = list(dom = 't'), rownames= FALSE) %>% formatStyle(
      'STATUT',
      target = 'row',
      backgroundColor = styleEqual(c("OUVERT", "FERMÉ"), c('green', 'red'))
    )
    
    output$portes<-renderDT(gates)
    
    url<-"https://marine.meteoconsult.fr/meteo-marine/bulletin-detaille/port-258/previsions-meteo-cherbourg-aujourdhui"
    page<-read_html(RETRY("GET",url,times = 3))
    a<-html_text(html_node(page,".sun > div:nth-child(2) > span:nth-child(2)"))
    a<-str_replace(a,"h",":")
    a<-as.POSIXct(a,tryFormats="%H:%M")
    a<-ymd_hms(a)-delta_time
    a<-as.character(a)
    a<-str_split(a," ")[[1]][2]
    a<-str_remove(a,"(:00)$")
    col1[1,2]<-a
    
    b<-html_text(html_node(page,".sun > div:nth-child(5) > span:nth-child(2)"))
    b<-str_replace(b,"h",":")
    b<-as.POSIXct(b,tryFormats="%H:%M")
    b<-ymd_hms(b)-delta_time
    b<-as.character(b)
    b<-str_split(b," ")[[1]][2]
    b<-str_remove(b,"(:00)$")
    col1[2,2]<-b
    
    colnames(col1)<-c("  "," ")
    
    
    output$col1<-renderDT(col1, selection = 'none', server = F, editable = T, extensions = 'Buttons',
                          options = list(dom = 'Bfrtip',
                                         buttons = list(list(extend = 'csv', filename= 'col1')),
                                         paging = F,
                                         searching = F, info = FALSE), rownames= FALSE)
    
    ###
    url<-"https://www.ndbc.noaa.gov/station_page.php?station=62305&uom=E&tz=STN"
    page<-read_html(RETRY("GET",url,times = 3))

    a<-str_remove_all(as.character(page),"[:space:]")
    a<-str_split(as.character(a),"WDIR")[[1]][2]
    a<-str_split(as.character(a),"WSPD")[[1]][1]
    a<-str_extract(a,"[:digit:]+")
    meteo1$VENT_1[2]<-a
    
    a<-str_remove_all(as.character(page),"[:space:]")
    a<-str_split(as.character(a),"WSPD")[[1]][2]
    a<-str_split(as.character(a),"kts")[[1]][1]
    a<-str_split(as.character(a),"<.+>")[[1]][2]
    meteo1$VENT_2[2]<-a
    
    a<-str_remove_all(as.character(page),"[:space:]")
    a<-str_split(as.character(a),"VIS")[[1]][2]
    a<-str_split(as.character(a),"nmi")[[1]][1]
    a<-str_split(as.character(a),"<.+>")[[1]][2]
    meteo1$VISI[2]<-a
    
    
    url<-"https://www.ndbc.noaa.gov/station_page.php?station=62305&uom=M&tz=STN"
    page<-read_html(url)
    a<-str_remove_all(as.character(page),"[:space:]")
    a<-str_split(as.character(a),"WTMP")[[1]][2]
    a<-str_split(as.character(a),"°C")[[1]][1]
    a<-str_split(as.character(a),"<.+>")[[1]][2]
    meteo1$TEMP[2]<-as.numeric(a)
    
    a<-str_remove_all(as.character(page),"[:space:]")
    a<-str_split(as.character(a),"WVHT")[[1]][2]
    a<-str_split(as.character(a),"m")[[1]][1]
    a<-str_split(as.character(a),"<.+>")[[1]][2]
    meteo1$MER[2]<-a
    
    
    url<-"https://www.ndbc.noaa.gov/station_page.php?station=62103&uom=E&tz=STN"
    page<-read_html(RETRY("GET",url,times = 3))

    a<-str_remove_all(as.character(page),"[:space:]")
    a<-str_split(as.character(a),"WDIR")[[1]][2]
    a<-str_split(as.character(a),"WSPD")[[1]][1]
    a<-str_extract(a,"[:digit:]+")
    meteo1$VENT_1[1]<-a
    
    a<-str_remove_all(as.character(page),"[:space:]")
    a<-str_split(as.character(a),"WSPD")[[1]][2]
    a<-str_split(as.character(a),"kts")[[1]][1]
    a<-str_split(as.character(a),"<.+>")[[1]][2]
    meteo1$VENT_2[1]<-a
    
    a<-str_remove_all(as.character(page),"[:space:]")
    a<-str_split(as.character(a),"VIS")[[1]][2]
    a<-str_split(as.character(a),"nmi")[[1]][1]
    a<-str_split(as.character(a),"<.+>")[[1]][2]
    meteo1$VISI[1]<-a
    
    
    url<-"https://www.ndbc.noaa.gov/station_page.php?station=62103&uom=M&tz=STN"
    page<-read_html(RETRY("GET",url,times = 3))
    a<-str_remove_all(as.character(page),"[:space:]")
    a<-str_split(as.character(a),"WTMP")[[1]][2]
    a<-str_split(as.character(a),"°C")[[1]][1]
    a<-str_split(as.character(a),"<.+>")[[1]][2]
    meteo1$TEMP[1]<-as.numeric(a)
    temp_eau<<-as.numeric(a)

    a<-str_remove_all(as.character(page),"[:space:]")
    a<-str_split(as.character(a),"WVHT")[[1]][2]
    a<-str_split(as.character(a),"m")[[1]][1]
    a<-str_split(as.character(a),"<.+>")[[1]][2]
    meteo1$MER[1]<-a
    colnames(meteo1)<-c(" ","VENT_1","VENT_2","MER","VISI","TEMP")
    
    meteo1$MER<-as.numeric(meteo1$MER)
    if(meteo1$MER[1]==0 ){meteo1$MER[1]<-0}
    else if(meteo1$MER[1]>0 & meteo1$MER[1]<=0.1){meteo1$MER[1]<-1}
    else if(meteo1$MER[1]>0.1 & meteo1$MER[1]<=0.5){meteo1$MER[1]<-2}
    else if(meteo1$MER[1]>0.5 & meteo1$MER[1]<=1.25){meteo1$MER[1]<-3}
    else if(meteo1$MER[1]>1.25 & meteo1$MER[1]<=2.5){meteo1$MER[1]<-4}
    else if(meteo1$MER[1]>2.5 & meteo1$MER[1]<=4){meteo1$MER[1]<-5}
    else if(meteo1$MER[1]>4 & meteo1$MER[1]<=6){meteo1$MER[1]<-6}
    else if(meteo1$MER[1]>6 & meteo1$MER[1]<=9){meteo1$MER[1]<-7}
    else if(meteo1$MER[1]>9 & meteo1$MER[1]<=14){meteo1$MER[1]<-8}
    else if(meteo1$MER[1]>14){meteo1$MER[1]<-9}
    else {meteo1$MER[1]<-NA}
    
    if(is.na(meteo1$MER[2])){meteo1$MER[2]<-0}
    if(meteo1$MER[2]==0 ){meteo1$MER[2]<-0}
    else if(meteo1$MER[2]>0 & meteo1$MER[2]<=0.1){meteo1$MER[2]<-1}
    else if(meteo1$MER[2]>0.1 & meteo1$MER[2]<=0.5){meteo1$MER[2]<-2}
    else if(meteo1$MER[2]>0.5 & meteo1$MER[2]<=1.25){meteo1$MER[2]<-3}
    else if(meteo1$MER[2]>1.25 & meteo1$MER[2]<=2.5){meteo1$MER[2]<-4}
    else if(meteo1$MER[2]>2.5 & meteo1$MER[2]<=4){meteo1$MER[2]<-5}
    else if(meteo1$MER[2]>4 & meteo1$MER[2]<=6){meteo1$MER[2]<-6}
    else if(meteo1$MER[2]>6 & meteo1$MER[2]<=9){meteo1$MER[2]<-7}
    else if(meteo1$MER[2]>9 & meteo1$MER[2]<=14){meteo1$MER[2]<-8}
    else if(meteo1$MER[2]>14){meteo1$MER[2]<-9}
    else{meteo1$MER[2]<-NA}

    output$meteo1<-renderDT(meteo1, selection = 'none', server = F, editable = T, options = list(dom = 't'), rownames= FALSE)
    
    
    colnames(meteo)<-c(" ","VENT_1","VENT_2","MER","VISI","NEB")
    output$meteo<-renderDT(meteo, 
                           selection = 'none',
                           server = F, 
                           editable = T,
                           extensions = 'Buttons',
                           options = list(dom = 'Bfrtip',
                                          buttons = list(list(extend = 'csv', filename= 'meteo')),
                                          paging = F,
                                          searching = F, info = FALSE
                                          ),
                           rownames= FALSE)
    
    output$bms<-renderDT(bms, 
                           selection = 'none',
                           server = F, 
                           editable = T,
                           extensions = 'Buttons',
                           options = list(dom = 'Bfrtip',
                                          buttons = list(list(extend = 'csv', filename= 'BMS')),
                                          paging = F,
                                          searching = F, info = FALSE
                           ),
                           rownames= FALSE)
    colnames(moyens)<-c("  "," ")
    output$moyens<-renderDT(moyens, 
                         selection = 'none',
                         server = F, 
                         editable = T,
                         extensions = 'Buttons',
                         options = list(dom = 'Bfrtip',
                                        buttons = list(list(extend = 'csv', filename= 'moyens')),
                                        paging = F,
                                        searching = F, info = FALSE
                         ),
                         rownames= FALSE)
    
    marees<-read.csv("marees.csv",encoding = "UTF-8")
    url="https://marine.meteoconsult.fr/meteo-marine/bulletin-detaille/port-247/previsions-meteo-saint-malo-aujourdhui"
    page=read_html(url)
    a<-html_text(html_node(page,"div.tide-line:nth-child(1)"))
    a<-str_remove_all(a,"[:space:]")
    a<-str_extract(a,"Maréehaute.....")
    a<-str_remove(a,"Maréehaute")
    marees$SAINT_MALO[1]<-a
    a<-html_text(html_node(page,"div.tide-line:nth-child(1)"))
    a<-str_remove_all(a,"[:space:]")
    a<-str_extract(a,"Maréebasse.....")
    a<-str_remove(a,"Maréebasse")
    marees$SAINT_MALO[2]<-a
    a<-html_text(html_node(page,"div.tide-line:nth-child(3)"))
    a<-str_remove_all(a,"[:space:]")
    a<-str_extract(a,"Maréehaute.....")
    a<-str_remove(a,"Maréehaute")
    marees$SAINT_MALO[3]<-a
    a<-html_text(html_node(page,"div.tide-line:nth-child(3)"))
    a<-str_remove_all(a,"[:space:]")
    a<-str_extract(a,"Maréebasse.....")
    a<-str_remove(a,"Maréebasse")
    marees$SAINT_MALO[4]<-a
    
    url="https://marine.meteoconsult.fr/meteo-marine/bulletin-detaille/port-247/previsions-meteo-saint-malo-demain"
    page=read_html(url)
    a<-html_text(html_node(page,"div.tide-line:nth-child(1)"))
    a<-str_remove_all(a,"[:space:]")
    a<-str_extract(a,"Maréehaute.....")
    a<-str_remove(a,"Maréehaute")
    marees$SAINT_MALO[5]<-a
    
    url="https://marine.meteoconsult.fr/meteo-marine/bulletin-detaille/port-258/previsions-meteo-cherbourg-aujourdhui"
    page=read_html(url)
    a<-html_text(html_node(page,"div.tide-line:nth-child(1)"))
    a<-str_remove_all(a,"[:space:]")
    a<-str_extract(a,"Maréehaute.....")
    a<-str_remove(a,"Maréehaute")
    marees$CHERBOURG[1]<-a
    a<-html_text(html_node(page,"div.tide-line:nth-child(1)"))
    a<-str_remove_all(a,"[:space:]")
    a<-str_extract(a,"Maréebasse.....")
    a<-str_remove(a,"Maréebasse")
    marees$CHERBOURG[2]<-a
    a<-html_text(html_node(page,"div.tide-line:nth-child(3)"))
    a<-str_remove_all(a,"[:space:]")
    a<-str_extract(a,"Maréehaute.....")
    a<-str_remove(a,"Maréehaute")
    marees$CHERBOURG[3]<-a
    a<-html_text(html_node(page,"div.tide-line:nth-child(3)"))
    a<-str_remove_all(a,"[:space:]")
    a<-str_extract(a,"Maréebasse.....")
    a<-str_remove(a,"Maréebasse")
    marees$CHERBOURG[4]<-a
    
    url="https://marine.meteoconsult.fr/meteo-marine/bulletin-detaille/port-258/previsions-meteo-cherbourg-demain"
    page=read_html(url)
    a<-html_text(html_node(page,"div.tide-line:nth-child(1)"))
    a<-str_remove_all(a,"[:space:]")
    a<-str_extract(a,"Maréehaute.....")
    a<-str_remove(a,"Maréehaute")
    marees$CHERBOURG[5]<-a
    
    a<-html_text(html_node(page,"div.tide-line:nth-child(1) > div:nth-child(1)"))
    b<-html_text(html_node(page,"div.tide-line:nth-child(3) > div:nth-child(1)"))
    c<-str_c(" · ",a,"/",b," · ")
    if(str_length(b)>3){c<-str_c(" · ",a," · ")}
    output$coeff<-renderText({c})
    output$temp<-renderText({str_c(meteo1$TEMP[2],"°C · ")})
    
    url="https://marine.meteoconsult.fr/meteo-marine/bulletin-detaille/port-275/previsions-meteo-le-havre-aujourdhui"
    page=read_html(url)
    a<-html_text(html_node(page,"div.tide-line:nth-child(1)"))
    a<-str_remove_all(a,"[:space:]")
    a<-str_extract(a,"Maréehaute.....")
    a<-str_remove(a,"Maréehaute")
    marees$LE_HAVRE[1]<-a
    a<-html_text(html_node(page,"div.tide-line:nth-child(1)"))
    a<-str_remove_all(a,"[:space:]")
    a<-str_extract(a,"Maréebasse.....")
    a<-str_remove(a,"Maréebasse")
    marees$LE_HAVRE[2]<-a
    a<-html_text(html_node(page,"div.tide-line:nth-child(3)"))
    a<-str_remove_all(a,"[:space:]")
    a<-str_extract(a,"Maréehaute.....")
    a<-str_remove(a,"Maréehaute")
    marees$LE_HAVRE[3]<-a
    a<-html_text(html_node(page,"div.tide-line:nth-child(3)"))
    a<-str_remove_all(a,"[:space:]")
    a<-str_extract(a,"Maréebasse.....")
    a<-str_remove(a,"Maréebasse")
    marees$LE_HAVRE[4]<-a
    
    
    url="https://marine.meteoconsult.fr/meteo-marine/bulletin-detaille/port-275/previsions-meteo-le-havre-demain"
    page=read_html(url)
    a<-html_text(html_node(page,"div.tide-line:nth-child(1)"))
    a<-str_remove_all(a,"[:space:]")
    a<-str_extract(a,"Maréehaute.....")
    a<-str_remove(a,"Maréehaute")
    marees$LE_HAVRE[5]<-a
    
    for (i in 1:length(marees$SAINT_MALO)) {
      if(is.na(marees$SAINT_MALO[i])){next}
      marees$SAINT_MALO[i]<-str_replace(marees$SAINT_MALO[i],"h",":")
      if(str_length(marees$SAINT_MALO[i])==5){
        marees$SAINT_MALO[i]<-str_c(marees$SAINT_MALO[i],":00")}
      if(str_length(marees$SAINT_MALO[i])<5 & str_length(marees$SAINT_MALO[i])>0){
        marees$SAINT_MALO[i]<-str_c("0",marees$SAINT_MALO[i],":00")}
      if(marees$SAINT_MALO[i]==""){marees$SAINT_MALO[i]<-NA}
    }
    marees$SAINT_MALO<-as.POSIXct(marees$SAINT_MALO,tryFormats="%H:%M:%S")
    marees$SAINT_MALO<-ymd_hms(marees$SAINT_MALO)-delta_time
    marees$SAINT_MALO[5]<-ymd_hms(marees$SAINT_MALO[5])+hm("24:00")
    
    if(marees$SAINT_MALO[2]<marees$SAINT_MALO[1]){
      a<-marees$SAINT_MALO[5]
      marees$SAINT_MALO[5]<-marees$SAINT_MALO[3]
      marees$SAINT_MALO[3]<-marees$SAINT_MALO[1]
      marees$SAINT_MALO[1]<-NA
      if(is.na(marees$SAINT_MALO[5])){marees$SAINT_MALO[5]<-a}
    }
    
    marees$SAINT_MALO<-as.character(marees$SAINT_MALO)
    marees$SAINT_MALO<-str_remove(marees$SAINT_MALO,as.character(Sys.Date()))
    
    for (i in 1:length(marees$CHERBOURG)) {
      if(is.na(marees$CHERBOURG[i])){next}
      marees$CHERBOURG[i]<-str_replace(marees$CHERBOURG[i],"h",":")
      if(str_length(marees$CHERBOURG[i])==5){
        marees$CHERBOURG[i]<-str_c(marees$CHERBOURG[i],":00")}
      if(str_length(marees$CHERBOURG[i])<5 & str_length(marees$CHERBOURG[i])>0){
        marees$CHERBOURG[i]<-str_c("0",marees$CHERBOURG[i],":00")}
      if(marees$CHERBOURG[i]==""){marees$CHERBOURG[i]<-NA}
    }
    marees$CHERBOURG<-as.POSIXct(marees$CHERBOURG,tryFormats="%H:%M:%S")
    marees$CHERBOURG<-ymd_hms(marees$CHERBOURG)-delta_time
    marees$CHERBOURG[5]<-ymd_hms(marees$CHERBOURG[5])+hm("24:00")
    
    if(marees$CHERBOURG[2]<marees$CHERBOURG[1]){
      a<-marees$CHERBOURG[5]
      marees$CHERBOURG[5]<-marees$CHERBOURG[3]
      marees$CHERBOURG[3]<-marees$CHERBOURG[1]
      marees$CHERBOURG[1]<-NA
      if(is.na(marees$CHERBOURG[5])){marees$CHERBOURG[5]<-a}
    }
    
    marees$CHERBOURG<-as.character(marees$CHERBOURG)
    marees$CHERBOURG<-str_remove(marees$CHERBOURG,as.character(Sys.Date()))
    
    for (i in 1:length(marees$LE_HAVRE)) {
      if(is.na(marees$LE_HAVRE[i])){next}
      marees$LE_HAVRE[i]<-str_replace(marees$LE_HAVRE[i],"h",":")
      if(str_length(marees$LE_HAVRE[i])==5){
        marees$LE_HAVRE[i]<-str_c(marees$LE_HAVRE[i],":00")}
      if(str_length(marees$LE_HAVRE[i])<5 & str_length(marees$LE_HAVRE[i])>0){
        marees$LE_HAVRE[i]<-str_c("0",marees$LE_HAVRE[i],":00")}
      if(marees$LE_HAVRE[i]==""){marees$LE_HAVRE[i]<-NA}
    }
    marees$LE_HAVRE<-as.POSIXct(marees$LE_HAVRE,tryFormats="%H:%M:%S")
    marees$LE_HAVRE<-ymd_hms(marees$LE_HAVRE)-delta_time
    marees$LE_HAVRE[5]<-ymd_hms(marees$LE_HAVRE[5])+hm("24:00")
    
    if(marees$LE_HAVRE[2]<marees$LE_HAVRE[1]){
      a<-marees$LE_HAVRE[5]
      marees$LE_HAVRE[5]<-marees$LE_HAVRE[3]
      marees$LE_HAVRE[3]<-marees$LE_HAVRE[1]
      marees$LE_HAVRE[1]<-NA
      if(is.na(marees$LE_HAVRE[5])){marees$LE_HAVRE[5]<-a}
    }
    
    marees$LE_HAVRE<-as.character(marees$LE_HAVRE)
    marees$LE_HAVRE<-str_remove(marees$LE_HAVRE,as.character(Sys.Date()))
    
    marees$LE_HAVRE<-str_remove(marees$LE_HAVRE,"(:00)$")
    marees$CHERBOURG<-str_remove(marees$CHERBOURG,"(:00)$")
    marees$SAINT_MALO<-str_remove(marees$SAINT_MALO,"(:00)$")
    
    output$marees<-renderDT(marees, 
                         selection = 'none',
                         server = F, 
                         editable = T,
                         options = list(dom = 'dt',
                                        paging = F,
                                        searching = F, info = FALSE
                         ),
                         rownames= FALSE)
    
    survie1$TEMP_EAU[1]<-as.numeric(meteo1$TEMP[1])
    if(input$sexe==1){
      survie_tab<-survie_tab[as.numeric(survie_tab$TEMP)==survie1$TEMP_EAU,]
      survie1$FONCTIONNEL<-survie_tab$FONCTIONNEL_M[1]
      survie1$SURVIE<-survie_tab$SURVIE_M[1]
    }
    else if(input$sexe==2){
      survie_tab<-survie_tab[as.numeric(survie_tab$TEMP)==survie1$TEMP_EAU,]
      survie1$FONCTIONNEL<-survie_tab$FONCTIONNEL_F[1]
      survie1$SURVIE<-survie_tab$SURVIE_F[1]
    }
    
    survie1$SURVIE<-str_remove(survie1$SURVIE,"(:00)$")
    survie1$FONCTIONNEL<-str_remove(survie1$FONCTIONNEL,"(:00)$")
    
    output$survie1<-renderDT(survie1, 
                             selection = 'none',
                             server = F, 
                             editable = T,
                             options = list(dom = 'dt',
                                            autoWidth = TRUE,
                                            paging = F,
                                            searching = F, info = FALSE
                             ),
                             rownames= FALSE)
    
    
    
  })
  
  observeEvent(input$sexe, {
    
    survie1$TEMP_EAU[1]<-temp_eau
    if(input$sexe==1){
      survie_tab<-survie_tab[as.numeric(survie_tab$TEMP)==survie1$TEMP_EAU,]
      survie1$FONCTIONNEL<-survie_tab$FONCTIONNEL_M[1]
      survie1$SURVIE<-survie_tab$SURVIE_M[1]
    }
    else if(input$sexe==2){
      survie_tab<-survie_tab[as.numeric(survie_tab$TEMP)==survie1$TEMP_EAU,]
      survie1$FONCTIONNEL<-survie_tab$FONCTIONNEL_F[1]
      survie1$SURVIE<-survie_tab$SURVIE_F[1]
    }
    
    survie1$SURVIE<-str_remove(survie1$SURVIE,"(:00)$")
    survie1$FONCTIONNEL<-str_remove(survie1$FONCTIONNEL,"(:00)$")
    
    output$survie1<-renderDT(survie1, 
                             selection = 'none',
                             server = F, 
                             editable = T,
                             options = list(dom = 'dt',
                                            autoWidth = TRUE,
                                            paging = F,
                                            searching = F, info = FALSE
                             ),
                             rownames= FALSE)
  })
  
  output$bms_upload <- reactive({
    return(!is.null(input$bms_upload))
  })
  outputOptions(output, 'bms_upload', suspendWhenHidden=FALSE)
  
  output$meteo_upload <- reactive({
    return(!is.null(input$meteo_upload))
  })
  outputOptions(output, 'meteo_upload', suspendWhenHidden=FALSE)
  
  output$col1_upload <- reactive({
    return(!is.null(input$col1_upload))
  })
  outputOptions(output, 'col1_upload', suspendWhenHidden=FALSE)
  
  output$moyens_upload <- reactive({
    return(!is.null(input$moyens_upload))
  })
  outputOptions(output, 'moyens_upload', suspendWhenHidden=FALSE)
  
  output$barfleur_upload <- reactive({
    return(!is.null(input$barfleur_upload))
  })
  outputOptions(output, 'barfleur_upload', suspendWhenHidden=FALSE)
  
  output$deauville_upload <- reactive({
    return(!is.null(input$deauville_upload))
  })
  outputOptions(output, 'deauville_upload', suspendWhenHidden=FALSE)
  
})