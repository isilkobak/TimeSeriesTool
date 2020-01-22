rm(list=ls())
require(shiny)
require(shinyjs)
require(magrittr) ## dplyr imports %>% but not %$%
## require(readr)
## require(tidyr)
## require(dplyr)
## require(ggplot2)
require(tidyverse)
require(ggrepel) ## for repelling texts/labels with geom_text or _label
require(jpeg) ## background image
require(grid) ## background image
require(forecast)
require(gridExtra)
require(forecastHybrid)
require(fpp)
library(zoo)
library(xts)
library(ggplot2)
library(reshape2)
library(devtools)
library(data.table)

library(DT)
library(plotly)
library(dygraphs)
library(dplyr)

# use the below options code if you wish to increase the file input limit, in this example file input limit is increased from 5MB to 9MB
# options(shiny.maxRequestSize = 9*1024^2)
#set theme for ggplot2 graphics
#set theme for ggplot2 graphics
BCTransform <- function(x, lambda) {
  if (lambda[1]==0) {
    log(x+lambda[2])
  } else {
    ((x+lambda[2])^lambda[1]-1)/lambda[1]
  }
}

invBCTransform <- function(y, lambda) {
  if (lambda[1]==0) {
    exp(y)-lambda[2]
  } else {
    (y*lambda[1]+1)^(1/lambda[1])-lambda[2]
  }
}

theme = theme_set(theme_minimal())
theme = theme_update(legend.position="top",
                     axis.text.x = element_text(angle = 45, hjust = 1, vjust =1.25))


#read in reference CSV for public holidays - dummy variables
pubhol <- read.csv('AuPublicHolidays.csv',
                   header=T,
                   sep=",",
                   quote='"',
                   strip.white=T,
                   stringsAsFactors=F,
                   fill=T)

pubhol$Date <- zoo::as.Date(pubhol[["Date"]], format = "%d/%m/%Y")
pubhol$weekday <- weekdays(pubhol$Date)

init.data <- function(filename) {
  d <- read_csv(filename)
  d$date <- as.Date(d$date,"%d.%m.%Y")
  tr_locale <- locale("en", decimal_mark = ".", grouping_mark=",")
  d <- d %>%
    
    rename(Tarih=date, MalKod=item_id, 
           MagazaGrup=shop_id,
           Siparis=item_cnt_day) %>%
    mutate(Yil=parse_number(strftime(Tarih, "%Y")),
           Hafta=parse_number(strftime(Tarih, "%V")),
           Ay=parse_number(strftime(Tarih, "%m")))
  
  ##str(d)
  
  
  
  ############################################################################
  ## Haftalik ve aylik veriler
  ############################################################################
  da <- d %>%
    group_by(MalKod, MagazaGrup, Yil, Ay) %>%
    summarise(Siparis=sum(Siparis)) %>%
    mutate(Zaman=Yil+(Ay-1)/12)
  
  dh <- d %>%
    group_by(MalKod, MagazaGrup, Yil, Hafta) %>%
    summarise(Siparis=sum(Siparis)) %>%
    mutate(Zaman=Yil+(Hafta-1)/53)
  
  
  ## Hafta, ay adlarini  "Donem"le degistir.
  dh <- dh %>% rename(Donem=Hafta)
  da <- da %>% rename(Donem=Ay)
  
  ## print(head(d))
  return(list(d=d,dh=dh,da=da))
}


################# 2. SHINY SERVER FUNCTION #################


shinyServer(
  function(input, output) {
  
  
  ###################################################################
  #########              1 TIME SERIES FORECASTING           ########
  ###################################################################
  ## toplu tahminler icin degerlendirilen

 
  ####### READ IN CSV FILE BASED ON SELECTION ####### 

  
  
  ####### DYNAMIC DROP DOWN LIST FOR TASK BASED ON INPUT FILE ####### 
  observe({
    file1 <- input$file1
    if (is.null(file1)){
      ## if a file has not been selected yet, this "if" stops
      ## the execution of the remainder of commands
      return(NULL)
    }
    userData <- init.data(file1$datapath)
    d <- userData$d
    da <- userData$da
    dh <- userData$dh
    
    #cat(str(d))
 #   column_levels <- unique(d[[input$MAlKod]])
   # updateSelectInput(session, "level", choices = column_levels,
    #                  label = paste("Choose level in", input$pic)
 #   updateSelectInput(session, 
    #                  'i_task_select', 
     #                 label = 'Select Series',
      #                choices = list(column_levels),
       #              list(str(d$MalKod))[1])
    
    urunMenuData <- reactive({
    #  dataFrame <- d %>%
    #    filter(Durum %in% input$Durum,
    #           CevrimBirim %in% input$CevrimBirim,
    #           between(Yil, input$sortInterval[1],
    #                   input$sortInterval[2]))
      
      
      ## Urun menusunu sirali olusurabilmek icin
 
      
      ProductList <- d %>%
        group_by(MalKod) %>%
        summarise(Siparis=sum(Siparis))
     
       ProductList <- ProductList %>%
        arrange(desc(Siparis)) %$%
        {list(menu=round(Siparis,0),
              MalKod=MalKod,
              sira="Siparis Miktar Sirali",
              sayi=nrow(ProductList))}
      
  
      
      return(list( ProductList=ProductList))
    })
    
    
    output$selectComp <- renderUI({
      ProductList <- urunMenuData()$ProductList
      choices <- as.character(unique(unlist(urunMenuData()$ProductList[["MalKod"]])))
      menulabels <- sprintf("[%3d] %-10s", seq_along(ProductList$MalKod),
                            ProductList$MalKod)
      names(choices) <- menulabels
      

      
      selectInput("MalKod",label=paste0("Urun", " (", ProductList$sayi,", ",
                                               ProductList$sira,")"), 
                  choices=choices)
    })
    
    TimeInterval <- reactiveValues(min=min(d$Yil),
                                   max=max(d$Yil),
                                   mid=floor(mean(range(d$Yil))))
    output$tahminUI <- renderUI({
   
      tagList(
        sliderInput(inputId="tahminUzunlugu",
                    label="Tahmin Uzunlugu",
                    min=2,
                    max = ifelse(input$frequency=="Hafta", 106, 24),
                    value = ifelse(input$frequency=="Hafta", 53, 24),
                    step=1,
                    dragRange=FALSE
        ),
        sliderInput(inputId="seriSonuUzunlugu",
                    label="Seri sonu uzunlugu",
                    min=2,
                    max = nrow(passData()$dataFrame),
                    value = nrow(passData()$dataFrame),
                    step=1,
                    dragRange=FALSE
        )
      )
    })
   
    passData <- reactive({
     # cat("*************passData:**************\n")
      if (input$frequency=="Ay") {
        ## Aylik veriler
        dataFrame <- da
       
        
      } else {
        ## Haftalik veriler
        dataFrame <- dh
       
      }
      
      cat(input$frequency)
      
      smryRank <- dataFrame %>%
        group_by(MalKod) %>%
        summarise(Siparis=sum(Siparis)) %>%
        mutate(SipRank=min_rank(desc(Siparis))) %>%
        transmute(MalKod, SipRank)
      
      
      
      # smryRank <- smryData()$smryRank
      #dataFrame <- smryData()$dataFrame
      
      #if (input$MalKod=="Hepsi"){
      # dataFrame <- dataFrame %>% ungroup %>%
      #  mutate(MalKod="Hepsi",
      #        Durum=paste(input$Durum, collapse="+"))
      #}
    #  cat(input$MalKod)
     
      dataFrame <- dataFrame %>%
        filter(MalKod==input$MalKod)
      #&cat(str(dataFrame))
    #  cat("*************done:**************\n")
      #if (input$MagazaGrup!="Hepsi"){
      # dataFrame <- dataFrame %>%
      #  filter(MagazaGrup==input$MagazaGrup)
      #}
      
      
      
      dataFrame <- dataFrame %>%
        group_by(MalKod, Yil, Donem) %>%
        summarise_at(c("Siparis"), sum) %>%
        mutate(Zaman=Yil + (Donem-1)/ifelse(input$frequency=="Hafta", 53, 12))
      
   #  cat(str(dataFrame))
    
      ## Kayitli en son siparis bilgisi (sifir
      ## olan siparisleri asagida bulabilmek icin)
      idmax <- dataFrame %$% which.max(Zaman)
      maxYil <- dataFrame %$% Yil[idmax]
     maxDonem <- dataFrame %$% Donem[idmax]
  
      ## figure basliklarinda her urun icin summary statistics
      ## verebilmek icin
      smry <- dataFrame %>%
        group_by(MalKod) %>%
        summarise(Siparis=sum(Siparis))%>%
        left_join(y=smryRank, by="MalKod")
      
      
      #cat("***smry:***\n")
      #print(smry)
      # cat(str(dataFrame))
      
     idmin <- dataFrame %$% which.min(Zaman)
         minYil <- dataFrame %$% Yil[idmin]
        minDonem <- dataFrame %$% Donem[idmin]
        
        ts.full <- ts(start=c(minYil, minDonem),
                      end=c(maxYil, maxDonem),
                      frequency=ifelse(input$frequency=="Hafta", 53, 12))
        
        dataFrame.full <- data.frame(
          
          MalKod=input$MalKod,
          ## MagazaGrup=input$MagazaGrup,
          ## SevkHesapKod=input$SevkHesapKod,
          Yil=floor(time(ts.full)+0.001), ## sometimes 2016.000
          ## (January) was rounded
          ## to 2015!
          Donem=cycle(ts.full),
          Siparis=0,
          Zaman=time(ts.full),
          stringsAsFactors=FALSE)
        
         # cat("***dataFrame.full:***\n")
          #print(dataFrame.full)
          #cat("***dataFrame:***\n")
          #print(dataFrame)
          
         # dataFrame <- dataFrame.full %>%
          #  anti_join(dataFrame, by=c("Yil", "Donem")) %>%
           # bind_rows(dataFrame) %>% arrange(Zaman) %>%
            #group_by(CevrimBirim, Durum, MalKod, Yil)
          
          dataFrame <- dataFrame %>%
            anti_join(dataFrame, by=c("Yil", "Donem")) %>%
            bind_rows(dataFrame) %>% arrange(Zaman) %>%
            group_by(MalKod, Yil)
          
          tseries <- dataFrame %$%
            ts(Siparis, start=min(Zaman),
               frequency = ifelse(input$frequency=="Hafta", 53, 12))
         # cat(str(tseries))
        
          
         # fr <- stlf(tseries, h=6,
          #           level=0.8,
           #          restrict=FALSE)
          
          
          
          
       #   print(g)
         
    
      list(dataFrame=dataFrame, tseries=tseries)
    })
    
    tahminler <- reactive({
      zamanSerisi.local <- passData()
      tseries <- zamanSerisi.local$tseries
      ## cat("tahminler| tseries:\n")
      ## print(tseries)
      ## cat("start:\n")
      ## print(start(tseries))
      ## cat("end:\n")
      ## print(end(tseries))
      ## cat("frequency:\n")
      ## print(frequency(tseries))
      
      
      if (input$tahminYontem=="stlETS") {
        fr <- stlf(tseries, h=input$tahminUzunlugu,
                   level=input$confLevel,
                   restrict=FALSE)
      } else if (input$tahminYontem=="stlSES") {
        fr <- stlf(tseries, h=input$tahminUzunlugu,
                   level=input$confLevel,
                   etsmodel="ANN",
                   restrict=FALSE)
      } else if (input$tahminYontem=="stlHolt") {
        fr <- stlf(tseries, h=input$tahminUzunlugu,
                   level=input$confLevel,
                   etsmodel="AAN",
                   restrict=FALSE)
      } else if (input$tahminYontem=="stlHoltDamped") {
        fr <- stlf(tseries, h=input$tahminUzunlugu,
                   level=input$confLevel,
                   etsmodel="AAN", damped=TRUE,
                   restrict=FALSE)
      } else if (input$tahminYontem=="stlARIMA") {
        fr <- stlf(tseries, h=input$tahminUzunlugu,
                   level=input$confLevel,
                   method="arima")
      } else if (input$tahminYontem=="ETS") {
        res <- ets(tseries, restrict=FALSE)
        fr <- forecast(res, h=input$tahminUzunlugu,
                       level=input$confLevel)
      } else if (input$tahminYontem=="SES") {
        res <- ets(tseries, model="ANN")
        fr <- forecast(res, h=input$tahminUzunlugu,
                       level=input$confLevel)
      } else if (input$tahminYontem=="Holt") {
        res <- ets(tseries, model="AAN")
        fr <- forecast(res, h=input$tahminUzunlugu,
                       level=input$confLevel)
      } else if (input$tahminYontem=="HoltDamped") {
        res <- ets(tseries, model="AAN", damped=TRUE)
        fr <- forecast(res, h=input$tahminUzunlugu,
                       level=input$confLevel)
      } else if (input$tahminYontem=="HoltWinters") {
        res <- ets(tseries, model="AAA")
        fr <- forecast(res, h=input$tahminUzunlugu,
                       level=input$confLevel)
      } else if (input$tahminYontem=="HoltWintersDamped") {
        res <- ets(tseries, model="AAA", damped=TRUE)
        fr <- forecast(res, h=input$tahminUzunlugu,
                       level=input$confLevel)
      } else if (input$tahminYontem=="HoltWintersMS") {
        res <- ets(tseries, model="AAM", restrict=FALSE)
        fr <- forecast(res, h=input$tahminUzunlugu,
                       level=input$confLevel)
      } else if (input$tahminYontem=="HoltWintersMSDamped") {
        res <- ets(tseries, model="AAM", restrict=FALSE, damped=TRUE)
        fr <- forecast(res, h=input$tahminUzunlugu,
                       level=input$confLevel)
      } else if (input$tahminYontem=="ARIMA") {
        res <- auto.arima(tseries)
        fr <- forecast(res, h=input$tahminUzunlugu,
                       level=input$confLevel)
      } else if (input$tahminYontem=="HYBRID") {
        res <- hybridModel(tseries, models = "ae")
        fr <- forecast(res, h=input$tahminUzunlugu,
                       level=input$confLevel)
      } else if (input$tahminYontem=="THETA") {
        fr <- thetaf(tseries, h=input$tahminUzunlugu,
                     level=input$confLevel)
      } else if (input$tahminYontem=="NNAR") {
        res <- nnetar(tseries)
        fr <- forecast(res, h=input$tahminUzunlugu)
      ##print(fr)
      }
      
      list(fr=fr)
    })
    tahminPlot <- reactive({
      tahminler.local <- tahminler()
    g <-autoplot(tahminler.local$fr, include=33,
                 PI=0.8) +
      scale_y_continuous(labels=scales::comma_format()) +
      labs(x=NULL,y=NULL) +
      theme_set(theme_grey(base_size = 20)) +
      theme(legend.position="none",
            plot.title=element_text(size=14))
    if (input$plotFitted) {
     ft <- fitted(tahminler.local$fr)
    df <- data.frame(x=as.numeric(time(ft)), y=as.numeric(ft)) %>%
      tail(33)
    g<-g + geom_line(data=df, aes(x,y), col="red", size=1) }
    return(g)
    
    })
    
    output$reacTimeIntervalUI <- renderUI({
      sliderInput(inputId = "interval",
                  label = "Tahmin Zaman Araligi",
                  min = TimeInterval$min,
                  max = TimeInterval$max,
                  ##value = c(TimeInterval$mid, TimeInterval$max),
                  value = c(TimeInterval$min, TimeInterval$max),
                  step = 1, sep ="", dragRange = TRUE)
    })
    timeSeriesPlot <- reactive({
      ## Siparis, sevk, kalan miktarlarinin zaman serilerini
      ## cizdirmek icin
 
        if(input$MalKod %in% passData()$dataFrame$MalKod){
          
          dataFrame <- passData()$dataFrame %>%
            gather(Anahtar, Deger, Siparis)
          
          
          g1 <- ggplot(dataFrame, aes(Zaman, Deger)) +
            labs(x=NULL, y=NULL) +
            ggtitle(passData()$figTitle) +
            scale_x_continuous(limits=input$interval+c(0,1)) +
            scale_y_continuous(labels=scales::comma_format()) +
            theme_set(theme_grey(base_size = 20))  +
            theme(legend.position="none",
                  strip.text.y= element_text(angle=0),
                  plot.title=element_text(size=14))
          
          
          if (input$free_y) {
            g1 <- g1+ facet_grid(Anahtar~., as.table=FALSE,
                                 scales="free_y")
          } else {
            g1 <- g1+ facet_grid(Anahtar~., as.table=FALSE)
          }
          
          if (input$timeSeriesType == "line") {
            g1 <- g1 + geom_line(size=1.5, aes(colour=Anahtar))
          } else {
            g1 <- g1 +  geom_linerange(aes(ymin=0, ymax=Deger,
                                           colour=Anahtar),
                                       size=1.5)
          }
          
          if (input$smoother){
            if (input$smootherType=="LOESS"){
              g1 <- g1 + geom_smooth(se=FALSE, method="loess",
                                     colour="black",size=1.5,
                                     span=input$smootherSpan)
            } else {
              dataFrame2 <- passData()$dataFrame %>% ungroup
              
              if (input$smootherDirection=="Cift Yonlu"){
                if (input$smootherPeriod%%2 == 0) {
                  smoother.filter <- c(1,
                                       rep(2, input$smootherPeriod-1),
                                       1)/2
                } else {
                  smoother.filter <- rep(1, input$smootherPeriod)
                }
                smoother.filter <- smoother.filter/input$smootherPeriod
                ## Deger.smooth <- stats::filter(dataFrame$Deger,
                ##                               smoother.filter)
                dataFrame2 <- dataFrame2 %>%
                  mutate_at(c("Siparis"),
                            function(x) stats::filter(x,
                                                      smoother.filter))
              } else {
                smoother.filter <- rep(1, input$smootherPeriod)/
                  input$smootherPeriod
                ## Deger.smooth <- stats::filter(dataFrame$Deger,
                ##                               smoother.filter,
                ##                               sides=1)
                dataFrame2 <- dataFrame2 %>%
                  mutate_at(c("Siparis"),
                            function(x) stats::filter(x,
                                                      smoother.filter,
                                                      sides=1))
              }
              
              
              dataFrame2 <- dataFrame2 %>%
                gather(Anahtar, Deger, Siparis)
              g1 <- g1 + geom_line(data=dataFrame2,
                                   colour="black", size=1.5)
            }
          }
          
          if (input$ozelGunler){
            g.ozg <- c(geom_vline(data=OG(),
                                  aes(xintercept=Zaman),
                                  lty="dashed", col="gray40"),
                       geom_text(data=OG(),
                                 aes(Zaman, 0, label=Gun)))
            g1 <- g1 + g.ozg
          }
        } else {
          g1 <- ggplot(data.frame(x=0,y=0), aes(x,y)) +
            geom_text(aes(label=paste0(
                                       " urun degil")),
                      size=12) +
            theme_void()
        }
 
      return(g1)
    })
    
    output$timeSeries <- renderPlot({
      print(timeSeriesPlot())
    })
    
    output$reacSortIntervalUI <- renderUI({
      sliderInput(inputId = "sortInterval",
                  label = "Siralama zaman araligi",
                  min = TimeInterval$min,
                  max = TimeInterval$max,
                  value = c(TimeInterval$mid, TimeInterval$max),
                  step = 1, sep ="", dragRange = TRUE)
    })
    
    safetystockbulma<- reactive({
      fr <- tahminler()$fr
      #r=auto.arima(gas) %>% forecast
      ## cat("*** Girdim ****\n")
      ## print(fr)
      expected=fr$mean
      ## cat("expected\n")
      ## print(expected)
      ## cat("upper\n")
      ## print(fr$upper)
      ## cat("lower\n")
      ## print(fr$lower)
      se=(fr$upper - fr$lower)/2/(-qnorm((1-input$confLevel)/2))
      ## cat("se\n")
      ## print(se)
      #### safety stock
      cumexp=expected %>% cumsum
      cumse2=se^2 %>% cumsum
      sstock= qnorm(input$servicelevel)*sqrt(cumse2)
      uprstock=cumexp + sstock
      
      return(data.frame(sure=seq_along(expected),
                        beklenen=expected,
                        guvenlikstogu=sstock,
                        ustguvenlik=uprstock))
      
    })#ss kapatma
    
    output$sstockTable <- renderTable({
      safetystockbulma()
    })
    
    yearPlot <- reactive({
      ## Siparislerin periyodik (haftalik ya da aylik zaman
      ## grafigini cizdirmek icin
      ##cat("**********Yillik plot:************\n")
      
        passData.local <- passData()
        if (input$MalKod %in% passData.local$dataFrame$MalKod) {
          ## Grafik: anahtar
          endpnts <- passData.local$dataFrame %>%
            summarise(Donem=last(Donem), Siparis=last(Siparis))
          ## Grafik: Siparisi yillarin aylik profilleri
          g2 <- passData.local$dataFrame %>%
            ggplot(aes(Donem, Siparis)) +
           # ggtitle(passData.local$figTitle) +
            labs(x=NULL,y=NULL) +
            scale_y_continuous(labels=scales::comma_format()) +
            geom_line(aes(colour=factor(Yil)), size=1.5) +
            geom_label_repel(data=endpnts,
                             aes(label=Yil, colour=factor(Yil)),
                             size=5, fontface="bold") +
            theme_set(theme_grey(base_size = 20))  +
            theme(legend.position="none",
                  plot.title=element_text(size=14))
          
          
          if (input$frequency=="Ay") {
            g2 <- g2 + scale_x_continuous(breaks=1:12)
          } else {
            g2 <- g2 +
              scale_x_continuous(breaks=seq(1,53,len=14))
          }
          
          if (input$smoother) {
            smoother <- passData.local$dataFrame %>%
              group_by(Donem) %>%
              summarise(Ortalama=mean(Siparis))
            g2 <- g2 + geom_line(data=smoother,
                                 aes(Donem, Ortalama),
                                 col="black", size=1.5)
          }
        } else {
          g2 <- ggplot(data.frame(x=0,y=0), aes(x,y)) +
            geom_text(aes(label=paste0(
                                       " urunu degil")), size=12) +
            theme_void()
        }
      
      return(g2)
    })
    
    output$yearPlot <- renderPlot({
      print(yearPlot())
    })
    
    output$stlPlot <- renderPlot({
  
        zamanSerisi.local <- passData()
        tseries <- zamanSerisi.local$tseries
        res <- stl(tseries, s.window="periodic")
        if (input$stlPlotType=="base") {
          plot(res,
               labels=c("veri", "sezonsal", "trend", "artik"),
               set.pars=list(mar = c(0, 6, 0, 6),
                             oma = c(2.5, 0, 4, 0), tck = -0.01,
                             cex.axis=2, mfrow=c(4,1)))
          title(main=zamanSerisi.local$figTitle,
                line=2, adj=0, cex.main=1.2)
        } else {
          autoplot(res,
                   labels=c("trend", "sezonsal", "artik")) +
            scale_y_continuous(labels=scales::comma_format())+
            ggtitle(zamanSerisi.local$figTitle) +
            labs(x=NULL,y=NULL) +
            theme_set(theme_grey(base_size = 20)) +
            theme(plot.title=element_text(size=14))
        }
       
    })
    output$accuracy <- renderTable({
    
        tahminler.local <- tahminler()
        MAPE1 <- (abs(fitted(tahminler.local$fr)-tahminler.local$fr$x)/
                    (tahminler.local$fr$x+1)) %>% mean %>%
          multiply_by(100) %>% round(2)
        YTGO <- (fitted(tahminler.local$fr)/
                   (tahminler.local$fr$x+1)) %>%
          mean %>% multiply_by(100) %>% round(2)
        accuracy(tahminler.local$fr) %>% data.frame %>%
          mutate(MAPE1=MAPE1, YTGO=YTGO) %>%
          select(c(RMSE, MAE, MAPE, MAPE1, YTGO, MASE))
    
      
    }, hover=TRUE)
    
    output$BoxTestResult <- renderTable({
      if (input$MalKod!="Hicbiri"){
        tahminler.local <- tahminler()
        resids <- resid(tahminler.local$fr)
        fitdf <-  length(coef(tahminler.local$fr$model))
        lagg <- sqrt(length(resids))
        lagg <- ifelse(lagg<=fitdf, fitdf+1, lagg)
        res.test <- Box.test(resids,
                             lag=lagg,
                             fitdf=fitdf, type="Ljung")
        data.frame("Chi-sqr"=res.test$statistic,
                   "DF"=lagg-fitdf, "p-value"=res.test$p.value,
                   "Yontem"=res.test$method)
      }
    }, hover=TRUE)
    
    
      
      output$forecast <- renderPlot({
        print(tahminPlot())
      })
     
       output$downloadPlot.forecast <- downloadHandler(
        filename = function() {
          tahminler.local <- tahminler()
          paste(input$MalKod, "-",
                input$frequency,
                "lik-tahminler-",
                tahminler.local$fr$method,"-",
                format(Sys.Date(), "%Y%m%d"), ".pdf", sep="")},
        content = function(file){
          cairo_pdf(file, width = 10, height = 10, pointsize = 12)
          print(tahminPlot())
          dev.off()} ## ,
        ## contentType = 'image/pdf'
      )

     output$sideBarMenuRight <- renderUI({
      tagList(
        
        
        conditionalPanel(
          condition="input.theTabs!='topluTahminler'",
          tagList(
            
           
            
            conditionalPanel(
              condition = "input.theTabs == 'modeller'",
              checkboxInput(inputId = "logMAE",
                            label = "MAE: logaritmik olcekte",
                            value = FALSE)
            ),
            
            conditionalPanel(
              condition = "input.theTabs == 'zamanSerisi' || input.theTabs == 'yillik'",
              
              checkboxInput(inputId = "smoother",
                            label = "Duzlestirici",
                            value = FALSE),
              
              conditionalPanel(
                condition = "input.smoother == true && input.theTabs == 'zamanSerisi'",
                radioButtons(inputId="smootherType",
                             label=NA,
                             choices=c("LOESS", "HO"),
                             selected="LOESS",
                             inline=TRUE)
              ),
              
              conditionalPanel(
                condition = "input.smoother == true && input.smootherType == 'HO' && input.theTabs == 'zamanSerisi'",
                radioButtons(inputId="smootherDirection",
                             label = "Duzlestirici Yonu",
                             choices = c("Cift Yonlu", "Tek Yonlu"),
                             inline=TRUE
                )
                
                #uiOutput("reacSmootherPeriodUI")
              )
            ),
            
            conditionalPanel(
              condition = "input.smoother == true && input.smootherType == 'LOESS' && input.theTabs == 'zamanSerisi'",
              sliderInput(inputId="smootherSpan",
                          label = "LOESS genisligi",
                          min=0, max=1, value=0.5,
                          step=0.05, sep=""
              )
            ),
            
            conditionalPanel(
              condition = "input.theTabs == 'zamanSerisi'",
              radioButtons(inputId="timeSeriesType",
                           label="Zaman serisi",
                           choices= c("Cizgi" = "line", "Cubuk"="spike"),
                           inline=TRUE),
              checkboxInput(inputId = "free_y",
                            label = "Zaman serisi y ekseni skalasi serbest",
                            value = FALSE),
              checkboxInput(inputId = "ozelGunler",
                            label = "Ozel gunler",
                            value = FALSE)
            ),
            
            conditionalPanel(
              condition = "input.theTabs == 'stl'",
              radioButtons(inputId="stlPlotType",
                           label="Sezon-Trend Cizelge Turu",
                           choices= c("base" = "base", "ggplot"="ggplot"),
                           selected="ggplot",
                           inline=TRUE)
            ),
            
            conditionalPanel(
              condition = "input.theTabs == 'tahmin' || input.theTabs == 'analiz'",
              ## radioButtons(inputId="tahminYontem",
              ##              label="Tahmin Yontemi",
              ##              choices= c("ETS" = "ETS", "STL+ARIMA"="stlARIMA", "ARIMA"="ARIMA"),
              ##              inline=TRUE)
              
              selectInput(inputId="tahminYontem",
                          label="Tahmin Yontemi",
                          choices= c("STL+ETS" = "stlETS",
                                     "STL+SES" = "stlSES",
                                     "STL+HOLT" = "stlHolt",
                                     "STL+HOLT+DAMPED" = "stlHoltDamped",
                                     "STL+ARIMA" = "stlARIMA",
                                     "ETS" = "ETS",
                                     "SES" = "SES",
                                     "HOLT" = "Holt",
                                     "HOLT+DAMPED" = "HoltDamped",
                                     "HOLT-WINTERS" = "HoltWinters",
                                     "HOLT-WINTERS+DAMPED" = "HoltWintersDamped",
                                     "HOLT-WINTERS MULT. SEAS." = "HoltWintersMS",
                                     "HOLT-WINTERS MULT. SEAS.+DAMPED" = "HoltWintersMSDamped",
                                     "ARIMA" = "ARIMA",
                                     "HYBRID" ="HYBRID",
                                     "THETA" = "THETA",
                                     "NNAR" = "NNAR"))
            ),
            
            conditionalPanel(
              condition = "input.theTabs == 'tahmin'",
              uiOutput("tahminUI"),
              
              
              checkboxInput(inputId="plotFitted",
                            label="Gecmis verilerin tahmini",
                            value=FALSE),
              
              checkboxInput(inputId="confInterval",
                            label="Tahmin guvenirlik",
                            value=TRUE),
              
              conditionalPanel(
                condition= "input.confInterval",
                sliderInput(inputId="confLevel",
                            label="Tahmin guvenirlik seviyesi",
                            min=0.50, max = 0.95,
                            value = 0.80,
                            step=0.05,
                            dragRange=FALSE
                )
              )
            ),
            conditionalPanel(
              condition = "input.theTabs == 'sstockTab'",
              
              sliderInput("servicelevel", "Servis Duzeyi:",
                          min=0.5, max=1, value=0.8, step=0.05)
              
            )
          ) ## taglist
        ) ## conditionalPanel: input.theTabs != 'topluTahminler'
        
        ## conditionalPanel(
        ##     condition="input.theTabs=='topluTahminler'",
        ##     p("Tum urunlerin tahminlerini hesaplamak uzere Baslat dugmesine basiniz:"),
        ##     actionButton("topluTahminBaslat", "Baslat")
        ## )
      )
    })
  })
    
    
    
    

 # mySeries_filtered <- eventReactive(input$goButton, {
    
    #dependency on 'start forecasting' button being pressed
    #input$goButton
    
    #if (nrow(mySeries_raw())==0) 
    #  return()
    

    
    #use existing reactive structures
  #  mySeries <- mySeries_raw()
    
    
  
      #  task_type = input$i_task_select
      
  
    
    #BUILD DATAFRAME

    
   # mySeries <-  mySeries %>%
     # filter(
        #     between(mySeries$Yil, input$i_forecast_n[1], input$i_forecast_n[2]))
    
    #tseries <-  ts(select_(mySeries, input$i_task_select),
          #    start=min(as.numeric(mySeries$Yil)),
         #      frequency = 12)               #monthly 
    
    
    
    

 # })
 
  


  

  
  
})