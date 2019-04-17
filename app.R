#App Submit to canvas.....zip... If you use dataset, include in zip flie... 
#check app runs in different apps. If you are late 5 minutes, no score.... must attend all
#all lectures... Demo how to use ur software, if you are super early... 
#Lecture 5.5 check guideline...




install <- function(packages){
  new.packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(new.packages)) 
    install.packages(new.packages, dependencies = TRUE)
  sapply(packages, require, character.only = TRUE)
} 
required.packages <- c("shiny",
                       "shinydashboard",
                       "shinythemes",
                       "readxl",
                       "dplyr",
                       "tidyr",
                       "ggplot2",
                       "ggpubr",
                       "scales",
                       "plotly",
                       "DT",
                       "broom")
install(required.packages)



##install packages....
#install.packages("ggpubr",repo="http://cran.r-project.org")
#install.packages("DT",repo="http://cran.r-project.org")
#install.packages("plotly",repo="http://cran.r-project.org")
##upload libraries....
library(ggplot2)
library(xtable)
library(shiny)
library(shinyWidgets)
library(readxl)
library(plyr)
library(DT)
library(Hmisc)
library(plotly)
library(MASS)
library(dplyr,warn.conflicts=FALSE)
#DATA UPLOADING UPDATES:
#nba_2013<-read.csv("https://raw.githubusercontent.com/chrismphy/NBA2013/master/2013_nba_no_player_name.csv")
#nba_2013<-as.data.frame(nba_2013)
#Data #1
NFL_DATA<-read_excel("NFL_DATA.xlsx") 
NFL_DATA<-as.data.frame(NFL_DATA)
global.R<-NFL_DATA
NFL_DATA$MONEYLINE<-as.integer(NFL_DATA$MONEYLINE)

#Data #2
NFL_Player<-read_excel("NFL_Player.xlsx")
NFL_Player<-as.data.frame(NFL_Player)

#interaction term.... https://www.theanalysisfactor.com/interpreting-interactions-in-regression/


 


ui_nfl8<-shinyUI(fluidPage(
  titlePanel("NFL Stats(2018-2019)"),
  #Application title
  #The plot created in server.R is displayed 
  tabsetPanel(
    tabPanel("NFL SUMMARIES ",
             sidebarLayout(#sidebar layout is for entire tab.
               #sidebar  
               sidebarPanel(#inputs go here
                 fluidRow(
                   selectInput("highlight","Color",choices=c("None","Home vs Away","TEAM")),
                   selectInput("yvar","Select vertical axis: y",choices=c("Margin_of_Victory","Final_Score")),
                   selectInput("xvar","Select the horizontal axis: x",names(NFL_DATA)),
                   selectInput("zvar","select interaction variable: z", choices=names(NFL_DATA)),
                   selectInput("line","Add linear regression line?",choices=c("no","yes")),
                   selectInput("summary","Summary: y/n", choices=c("no","linear","anova, y= x+z+x:z","interaction y=x+z:a")),
                   selectInput("rf","residuals vs fited: y/n", choices=c("no","yes" ))
                 )),mainPanel( tabsetPanel(tabPanel("PLOT",fluidRow(
                   plotlyOutput("custom.plot"),
                   verbatimTextOutput("summary.plot")))))
             )
               ),                                         #Plot output is called on sidebarLayout... 
    tabPanel("NFL boxplot analysis",
             selectInput("x2var","Select horizontal axis",c("DATASET","TEAM")),
             selectInput("y2var","Select vertical axis",names(NFL_DATA)),
             column(12,plotlyOutput("custom.plot4"))),
    
    tabPanel("Home vs. Away dist.",                     
             #sidebar2 FOR Normal distribution... 
             fluidRow(sidebarPanel(pickerInput("team","Select which team to plot:",choices=unique(NFL_DATA$TEAM),options = list(
               `actions-box` = TRUE, 
               size = 10,
               `selected-text-format` = "count > 3"
             ), multiple=TRUE)),
             column(12, plotOutput("custom.plot2")) ) ),
    tabPanel("NFL Summaries by Game",column(12,div(dataTableOutput("dataTable1")))),
    tabPanel("NFL Summaries by Players",column(12, div(dataTableOutput("dataTable2"))))
  ) ) ) 

server_nfl8<-shinyServer(function(input,output){
  nfl.ss<-reactive(subset(NFL_DATA,WEEK %in% input$week)) 
  nfl.ss2<-reactive(subset(NFL_DATA,WEEK %in% input$weekbox))
  nfl.tt<-reactive(subset(NFL_DATA,TEAM %in% input$team))
  d_filered<-reactive(NFL_DATA %>% group_by(TEAM)%>%filter(max(input$yvar)>20)%>%ungroup())
  output$custom.plot<-renderPlotly({
    nfl.ggplot<-ggplot()+geom_point(aes_string(input$xvar,input$yvar,colour="TEAM"),data=d_filered())
    
    #   nfl.ggplot<-nfl.ggplot+ geom_point(data=nfl.tt(),colour=alpha("grey",0.7))
    if(input$highlight=="None"){
      nfl.ggplot<-ggplot(data=NFL_DATA,aes_string(input$xvar,input$yvar))+theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))   
      nfl.ggplot<-nfl.ggplot+geom_point(data=d_filered(),aes_string(input$xvar,input$yvar))+geom_count(aes(size = ..n..)) + guides(color = 'legend') 
      nfl.ggplot<-nfl.ggplot+ggtitle(paste0(input$xvar," vs.",input$yvar))
    }
    if(input$highlight=="Home vs Away"){
      nfl.ggplot<-ggplot()+geom_point(aes_string(input$xvar,input$yvar),data=NFL_DATA)+geom_point(aes_string(input$xvar,input$yvar,colour="VENUE"),data=d_filered())
      #theme(axis.text=element_text(size=8), axis.title=element_text(size=8,face="bold"))+geom_count(aes(size = ..n..)) + guides(color = 'legend') +ggtitle(paste0(input$xvar," vs.",input$yvar))  
    }
    if(input$highlight=="TEAM"){
      nfl.ggplot<-ggplot()+geom_point(aes_string(input$xvar,input$yvar),data=NFL_DATA)+geom_point(aes_string(input$xvar,input$yvar,colour="TEAM"),data=d_filered())}
    #   if(input$homeaway=="yes"){nfl.ggplot<-nfl.ggplot+facet_wrap(~VENUE,ncol=4)}
    if(input$line=="yes"){
      nfl.ggplot<-nfl.ggplot+stat_smooth(data=NFL_DATA,aes_string(input$xvar,input$yvar),method=lm)}  
    ggplotly(nfl.ggplot)  #plot(nfl.ggplot)
    if(input$rf=="yes"){
      fitrf<-lm(NFL_DATA[,input$yvar]~NFL_DATA[,input$xvar])
      mod<-fortify(fitrf)
    nfl.ggplot<-ggplot(mod,aes(x=.fitted,y=.resid))+geom_point()
    ggplotly(nfl.ggplot)
      #ggplot(fitrf,which=1)
        }
    ggplotly(nfl.ggplot)
  })
  ## SUMMARY IN TAB 1 BELOW
  output$summary.plot<-renderPrint({
    if(input$summary=="linear"){
      fit2<-lm(NFL_DATA[,input$yvar]~NFL_DATA[,input$xvar])
      summary(fit2)}
    else if(input$summary=="interaction y=x+z:a"){ 
      fit<-lm(NFL_DATA[,input$yvar]~NFL_DATA[,input$xvar]+NFL_DATA[,input$zvar]:NFL_DATA[,input$xvar])
      summary(fit)} })
  ###TAB 3 BELOW
  output$custom.plot2<-  renderPlot({
    nfl.ggplot1<-ggplot(data=nfl.tt())+geom_density(aes(x=Margin_of_Victory,fill=VENUE),alpha=0.4)+labs(x="Margin of Victory")
    plot(nfl.ggplot1)})
  ##TAB 2 BELOW
  output$custom.plot4<- renderPlotly({
    nfl.ggboxplot<-ggplot()+geom_boxplot(aes_string(input$x2var,input$y2var),data=NFL_DATA)+theme(axis.text.x=element_text(angle=90,hjust=1))+geom_point(aes_string(input$x2var,input$y2var,fill="TEAM"),data=NFL_DATA)
    #nfl.ggplotboxplot<-ggplot(data=nfl.ss2(),aes_string("DATASET",input$x2var),colour="TEAM")+geom_boxplot()
    ggplotly(nfl.ggboxplot)
  })
  output$dataTable1<-renderDT(
    NFL_DATA,
    class= "display nowrap compact",
    filter="top"
    
  ) 
  output$dataTable2 <- renderDT(
    NFL_Player, # data       
    class = "display nowrap compact", # style
    filter = "top" # location of column filters
  )
}) # End 


shinyApp(ui=ui_nfl8,server=server_nfl8)
