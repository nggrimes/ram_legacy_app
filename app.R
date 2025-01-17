library(here)
library(tidyverse)
library(shiny)
library(shinyWidgets)
load("https://github.com/nggrimes/ram_legacy_app/ram_clean_shiny.Rdata")


ui<-fluidPage(
  sidebarPanel(
    pickerInput("locInput","Select Region",unique(stocks$region),options=list(`actions-box`=TRUE),multiple=T)
  ),
  mainPanel(plotOutput("kobe"))
)

server<-function(input,output){
  load(here("ram_clean_shiny.Rdata"))
  
  kobe_df<-reactive({stocks %>% 
      filter(region %in% input$locInput)})

    output$kobe<-renderPlot({
      
      ggplot(data=kobe_df(),aes(x=BvBmsy,y=FvFmsy))+
        geom_rect(data=data.frame(panel=c("bottom_left","top_right",
                                          "bottom_right","top_left"),
                                  x_min=c(-Inf,1,1,-Inf),
                                  x_max=c(1,Inf,Inf,1),
                                  y_min=c(-Inf,1,-Inf,1),
                                  y_max=c(1,Inf,1,Inf)),
                  aes(xmin=x_min,ymin=y_min,xmax=x_max,ymax=y_max,fill=panel),
                  inherit.aes = F,alpha=0.5)+
        scale_fill_manual(values=c("lightgoldenrod1","limegreen","#FF4500","lightgoldenrod1")
                          ,guide=F)+
        geom_hline(aes(yintercept=1),linetype="longdash")+
        geom_vline(aes(xintercept=1),linetype="longdash")+
        geom_point(aes(x=BvBmsy,y=FvFmsy,size=MSY))+
        scale_color_manual(guide=F,values=c("#383737","red"))+
        scale_size_continuous(guide = F)+
        scale_alpha_continuous(guide=F,range=c(0.9,1))+
        xlab("BvBmsy")+
        ylab("FvFmsy")+
        theme_classic()+
        theme(text=element_text(size=16))+
        scale_x_continuous(limits = c(-1,4),breaks=seq(-1,4,by=0.5),labels = c(seq(-1,2,by=0.5),expression(phantom(x)>=2.5),seq(3,4,by=0.5)))+
        scale_y_continuous(limits = c(-1,6),breaks=seq(-1,6,by=0.5),labels = c(seq(-1,3.5,by=0.5),expression(phantom(x)>=4),seq(4.5,6,by=0.5)))+
        coord_cartesian(xlim = c(0,2.5),ylim = c(0,4))
    })
  
}

shinyApp(ui=ui,server=server)
