# install.packages("RCurl")
# rm(list=ls(all.names = T))
# library(RCurl)
library(data.table)
library(ggplot2)
library(dplyr)


df<-fread("google_mobility_change_CH.csv")
# df<-df[which(df$iso_3166_2_code==""),]
df$kanton<-gsub("CH-","", df$iso_3166_2_code)
df$kanton[which(df$kanton=="")]<-"CH"
# df$kanton[which(df$kanton=="")]<-"CHFL"
gp<-ggplot(data=df, aes(x=date,
                        color=daytype))+
    # scale_color_manual(values=c("red","blue"))+
    scale_x_date(date_breaks = "1 month", date_labels = "%d %m")+
    # facet_grid(rows = vars(iso_3166_2_code))+
    facet_grid(rows = vars(country_region_code))+
    ggtitle(max(df$date, na.rm=T))

# gpmobil1<-gp+geom_point(aes(y=workplaces_percent_change_from_baseline))
# gpmobil2<-gp+geom_point(aes(y=retail_and_recreation_percent_change_from_baseline))
# gpmobil3<-gp+geom_point(aes(y=grocery_and_pharmacy_percent_change_from_baseline))
# 
# gpmobil4<-gp+geom_point(aes(y=parks_percent_change_from_baseline))
# gpmobil5<-gp+geom_point(aes(y=transit_stations_percent_change_from_baseline))
# gpmobil6<-gp+geom_point(aes(y=residential_percent_change_from_baseline))

# 

coviddf<-fread("cases_CH.csv")
# coviddf$geoRegion<-gsub("CHFL","FL",coviddf$geoRegion)



library(cowplot)
# plot_grid(gpcases, gpdeaths, ncol=1, align="v")

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("google mobility changes"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            selectInput("country",
                        "country",
                        choices=levels(as.factor(df$country_region)),
                        multiple = T,
                        selected = c("Switzerland"),
                        selectize = F),
            selectInput("parameter",
                        "google mobility parameter",
                        choices=colnames(df)[9:14],
                        multiple = F,
                        selectize = F,size = 10),
            selectInput("kanton",
                        "kanton",
                        choices=levels(as.factor(coviddf$geoRegion)),
                        multiple = F,
                        # selected = c("CH"),
                        selectize = F),
            checkboxInput("inzidenz", "inzidenz?",value = F)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot", height = "600px")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({

        
        df<-df[which(df$country_region==input$country & df$kanton==input$kanton),]
        # dfcases<-coviddf[which(coviddf$geoRegion=="CH" & 
        #                            coviddf$altersklasse_covid19!="Unbekannt"),]

        dfcases<-coviddf[which(coviddf$geoRegion==input$kanton &
                                   coviddf$altersklasse_covid19!="Unbekannt"),]
        means<-dfcases %>% group_by(datum) %>% 
            summarise(entries=sum(entries),
                      entries1=sum(entries1))
        meanspop<-dfcases %>% group_by(dfcases$altersklasse_covid19) %>% 
            summarise(pop=mean(pop))
        kantonspop<-sum(meanspop$pop)
        means$inz_entries<-means$entries/kantonspop*100000
        means$inz_entries1<-means$entries1/kantonspop*100000
        
        gp1<-ggplot(dfcases,
                   aes(as.numeric(gsub("2020","",datum))))+xlab("Kalenderwoche")+
            scale_x_continuous(limits=c(0,max(as.numeric(gsub("2020","",dfcases$datum)))), 
                               expand = expand_scale(mult = 0, add = 0))+
            theme(legend.position = "top")
        
        if (input$inzidenz){
            gp1<-ggplot(means,
                        aes(as.numeric(gsub("2020","",datum))))+xlab("Kalenderwoche")+
                scale_x_continuous(limits=c(0,max(as.numeric(gsub("2020","",means$datum)))),
                                   expand = expand_scale(mult = 0, add = 0))+
                theme(legend.position = "top")
            
            
        gpcases<-gp1+geom_col(aes(y=inz_entries))+
            # facet_grid(rows = vars(altersklasse_covid19))
            theme(legend.title = element_blank())+
            ylab("positive tests / week per 100000")+
            theme(axis.title.x = element_blank(), axis.text.x = element_blank())
        
        gpdeaths<-gp1+geom_col(aes(y=inz_entries1))+
            theme(legend.title = element_blank())+
            ylab("deaths / week per 100000")
        
        
        
        }else{
            gpcases<-gp1+geom_col(aes(y=entries,
                                      fill=altersklasse_covid19)
                                  )+
                # facet_grid(rows = vars(altersklasse_covid19))
                theme(legend.title = element_blank())+
                ylab("positive tests / week")+
                theme(axis.title.x = element_blank(), axis.text.x = element_blank())
            
            gpdeaths<-gp1+geom_col(aes(y=entries1, 
                                       fill=altersklasse_covid19))+
                theme(legend.title = element_blank())+
                ylab("deaths / week")
            
        }
        
     
        
        
        
        gp2<-ggplot(data=df, aes(x=date,
                                color=daytype))+
            scale_color_manual(values=c("red","blue"))+
            scale_x_date( date_labels = "%W",
                          date_breaks = "10 weeks",
                         limits=as.Date(c("2020-01-01","2020-12-31")), expand = expand_scale(mult = 0, add = 0))+
                         # limits=as.Date(c("2020-01-01","2020-12-31", format="%W")))+
            # facet_grid(rows = vars(country_region_code))+
            ylab("deviation from baseline")+
            ggtitle(paste(input$country,max(df$date, na.rm=T), input$parameter))+
            theme(legend.position = "top", axis.title.x = element_blank())
        # 
         plot_grid(gp2+geom_point(aes(y=get(input$parameter))),
                   gpcases,gpdeaths, ncol=1, label_y = c(.9), labels = df$kanton[1] ,align = "v")
        # 
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
