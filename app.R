#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

#install and load necessary packages
#if packages already installed and loaded, skip.
#But if packages installed and not loaded then load
#If packages not installed, install and load.
# packages = c("shiny", "dplyr", "shinydashboard")
# 
# 
# ## load or install packages
# package.check <- lapply(
#     packages,
#     FUN = function(x) {
#         if (!require(x, character.only = TRUE)) {
#             install.packages(x, dependencies = TRUE)
#             library(x, character.only = TRUE)
#         }#if
#     }
# )
# install.packages("shiny")
# install.packages("dplyr")
# install.packages("shinydashboard")
# install.packages("ggplot2")
# install.packages("gridExtra")
# install.packages("readxl")
# install.packages("plotly")
library(plotly)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(shiny)
library(shinydashboard)
library(dplyr)


data<-read.csv2("proteomic2.csv", header = TRUE, sep = ";", dec = ",")
translatome_df <- read.csv2("translatome.csv", header = TRUE, sep = ";", dec = ",")
transcriptDDf <- read.csv2("transcriptomes.csv", header = TRUE, sep = ";", dec = ",")
#transcriptDDfCtr <- readxl::read_excel("transcriptomesCtr.xlsx")
abDf<-read.csv2("ab_titre.csv", header = TRUE, sep = ";", dec = ",")#, header = TRUE, sep = ",")
abDfnonSevereIgG <- dplyr::filter(abDf, IgGPresence == "+" & Severity == 0) %>%
  dplyr::select("IgG", "Days.after.symptoms.onset")

abDfnonSevereIgG[, 1] <- log(abDfnonSevereIgG[, 1], 2)

abDfnonSevereIgG2 <- cut(abDfnonSevereIgG$`Days.after.symptoms.onset`, c(seq(1, 26, by = 6), 26), include.lowest = TRUE)

#group and calculate mean
abDfnonSevereIgG <- aggregate(IgG ~ abDfnonSevereIgG2, abDfnonSevereIgG, mean)
#abDfnonSevereIgG <- aggregate(abDfnonSevereIgG[,1:2 ], list(abDfnonSevereIgG$"Days.after.symptoms.onset"), mean)

#IgM
abDfnonSevereIgM <- dplyr::filter(abDf, IgMPresence == "+" & Severity == 0)%>%
  select("IgM", "Days.after.symptoms.onset")

abDfnonSevereIgM[, 1] <- log(abDfnonSevereIgM[, 1], 2)

abDfnonSevereIgM2 <- cut(abDfnonSevereIgM$`Days.after.symptoms.onset`, c(seq(1, 26, by = 6), 26), include.lowest = TRUE)

#group and calculate mean
abDfnonSevereIgM <- aggregate(IgM ~ abDfnonSevereIgM2, abDfnonSevereIgM, mean)



abDfSevere<-dplyr::filter(abDf, Severity==1)%>%
  select("IgG", "IgM", "Days.after.symptoms.onset")

#group and calculate mean
abDfSevere <- aggregate(abDfSevere[,1:2 ], list(abDfSevere$"Days.after.symptoms.onset"), mean)




data2<-select(data,"GeneSymbol", "X2hrs.infection_1",	"X2hrs.infection_2",
              "X2hrs.infection_3",	"X6hrs.infection_1",	"X6hrs.infection_2",
              "X6hrs.infection_3",	"X10hrs.infection_1",	"X10hrs.infection_2",
              "X10hrs.infection_3",	"X24hrs.infection_1",	"X24hrs.infection_2",
              "X24hrs.infection_3"
)

data12<-select(data,"GeneSymbol", "Fold_2h", "Fold_6h", "Fold_10h", "Fold_24h")

###############Global


library(shiny)

# Define UI for application that draws a histogram
ui <- dashboardPage(
    #Application title
    dashboardHeader(title = "CoV9db",
                    titleWidth = 300),
    
    # Sidebar with a slider input for number of bins 
    dashboardSidebar(
      width=300,
      
      sidebarMenu(
        # Setting id makes input$tabs give the tabName of currently-selected tab
        id = "tabs",
        menuItem("Home", tabName = "home"),
        menuItem("Proteomes",tabName = "Proteomes"),
        menuItem("Transcriptomes",tabName = "Transcriptomes"),
        menuItem("Translatomes",tabName = "Translatomes"),#,
        menuItem("Biomakers", tabName = "Biomarkers"),
        menuItem("Immune response",tabName = "Immunoglobulins"),
        menuItem("References",tabName = "REF")
        #menuItem("Immunoglobulin",tabName = "Immunoglobulins"),
        
        #menuItem("Test kits sensitivity",tabName = "Testkits effciency"),
        #menuItem("Variants",tabName = "Variants"),
        #menuItem("Charts", icon = icon("bar-chart-o"),
                 #menuSubItem("Sub-item 1", tabName = "subitem1"),
                 #menuSubItem("Sub-item 2", tabName = "subitem2")
        #)
        )

        
        
    ),#),
    
    # Show a plot of the generated distribution
    
    
    dashboardBody(
      tabItems(
        
        tabItem(tabName = "home",
                #textOutput("homePage")
                tags$h1(strong("CoV9db")),
                tags$h4(
                "CoV9db serves as a one stop platform to understanding the host biomolecular changes
                that occurs upon SARS-CoV-2 infection. It contains interactive graphs that displays dynamic
                changes of various biomolecules  at different time point of infection based on current published data.")
                
            ),
        
        
        # tabItem(tabName = "REF",
        #        
        #         textOutput("REF1"),
        #         textOutput("REF2"),
        #         textOutput("REF3"),
        #         textOutput("REF4"),
        #         textOutput("REF5"),
        #         textOutput("REF6"),
        #         textOutput("REF7"),
        #         textOutput("REF8"),
        #         textOutput("REF9"),
        #         textOutput("REF10")
        #         
        #         
        # ),
        
        
        tabItem(tabName = "Immunoglobulins",
                tabsetPanel(type = "pills",
                            tabPanel("Line Plot",
                                     
                                     fluidRow(
                                       selectInput("immune","Select antibody",
                                                   choices=c("IgG", "IgM", "IgA",
                                                             "IgG & IgM"),
                                                   selected ='IgG'
                                                   ),
                                       plotlyOutput("plotAb", height = 500))
                            )#,#tabpanel
                            # tabPanel("Line plot",
                            #          #fluidRow(
                            #          #box(
                            #          fluidRow(
                            #            selectInput("genename2","Select proteins",choices=data2$GeneSymbol),
                            #            plotlyOutput("plot2", height = 500))
                            # ),#tabpanel
                            
                            # tabPanel("Heat map",
                            #          
                            # ),#tabpanel
                            # 
                            # tabPanel("Volcano plot",
                            #          
                            #          
                            # )#tabpanel
                            
                )#tabsetpanel
        ),
        
      tabItem(tabName = "Proteomes",
              tabsetPanel(type = "pills",
                          tabPanel("Box plot",
  
               fluidRow(
                  selectInput("genename","Select proteins",
                              choices=data2$GeneSymbol, selectize = FALSE),
                   plotlyOutput("plot1", height = 500))
            ),#tabpanel
            tabPanel("Line plot",
                     #fluidRow(
                     #box(
                     fluidRow(
                       selectInput("genename2","Select proteins",
                                   choices=data2$GeneSymbol, selectize = FALSE),
                       plotlyOutput("plot2", height = 500))
            ),#tabpanel
            
            tabPanel("Volcano plot",
                     fluidRow(
                       selectInput("genenameVolc","Select time point",
                                   choices=c("2hrs after infection", "6hrs after infection",
                                             "10hrs after infection", "24hrs after infection"),
                                   selected = "2hrs after infection"
                                   ),
                       plotlyOutput("plotVolc", height = 700))
                     
                     
            )#tabpanel
            
               )#tabsetpanel
      ),
      
      tabItem(tabName = "Transcriptomes",
              tabsetPanel(type = "pills",
                          tabPanel("Bar Chart",
                                   
                                   fluidRow(
                                     selectInput("genename3", "Select Genes", choices=transcriptDDf$Gene, selectize = FALSE),
                                                 #selected = transcriptDDf$Gene[1]# & transcriptDDf$Gene[3],
                                                 #multiple = TRUE
                                                 #),
                                     plotlyOutput("plot3", height = 500))
                          ),#tabpanel
                          tabPanel("Volcano Plot",
                                   #fluidRow(
                                   #box(
                                   fluidRow(
                                     selectInput("genename4","Select proteinss",choices=transcriptDDf$Gene, selectize = FALSE),
                                     plotlyOutput("plot4", height = 500))
                          )
                          
              )#tabsetpanel
      ),#tabItem Transcriptomes
      
      tabItem(tabName = "Translatomes",
              tabsetPanel(type = "pills",
                          tabPanel("Box plot",
                                   
                                   fluidRow(
                                     selectInput("genenameT","Select proteins",
                                                 choices=translatome_df$GeneSymbol1, selectize = FALSE),
                                     plotlyOutput("plot5", height = 500))
                          ),#tabpanel
                          tabPanel("Line plot",
                                   #fluidRow(
                                   #box(
                                   fluidRow(
                                     selectInput("genenameT2","Select proteins",
                                                 choices=translatome_df$GeneSymbol1, selectize = FALSE),
                                     plotlyOutput("plot6", height = 500))
                          ),#tabpanel

                          tabPanel("Volcano plot",
                                   fluidRow(
                                     selectInput("genenameVolc2","Select time point",
                                                 choices=c("2hrs after infection", "6hrs after infection",
                                                           "10hrs after infection", "24hrs after infection"),
                                                 selected = "2hrs after infection"
                                     ),
                                     plotlyOutput("plotVolc2", height = 700))


                          )#tabpanel
                          
              )#tabsetpanel
      )#
      
      )
      
      
                 
     
      
    )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  #output$homePage <- renderText({ input$homePage })
  
  
  output$plotAb<-renderPlotly({
    if (input$immune == "IgG") {
      
      plotAb <- plot_ly(abDfnonSevereIgG, y = abDfnonSevereIgG$IgG, x = abDfnonSevereIgG$abDfnonSevereIgG2, type = 'scatter', mode = 'lines',
                        showlegend = F)
      
    }
    
    else if (input$immune == "IgM"){
      plotAb <- plot_ly(abDfnonSevereIgM, y = abDfnonSevereIgM$'IgM', x = abDfnonSevereIgM$abDfnonSevereIgM2, type = 'scatter', mode = 'lines',
                        showlegend = F)
      
    }
    
    
  } )
  
  
  output$plot1<-renderPlotly({
    
    data3<-dplyr::filter(data2, GeneSymbol == input$genename)%>%
      select("X2hrs.infection_1",	"X2hrs.infection_2",
             "X2hrs.infection_3",	"X6hrs.infection_1",	"X6hrs.infection_2",
             "X6hrs.infection_3",	"X10hrs.infection_1",	"X10hrs.infection_2",
             "X10hrs.infection_3",	"X24hrs.infection_1",	"X24hrs.infection_2",
             "X24hrs.infection_3")
    
    
    data4<-gather(data3, "Conditions")%>%
      dplyr::rename(Fold_change=value)
    
    data4$Conditions<- sub('_.*',"",data4$Conditions)
    
    ax <- list(
      type = "category",
      categoryorder = "array",
      categoryarray = c("2hrs infection", "6hrs infection",
                        "10hrs infection", "24hrs infection")
    )
    
    p <- plot_ly(data4, y = ~Fold_change, showlegend = F)
    #alpha = 0.1, boxpoints = "suspectedoutliers")
    plot1 <- p %>% add_boxplot(x = ~Conditions)%>%
      layout(xaxis = ax)
  
    
 } )
  
  
  output$plotVolc<-renderPlotly({
    
    ####################################
    volc_df <- select(data, "GeneSymbol", "Fold_2h", "Fold_6h", "Fold_10h", "Fold_24h",
                      "Pvalue", "Pvalue_6h", "Pvalue_10h", "Pvalue_24h")
    volc_df$Pvalue<- -log(volc_df$Pvalue, base = 10)
    volc_df$Pvalue_6h<- -log(volc_df$Pvalue_6h, base = 10)
    volc_df$Pvalue_10h<- -log(volc_df$Pvalue_10h, base = 10)
    volc_df$Pvalue_24h<- -log(volc_df$Pvalue_24h, base = 10)
    
    # add a grouping column; default value is "not significant"
    volc_df["group2h"] <- "NotSignificant"
    volc_df["group6h"] <- "NotSignificant"
    volc_df["group10h"] <- "NotSignificant"
    volc_df["group24h"] <- "NotSignificant"
    
    # for our plot, we want to highlight 
    # FDR < 0.05 (significance level)
    # Fold Change > 1.5
    
    # change the grouping for the entries with significance but not a large enough Fold change
    volc_df[which(volc_df['Pvalue'] < 0.05 & abs(volc_df['Fold_2h']) < 1.5 ),"group2h"] <- "Significant"
    volc_df[which(volc_df['Pvalue_6h'] < 0.05 & abs(volc_df['Fold_6h']) < 1.5 ),"group6h"] <- "Significant"
    volc_df[which(volc_df['Pvalue_10h'] < 0.05 & abs(volc_df['Fold_10h']) < 1.5 ),"group10h"] <- "Significant"
    volc_df[which(volc_df['Pvalue_24h'] < 0.05 & abs(volc_df['Fold_24h']) < 1.5 ),"group24h"] <- "Significant"
    
    # change the grouping for the entries a large enough Fold change but not a low enough Pvalue
    volc_df[which(volc_df['Pvalue'] > 0.05 & abs(volc_df['Fold_2h']) > 1.5 ),"group2h"] <- "FoldChange"
    volc_df[which(volc_df['Pvalue_6h'] > 0.05 & abs(volc_df['Fold_6h']) > 1.5 ),"group6h"] <- "FoldChange"
    volc_df[which(volc_df['Pvalue_10h'] > 0.05 & abs(volc_df['Fold_10h']) > 1.5 ),"group10h"] <- "FoldChange"
    volc_df[which(volc_df['Pvalue_24h'] > 0.05 & abs(volc_df['Fold_24h']) > 1.5 ),"group24h"] <- "FoldChange"
    
    # change the grouping for the entries with both significance and large enough fold change
    volc_df[which(volc_df['Pvalue'] < 0.05 & abs(volc_df['Fold_2h']) > 1.5 ),"group2h"] <- "Significant&FoldChange"
    volc_df[which(volc_df['Pvalue_6h'] < 0.05 & abs(volc_df['Fold_6h']) > 1.5 ),"group6h"] <- "Significant&FoldChange"
    volc_df[which(volc_df['Pvalue_10h'] < 0.05 & abs(volc_df['Fold_10h']) > 1.5 ),"group10h"] <- "Significant&FoldChange"
    volc_df[which(volc_df['Pvalue_24h'] < 0.05 & abs(volc_df['Fold_24h']) > 1.5 ),"group24h"] <- "Significant&FoldChange"
    ##########################################
    
    if (input$genenameVolc == "2hrs after infection") {
      plotVolc <- plot_ly(volc_df, x = volc_df$Fold_2h, y = volc_df$Pvalue, text =volc_df$GeneSymbol, mode = "markers", color =volc_df$group2h)
    }
    
    else if (input$genenameVolc == "6hrs after infection") {
      plotVolc <- plot_ly(volc_df, x = ~Fold_6h, y = ~Pvalue_6h, text =~GeneSymbol, mode = "markers", color =~group6h)
    }

    else if (input$genenameVolc == "10hrs after infection") {
      plotVolc <- plot_ly(volc_df, x = ~Fold_10h, y = ~Pvalue_10h, text =~GeneSymbol, mode = "markers", color =~group10h)
    }

    else {
      plotVolc <- plot_ly(volc_df, x = ~Fold_24h, y = ~Pvalue_24h, text =~GeneSymbol, mode = "markers", color =~group24h)
    }
    
    #plot_ly(volc_df, x = ~Fold_2h, y = ~Pvalue, text =~GeneSymbol, mode = "markers", color =~group2h)# %>% 
      # layout(list(title ="Volcano Plot"),
      #       annotations = list())
    
    
  } )
  
  
  output$plot2<-renderPlotly({
    
    data3<-dplyr::filter(data12, GeneSymbol == input$genename2)%>%
      select("Fold_2h", "Fold_6h", "Fold_10h", "Fold_24h")%>%
      rename("2hrs after infection"="Fold_2h","6hrs after infection"= "Fold_6h",
             "10hrs after infection"="Fold_10h","24hrs after infection"= "Fold_24h")
    
    
    data4<-gather(data3, "Conditions")%>%
      dplyr::rename(Fold_change=value)
    
    #data4$Conditions<- sub('_.*',"",data4$Conditions)
    ax <- list(
      type = "category",
      categoryorder = "array",
      categoryarray = c("2hrs after infection", "6hrs after infection",
                        "10hrs after infection", "24hrs after infection")
    )
    
    plot2 <- plot_ly(data4, y = ~Fold_change, x = ~Conditions, type = 'scatter', mode = 'lines',
                     showlegend = F)%>%
      layout(xaxis = ax)
    
    
  } )
  
  
  output$plot3<-renderPlotly({
    
    data_trans<-dplyr::filter(transcriptDDf, Gene == input$genename3)
    colnames(data_trans)[2]<-data_trans$Gene[1] #%>%
    data_trans<-data_trans[1,2:3]
    #data_trans<-rbind(data_trans, transcriptDDfCtr)
    #
    data_trans<-gather(data_trans, "Conditions")%>%
       dplyr::rename(Fold_change=value)


    #data4$Conditions<- sub('_.*',"",data4$Conditions)
    ax <- list(
      type = "category",
      categoryorder = "array",
      categoryarray = c("Control", "")
    )

    plot2 <- plot_ly(data_trans, y = ~Fold_change, x = ~Conditions, type = 'bar',
                     showlegend = F)%>%
      layout(xaxis = ax)
    
    
  } )
  
  
  
  output$plot4<-renderPlotly({
    transcriptDDf$Pvalue<- -log(transcriptDDf$Pvalue, base = 10)
    
    plot4 <- plot_ly(transcriptDDf, x = ~log2FoldChange, y = ~Pvalue, text = ~Gene)#, mode = "markers", color =volc_df$group2h)
    
    
  } )
  
  
  output$plot5<-renderPlotly({
    
    data<-select(translatome_df,"GeneSymbol1", "X2hrs.infection_1",	"X2hrs.infection_2",
                  "X2hrs.infection_3",	"X6hrs.infection_1",	"X6hrs.infection_2",
                  "X6hrs.infection_3",	"X10hrs.infection_1",	"X10hrs.infection_2",
                  "X10hrs.infection_3",	"X24hrs.infection_1",	"X24hrs.infection_2",
                  "X24hrs.infection_3"
    )
    
    data<-dplyr::filter(data, GeneSymbol1 == input$genenameT)%>%
      select("X2hrs.infection_1",	"X2hrs.infection_2",
             "X2hrs.infection_3",	"X6hrs.infection_1",	"X6hrs.infection_2",
             "X6hrs.infection_3",	"X10hrs.infection_1",	"X10hrs.infection_2",
             "X10hrs.infection_3",	"X24hrs.infection_1",	"X24hrs.infection_2",
             "X24hrs.infection_3")
    
    
    data4<-gather(data, "Conditions")%>%
      dplyr::rename(Fold_change=value)
    
    data4$Conditions<- sub('_.*',"",data4$Conditions)
    
    ax <- list(
      type = "category",
      categoryorder = "array",
      categoryarray = c("2hrs infection", "6hrs infection",
                        "10hrs infection", "24hrs infection")
    )
    
    p <- plot_ly(data4, y = ~Fold_change, showlegend = F)
    #alpha = 0.1, boxpoints = "suspectedoutliers")
    plot5 <- p %>% add_boxplot(x = ~Conditions)%>%
      layout(xaxis = ax)
    
    
    
  } )
  
  
  output$plot6<-renderPlotly({
    
    data22<-select(translatome_df,"GeneSymbol1", "Fold_2h", "Fold_6h", "Fold_10h", "Fold_24h")
    
    data3<-dplyr::filter(data22, GeneSymbol1 == input$genenameT2)%>%
      select("Fold_2h", "Fold_6h", "Fold_10h", "Fold_24h")%>%
      rename("2hrs after infection"="Fold_2h","6hrs after infection"= "Fold_6h",
             "10hrs after infection"="Fold_10h","24hrs after infection"= "Fold_24h")
    
    
    data4<-gather(data3, "Conditions")%>%
      dplyr::rename(Fold_change=value)
    
    #data4$Conditions<- sub('_.*',"",data4$Conditions)
    ax <- list(
      type = "category",
      categoryorder = "array",
      categoryarray = c("2hrs after infection", "6hrs after infection",
                        "10hrs after infection", "24hrs after infection")
    )
    
    plot2 <- plot_ly(data4, y = ~Fold_change, x = ~Conditions, type = 'scatter', mode = 'lines',
                     showlegend = F)%>%
      layout(xaxis = ax)
    
    
  } )
    
  
  
  
  output$plotVolc2<-renderPlotly({
    
    ####################################
    volc_df <- select(translatome_df, "GeneSymbol1", "Fold_2h", "Fold_6h", "Fold_10h", "Fold_24h",
                      "Pvalue", "Pvalue_6h", "Pvalue_10h", "Pvalue_24h")
    volc_df$Pvalue<- -log(volc_df$Pvalue, base = 10)
    volc_df$Pvalue_6h<- -log(volc_df$Pvalue_6h, base = 10)
    volc_df$Pvalue_10h<- -log(volc_df$Pvalue_10h, base = 10)
    volc_df$Pvalue_24h<- -log(volc_df$Pvalue_24h, base = 10)
    
    # add a grouping column; default value is "not significant"
    volc_df["group2h"] <- "NotSignificant"
    volc_df["group6h"] <- "NotSignificant"
    volc_df["group10h"] <- "NotSignificant"
    volc_df["group24h"] <- "NotSignificant"
    
    # for our plot, we want to highlight 
    # FDR < 0.05 (significance level)
    # Fold Change > 1.5
    
    # change the grouping for the entries with significance but not a large enough Fold change
    volc_df[which(volc_df['Pvalue'] < 0.05 & abs(volc_df['Fold_2h']) < 1.5 ),"group2h"] <- "Significant"
    volc_df[which(volc_df['Pvalue_6h'] < 0.05 & abs(volc_df['Fold_6h']) < 1.5 ),"group6h"] <- "Significant"
    volc_df[which(volc_df['Pvalue_10h'] < 0.05 & abs(volc_df['Fold_10h']) < 1.5 ),"group10h"] <- "Significant"
    volc_df[which(volc_df['Pvalue_24h'] < 0.05 & abs(volc_df['Fold_24h']) < 1.5 ),"group24h"] <- "Significant"
    
    # change the grouping for the entries a large enough Fold change but not a low enough Pvalue
    volc_df[which(volc_df['Pvalue'] > 0.05 & abs(volc_df['Fold_2h']) > 1.5 ),"group2h"] <- "FoldChange"
    volc_df[which(volc_df['Pvalue_6h'] > 0.05 & abs(volc_df['Fold_6h']) > 1.5 ),"group6h"] <- "FoldChange"
    volc_df[which(volc_df['Pvalue_10h'] > 0.05 & abs(volc_df['Fold_10h']) > 1.5 ),"group10h"] <- "FoldChange"
    volc_df[which(volc_df['Pvalue_24h'] > 0.05 & abs(volc_df['Fold_24h']) > 1.5 ),"group24h"] <- "FoldChange"
    
    # change the grouping for the entries with both significance and large enough fold change
    volc_df[which(volc_df['Pvalue'] < 0.05 & abs(volc_df['Fold_2h']) > 1.5 ),"group2h"] <- "Significant&FoldChange"
    volc_df[which(volc_df['Pvalue_6h'] < 0.05 & abs(volc_df['Fold_6h']) > 1.5 ),"group6h"] <- "Significant&FoldChange"
    volc_df[which(volc_df['Pvalue_10h'] < 0.05 & abs(volc_df['Fold_10h']) > 1.5 ),"group10h"] <- "Significant&FoldChange"
    volc_df[which(volc_df['Pvalue_24h'] < 0.05 & abs(volc_df['Fold_24h']) > 1.5 ),"group24h"] <- "Significant&FoldChange"
    ##########################################
    
    if (input$genenameVolc2 == "2hrs after infection") {
      plotVolc <- plot_ly(volc_df, x = volc_df$Fold_2h, y = volc_df$Pvalue, text =volc_df$GeneSymbol1, mode = "markers", color =volc_df$group2h)
    }
    
    else if (input$genenameVolc2 == "6hrs after infection") {
      plotVolc <- plot_ly(volc_df, x = ~Fold_6h, y = ~Pvalue_6h, text =~GeneSymbol1, mode = "markers", color =~group6h)
    }
    
    else if (input$genenameVolc2 == "10hrs after infection") {
      plotVolc <- plot_ly(volc_df, x = ~Fold_10h, y = ~Pvalue_10h, text =~GeneSymbol1, mode = "markers", color =~group10h)
    }
    
    else {
      plotVolc <- plot_ly(volc_df, x = ~Fold_24h, y = ~Pvalue_24h, text =~GeneSymbol1, mode = "markers", color =~group24h)
    }
    
    #plot_ly(volc_df, x = ~Fold_2h, y = ~Pvalue, text =~GeneSymbol1, mode = "markers", color =~group2h)# %>% 
    # layout(list(title ="Volcano Plot"),
    #       annotations = list())
    
    
  } )
  
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
