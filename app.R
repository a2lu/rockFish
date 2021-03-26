#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(stringr)
library(ggplot2)
library(shinythemes)
library(dplyr)
library(shinyWidgets)
library(shinythemes)


fpath1 = paste0(getwd(),"/data2.csv")
dat= read.csv(fpath1)
dat$year = strtoi(str_split_fixed(dat$time, "-", 3)[,1])
dat$month = strtoi(str_split_fixed(dat$time, "-", 3)[,2])
dat = dat[2:dim(dat)[1],]
dat_n = filter(dat,strata == "N")
dat_c = filter(dat,strata == "C")
dat_s = filter(dat,strata == "S")
fishCount_n = c()
fishCount_c = c()
fishCount_s = c()

for (i in 1990:2018) {
    if (dim(filter(dat_n, year == i))[1]!=0) {
        fishCount_n = append(fishCount_n,mean(filter(dat_n, year == i)$catch))
    }
    if (dim(filter(dat_c, year == i))[1]!=0) {
        fishCount_c = append(fishCount_c,mean(filter(dat_c, year == i)$catch))
    }
    if (dim(filter(dat_s, year == i))[1]!=0) {
        fishCount_s = append(fishCount_s,mean(filter(dat_s, year == i)$catch))
    }
}
year_n = unique(dat_n$year)
year_c = unique(dat_c$year)
year_s = unique(dat_s$year)

dat$bottom_depth = as.integer(dat$bottom_depth)

Years <- append(append(year_n,year_c),year_s)
Amount <- append(append(fishCount_n,fishCount_c),fishCount_s)
Rating <- rep(c('North','Central','South'),times=c(length(year_n),length(year_c),length(year_s)))

df1 = data.frame(Years, Amount, Rating)


ui1 <- fluidPage(
    theme = shinytheme("slate"),
    sidebarLayout(
        sidebarPanel(
            sliderTextInput(inputId = "range", 
                            label = "Year Range", 
                            choices = unique(dat$year), 
                            selected = range(unique(dat$year)), 
                            grid = TRUE),
            selectInput(inputId = "rat",
                        label = "Choose the rating",
                        choices = c("North", "Central", "South"))
        ),
        mainPanel(plotOutput("graph1", hover = "plot_hover"),
                  verbatimTextOutput("info"))
    )
)


server1 = function(input, output) {
    
    df_final <- reactive({
        if(input$rat %in% c("North", "Central", "South")) {
            df1 %>% filter(between(Years,input$range[1],input$range[2]), Rating == input$rat) %>% 
                group_by(Years) %>%
                summarise(Average_Catch = sum(Amount))
        }
        
    })
    
    output$graph1 <- renderPlot({
        if(input$rat == "Central") {
            ggplot(df_final(), aes(x=Years)) +
                geom_smooth(aes(y=Average_Catch),stat='identity', col="blue") + 
                geom_point(aes(y=Average_Catch)) +
                theme(
                    axis.title.y=element_text(color="blue"),
                    axis.text.y.left=element_text(color="blue"),
                )
        } else if (input$rat == "North") {
            ggplot(df_final(), aes(x=Years)) +
                geom_smooth(aes(y=Average_Catch),stat='identity',col="blue") +
                geom_point(aes(y=Average_Catch))+
                theme(
                    axis.title.y=element_text(color="blue"),
                    axis.text.y=element_text(color="blue"),
                )
        } else if (input$rat == "South") {
            ggplot(df_final(), aes(x=Years)) +
                geom_smooth(aes(y=Average_Catch),stat='identity',col="blue") +
                geom_point(aes(y=Average_Catch))+
                theme(
                    axis.title.y.left=element_text(color="blue"),
                    axis.text.y.left=element_text(color="blue")
                )
        }
        
        
    })
    output$info <- renderText({
        paste0("Year = ", input$plot_hover$x, "\nRock Fish Count = ", input$plot_hover$y)
    })
}

shinyApp(ui1, server1)
