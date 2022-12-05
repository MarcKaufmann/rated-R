source("utils.R")
# sapply(parseRequirements(), function(x) library(x[1], character.only = TRUE))
library(forecast)
library(tidyverse)
library(fable)
library(tsibble)
library(feasts)
library(shiny)
library(glue)

keywords <- get_keywords()
languages <- c("R", "Python", "Stata")

ui <- fluidPage(
    h1("R vs. Python vs. Stata in Google Trends"),
    sidebarLayout(
        sidebarPanel(
            selectInput("keyword", "", keywords)
        ),
        mainPanel(
            plotOutput(outputId = "trendplot", height = "600px")
        )
    )
)

server <- function(input, output) {
    output$trendplot <- renderPlot({
        req(input$keyword)
        levels <- gsub(" ", "_", paste(languages, input$keyword))
        gtrends_data_ts <- gtrends(input$keyword) %>% 
            mutate_at(vars(-date), as.numeric) %>% 
            pivot_longer(!date, names_to = "keyword", values_to = "hits") %>% 
            mutate(date = yearmonth(date)) %>%
            mutate(keyword = factor(keyword, levels = levels, labels = languages)) %>% 
            as_tsibble(index = date, key = keyword)
        forecast <- gtrends_data_ts %>%
            model(ARIMA(hits)) %>% 
            forecast(h = "24 months")
        gtrends_plot(gtrends_data_ts, forecast)
    })
}

shinyApp(ui = ui, server = server)
