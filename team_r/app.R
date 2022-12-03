source("utils.R")
sapply(parseRequirements(), function(x) library(x[1], character.only = TRUE))

keywords <- get_keywords()

ui <- fluidPage(
    titlePanel("R vs. Python vs. Stata"),
    sidebarLayout(
        sidebarPanel(
            selectInput("keyword", "Keyword", keywords)
        ),
        mainPanel(
            plotOutput(outputId = "trendplot", height = "600px")
        )
    )
)

server <- function(input, output) {
    output$trendplot <- renderPlot({
        req(input$keyword)
        gtrends_data_ts <- gtrends(input$keyword) %>% 
            mutate_at(vars(-date), as.numeric) %>% 
            pivot_longer(!date, names_to = "keyword", values_to = "hits") %>% 
            mutate(date = yearmonth(date)) %>% 
            as_tsibble(index = date, key = keyword)
        gtrends_data_ts %>%
            model(ARIMA(hits)) %>% 
            forecast(h = "24 months") %>% 
            autoplot(gtrends_data_ts) + coord_cartesian(xlim = c(yearmonth("2018 Jan"), NA)) + theme_bw()
    })
}

shinyApp(ui = ui, server = server)
