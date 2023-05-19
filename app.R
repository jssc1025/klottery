library(shiny)
library(DT)
library(readxl)
library(dplyr)
library(combinat)
library(ggplot2)
library(shinythemes)

# Load the lottery data
lottery <- read_excel("lottery.xlsx")

# Calculate the total occurrences of each number
num_occur <- table(unlist(lottery[,2:7]))

# Calculate the probability of each number
num_prob <- num_occur / sum(num_occur)

num_rank <- rank(num_occur)


# Calculate the average winnings for 1st and 2nd place
avg_winning_1st <- mean(lottery$Winning)
avg_winning_2nd <- mean(lottery$'2nd_Winning')

# Calculate the win probability for the combination for 1st place
total_combinations_1st <- choose(45, 6)
win_prob_1st <- 1 / total_combinations_1st

# Calculate the win probability for the combination for 2nd place
total_combinations_2nd <- choose(45, 5) * choose(45 - 5, 1) # Account for the bonus number
win_prob_2nd <- 1 / total_combinations_2nd

# Save the probabilities, ranks, and expected winnings into a dataframe
num_df <- data.frame(Number = as.numeric(names(num_prob)),
                     Probability = as.numeric(num_prob),
                     Rank = as.numeric(num_rank))

# Select the top 6 numbers with the lowest ranks (least frequent)
top_numbers <- num_df %>% arrange(Rank) %>% head(6) %>% pull(Number)

ui <- fluidPage(theme = shinytheme("flatly"),
                titlePanel("Lottery Win Probability Calculator"),
                tabsetPanel(
                  tabPanel("Selected Numbers",
                           sidebarLayout(
                             sidebarPanel(
                               textOutput("topNumbersDisplay"),
                               numericInput("num1", "Number 1", value = 1, min = 1, max = 45),
                               numericInput("num2", "Number 2", value = 2, min = 1, max = 45),
                               numericInput("num3", "Number 3", value = 3, min = 1, max = 45),
                               numericInput("num4", "Number 4", value = 4, min = 1, max = 45),
                               numericInput("num5", "Number 5", value = 5, min = 1, max = 45),
                               numericInput("num6", "Number 6", value = 6, min = 1, max = 45),
                               actionButton("calculate", "Calculate Win Probability")
                             ),
                             mainPanel(
                               DTOutput("selectedTable"),
                               plotOutput("probabilityPlot"),
                               plotOutput("rankPlot")
                             )
                           )
                  ),
                  tabPanel("All Numbers",
                           fluidRow(
                             column(
                               width = 6,
                               dataTableOutput("allNumbersTable")
                             ),
                             column(
                               width = 6,
                               plotOutput("allNumbersProbabilityPlot"),
                               plotOutput("allNumbersRankPlot")
                             )
                           )
                           
                  ),
                  tabPanel("About",
                           uiOutput("aboutPage")
                  )
                )
)

server <- function(input, output) {
  
  output$aboutPage <- renderUI({
    includeMarkdown("about.Rmd")  # Replace with the path to your about.Rmd file if it's located in a different folder
  })
  
  output$topNumbersDisplay <- renderText({
    paste("Suggested numbers based on historical data: ", paste(top_numbers, collapse = ", "))
  })
  
  output$allNumbersTable <- DT::renderDataTable({
    num_df$`1st Place Winning` <- paste0("KRW ", format(round(win_prob_1st * avg_winning_1st * num_df$Probability, 4), nsmall = 4, big.mark = ","))
    num_df$`2nd Place Winning` <- paste0("KRW ", format(round(win_prob_2nd * avg_winning_2nd * num_df$Probability, 4), nsmall = 4, big.mark = ","))
    num_df$Probability <- paste0(round(num_df$Probability * 100, 2), "%")
    DT::datatable(num_df[, c("Number", "Probability", "Rank", "1st Place Winning", "2nd Place Winning")],
                  options = list(scrollY = "500px", scrollX = TRUE),
                  style = "bootstrap",
                  class = "compact")
  })
  
  
  
  
  
  
  
  
  
  
  
  output$allNumbersProbabilityPlot <- renderPlot({
    ggplot(num_df, aes(x = as.factor(Number), y = Probability)) +
      geom_bar(stat = "identity", fill = "blue", color = "black") +
      labs(x = "Number", y = "Probability (%)", title = "Bar Chart of Probabilities") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  output$allNumbersRankPlot <- renderPlot({
    ggplot(num_df, aes(x = as.factor(Number), y = Rank)) +
      geom_bar(stat = "identity", fill = "blue", color = "black") +
      labs(x = "Number", y = "Rank", title = "Bar Chart of Ranks") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  observeEvent(input$calculate, {
    selected_nums <- c(input$num1, input$num2, input$num3, input$num4, input$num5, input$num6)
    
    # Get the subset of the num_df for the selected numbers
    sel_df <- num_df[num_df$Number %in% selected_nums, ]
    
    # Calculate the expected winnings for the selected numbers
    sel_df$`1st Place Winning` <- paste0("KRW ", format(round(win_prob_1st * avg_winning_1st * sel_df$Probability, 2), nsmall = 2, big.mark = ","))
    sel_df$`2nd Place Winning` <- paste0("KRW ", format(round(win_prob_2nd * avg_winning_2nd * sel_df$Probability, 2), nsmall = 2, big.mark = ","))
    
    # Display the combination tables
    output$selectedTable <- renderDT({
      sel_df$Probability <- paste0(round(sel_df$Probability * 100, 2), "%")
      datatable(sel_df[, c("Number", "Probability", "Rank", "1st Place Winning", "2nd Place Winning")])
    })
    
    output$probabilityPlot <- renderPlot({
      ggplot(sel_df, aes(x = as.factor(Number), y = Probability)) +
        geom_bar(stat = "identity", fill = "blue", color = "black") +
        labs(x = "Number", y = "Probability", title = "Bar Chart of Probabilities") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
    })
    # Create a bar chart of the ranks for the selected numbers
    output$rankPlot <- renderPlot({
      ggplot(sel_df, aes(x = as.factor(Number), y = Rank)) +
        geom_bar(stat = "identity", fill = "blue", color = "black") +
        labs(x = "Number", y = "Rank", title = "Bar Chart of Ranks") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
    })
  })
}

shinyApp(ui = ui, server = server)

                         