require(shiny)
require(shinydashboard)
require(ggthemes)
require(hrbrthemes)
require(plotly)
theme_set(theme_ipsum())
require(magrittr)
require(wordcloud) #creative visualizations
require(tidytext) #text mining
require(stringr) #string manipulation
library(knitr) # for dynamic reporting
library(kableExtra) # create a nicely formated HTML table
library(formattable) # for the color_tile function
#
# BASIC CLEANING OF TEXT
fix_contaractions <-function(txt) {
  txt <- gsub("won't", "will not", txt)
  txt <- gsub("can't", "can not", txt)
  txt <- gsub("n't", " not", txt)
  txt <- gsub("'ll", " will", txt)
  txt <- gsub("'re", " are", txt)
  txt <- gsub("'ve", " have", txt)
  txt <- gsub("'m", " am", txt)
  txt <- gsub("'d", " would", txt)
  txt <- gsub("'s", " ", txt) # 's could be is or possessive: has no expansion
  txt <- gsub("[^a-zA-Z ]", " ", txt) #special characters
  return(txt)
}
#define some colors to use throughout
my_colors <- c("#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#D55E00")

theme_text <- function() {
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")
}
#
train_tweet <- updated_train %>% select(-1) %>% 
  mutate(full_word_count = unlist(lapply(str_split(text, " "), length)),
         target = plyr::mapvalues(factor(target), from = c(0, 1), to = c('Not-Related', 'Related')),
         clean_text = sapply(updated_train$text, fix_contaractions),
         clean_text = sapply(clean_text, tolower)) %>% 
  dplyr::rename(covid_related = target)
#
#get the frequency of words
#top 10 word counts
top_words <- train_tweet %>% tidytext::unnest_tokens(word, clean_text) %>% #'split words
  dplyr::anti_join(stop_words) %>% # remove stop words/ english-like words
  dplyr::count(word, sort = TRUE) %>%  #count ocurences
  dplyr::rename(num_words = n)


#
header = dashboardHeader(title = 'COVID-19 TWEET CLASSIFICATION', titleWidth = 400)
#
sidebar = dashboardSidebar(
  sidebarMenu(
    menuItem(text = 'Dashboard', tabName = 'dashboard'),
    menuItem(text = 'Analysis', tabName = 'analysis')
  )
)
#
body = dashboardBody(
  tabItems(
    tabItem(tabName = 'dashboard',
            fluidRow(
              valueBox(value = nrow(train_tweet), color = 'green', subtitle = 'No. of Tweets'),
              valueBox(value = sum(train_tweet$full_word_count), color = 'aqua', subtitle = 'Total no. of words'),
              valueBox(value = 2, color = 'blue', subtitle = 'Tweet Classes (COVID relationship)')),
            fluidRow(
              box(plotlyOutput('plot1'), width = 6),
              box(plotlyOutput('plot2'), width = 6)),
            fluidRow(valueBox(value = top_words[1, 1], color = 'maroon', subtitle = 'Frequently used Word'),
                     valueBox(value = round(mean(train_tweet$full_word_count), 2), color = 'olive', subtitle = 'Average No. of words'),
                     valueBox(value = top_words[13303, 1], color = 'purple', subtitle = 'Least word used'))),
    tabItem(tabName = 'analysis',
           fluidRow(dataTableOutput('plot3')))
  )
)
#
ui <- dashboardPage(header, sidebar, body)
#
server <- function(input, output) {
  output$plot1 <- renderPlotly({
    train_tweet %>% ggplot(aes(covid_related))+
      geom_bar(stat = 'count', fill = my_colors[3])+
      labs(x = 'COVID-19 Related Tweets', y = 'Value Counts', title = 'Frequency of COVID-19 Related Tweets')+
      theme(axis.title = element_text(size = 12),
            axis.title.x = element_text(face = 'bold', size = 14),
            axis.title.y = element_text(face = 'bold', size = 14),
            axis.text.x = element_text(face = 'bold', size = 12, angle = 0),
            axis.text.y = element_text(face = 'bold', size = 12))+
      coord_flip() -> p1
    ggplotly(p1)
  }) 
  #
  output$plot2 <- renderPlotly({
    p2 <- top_words[1:20,] %>% ggplot(aes(word, num_words))+
      geom_bar(stat = 'identity', fill=my_colors[4])+
      labs(x = 'Top words', y = 'Occurrence of the top words', title = 'Frequency Distribution of Top 20 Words')+
      theme(axis.title = element_text(size = 12),
            axis.title.x = element_text(face = 'bold', size = 14),
            axis.title.y = element_text(face = 'bold', size = 14),
            axis.text.x = element_text(face = 'bold', size = 12, angle = 45),
            axis.text.y = element_text(face = 'bold', size = 12))
    ggplotly(p2)
  })
  output$plot3 <- renderDataTable({
    top_20_words <- top_words[1:20,] %>%
      mutate(num_words = color_bar("lightblue")(num_words),
             word = color_tile("lightpink","lightpink")(word)) %>% tibble::tibble() %>% 
      kable("html", escape = FALSE, align = "c", caption = "Tweets With Highest Word Count") %>%
      kable_styling( full_width = FALSE)
    

  })
}
#
shinyApp(ui, server)