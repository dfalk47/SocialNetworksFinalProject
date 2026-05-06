# Shiny App
# Section 1. First install and activate all your required packages.

library(shiny)
library(bslib)

library(tidyverse)
library(igraph)
library(tidygraph)
library(ggraph)

library(visNetwork)




# Section 2. Design the site in the UI section (US = User Interface). This is where we define how everything looks and
# how people can use the app.

ui <-fluidPage(

  titlePanel("NFL Head Coaches Network"),

  page_sidebar(
    title = "By: Jay Falk",
    sidebar = sidebar ("Menu options"),
    card(
      card_header("Introduction to the NFL Coaches Network"), "My network is of the NFL head coaches who were active during the 2025 season. An edge between coaches occurs when they spent at least one year on an NFL staff at the same time and the edges are weighted by the amount of years the two coaches worked together on a staff. With this network, I hoped to understand if certain centrality measures could be used to predict how successful a head coach is in the NFL. To measure this success, I created a variable called weighted_win which takes a coach's winning percentage as a head coach and multiplies it by the amount of years they have been a head coach. This variable was compared on a bargraph to a few different centrality measures to search for a correlation.
Using the network toggle, you can view the full network visualization or one thresholded to only include edges with a weight greater than four to see how certain clusters are formed. After looking at the thresholded network, you can see what the coaches who appear on this visualization have for a weighted_win to see if having strong ties helps to win more games.
By toggling between the eigenvector and degree centrality measures, it becomes clear that neither measure seems to capture how successful a coach will be as there are some coaches with high centrality degrees with low weighted_win and vice versa. This finding highlights the fact that there are many different reasons a coach could be successful. For example, some coaches with high degrees of centrality are journeymen, meaning that the reason they have high degrees of centrality is because they have bounced around many teams. Usually a coach who bounces around a lot does so because they are fired often, meaning they aren't successful. Two coaches that fit this category are Dan Quinn and Raheem Morris. On the other hand, coaches such as Andy Reid and Pete Carroll who spent a long time with one team have low centrality degrees because they were not moving around teams constantly.
"),
    card(
      card_header("Data Gathering Process"), "All of the information I gathered came from profootballreference.com, a credible and up to date website containing a wide array of football data. On this website, I searched for each NFL head coach who was active during the 2025 season, and collected a few different pieces of information which were separated into two google sheets. In one google sheet, I listed each head coach and every NFL team they worked for as an assistant or a head coach, and from what years they worked at each team. On the other google sheet I listed the node attributes. Every coach was given a unique label 1-32, and then their name, winning percentage as a head coach, age, and length of time as a head coach were recorded. Once all of this data had been collected, I moved on to collecting the edges data. In order to collect this data, I went on to the google sheet I had made with every head coach and their coaching history and searched by each NFL team. When I searched by team, I was able to see which coaches had worked for the team and noted any time two coaches worked for a team at the same time and took account of how long they worked for this team together. I used the unique label from my node data for the two coaches and put one in a source column and the other in the target column. I also created a weight category in which the length of their overlap was placed. If coaches worked together on multiple staffs, which happened a few times, these tenures were added together and the weight category was the sum of their whole time working together.
 "

      ),


    card(card_header("Coaching Network"),
         selectInput("size",
                     "Choose a Threshold Measure",
                     choices = list("No Threshold" = "Full_graph",
                                    "Threshold" = "Edge Weight Above 4"),
                     selected = "Full_graph"),
         plotOutput("Full_network"),
         height = "1000px"
    ),

    card(
      card_header("Centrality Comparison"),
      radioButtons("plot_toggle", "Select Plot View:",
                   choices = c("Degree Centrality" = "deg",
                               "Eigenvector Centrality" = "btwn"),
                   selected = "deg",
                   inline = TRUE),
      plotOutput("centrality_plot", height = "500px")
    )


    )


)

# Section 2. The server section defines how our app works. Here's where we will put all the network analysis.

server <- function(input, output) {

  # CARD 1

  output$ourVariable <- renderText({
    paste("Our selected option is", input$select)
  })

# let's create a simple example network with 10 nodes and calulate the degree centrality

  #Full Network Card

  network <- reactive({
    nodes <- read.csv("coaches_nodes.csv")
    edges <- read.csv("coaches_edges.csv")

    net_coaches <- graph_from_data_frame(d = edges, vertices = nodes, directed = FALSE)

    net_coaches <- as_tbl_graph(net_coaches) |>
      activate(nodes) |>
      mutate(degree = centrality_degree(),
             weighted_win=Win.Perct * Years.As.A.Head.Coach)

    net_coaches
  })

output$Full_network <- renderPlot({
  net_coaches <- network()


  if(input$size == "Edge Weight Above 4") {
    net_coaches <- net_coaches |>
      activate(edges) |>
      filter(weight > 4) |>
      activate(nodes) |>
      mutate(degree = centrality_degree()) |>
      filter(degree > 0)
  }


  ggraph(net_coaches, layout = "nicely") +
    geom_edge_link(aes(edge_width = weight), alpha = .7, color = "blue") +
    geom_node_point(aes(size = weighted_win), color = "grey") +
    geom_node_text(aes(label = Name), size = 5, color = "black", repel = FALSE) +
    labs(title = ifelse(input$size == "Edge Weight Above 4",
                        "Network of NFL Head Coaches Thresholded with Edge Weight Greater Than 4",
                        "Full Network of NFL Head Coaches"))+theme_void()
})

#Centrality Bar Plot
output$centrality_plot <- renderPlot({
  net_coaches <- network() |>
    activate(nodes) |>
    mutate(
      degree = centrality_degree(),
      btwn = centrality_betweenness(),
      weighted_win = Win.Perct * Years.As.A.Head.Coach,
      eigenvector=centrality_eigen()
    )
  coach_df <- net_coaches |> as_tibble()
  if (input$plot_toggle == "deg") {

    ggplot(coach_df, aes(y = reorder(Name, weighted_win), x = degree)) +
      geom_col(fill = "limegreen") +
      theme_classic() +
      labs(title = "NFL Head Coaches Degree Centrality Ordered by Weighted Win %",
           x = "Degree Centrality", y = "Coach")

  } else {
    ggplot(coach_df, aes(y = reorder(Name, weighted_win), x = eigenvector)) +
      geom_col(fill = "red") +
      theme_classic() +
      labs(title = "NFL Head Coaches Eigenvector Centrality Ordered by Weighted Win %",
           x = "Eigenvector Centrality", y = "Coach")
  }
})









#Interactive Card


}

# Run the application
shinyApp(ui = ui, server = server)



