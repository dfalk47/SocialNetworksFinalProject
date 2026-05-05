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
    title = "subtitle here",
    sidebar = sidebar ("Menu options"),
    card(
      card_header("Here's where I would introduce your project"), "put some information here"),
    card(
      card_header("Data Gathering Process"), "All of the information I gathered came from profootballreference.com, a credible and up to date website containing a wide array of football data. On this website, I searched for each NFL head coach who was active during the 2025 season, and collected a few different pieces of information which were separated into two google sheets. In one google sheet, I listed each head coach and every NFL team they worked for as an assistant or a head coach, and from what years they worked at each team. On the other google sheet I listed the node attributes. Every coach was given a unique label 1-32, and then their name, winning percentage as a head coach, age, and length of time as a head coach were recorded. Once all of this data had been collected, I moved on to collecting the edges data. In order to collect this data, I went on to the google sheet I had made with every head coach and their coaching history and searched by each NFL team. When I searched by team, I was able to see which coaches had worked for the team and noted any time two coaches worked for a team at the same time and took account of how long they worked for this team together. I used the unique label from my node data for the two coaches and put one in a source column and the other in the target column. I also created a weight category in which the length of their overlap was placed. If coaches worked together on multiple staffs, which happened a few times, these tenures were added together and the weight category was the sum of their whole time working together.
 ",
      selectInput("select",
                  "select an option",
                  choices = list("Option A" = "A",
                                 "Option B" = "B"),
                  selected =1),
      textOutput("ourVariable")
      ),


    card(card_header("Coaching Network"),
         selectInput("size",
                     "Choose a Threshold Measure",
                     choices = list("No Threshold" = "Full_graph",
                                    "Threshold" = "Edge Weight Above 4"),
                     selected = "Full_graph"),
         plotOutput("Full_network"),
         height = "400px"
    ),


    card(card_header("Betweenness Centrality Bar Plots"),
         plotOutput("btwn_barplot", height = "400px")
    ,
         radioButtons("size_by", "Centrality Measure",
                      choices = c("Degree" = "degree",
                      "Betweenness Centrality" = "betweenness"),
         selected = "degree"),
         visNetworkOutput("int_network"), height = "600px")
    )
  )


# Section 2. The server section defines how our app works. Here's where we will put all the network analysis.

server <- function(input, output) {

  # CARD 1

  output$ourVariable <- renderText({
    paste("Our selected option is", input$select)
  })

# let's create a simple example network with 10 nodes and calulate the degree centrality

  # CARD 2

  network <- reactive({
    nodes <- read.csv("coaches_nodes.csv")
    edges <- read.csv("coaches_edges.csv")

    net_coaches <- graph_from_data_frame(d = edges, vertices = nodes, directed = FALSE)

    net_coaches <- as_tbl_graph(net_coaches) |>
      activate(nodes) |>
      mutate(degree = centrality_degree())

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
    geom_node_point(aes(size = degree), color = "white") +
    geom_node_text(aes(label = Name), size = 3, color = "black", repel = FALSE) +
    labs(title = ifelse(input$size == "Edge Weight Above 4",
                        "Thresholded Network (Weight > 4, No Isolates)",
                        "Full Network of NFL Head Coaches"))+theme_void()
})
# CARD 3

# we're going to use another example network like from above but visNetwork requires separate edge and nodes lists

network2 <- reactive({
  nodes <- read.csv("coaches_nodes.csv")
  edges <- read.csv("coaches_edges.csv")

  net_coaches <- graph_from_data_frame(d = edges, vertices = nodes, directed = FALSE)

  net_coaches <- as_tbl_graph(net_coaches) |>
    activate(nodes) |>
    mutate(degree = centrality_degree())

  net_coaches
})

output$btwn_barplot <- renderPlot({
  net_coaches <- network()


  net_coaches <- net_coaches |> activate(nodes) |> mutate(btwn =  centrality_betweenness())

  btwn_Coach<-net_coaches |> activate(nodes) |> filter(btwn>0)

  btwn_df <- btwn_Coach |> activate(nodes) |> as_tibble()
  btwn_Coach |> activate(edges) |> as_tibble()

  btwn_Coach10<-btwn_df|> filter(Years.As.A.Head.Coach>7)

  ggplot(btwn_Coach10, aes(x= reorder(Name, Years.As.A.Head.Coach), y=btwn)) +
    geom_col(fill="lightblue")+theme_classic()+
    labs(title="Betweeness Centrality of the 9 Coaches with the Most Head Coaching Experience", x="Head Coaches Arranged from Shortest to Longest Tenure", y="Betweenness Centrality Degree")



})


#Interactive Card
network3 <- reactive({
  set.seed(123)
  ex_net2 <- play_gnp(n = 15, p = 0.25, directed = FALSE)

  ex_net2 <- ex_net2 |>
    as_tbl_graph()|>
    activate(nodes) |>
    mutate(
      degree = centrality_degree(),
      betweenness = centrality_betweenness())

  nodes_df <- ex_net2 |>
    activate(nodes) |>
    as_tibble() |>
    rowid_to_column("id") |>
    mutate(value = if (input$size_by == "degree") degree else betweenness) # have to give size based on "value" for visNetwork

  edges_df <- ex_net2 |>
    activate(edges) |>
    as_tibble() |>
    rename(from = 1, to =2 )

  list(nodes = nodes_df, edges = edges_df)
})

output$int_network <- renderVisNetwork({
   net2 <- network2()
   nodes <- net2$nodes
   edges <- net2$edges


  visNetwork(nodes, net2$edges) |>

    visNodes(borderWidth = 1,
             color = list(
               background= "pink",
               border = "red",
               highlight =  "purple"))|>

    visEdges(
      color = list(color = "purple", highlight = "black")) |>

    visOptions(
      highlightNearest = list(enabled = TRUE, hover = TRUE),
      nodesIdSelection = FALSE) |>

    visInteraction(
      dragNodes = TRUE,
      dragView = TRUE,
      zoomView = TRUE) |>

    visPhysics(stabilization = TRUE)

})

}

# Run the application
shinyApp(ui = ui, server = server)



