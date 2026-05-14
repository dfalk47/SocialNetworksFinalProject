library(shiny)
library(bslib)
library(tidyverse)
library(igraph)
library(tidygraph)
library(ggraph)
library(visNetwork)



ui <- page_sidebar(
  title = span("NFL Head Coaches Network",style = "color: #8B4513; font-weight: bold;"),
  sidebar = sidebar(
    width = 250,
    h4("Menu", class = "text-center mb-3"),
    actionButton("page1_btn",
                 "Introduction",
                 style = "background-color: #8B4513; color: white;"),
    actionButton("page2_btn",
                 "Network Visualization",
                 style = "background-color: #8B4513; color: white;"),
    actionButton("page3_btn",
                 "Centrality Analysis",
                 style = "background-color: #8B4513; color: white;"),
    hr(),
    div(
      style = "text-align: center; padding: 15px; background-color: rgba(1, 51, 105, 0.1); border-radius: 8px;",
      tags$strong("By: Jay Falk"),
      br(),
      tags$small("2025 NFL Season Analysis", style = "color: #666;")
    )
  ),
  navset_hidden(
    id = "pages",
    # PAGE 1: Introduction
    nav_panel_hidden(
      "page1",
      layout_columns(
        col_widths = c(12, 12, 12, 12),
        card(
          full_screen = TRUE,
          card_header(
            style = "background-color: darkgreen; color: white;",
            "Introduction to the NFL Coaches Network"
          ),
          card_body(
            p("My network is of the NFL head coaches who were active during the 2025 season. An edge between coaches occurs when they spent at least one year on an NFL staff at the same time and the edges are weighted by the amount of years the two coaches worked together on a staff.")
          ),p("With this network, I hoped to understand if certain centrality measures could be used to predict how successful a head coach is in the NFL. To measure this success, I created a variable called weighted_win which takes a coach's winning percentage as a head coach and multiplies it by the amount of years they have been a head coach.")
        ),
        card(
          full_screen = TRUE,
          card_header(
            style = "background-color: darkred; color: white;",
            "How to Use This App"
          ),
          card_body(

            p("Using the network slider, you can view the full network or increase the threshold to only include ties of a certain amount of years. As the network is thresholded higher, the strength of the ties present increases. By using this slider, you can see what coaches have the strongest connections and compare it to their weighted win percentage (the size of the node) to see if there is any correlation."),
         p("For the centrality comparisons, you can toggle between degree centrality (The total number of  edges connected to a node) and eigenvector centrality (How central a node is in a network based on the degree centrality of the nodes it is connected to). These two metrics are sorted by weighted_win so that you can see if a higher centrality measure leads to being more successful as a head coach.") )
        ),

        card(
          full_screen = TRUE,
          card_header(
            style = "background-color: darkgreen; color: white;",
            "Data Gathering Process"
          ),
          card_body(
            p("All of the information I gathered came from profootballreference.com, a credible and up to date website containing a wide array of football data."),
            p("Every coach was given a unique label 1-32, and their name, winning percentage as a head coach, age, and length of time as a head coach were recorded from the website."),
            p("To find ties, I listed every job the 32 coaches had in the NFL and which years they worked in each place. Then, I compared these tenures to those of other coaches and noted a tie in my edge data when two coaches crossed over. These edges were then weighted by how many years the coaches worked together. If coaches worked together on multiple staffs, which happened a few times, these tenures were added together and the weight category was the sum of their whole time working together.")

      )
    ))),

    # PAGE 2: Network Visualization
    nav_panel_hidden(
      "page2",
      card(
        full_screen = TRUE,
        card_header(
          style = "background-color: darkgreen; color: white;",
          "Coaching Network Visualization"
        ),
        card_body(
          sliderInput("threshold",
                       "Edge Weight Threshold (Years Worked Togther):",
                    min=0,
                    max=10,
                    value=0,
                    step=1,
                    ticks=TRUE),
          plotOutput("Full_network", height = "700px")
        )
      )
    ,
    card(
      full_screen = TRUE,
      card_header(
        style = "background-color: darkred; color: white;",
        "Findings"
      ),
      card_body(
        p("By increasing the thresholding, only the strongest ties between coaches remain. An interesting note is that the remaining clusters contain some of the most successful coaches in the NFL. For example, Andy Reid, Sean McDermott, and John Harbaugh have led the three best teams in the AFC over the last 10 years. These three coaches are extremely well connected with their triad maintaining fully intact all the way to edge weight of 7."),
        p("Mike MacDonald, the coach of the reigning Super Bowl champion Seahawks, is also linked to John Harbaugh all the way to edge weight of 9. There are a few other successful coaches connected to each other after a fair amount of thresholding including Sean McVay, Matt LaFleur, and Kyle Shanahan."),
        p("Through this observation, it becomes clear that the sheer number of connections a coach has are less important than the quality of the connections.")
))),

    # PAGE 3: Centrality Analysis
    nav_panel_hidden(
      "page3",
      card(
        full_screen = TRUE,
        card_header(
          style = "background-color: darkred; color: white;",
          "Centrality Comparison"
        ),
        card_body(
          radioButtons("plot_toggle",
                       "Select Centrality Measure:",
                       choices = c("Degree Centrality" = "deg",
                                   "Eigenvector Centrality" = "btwn"),
                       selected = "deg",
                       inline = TRUE),
          plotOutput("centrality_plot", height = "600px")
        ),card(
          full_screen = TRUE,
          card_header(
            style = "background-color: darkgreen; color: white;",
            "Findings"
          ),
          card_body(
            p("By toggling between the eigenvector and degree centrality measures, it becomes clear that neither measure seems to capture how successful a coach will be as there are some coaches with high centrality degrees with low weighted_win and vice versa. This finding highlights the fact that there are many different reasons a coach could be successful."),
            p("For example, some coaches with high degrees of centrality are journeymen, meaning that the reason they have high degrees of centrality is because they have bounced around many teams. Usually a coach who bounces around a lot does so because they are fired often, meaning they aren't successful. Two coaches that fit this category are Dan Quinn and Raheem Morris. On the other hand, coaches such as Andy Reid and Pete Carroll who have been very successful but spent a long time with one team have low centrality degrees because they were not moving around teams constantly."),
            p("However, there are also successful coaches with high centrality measures (Kyle Shanahan and Sean McVay) along with unsuccessful coaches with low centrality measures (Brian Callahan and Jonathan Gannon). Based on my findings, it is clear that there is no correlation between these two centrality measures and NFL coaching success. Therefore, the network visualization finding that quality of connection is more important than quantity is given more weight.")
          )
      )
    )
  )
)
)
server <- function(input, output, session) {

  observeEvent(input$page1_btn, {
    nav_select("pages", "page1")
  })

  observeEvent(input$page2_btn, {
    nav_select("pages", "page2")
  })

  observeEvent(input$page3_btn, {
    nav_select("pages", "page3")
  })

  network <- reactive({
    nodes <- read.csv("coaches_nodes.csv")
    edges <- read.csv("coaches_edges.csv")

    net_coaches <- graph_from_data_frame(d = edges, vertices = nodes, directed = FALSE)

    net_coaches <- as_tbl_graph(net_coaches) |>
      activate(nodes) |>
      mutate(degree = centrality_degree(),
             weighted_win = Win.Perct * Years.As.A.Head.Coach)

    net_coaches
  })

  # Network visualization
  output$Full_network <- renderPlot({
    net_coaches <- network()

    if(input$threshold>0) {
      net_coaches <- net_coaches |>
        activate(edges) |>
        filter(weight > input$threshold) |>
        activate(nodes) |>
        mutate(degree = centrality_degree()) |>
        filter(degree > 0)
    }

    ggraph(net_coaches, layout = "nicely") +
      geom_edge_link(aes(edge_width = weight), alpha = .7, color = "darkblue") +
      geom_node_point(aes(size = weighted_win), color = "darkred") +
      geom_node_text(aes(label = Name), size = 4, color = "black", repel = FALSE) +
      labs(title = ifelse(input$threshold > 0,
                          paste0("Network of NFL Head Coaches (Threshold: Edge Weight > ", input$threshold, ")"),
                          "Full Network of NFL Head Coaches")) +
      theme_void() +
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
  })

  # Centrality bar plots
  output$centrality_plot <- renderPlot({
    net_coaches <- network() |>
      activate(nodes) |>
      mutate(
        degree = centrality_degree(),
        btwn = centrality_betweenness(),
        weighted_win = Win.Perct * Years.As.A.Head.Coach,
        eigenvector = centrality_eigen()
      )

    coach_df <- net_coaches |> as_tibble()

    if (input$plot_toggle == "deg") {
      ggplot(coach_df, aes(y = reorder(Name, weighted_win), x = degree)) +
        geom_col(fill = "darkgreen") +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 16, face = "bold"),
          axis.title = element_text(size = 12, face = "bold"),
          axis.text = element_text(size = 10)
        ) +
        labs(title = "NFL Head Coaches Degree Centrality Ordered by Weighted Win %",
             x = "Degree Centrality", y = "Coach")
    } else {
      ggplot(coach_df, aes(y = reorder(Name, weighted_win), x = eigenvector)) +
        geom_col(fill = "darkred") +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 16, face = "bold"),
          axis.title = element_text(size = 12, face = "bold"),
          axis.text = element_text(size = 10)
        ) +
        labs(title = "NFL Head Coaches Eigenvector Centrality Ordered by Weighted Win %",
             x = "Eigenvector Centrality", y = "Coach")
    }
  })
}

shinyApp(ui, server)
