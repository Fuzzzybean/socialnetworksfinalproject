# Shiny App 
# Section 1. First install and activate all your required packages. 

library(shiny)
library(bslib)
library(tidyverse)
library(igraph)
library(tidygraph)
library(ggraph)
library(visNetwork)

# Section 2. Design the site in the UI section

ui <- page_sidebar(
  title = "SUPREME COURT CASES AND THEIR LAWYERS",
  fillable = FALSE,  # KEY CHANGE: Allows natural scrolling
  
  sidebar = sidebar(
    "Controls",
    selectInput("select", 
                "Select an option", 
                choices = list("Option A" = "A", 
                               "Option B" = "B"),
                selected = "A"),
    selectInput("size",
                "Choose a centrality measure", 
                choices = list("Degree Centrality" = "degree", 
                               "Betweenness Centrality" = "betweenness"), 
                selected = "degree"),
    radioButtons("size_by", 
                 "Centrality Measure (Interactive)", 
                 choices = c("Degree" = "degree", 
                             "Betweenness" = "betweenness"), 
                 selected = "degree")
  ),
  
  card(
    card_header("Using Social Networks to Analyze The United States' Most Impactful Legal Individuals"), 
    card_body(
      "In this app I have used Social Network Analysis (SNA) to create a systematic review of 
      influential litigants in United States Supreme Court cases. 
      The first step in building the dataset involved identifying a set of historically and legally significant cases using an article from HeinOnline (2018),
      which compiles the most frequently cited Supreme Court cases in both journal articles and judicial opinions within the HeinOnline legal database. 
      In doing this, I developed a set of fifty of the most socially and legally impactful cases in United States legal precedent with several cases overlapping in both article and case citations.
      After compiling the case list, I gathered structured case-level data using The Supreme Court Database (Spaeth et al. 2023),
      which provides standardized information on all cases decided by the U.S. Supreme Court, including party roles, case outcomes, and other classifications. 
      From this database, I extracted information on petitioners and respondents for each case in order to construct the relational structure of a bipartite network 
      Each case was then linked to its corresponding legal actors, forming the basis of a bipartite network in which one set of nodes represents Supreme Court cases and the other represents forms of legal representation. There is no edgeweight in this system, however the number of citations for each case is used to create the size of each of these nodes. Furthermore, due to the different roles of a representative in any legal case– petitioner v. respondent, prosecutor v. defendant– I codified each edge as either 'R' for respondent or 'P' for petitioner to better visualize by color how these edges connect certain types of representation to certain types of cases. For further visual mapping purposes, the fifty cases are also coded 'A1 - A25' for article citation and 'C1-C25' for case citation as another element which can use color to distinguish whether the case is more impactful as a legal precedent or as a social/political turning point. In summary, the dataset consists of a node file containing id, type (case or representative coded as 0 or 1), case/rep name, and citation number, and an edge file with edges capturing the relationship between each Supreme Court case and its associated petitioner or respondent. This structure shows the distribution of different types of legal actors across highly cited and historically significant Supreme Court decisions."
    )
  ),
  
  card(
    card_header("Dynamic Demo 1"), 
    card_body(
      "You could put a caption like so",
      textOutput("ourVariable")
    )
  ), 
  
  card(
    card_header("Supreme Court Network"),
    card_body(
      plotOutput("example_network", height = "600px")
    )
  ),
  
  card(
    card_header("An interactive network?!"),
    card_body(
      p("We can use the package visNetwork to make it happen"),
      visNetworkOutput("int_network", height = "800px")  # Reduced height for better fit
    )
  )
)

# Section 3. The server section

server <- function(input, output) {
  
  # CARD 1 - Dynamic Demo
  output$ourVariable <- renderText({
    paste("Our selected option is", input$select)
  })
  
  # CARD 2 - Supreme Court Network
  network <- reactive({
    # Check if files exist, otherwise return NULL
    if (!file.exists("Data/Supremecourtnodes.csv") || 
        !file.exists("Data/Supremecourtedges.csv")) {
      return(NULL)
    }
    
    nodes <- read.csv("Data/Supremecourtnodes.csv", stringsAsFactors = FALSE)
    nodes$Citations <- as.numeric(nodes$Citations)
    edges <- read.csv("Data/Supremecourtedges.csv") 
    
    net_sc <- graph_from_data_frame(d = edges, vertices = nodes, directed = FALSE)
    sc_tidy <- as_tbl_graph(net_sc)
    
    sc_tidy <- sc_tidy |>
      activate(nodes) |>
      mutate(type = type == 0)   
    
    scbp_tidy <- sc_tidy |>
      activate(nodes) |>
      mutate(component = group_components()) |>
      group_by(component) |>
      filter(n() > 3) |>
      ungroup()
    
    C_graph <- scbp_tidy |>
      activate(nodes) |>
      filter(str_starts(name, "C") | str_starts(name, "[0-9]")) |>
      activate(nodes) |>
      filter(centrality_degree() > 0)
    
    C_graph
  })
  
  output$example_network <- renderPlot({
    C_graph <- network()
    
    if (is.null(C_graph)) {
      plot.new()
      text(0.5, 0.5, "Data files not found. Please ensure:\nData/Supremecourtnodes.csv\nData/Supremecourtedges.csv\nexist", cex = 1.2)
      return()
    }
    
    C_graph |> 
      ggraph(layout = "fr") + 
      geom_edge_link(aes(color = as.factor(Type)), alpha = 0.5) + 
      geom_node_point(aes(size = Citations + 2, color = type),
                      alpha = 0.9,
                      show.legend = c(size = FALSE, color = TRUE)) +  
      geom_node_label(aes(label = Name), size = 3, repel = TRUE) +
      labs(
        title = "Supreme Court Citation Network",
        subtitle = "Node size reflects number of citations",
        color = "Node Type",
        size = "Citations"
      ) +
      theme_void() +
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 11, hjust = 0.5),
        legend.position = "right"
      )
  })
  
  # CARD 3 - Interactive network with visNetwork
  network_data <- reactive({
    if (!file.exists("Data/Supremecourtnodes.csv") || 
        !file.exists("Data/Supremecourtedges.csv")) {
      # Return empty network if files don't exist
      return(list(
        nodes = data.frame(id = 1, label = "", title = "Data not found"),
        edges = data.frame(from = numeric(), to = numeric())
      ))
    }
    
    # Load data
    nodes <- read.csv("Data/Supremecourtnodes.csv", stringsAsFactors = FALSE)
    nodes$Citations <- as.numeric(nodes$Citations)
    edges <- read.csv("Data/Supremecourtedges.csv") 
    
    # Create network
    net_sc <- graph_from_data_frame(d = edges, vertices = nodes, directed = FALSE)
    sc_tidy <- as_tbl_graph(net_sc)
    
    # Process network
    sc_tidy <- sc_tidy |>
      activate(nodes) |>
      mutate(type = type == 0)   
    
    scbp_tidy <- sc_tidy |>
      activate(nodes) |>
      mutate(component = group_components()) |>
      group_by(component) |>
      filter(n() > 3) |>
      ungroup()
    
    C_graph <- scbp_tidy |>
      activate(nodes) |>
      filter(str_starts(name, "C") | str_starts(name, "[0-9]")) |>
      activate(nodes) |>
      filter(centrality_degree() > 0) |>
      mutate(
        degree = centrality_degree(),
        betweenness = centrality_betweenness()
      )
    
    # Prepare nodes for visNetwork
    nodes_df <- C_graph |>
      activate(nodes) |>
      as_tibble() |>
      mutate(
        id = row_number(),
        label = "",  # Empty - shows on hover only
        title = paste0(
          "<b>", Name, "</b><br>",
          "Type: ", ifelse(type, "Case", "Other"), "<br>",
          "Citations: ", Citations, "<br>",
          "Degree: ", round(degree, 2), "<br>",
          "Betweenness: ", round(betweenness, 2)
        ),
        value = if (input$size_by == "degree") degree else betweenness,
        group = ifelse(type, "Case", "Other")
      )
    
    # Prepare edges for visNetwork
    edges_df <- C_graph |>
      activate(edges) |>
      as_tibble() |>
      rename(from = 1, to = 2)
    
    list(nodes = nodes_df, edges = edges_df)
  })
  
  output$int_network <- renderVisNetwork({
    data <- network_data()
    
    visNetwork(data$nodes, data$edges) |>
      visNodes(
        borderWidth = 2,
        color = list(
          background = "lightblue",
          border = "darkblue",
          highlight = list(background = "orange", border = "darkorange")
        ),
        font = list(size = 0)  # Hide labels by default
      ) |>
      visEdges(
        color = list(color = "gray", highlight = "black"),
        width = 1.5
      ) |>
      visGroups(
        groupname = "Case", 
        color = list(background = "#FF6B6B", border = "#C92A2A")
      ) |>
      visGroups(
        groupname = "Other", 
        color = list(background = "#4ECDC4", border = "#0B7285")
      ) |>
      visOptions(
        highlightNearest = list(enabled = TRUE, hover = TRUE, degree = 1),
        nodesIdSelection = FALSE
      ) |>
      visInteraction(
        dragNodes = TRUE,
        dragView = TRUE,
        zoomView = TRUE,
        hover = TRUE,
        tooltipDelay = 100
      ) |>
      visPhysics( stabilization = TRUE, barnesHut = list(gravitationalConstant = -2000))
  })
}


# Run the application 
shinyApp(ui = ui, server = server)