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
my_theme <- bs_theme(
  version = 5,
  bg = "#ffffff",
  fg = "#212529",
  primary = "#1e3a8a",
  secondary = "#64748b",
  success = "#059669",
  base_font = font_google("Inter"),
  heading_font = font_google("Playfair Display"),
  font_scale = 0.95
) |> 
  bs_add_rules(
    ".card { 
      box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.1), 0 2px 4px -1px rgba(0, 0, 0, 0.06); 
      border-radius: 12px; 
      border: none;
      margin-bottom: 1.5rem;
    }
    .card-header { 
      background: linear-gradient(135deg, #1e3a8a 0%, #3b82f6 100%); 
      color: white; 
      font-weight: 600; 
      padding: 1.25rem 1.5rem;
      border-radius: 12px 12px 0 0 !important;
    }
    .sidebar { 
      background-color: #f8fafc; 
      border-right: 1px solid #e2e8f0;
    }
    .bslib-page-title { 
      font-size: 1.6rem; 
      font-weight: 700; 
      color: #1e3a8a;
      padding: 1rem 0;
    }
    .form-control, .form-select { 
      border-radius: 8px;
      border: 1px solid #cbd5e1;
    }
    .form-control:focus, .form-select:focus {
      border-color: #3b82f6;
      box-shadow: 0 0 0 3px rgba(59, 130, 246, 0.1);
    }
    .card-body { 
      padding: 1.5rem;
      line-height: 1.7;
    }
    .card-footer {
      background-color: #f8fafc;
      border-top: 1px solid #e2e8f0;
      padding: 1rem 1.5rem;
      border-radius: 0 0 12px 12px !important;
    }
    h1, h2, h3, h4, h5 { 
      color: #1e293b;
      margin-top: 0;
    }
    .sidebar-header {
      background: white;
      padding: 1.25rem;
      border-radius: 8px;
      margin-bottom: 1.5rem;
      box-shadow: 0 1px 3px rgba(0,0,0,0.08);
      text-align: center;
    }
    .control-section {
      background: white;
      padding: 1.25rem;
      border-radius: 8px;
      margin-bottom: 1rem;
      box-shadow: 0 1px 3px rgba(0,0,0,0.08);
    }
    .info-box {
      background: #eff6ff;
      border-left: 4px solid #3b82f6;
      padding: 1rem 1.25rem;
      border-radius: 6px;
      margin: 1rem 0;
    }"
  )

# Section 2. UI definition
ui <- page_sidebar(
  title = div(
    icon("balance-scale", style = "margin-right: 0.5rem;"),
    "Supreme Court Cases & Their Lawyers"
  ),
  theme = my_theme,
  fillable = FALSE,
  
  sidebar = sidebar(
    width = 320,
    
    # Sidebar header
    div(
      class = "sidebar-header",
      icon("sliders", style = "font-size: 2.5rem; color: #1e3a8a; margin-bottom: 0.5rem;"),
      h4("Visualization Controls", style = "margin: 0.5rem 0 0 0; color: #1e3a8a;")
    ),
    
    # Static network controls
    div(
      class = "control-section",
      h5(
        icon("chart-network"), 
        "Static Network Options", 
        style = "color: #475569; margin-bottom: 1rem; font-weight: 600;"
      ),
      selectInput(
        "select", 
        "Display Mode",
        choices = list(
          "Standard View" = "A", 
          "Alternative View" = "B"
        ),
        selected = "A"
      ),
      selectInput(
        "size",
        "Node Size By",
        choices = list(
          "Degree Centrality" = "degree", 
          "Betweenness Centrality" = "betweenness"
        ), 
        selected = "degree"
      )
    ),
    
    # Interactive network controls
    div(
      class = "control-section",
      h5(
        icon("hand-pointer"), 
        "Interactive Network", 
        style = "color: #475569; margin-bottom: 1rem; font-weight: 600;"
      ),
      radioButtons(
        "size_by", 
        "Centrality Measure",
        choices = c(
          "Degree" = "degree", 
          "Betweenness" = "betweenness"
        ), 
        selected = "degree"
      )
    ),
    
    # Debug section (collapsible)
    accordion(
      accordion_panel(
        "Debug Information",
        verbatimTextOutput("debug_info")
      ),
      open = FALSE
    )
  ),
  
  # Main content area
  card(
    card_header(
      icon("book-open"),
      "Using Social Networks to Analyze The United States' Most Impactful Legal Individuals"
    ),
    card_body(
      p(
        style = "font-size: 1.05rem; color: #334155;",
        "In this app I use ", strong("Social Network Analysis (SNA)"), 
        " to create a systematic review of influential litigants in United States Supreme Court cases."
      ),
      
      div(
        class = "info-box",
        h5(icon("database"), "Dataset Construction", style = "margin-top: 0; color: #1e3a8a;"),
        p(
          style = "margin-bottom: 0.5rem; color: #475569;",
          "The first step in building the dataset involved identifying a set of historically and legally 
        significant cases using an article from HeinOnline (2018), which compiles the most frequently 
        cited Supreme Court cases in both journal articles and judicial opinions within the HeinOnline legal database. 
          After compiling the case list of ", strong("fifty"), " of the most socially and legally impactful cases in 
          United States legal precedent, I gathered structured case-level data using The Supreme Court Database 
          (Spaeth et al. 2023), which provides standardized information on all cases decided by the U.S. Supreme Court, 
          including party roles, case outcomes, and other classifications."
        )
      ),
      
      p(
        style = "font-size: 1.05rem; color: #334155;",
        "From this database, I extracted information on petitioners and respondents for each case in order to 
        construct the relational structure of a bipartite network. Each case was then linked to its corresponding 
        legal actors, forming the basis of a bipartite network in which one set of nodes represents Supreme Court 
        cases and the other represents forms of legal representation. There is no edgeweight in this system, however 
        the number of citations for each case is used to create the size of each node."
      ),
      
      p(
        style = "font-size: 1.05rem; color: #334155;",
        "Due to the different roles of a representative in any legal case– petitioner v. respondent
– I codified each edge as either 'R' for respondent or 'P' for petitioner to better 
        visualize by color how these edges connect certain types of litigants to certain cases. For 
        further visual mapping purposes, the fifty cases are also color coded as an article citation or
         case citation to distinguish whether the case is more impactful 
        as a legal precedent or as a social/political turning point."
      ),
      
      div(
        class = "info-box",
        h5(icon("project-diagram"), "Network Structure Summary", style = "margin-top: 0; color: #1e3a8a;"),
        tags$ul(
          style = "color: #475569; margin-bottom: 0;",
          tags$li(strong("Node file: "), "ID, type (case=0 or representative=1), name, citation count"),
          tags$li(strong("Edge file: "), "Relationships between cases and their petitioners/respondents"),
          tags$li(strong("Edge types: "), "'R' for respondent, 'P' for petitioner"),
          tags$li(strong("Case coding: "), "'A1-A25' (article citations), 'C1-C25' (case citations)")
        )
      ),
      
      p(
        style = "font-size: 1.05rem; color: #334155;",
        "This structure shows the distribution of different types of legal actors across highly cited and 
        historically significant Supreme Court decisions."
      )
    )
  ),
  
  card(
    full_screen = TRUE,
    card_header(
      icon("project-diagram"),
      "Supreme Court Citation Network"
    ),
    card_body(
      plotOutput("example_network", height = "500px")
    ),
    card_footer(
      icon("info-circle"), 
      " Node size represents citation frequency. Colors distinguish case types and legal actors. 
      Use the controls in the sidebar to adjust visualization parameters."
    )
  ),
  
  card(
    full_screen = TRUE,
    card_header(
      icon("sitemap"),
      "Interactive Network Explorer"
    ),
    card_body(
      div(
        class = "info-box",
        style = "margin-bottom: 1.5rem;",
        p(
          style = "margin: 0; color: #475569;",
          icon("hand-pointer"), 
          strong(" Interactive Features: "),
          "Hover over nodes for detailed information, drag nodes to rearrange the layout, 
          use mouse wheel to zoom, and click nodes to highlight their connections."
        )
      ),
      visNetworkOutput("int_network", height = "350px")
    ),
    card_footer(
      div(
        style = "display: flex; justify-content: space-between; align-items: center; flex-wrap: wrap; gap: 1rem;",
        div(
          icon("palette"), " ",
          tags$span(style = "color: #FF6B6B; font-size: 1.2rem;", "■"), 
          " Supreme Court Cases  ",
          tags$span(style = "color: #4ECDC4; font-size: 1.2rem;", "■"), 
          " Legal Representatives"
        )
      )
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
    if (!file.exists("Supremecourtnodes.csv") || 
        !file.exists("Supremecourtedges.csv")) {
      return(NULL)
    }
    
    nodes <- read.csv("Supremecourtnodes.csv", stringsAsFactors = FALSE)
    nodes$Citations <- as.numeric(nodes$Citations)
    edges <- read.csv("Supremecourtedges.csv") 
    
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
      text(0.5, 0.5, "Data files not found. Please ensure:\nSupremecourtnodes.csv\nSupremecourtedges.csv\nexist", cex = 1.2)
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
    if (!file.exists("Supremecourtnodes.csv") || 
        !file.exists("Supremecourtedges.csv")) {
      # Return empty network if files don't exist
      return(list(
        nodes = data.frame(id = 1, label = "", title = "Data not found"),
        edges = data.frame(from = numeric(), to = numeric())
      ))
    }
    
    # Load data
    nodes <- read.csv("Supremecourtnodes.csv", stringsAsFactors = FALSE)
    nodes$Citations <- as.numeric(nodes$Citations)
    edges <- read.csv("Supremecourtedges.csv") 
    
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