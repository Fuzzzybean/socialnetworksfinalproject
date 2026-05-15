### Supreme court app
## Grady Boss

# Shiny App
library(shiny)
library(bslib)
library(tidyverse)
library(igraph)
library(tidygraph)
library(ggraph)
library(visNetwork)


# Theme
my_theme <- bs_theme(
  version = 5,
  bg = "#ffffff", fg = "#212529",
  primary = "#1e3a8a", secondary = "#64748b", success = "#059669",
  base_font = font_google("Inter"),
  heading_font = font_google("Playfair Display"),
  font_scale = 0.95
)

app_css <- tags$style(HTML("
  body { padding-top: 70px; }
  .navbar { box-shadow: 0 2px 8px rgba(0,0,0,0.12); }
  .key-finding {
    background-color: #f0fdf4 !important;
    border-left: 4px solid #059669 !important;
    padding: 1rem 1.25rem;
    border-radius: 6px;
    margin: 1rem 0;
  }
  .highlight-box {
    background-color: #fef3c7 !important;
    border: 2px solid #f59e0b !important;
    padding: 1.25rem;
    border-radius: 8px;
    margin: 1.5rem 0;
  }
  .info-box {
    background-color: #eff6ff !important;
    border-left: 4px solid #3b82f6 !important;
    padding: 1rem 1.25rem;
    border-radius: 6px;
    margin: 1rem 0;
  }
  .card {
    box-shadow: 0 4px 6px -1px rgba(0,0,0,.1), 0 2px 4px -1px rgba(0,0,0,.06);
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
  .card-body {
    padding: 1.5rem;
    line-height: 1.7;
    background-color: white;
  }
  .card-footer {
    background-color: #f8fafc;
    border-top: 1px solid #e2e8f0;
    padding: 1rem 1.5rem;
    border-radius: 0 0 12px 12px !important;
  }
  h1, h2, h3, h4, h5 { color: #1e293b; margin-top: 0; }
"))

scroll_js <- tags$script(HTML("
  $(document).on('click', '.nav-link', function() {
    setTimeout(function() { window.scrollTo(0, 0); }, 50);
  });
"))

# UI 

guide_box <- div(class = "highlight-box",
                 h4(icon("lightbulb"), "Quick Start Guide", style = "margin-top:0; color:#92400e;"),
                 p(style = "color:#78350f;",
                   strong("Navigate through this app using the three bars on the top right to discover:"),
                   tags$ul(
                     tags$li("How legal representatives connect landmark Supreme Court cases"),
                     tags$li("Which litigants appear in multiple influential cases"),
                     tags$li("The difference between highly connected vs. broker positions in legal networks")
                   )))

interactive_list <- tags$ul(style = "color:#334155; line-height:1.8;",
                            tags$li(strong("Switch between tabs"), " to explore different visualisations."),
                            tags$li(strong("Use the interactive network"), " to hover over nodes for detailed information. Toggle between article and case citations."),
                            tags$li(strong("Operate the slider in the Rep-Rep Network"), " to reveal structural patterns in how different litigants connect."),
                            tags$li(strong("Examine the bar charts"), " to identify top actors by degree or betweenness centrality."),
                            tags$li(strong("Hover over nodes in the Rep-Rep network"), " to see which modularity community each representative belongs to."))

welcome_card <- card(
  card_header(icon("compass"), "Welcome: How to Explore This Network Analysis"),
  card_body(
    guide_box,
    h5(icon("mouse-pointer"), "Interactive Features", style = "color:#1e3a8a; margin-top:1.5rem;"),
    interactive_list))

dataset_box <- div(class = "info-box",
                   h5(icon("database"), "Dataset Construction", style = "margin-top:0; color:#1e3a8a;"),
                   p(style = "color:#475569;",
                     "The first step in building the dataset involved identifying a set of historically and legally
    significant cases using an article from HeinOnline (2018), which compiles the most frequently
    cited Supreme Court cases in both journal articles and judicial opinions within the HeinOnline
    legal database. After compiling the case list of fifty of the most socially and
    legally impactful cases in United States legal precedent, I gathered data including litigant infortmation using The Supreme Court
    Database (Spaeth et al. 2023), which provides standardized information on all cases decided by
    the U.S. Supreme Court. From this database, I extracted information on the petitioners and
    respondents linked to their associated cases to create a bipartite 
                     network where one node set represents Supreme Court cases and the other represents litigant parties.
                     Citation counts are used to scale node sizes."))
                     
                     
structure_box <- div(class = "info-box",
                     h5(icon("project-diagram"), "Network Structure Summary", style = "margin-top:0; color:#1e3a8a;"),
                     tags$ul(style = "color:#475569; margin-bottom:0;",
                             tags$li(strong("Node file: "), "ID, type (case=0 or representative=1), name, citation count"),
                             tags$li(strong("Edge file: "), "Relationships between cases and their petitioners/respondents"),
                             tags$li(strong("Edge types: "), "'R' for respondent, 'P' for petitioner"),
                             tags$li(strong("Case coding: "), "'A1-A25' (article citations), 'C1-C25' (case citations)")))

finding1 <- div(class = "key-finding",
                h5(icon("star"), "Key Finding #1: The Power of Legal Individuals", style = "margin-top:0; color:#065f46;"),
                p(style = "color:#047857; margin-bottom:0;",
                  "Several litigants have only moderate degree centrality but high betweenness centrality. These individuals may 
    connect otherwise seperate clusters of cases, potentially spanning separate legal communities and precedents like civil rights, criminal
    procedure, and constitutional law."))

finding2 <- div(class = "key-finding",
                h5(icon("star"), "Key Finding #2: States V. Prisoners", style = "margin-top:0; color:#065f46;"),
                p(style = "color:#047857; margin-bottom:0;",
                  "The network reveals that a noticeable share of landmark cases (fourteen of the top fifty) involve states acting against criminal defendants.
    This pattern suggests that landmark precedents form through individual resistance against state
    legal doctrines through the work of specialized appellate procedure."))


tab_about <- nav_panel(
  title = "About", icon = icon("book-open"),
  card(
    card_header(icon("book-open"),
                "Using Social Networks to Analyze The United States' Most Impactful Legal Individuals"),
    card_body(
      p(style = "font-size:1.05rem; color:#334155;",
        "In this app I use ", strong("Social Network Analysis (SNA)"),
        " to create a systematic review of influential litigants in United States Supreme Court cases."),
      welcome_card, dataset_box, structure_box, finding1, finding2)))


case_controls <- div(
  style = "display:flex; align-items:center; gap:2rem; margin-bottom:1rem;",
  div(style = "font-weight:600; color:#475569; font-size:0.9rem;", icon("filter"), " Show cases:"),
  radioButtons("case_filter", label = NULL, inline = TRUE,
               choices = c("All 50" = "all", "Case citations (C)" = "C", "Article citations (A)" = "A"),
               selected = "all"))

network_legend <- div(style = "display:flex; gap:1.5rem; align-items:center;",
                      div(tags$span(style = "color:#FF6B6B; font-size:1.2rem;", "■"), " Supreme Court Cases"),
                      div(tags$span(style = "color:#4ECDC4; font-size:1.2rem;", "■"), " Legal Representatives"))

proj_controls <- div(
  style = "display:flex; align-items:center; gap:2rem; margin-bottom:1rem; flex-wrap:wrap;",
  div(style = "font-weight:600; color:#475569; font-size:0.9rem;", icon("sliders-h"), " Minimum shared cases:"),
  div(style = "flex:1; max-width:100px; padding-top:0.5rem;",
      sliderInput("rep_proj_threshold", label = NULL, min = 1, max = 5, value = 1, step = 1, width = "200%")),
  div(style = "display:flex; max-width:50px;align-items:center; gap:1rem;",
      actionButton("show_modularity", label = div(icon("circle-nodes"), " Show Modularity Colours"),
                   class = "btn btn-outline-primary btn-sm"),
      uiOutput("modularity_score")))

tab_network <- nav_panel(
  title = "Network", icon = icon("project-diagram"),
  card(
    full_screen = TRUE,
    card_header(icon("sitemap"), "Interactive Bipartite Network"),
    card_body(
      div(class = "info-box", style = "margin-bottom:1rem;",
          p(style = "margin:0; color:#475569;",
            icon("hand-pointer"), strong(" Interactive Features: "),
            "Hover, drag, zoom, and click nodes to inspect relationships.")),
      case_controls,
      visNetworkOutput("int_network", height = "350px")),
    card_footer(network_legend)),
  card(
    full_screen = TRUE,
    card_header(icon("circle-nodes"), "Representative-to-Representative Projection"),
    card_body(
      div(class = "info-box", style = "margin-bottom:1rem;",
          p(style = "margin:0; color:#475569;",
            icon("info-circle"), " ",
            strong("One-mode projection: representatives only. "),
            "Representatives connect when they share enough cases.")),
      proj_controls,
      visNetworkOutput("proj_reps", height = "300px")),
    card_footer(
      tags$span(style = "color:#4ECDC4; font-size:1.2rem;", "■"),
      " Representatives — edge width = shared cases")))

rank_controls <- div(
  style = "display:flex; align-items:flex-end; gap:2rem; margin-bottom:1.5rem;
           padding:1rem 1.25rem; background:#f8fafc; border-radius:8px;
           border:1px solid #e2e8f0; flex-wrap:wrap;",
  div(style = "flex:1; min-width:180px;",
      selectInput("centrality_measure", "Rank representatives by",
                  choices = list("Degree Centrality" = "degree", "Betweenness Centrality" = "betweenness"),
                  selected = "degree")),
  div(style = "flex:1; min-width:200px;",
      sliderInput("top_n", "Number to show", min = 5, max = 20, value = 10, step = 1)))

tab_rankings <- nav_panel(
  title = "Rankings", icon = icon("chart-bar"),
  card(
    card_header(icon("chart-bar"), "Top Representatives by Centrality"),
    card_body(rank_controls, plotOutput("centrality_chart", height = "450px"))))

ui <- page_navbar(
  title = div(icon("balance-scale", style = "margin-right:0.5rem;"), "Supreme Court Cases & Their Lawyers"),
  theme = my_theme, fillable = FALSE, position = "fixed-top",
  header = tagList(scroll_js, app_css),
  tab_about, tab_network, tab_rankings)

server <- function(input, output) {
  
  app_data <- reactive({
    node_data <- read.csv("Supremecourtnodes.csv", stringsAsFactors = FALSE)
    node_data$Citations <- as.numeric(node_data$Citations)
    
    edge_data <- read.csv("Supremecourtedges.csv", stringsAsFactors = FALSE) |>
      rename(from = Case, to = Rep)
    
    list(nodes = node_data, edges = edge_data)
  })
  
  network_data <- reactive({
    d <- app_data()
    filter_type <- input$case_filter
    
    graph_tbl <- graph_from_data_frame(d$edges, vertices = d$nodes, directed = FALSE) |>
      as_tbl_graph() |>
      activate(nodes) |>
      mutate(type = type == 0, component = group_components()) |>
      group_by(component) |>
      filter(n() > 3) |>
      ungroup() |>
      filter(if (filter_type == "all") TRUE else str_starts(name, filter_type) | !type) |>
      filter(centrality_degree() > 0) |>
      mutate(degree = centrality_degree(), betweenness = centrality_betweenness())
    
    nodes_df <- graph_tbl |>
      activate(nodes) |>
      as_tibble() |>
      mutate(
        id = row_number(),
        label = "",
        title = paste0("<b>", Name, "</b><br>Type: ", ifelse(type, "Case", "Representative"),
                       "<br>Citations: ", Citations,
                       "<br>Degree: ", round(degree, 2),
                       "<br>Betweenness: ", round(betweenness, 2)),
        value = Citations,
        group = ifelse(type, "Case", "Representative"))
    
    edges_df <- graph_tbl |>
      activate(edges) |>
      as_tibble() |>
      rename(from = 1, to = 2)
    
    list(nodes = nodes_df, edges = edges_df)
  })
  
  output$int_network <- renderVisNetwork({
    net <- network_data()
    
    visNetwork(net$nodes, net$edges) |>
      visGroups(groupname = "Case", color = list(background = "#FF6B6B", border = "#C92A2A")) |>
      visGroups(groupname = "Representative", color = list(background = "#4ECDC4", border = "#0B7285")) |>
      visNodes(borderWidth = 2, font = list(size = 0)) |>
      visEdges(color = list(color = "gray", highlight = "black"), width = 1.5) |>
      visOptions(highlightNearest = list(enabled = TRUE, hover = TRUE, degree = 1), nodesIdSelection = FALSE) |>
      visInteraction(dragNodes = TRUE, dragView = TRUE, zoomView = TRUE, hover = TRUE, tooltipDelay = 100) |>
      visPhysics(barnesHut = list(gravitationalConstant = -2000))
  })
  
  bip_graph <- reactive({
    d <- app_data()
    
    graph_from_data_frame(d$edges, vertices = d$nodes, directed = FALSE) |>
      as_tbl_graph() |>
      activate(nodes) |>
      mutate(type = type == 1) |>
      as.igraph()
  })
  
  make_projection <- function(g, threshold = 1, modularity_on = FALSE) {
    g <- delete_edges(g, which(E(g)$weight < threshold))
    g <- delete_vertices(g, which(degree(g) == 0))
    
    comps <- components(g)
    g <- induced_subgraph(g, which(comps$membership %in% which(comps$csize >= 3)))
    
    comm <- cluster_louvain(g)
    
    palette_cols <- c( "#f4a261",
                      "#8338ec", "#fb5607", "#06d6a0", "#118ab2", "#ffd166")
    
    bg_col <- ifelse(rep(modularity_on, vcount(g)),
                     palette_cols[(membership(comm) %% length(palette_cols)) + 1],
                     "#4ECDC4")
    
    edge_tbl <- as_data_frame(g, what = "edges")
    
    nodes_df <- data.frame(
      id = as.character(V(g)$name),
      label = "",
      title = paste0("<b>", V(g)$Name, "</b><br>Citations: ", V(g)$Citations,
                     "<br>Connections: ", degree(g),
                     "<br>Community: ", membership(comm)),
      value = pmax(V(g)$Citations, 1),
      color.background = bg_col,
      color.border = "#1e293b",
      stringsAsFactors = FALSE)
    
    edges_df <- data.frame(
      from = as.character(edge_tbl$from),
      to = as.character(edge_tbl$to),
      value = edge_tbl$weight,
      stringsAsFactors = FALSE)
    
    visNetwork(nodes_df, edges_df) |>
      visNodes(borderWidth = 2, font = list(size = 0), scaling = list(min = 10, max = 50)) |>
      visEdges(scaling = list(min = 1, max = 8), color = list(color = "gray", highlight = "black")) |>
      visOptions(highlightNearest = list(enabled = TRUE, hover = TRUE, degree = 1)) |>
      visInteraction(dragNodes = TRUE, dragView = TRUE, zoomView = TRUE, hover = TRUE, tooltipDelay = 100) |>
      visPhysics(barnesHut = list(gravitationalConstant = -3000))
  }
  
  modularity_on <- reactiveVal(FALSE)
  
  observeEvent(input$show_modularity, {
    modularity_on(!modularity_on())
  })
  
  visible_projection <- reactive({
    g <- bip_graph()
    
    proj <- bipartite_projection(g, which = TRUE)
    proj <- delete_edges(proj, which(E(proj)$weight < input$rep_proj_threshold))
    proj <- delete_vertices(proj, which(degree(proj) == 0))
    
    comps <- components(proj)
    induced_subgraph(proj, which(comps$membership %in% which(comps$csize >= 3)))
  })
  
  output$modularity_score <- renderUI({
    proj <- visible_projection()
    
    comm <- cluster_louvain(proj)
    score <- round(modularity(comm), 3)
    n_groups <- length(unique(membership(comm)))
    
    div(style = "background:#f0fdf4; border:1px solid #059669; border-radius:6px;
                 padding:0.4rem 0.8rem; font-size:0.85rem; color:#065f46; white-space:nowrap;",
        icon("chart-pie"), " ", strong(n_groups), " communities | modularity = ", strong(score))
  })
  
  output$proj_reps <- renderVisNetwork({
    g <- bip_graph()
    
    make_projection(
      bipartite_projection(g, which = TRUE),
      threshold = input$rep_proj_threshold,
      modularity_on = modularity_on())
  })
  
  output$centrality_chart <- renderPlot({
    d <- app_data()
    metric <- input$centrality_measure
    
    top_reps <- graph_from_data_frame(d$edges, vertices = d$nodes, directed = FALSE) |>
      as_tbl_graph() |>
      activate(nodes) |>
      mutate(is_rep = type == 1,
             degree = centrality_degree(),
             betweenness = centrality_betweenness()) |>
      as_tibble() |>
      filter(is_rep) |>
      arrange(desc(.data[[metric]])) |>
      slice_head(n = input$top_n)
    
    role_counts <- d$edges |>
      mutate(rep_id = as.character(to)) |>
      filter(rep_id %in% as.character(top_reps$name)) |>
      group_by(rep_id, Type) |>
      summarise(n = n(), .groups = "drop")
    
    rep_lookup <- top_reps |>
      mutate(rep_id = as.character(name)) |>
      select(rep_id, Name, cent_val = all_of(metric))
    
    plot_df <- role_counts |>
      left_join(rep_lookup, by = "rep_id") |>
      mutate(Name = fct_reorder(Name, cent_val),
             Type = factor(Type, levels = c("P", "R")))
    
    if (metric == "degree") {
      ggplot(plot_df, aes(x = Name, y = n, fill = Type)) +
        geom_col(width = 0.7, alpha = 0.9) +
        coord_flip() +
        scale_fill_manual(values = c("P" = "#3b82f6", "R" = "#f59e0b")) +
        labs(
          title = paste("Top", input$top_n, "Representatives by Degree Centrality"),
          subtitle = "Bar length = number of connected cases",
          x = NULL, y = "Case Count", fill = "Role") +
        theme_minimal(base_size = 13)
      
    } else {
      plot_df <- plot_df |>
        group_by(rep_id) |>
        mutate(y_val = (n / sum(n)) * cent_val) |>
        ungroup()
      
      ggplot(plot_df, aes(x = Name, y = y_val, fill = Type)) +
        geom_col(width = 0.7, alpha = 0.9) +
        coord_flip() +
        scale_fill_manual(values = c("P" = "#3b82f6", "R" = "#f59e0b")) +
        labs(
          title = paste("Top", input$top_n, "Representatives by Betweenness Centrality"),
          subtitle = "Bar length = betweenness score",
          x = NULL, y = "Betweenness Score", fill = "Role") +
        theme_minimal(base_size = 13)
    }
  })
}

shinyApp(ui = ui, server = server)
