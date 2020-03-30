#Step 1. Load the necessary packages and data files. Merge both data files to create a new one to work with.

library(readr)
library(dplyr)
library(igraph)
library(ggraph)

# Import the edgelist from the naccho2016clean.csv file
health.dep.edges <- read_csv(file = "datasets/naccho2016clean.csv")

# Import the attributes from the naccho2016att.csv file
health.dep.nodes <- read_csv(file = "datasets/naccho2016att.csv")

# Merge the edgelist and attributes into a network object
health.dep.net <- graph_from_data_frame(d = health.dep.edges, 
                                        vertices = health.dep.nodes, 
                                        directed = FALSE)

# Show the network object
health.dep.net

#Step 2. Clean up the data to remove loops inside health departments connected with themselves.

# Check for loops and multiples
is_simple(health.dep.net)

# Remove loops and multiples
health.dep.net <- simplify(health.dep.net, 
                           remove.multiple = TRUE, 
                           remove.loops = TRUE)

# Check for loops and multiples again
is_simple(health.dep.net)

#Step 3. Analyze how the network is displayed. Make an exploratory analysis.

# Count the number of vertices in the network
( num.health.dep <- vcount(graph = health.dep.net) )

# Count the number of edges in the network
( num.connections <- ecount(graph = health.dep.net) )

# Compute network density 
( net.density <- edge_density(graph = health.dep.net, loops = FALSE) )

#Step 4. Identify the nodes with the higest degree and betweenness centrality.
#A) Degree centrality: is a count of the number of connections a node has.
#B) Betweenness centrality quantifies the extent to which a node lies in the shortest path between any two other nodes in the network,often playing a bridging role.

# Identify highly connected nodes using degree
health.dep.nodes$health.dep.degree <- degree(health.dep.net)

# List the health departments with the highest degree
arrange(health.dep.nodes, -health.dep.degree)

# Identify bridges nodes using betweenness
health.dep.nodes$health.dep.between <- betweenness(health.dep.net)

# List the health departments with the highest betweenness
arrange(health.dep.nodes, -health.dep.between)

#Step 5. Identify key players and gaps in the network across Texas and Louisiana.

# Subset the network so it includes TX, LA 
region.net <- induced_subgraph(graph = health.dep.net, 
                               vids = which(V(health.dep.net)$state %in% c('LA', 'TX')))

# Find the number of vertices (i.e., network size) using vcount()
vcount(region.net)

# Use edge_density() to find the density of region.net
edge_density(region.net)

# Plot the network() with theme_graph
lhd.net.theme <- ggraph(graph = region.net, layout = "with_kk") +
  geom_edge_link() +
  geom_node_point(aes(color = state)) +
  theme_graph()

#Step 6. Use degree and betweenness centrality to find the key health departments in each state.

#Identify important nodes in each state using degree
region.net$degree <- degree(region.net)

#Get the top degree health depts for each state
top.degree.LA <- head(sort(region.net$degree[V(region.net)$state == "LA"], 
                           decreasing = T))

top.degree.TX <- head(sort(region.net$degree[V(region.net)$state == "TX"], 
                           decreasing = T))

#Identify important nodes in each state using betweenness
region.net$between <- betweenness(region.net)

#Get the top betweenness health depts for each state
top.bet.LA <- head(sort(region.net$between[V(region.net)$state == "LA"], 
                        decreasing = TRUE))
top.bet.TX <- head(sort(region.net$between[V(region.net)$state == "TX"], 
                        decreasing = TRUE))

#Step 7. Visualize several central health departments that were either highly connected (degree centrality) or were forming bridges between other health departments (betweenness centrality).

#Add degree to the node attributes
V(region.net)$degree <- degree(region.net)

#Plot with node size by degree, color by state, theme graph, Kamada Kawai layout
region.plot.degree <- ggraph(graph = region.net, layout = "with_kk") +
  geom_edge_link() +
  geom_node_point(aes(colour = state, size = degree)) +
  geom_node_text(aes(label = name, size = 1), nudge_y = .25) +
  theme_graph()
region.plot.degree

#Add betweenness to the node attributes
V(region.net)$between <- betweenness(region.net)

#Plot with node size by betweenness, color by state, theme graph, Kamada Kawai layout
region.plot.between <- ggraph(graph = region.net, layout = "with_kk") +
  geom_edge_link() +
  geom_node_point(aes(colour = state, size = between)) +
  geom_node_text(aes(label = name, size = 1), nudge_y = .25) +
  theme_graph()
region.plot.between

#Step 8. Subset in only one of the states network.

#Subset the network so it includes only CA
cali.net <- induced_subgraph(graph = health.dep.net, 
                             vids = which(V(health.dep.net)$state %in% "CA"))

#Find the number of vertices (i.e., network size) using vcount()
vcount(cali.net)

#Use edge_density() to find the density 
edge_density(cali.net)

#Find and sort degree centrality for each health department
top.cali.degree <- head(sort(degree(cali.net), decreasing = TRUE))

#Find and sort betweenness centrality for each health department
top.cali.between <- head(sort(betweenness(cali.net), decreasing = TRUE))

cali.net

#Step 9. Other characteristics of Health departments are:
#Rurality (rural/urban): Urban health departments are more central to the network/Rural health departmentsmight have more incentive to partner to fill gaps in service provision.
#Full-time employees(fte): More full-time employees, more stability.
#Leader.tenure (years the leader has been at the department). More years, more stability

# Fill in the `colour` parameter with the rurality attribute and the `size` parameter with degree to visualize rurality in cali.net
cali.net.rural.deg <- ggraph(graph = cali.net, layout = "with_kk") +
  geom_edge_link() +
  geom_node_point(aes(colour = rurality, size = degree(cali.net))) +
  geom_node_text(aes(label = name, size = 3), nudge_y = .2) +
  theme_graph()
cali.net.rural.deg

# Fill in the `colour` parameter with the population attribute and the `size` parameter with degree to visualize population in cali.net
cali.net.pop.deg <- ggraph(graph = cali.net, layout = "with_kk") +
  geom_edge_link() +
  geom_node_point(aes(colour = population, size = degree(cali.net))) +
  geom_node_text(aes(label = name, size = 3), nudge_y = .2) +
  theme_graph()
cali.net.pop.deg

# Fill in the `colour` parameter with the fte attribute and the `size` parameter with degree to visualize fte in cali.net
cali.net.fte.deg <- ggraph(graph = cali.net, layout = "with_kk") +
  geom_edge_link() +
  geom_node_point(aes(colour = fte, size = degree(cali.net))) +
  geom_node_text(aes(label = name, size = 3), nudge_y = .2) +
  theme_graph()
cali.net.fte.deg

#Step 10. Analize which health departments have high betweenness.
#The health departments with the most connections were a mix of urban and rural for the region.net and cali.net. 

#Compute betweenness for both networks
V(region.net)$between <- betweenness(region.net)
V(cali.net)$between <- betweenness(cali.net)

#Cali.net with rurality color nodes sized by betweenness
cali.net.rural.bet <- ggraph(graph = cali.net, layout = "with_kk") +
  geom_edge_link() +
  geom_node_point(aes(colour = rurality, size = between)) +
  geom_node_text(aes(label = name, size = 3), nudge_y = .2) +
  theme_graph()
cali.net.rural.bet

#Cali.net with population color nodes sized by betweenness
cali.net.pop.bet <- ggraph(graph = cali.net, layout = "with_kk") +
  geom_edge_link() +
  geom_node_point(aes(colour = population, size = between)) +
  geom_node_text(aes(label = name, size = 3), nudge_y = .2) +
  theme_graph()
cali.net.pop.bet

#Cali.net with fte color nodes sized by betweenness
cali.net.fte.bet <- ggraph(graph = cali.net, layout = "with_kk") +
  geom_edge_link() +
  geom_node_point(aes(colour = fte, size = between)) +
  geom_node_text(aes(label = name, size = 3), nudge_y = .2) +
  theme_graph()
cali.net.fte.bet
