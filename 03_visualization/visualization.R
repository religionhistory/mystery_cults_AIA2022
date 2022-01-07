# Visualize data

rm(list = ls())

source("../project_support.R")

# Load data
data <- read_csv("./input/data_cluster.csv")
tree <- read.nexus(file = "./input/mystery_cults.tree")

# Order data by order in tree
tip_lab <- data.frame(`Entry ID` = tree$tip.label, check.names = FALSE)
data_ordered <- tip_lab %>%
  mutate(`Entry ID` = as.numeric(`Entry ID`)) %>%
  left_join(data) %>%
  mutate(label = `Entry ID`) %>%
  select(label, everything()) %>%
  mutate(`Entry ID` = as.character(`Entry ID`))

# Find edge lengths
edges <- data.frame(tree$edge, edge_length=round(tree$edge.length,2)) %>% 
  rename("parent" = "X1", "node" = "X2")

# Extract clusters
cluster_list <- extract_clusters(data_ordered)

# Remove cluster group for plotting
data_plot <- data_ordered %>%
  select(-Cluster)

# Plot tree figure
cluster_tree <- groupOTU(tree, cluster_list, group_name = "Cluster")
tree_plot <- ggtree(cluster_tree, aes(color = Cluster))
tree_plot <- ggtree(cluster_tree, aes(color= Cluster)) +
  scale_color_manual(breaks = c("C1.1", "C1.2.1", "C1.2.2", "C2"), values = c("black", "#0072B2","#ec5dbc", "#52c2f9", "#cc4d53",  "#7935c6"), labels = c("C1.1", "C1.2.1", "C1.2.2", "C2")) 
tree_plot <- tree_plot %<+% 
  edges + geom_text(aes(x = branch, label = edge_length), size = 2, hjust = -.2, vjust=-.2, show.legend = FALSE) 
tree_plot <-  tree_plot %<+% data_plot +
  geom_tiplab(aes(label = `Entry name`), size=2.5, offset=0.06, color = "black", show.legend = FALSE) +
  xlim(0, 2.5) +
  theme(legend.text=element_text(size=8), legend.title=element_text(size=9))
# Save figure
cairo_pdf("../figures/tree_figure.pdf", height = 4.5, width = 7.4)
plot(tree_plot) 
dev.off()

