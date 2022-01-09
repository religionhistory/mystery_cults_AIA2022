# Visualize data

rm(list = ls())

source("../project_support.r")

# Load data
data <- read_csv("./input/mystery_cults.csv")
tree <- read.nexus(file = "./input/mystery_cults.tree")

# Order data by order in tree
tip_lab <- data.frame(`Entry ID` = tree$tip.label, check.names = FALSE)
data_ordered <- tip_lab %>%
  mutate(`Entry ID` = as.numeric(`Entry ID`)) %>%
  left_join(data) %>%
  mutate(label = `Entry ID`) %>%
  select(label, everything())

# Plot tree figure
tree_plot <- plot_tree_edge(tree)
tree_plot <- tree_plot %<+% data_ordered +
  geom_tiplab(aes(label = `Entry name`), size=2.5, offset=0.05, color = "black") +
  xlim(0, 2)
# Save figure
cairo_pdf("../figures/tree_figure.pdf", height = 4.5, width = 6.5)
plot(tree_plot) 
dev.off()

