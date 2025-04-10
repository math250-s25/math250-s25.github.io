library(tidyverse)
library(igraph)
library(palmerpenguins)
data(penguins) 
ggplot(penguins, 
       aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_point()
gaussian_kernel <- function(x, y, h) {
  return(exp(-1 * sum((x - y) ^ 2) / h ^ 2))
}
penguins <- penguins |>
  na.omit() 
species_label <- penguins$species
penguins <- penguins |>
  select(-species, -island, -sex, -year) 

dist_mat <- as.matrix(dist(penguins))
edge_list <- data.frame(V1 = NULL, V2 = NULL)
for (i in 1:nrow(dist_mat)) {
  edge_list <- 
    bind_rows(edge_list,
              data.frame(V1 = i,
                         V2 = order(dist_mat[i, ])[2:11])
    )
}

g <- graph_from_data_frame(edge_list, directed = F)
plot(g)
E(g)$weight <- 
  apply(edge_list, 1,
        function(x) gaussian_kernel(penguins[x[1],], penguins[x[2],], 1))
L <- as.matrix(laplacian_matrix(g))
L_eigen <- eigen(L)

L_eigen$values
plot_dat <- data.frame(x = L_eigen$vectors[, 331],
                       y = L_eigen$vectors[, 330],
                       species = species_label)
ggplot(plot_dat,
       aes(x = x, y = y, color = species)) +
  geom_point()
