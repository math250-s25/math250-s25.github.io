set.seed(119)

# generate independent normal rvs
y1 <- rnorm(1000, 0, 3)
y2 <- rnorm(1000, 0, 1)

A <- matrix(c(1, 1, 0, 1), nrow = 2)
Y <- rbind(y1, y2)
 
# set x1 to be y1 + y2
# set x2 to be y2
X <- t(A %*% Y)
p <- prcomp(X, scale = F)

plot(X, xlab = "x", ylab = "y",
     asp = 1, pch = 16, 
     col = rgb(red = 1, green = 0, blue = 0, alpha = 0.5))
arrows(x0 = 0, y0 = 0, x1 = (p$rotation %*% diag(p$sdev))[1, 1],
       y1 = (p$rotation %*% diag(p$sdev))[2, 1], col = "black", lwd = 3)
arrows(x0 = 0, y0 = 0, x1 = -(p$rotation %*% diag(p$sdev))[1, 2],
       y1 = -(p$rotation %*% diag(p$sdev))[2, 2], col = "black", lwd = 3)


plot(p$x, xlab = "PC1", ylab = "PC2", 
     asp = 1, pch = 16, 
     col = rgb(red = 1, green = 0, blue = 0, alpha = 0.5))
arrows(x0 = 0, y0 = 0, x1 = p$sdev[1],
       y1 = 0, col = "black", lwd = 3)
arrows(x0 = 0, y0 = 0, y1 = p$sdev[2],
       x1 = 0, col = "black", lwd = 3)


library(nwslR)
library(tidyverse)
teams <- load_teams() |> filter(last_season == 2023)
nwsl23 <- lapply(teams$team_abbreviation, 
                 function(x) load_team_season_stats(team_id = x, season = "2023")) |>
  bind_rows()

nwsl23_mat <- nwsl23[, c(-1, -2)] |>
  dplyr::select("possession_pct", 
                "goals", "assists", "pass_pct",
                "goal_conversion_pct", "clean_sheets",
                "shot_accuracy", "shots_total", "goals_conceded",
                "tackled") |>
  dplyr::select(where(is.numeric)) |>
  dplyr::select(where(~!any(is.na(.)))) |>
  dplyr::select(where(~ (length(unique(.)) > 1))) |>
  as.matrix(nrow = nrow(nwsl23))
rownames(nwsl23_mat) <- teams$team_abbreviation[match(nwsl23$team_id, teams$team_id)]
pca <- prcomp(nwsl23_mat, scale = T)

# biplot
biplot(pca)

# variable loadings
plot(pca$rotation[,1], pca$rotation[,2], col = "white", 
     xlim = c(-.75, .75), ylim = c(-.75, .75), xlab = "PC1", ylab = "PC2")
text(pca$rotation[,1], pca$rotation[,2], rownames(pca$rotation))

# projected coordinates
plot(pca$x[,1], pca$x[,2], col = "white", 
     xlim = c(-3, 3), ylim = c(-3, 3),
     xlab = "PC1", ylab = "PC2")
text(pca$x[,1], pca$x[,2], teams$team_abbreviation[match(nwsl23$team_id, teams$team_id)], 
     )

var_explained = pca$sdev^2 / sum(pca$sdev^2)

#create scree plot
library(ggplot2)
ggplot(data = data.frame(x = c(1:10), y = var_explained), aes(x = x, y = y)) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1)

state.pca <- prcomp(state.x77, scale. = TRUE)
biplot(state.pca)


plot(state.pca$rotation[,1], state.pca$rotation[,2], col = "white", 
     xlim = c(-.75, .75), ylim = c(-.75, .75), xlab = "PC1", ylab = "PC2")
text(state.pca$rotation[,1], state.pca$rotation[,2], rownames(state.pca$rotation))


plot(state.pca$x[,1], state.pca$x[,2], col = "white", 
     xlim = c(-4.5, 4.5), ylim = c(-2, 6),
     xlab = "PC1", ylab = "PC2")
text(state.pca$x[,1], state.pca$x[,2], rownames(state.x77))


