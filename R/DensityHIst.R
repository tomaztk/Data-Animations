library(ggplot2)
library(ggridges)
# install.packages("ggridges")
theme_set(theme_minimal())


# Opened polygons
ggplot(iris, aes(x = Sepal.Length, y = Species, group = Species)) + 
  geom_density_ridges(fill = "#00AFBB")

# Closed polygons
ggplot(iris, aes(x = Sepal.Length, y = Species, group = Species)) + 
  geom_density_ridges2(fill = "#00AFBB")

# Cut off the trailin tails. 
# Specify `rel_min_height`: a percent cutoff
ggplot(iris, aes(x = Sepal.Length, y = Species)) + 
  geom_density_ridges(fill = "#00AFBB", rel_min_height = 0.01)



# scale = 0.6, not touching
ggplot(iris, aes(x = Sepal.Length, y = Species)) + 
  geom_density_ridges(scale = 0.6)

# scale = 1, exactly touching
ggplot(iris, aes(x = Sepal.Length, y = Species)) + 
  geom_density_ridges(scale = 1)

# scale = 5, substantial overlap
ggplot(iris, aes(x = Sepal.Length, y = Species)) + 
  geom_density_ridges(scale = 5, alpha = 0.7)

# Change the density area fill colors by groups
ggplot(iris, aes(x = Sepal.Length, y = Species)) +
  geom_density_ridges(aes(fill = Species)) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
  theme(legend.position = "none")




ggplot(
  lincoln_weather, 
  aes(x = `Min Temperature [F]`, y = `Month`, fill = stat(x))
) +
  geom_density_ridges_gradient(scale = 3, size = 0.3, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "Temp. [F]", option = "C") +
  labs(title = 'Temperatures in Lincoln NE') 




# Add quantiles Q1, Q2 (median) and Q3
ggplot(iris, aes(x = Sepal.Length, y = Species)) +
  stat_density_ridges(quantile_lines = TRUE)

# Show only the median line (50%)
# Use quantiles = 2 (for Q2) or quantiles = 50/100
ggplot(iris, aes(x = Sepal.Length, y = Species)) +
  stat_density_ridges(quantile_lines = TRUE, quantiles = 0.5)

# Indicate the 2.5% and 97.5% tails
ggplot(iris, aes(x = Sepal.Length, y = Species)) +
  stat_density_ridges(quantile_lines = TRUE, quantiles = c(0.025, 0.975), alpha = 0.7)



# Color by quantiles
ggplot(iris, aes(x = Sepal.Length, y = Species, fill = factor(stat(quantile)))) +
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  scale_fill_viridis_d(name = "Quartiles")



# Highlight the tails of the distributions
ggplot(iris, aes(x = Sepal.Length, y = Species, fill = factor(stat(quantile)))) +
  stat_density_ridges(
    geom = "density_ridges_gradient",
    calc_ecdf = TRUE,
    quantiles = c(0.025, 0.975)
  ) +
  scale_fill_manual(
    name = "Probability", values = c("#FF0000A0", "#A0A0A0A0", "#0000FFA0"),
    labels = c("(0, 0.025]", "(0.025, 0.975]", "(0.975, 1]")
  )


# Add jittered points
ggplot(iris, aes(x = Sepal.Length, y = Species)) +
  geom_density_ridges(jittered_points = TRUE)

# Control the position of points
# position = "raincloud"
ggplot(iris, aes(x = Sepal.Length, y = Species)) +
  geom_density_ridges(
    jittered_points = TRUE, position = "raincloud",
    alpha = 0.7, scale = 0.9
  )

# position = "points_jitter"
ggplot(iris, aes(x = Sepal.Length, y = Species)) +
  geom_density_ridges(
    jittered_points = TRUE, position = "points_jitter",
    alpha = 0.7, scale = 0.9
  )

# Add marginal rug
ggplot(iris, aes(x = Sepal.Length, y = Species)) +
  geom_density_ridges(
    jittered_points = TRUE,
    position = position_points_jitter(width = 0.05, height = 0),
    point_shape = '|', point_size = 3, point_alpha = 1, alpha = 0.7,
  )



# Styling jittered points
ggplot(iris, aes(x = Sepal.Length, y = Species, fill = Species)) +
  geom_density_ridges(
    aes(point_color = Species, point_fill = Species, point_shape = Species),
    alpha = .2, point_alpha = 1, jittered_points = TRUE
  ) +
  scale_point_color_hue(l = 40) +
  scale_discrete_manual(aesthetics = "point_shape", values = c(21, 22, 23))


# Styling vertical quantile lines
ggplot(iris, aes(x = Sepal.Length, y = Species)) +
  geom_density_ridges(
    jittered_points = TRUE, quantile_lines = TRUE, scale = 0.9, alpha = 0.7,
    vline_size = 1, vline_color = "red",
    point_size = 0.4, point_alpha = 1,
    position = position_raincloud(adjust_vlines = TRUE)
  )


ggplot(iris, aes(x = Sepal.Length, y = Species, height = stat(density))) + 
  geom_density_ridges(
    stat = "binline", bins = 20, scale = 0.95,
    draw_baseline = FALSE
  )



library(readr)
library(gridExtra)
library(grid)
library(plyr)

iris <- iris
colnames(iris) <- c("SepalLengthCm","SepalWidthCm","PetalLengthCm","PetalWidthCm","Species")

DhistPl <- ggplot(iris, aes(x=PetalLengthCm, colour=Species, fill=Species)) +
  geom_density(alpha=.3) +
  geom_vline(aes(xintercept=mean(PetalLengthCm),  colour=Species),linetype="dashed",color="grey", size=1)+
  xlab("Petal Length (cm)") +  
  ylab("Density")+
  theme(legend.position="none")

DhistPw <- ggplot(iris, aes(x=PetalWidthCm, colour=Species, fill=Species)) +
  geom_density(alpha=.3) +
  geom_vline(aes(xintercept=mean(PetalWidthCm),  colour=Species),linetype="dashed",color="grey", size=1)+
  xlab("Petal Width (cm)") +  
  ylab("Density")

DhistSw <- ggplot(iris, aes(x=SepalWidthCm, colour=Species, fill=Species)) +
  geom_density(alpha=.3) +
  geom_vline(aes(xintercept=mean(SepalWidthCm),  colour=Species), linetype="dashed",color="grey", size=1)+
  xlab("Sepal Width (cm)") +  
  ylab("Density")+
  theme(legend.position="none")

DhistSl <- ggplot(iris, aes(x=SepalLengthCm, colour=Species, fill=Species)) +
  geom_density(alpha=.3) +
  geom_vline(aes(xintercept=mean(SepalLengthCm),  colour=Species),linetype="dashed", color="grey", size=1)+
  xlab("Sepal Length (cm)") +  
  ylab("Density")+
  theme(legend.position="none")

grid.arrange(DhistSl + ggtitle(""),
             DhistSw + ggtitle(""),
             DhistPl + ggtitle(""),
             DhistPw + ggtitle(""),
             nrow = 2,
             top = textGrob("Iris Density Histogram", gp=gpar(fontsize=14))
)
