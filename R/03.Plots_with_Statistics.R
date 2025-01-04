
# stat plots


library(ggstatsplot)
library(tidyverse)

??ggbetweenstats

plt <- ggbetweenstats(
  data = iris,
  x = Species,
  y = Sepal.Length,
  type = "parametric",
  p.adjust.method = "bonferroni",
  centrality.plotting = TRUE,
  centrality.type = "bayes"
  #stats::oneway.test()
)


plt <- plt + 
  labs(
    x = "Iris Species",
    y = "Sepal.Length Length",
    title = "Distribution of Sepal Length over Iris Species"
  ) + 
  theme(
    text = element_text(size = 8, color = "black"),
    plot.title = element_text(
      size = 20,
      face = "bold",
      color = "black"
    ),
    plot.subtitle = element_text(
      size = 15, 
      face = "bold",
      color="#1b2838"
    ),
    plot.title.position = "plot",
    axis.text = element_text(size = 10, color = "black"),
    axis.title = element_text(size = 12)
  )

plt



grouped_ggwithinstats(
  data            = dplyr::filter(bugs_long, region %in% c("Europe", "North America"), condition %in% c("LDLF", "LDHF")),
  x               = condition,
  y               = desire,
  type            = "np",
  xlab            = "Condition",
  ylab            = "Desire to kill an artrhopod",
  grouping.var    = region
)