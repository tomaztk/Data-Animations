library(ggpubr)


dfm <- mtcars

# Convert the cyl variable to a factor
dfm$cyl <- as.factor(dfm$cyl)

# Add the name colums
dfm$name <- rownames(dfm)

# Inspect the data
head(dfm[, c("name", "wt", "mpg", "cyl")])

ggbarplot(dfm, x = "name", y = "mpg",
          fill = "cyl",               # change fill color by cyl
          color = "white",            # Set bar border colors to white
          palette = "jco",            # jco journal color palett. see ?ggpar
          sort.val = "asc",          # Sort the value in dscending order
          sort.by.groups = TRUE,     # Don't sort inside each group
          x.text.angle = 90,           # Rotate vertically x axis texts
          ggtheme = theme_pubclean()
)+
  font("x.text", size = 8, vjust = 0.5)



### Rigdes

library(ggplot2)
library(ggridges)
theme_set(theme_ridges())


ggplot(iris, aes(x = Sepal.Length, y = Species)) +
  geom_density_ridges(aes(fill = Species)) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))

ggplot(iris, aes(x = Sepal.Length, y = Species)) +
  geom_density_ridges(scale = 0.9) 
