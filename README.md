# Morpheus
Morpheus.js-based heat map widget in R.

## Install

```
devtools::install_github('cmap/morpheus.R')
```

## Example 1
```
library(morpheus)
rowAnnotations <- data.frame(annotation1=1:32, annotation2=sample(LETTERS[1:3], nrow(mtcars), replace = TRUE))
morpheus(mtcars, colorScheme=list(scalingMode="fixed", colors=heat.colors(3)), rowAnnotations=rowAnnotations, overrideRowDefaults=FALSE, rows=list(list(field='annotation2', highlightMatchingValues=TRUE, display=list('color'))))
```
![Example Output](https://raw.githubusercontent.com/cmap/morpheus.R/master/images/heatmap1.png)

 ## Example 2
```
library(morpheus)
rowAnnotations <- data.frame(annotation1=1:32, annotation2=sample(LETTERS[1:3], nrow(mtcars), replace = TRUE))
morpheus(mtcars, dendrogram='column', colorScheme=list(scalingMode="fixed", colors=heat.colors(3)), rowAnnotations=rowAnnotations, tools=list(list(name='Hierarchical Clustering', params=list(group_rows_by=list('annotation2'), cluster='Rows'))), rowGroupBy=list(list(field='annotation2')),
rows=list(list(field='annotation2',display=list('color'))))
```
![Example Output](https://raw.githubusercontent.com/cmap/morpheus.R/master/images/heatmap2.png)

## Shiny Example

```
library(morpheus)
library(shiny)
ui <- fluidPage(
  titlePanel(h3("Example")),
  mainPanel(
    morpheusOutput("heatmap")
  )
)

server <- function(input, output) {
  output$heatmap <- renderMorpheus({
    x <- matrix(rnorm(200), 20)
    y <- data.frame(a=letters[1:10], b=rep(c("g","h"),5), stringsAsFactors = F)
    morpheus(x, columnAnnotations = y)
  })
}

shinyApp(ui, server)
```
![Example Output](https://raw.githubusercontent.com/cmap/morpheus.R/master/images/shiny.png)
