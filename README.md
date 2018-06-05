# Morpheus
Morpheus.js-based heat map widget in R.

## Install

```
devtools::install_github('cmap/morpheus.R')
```

## Example

```
library(morpheus)
rowAnnotations <- data.frame(annotation1=1:32, annotation2=sample(LETTERS[1:3], nrow(mtcars), replace = TRUE))
morpheus(mtcars, colorScheme=list(scalingMode="fixed", colors=heat.colors(3)), rowAnnotations=rowAnnotations, overrideRowDefaults=FALSE, rows=list(list(field='annotation2', highlightMatchingValues=TRUE, display=list('color'))))
```

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

