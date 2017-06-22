\name{morpheus}
\alias{morpheus}
\title{Morpheus heat map widget}
\description{Creates a morpheus.js-based heat map widget.}
\usage{
morpheus(x, 
  labRow = rownames(x), 
  labCol = colnames(x),
  # dendrogram control 
  Rowv = TRUE,
  Colv = if (symm) "Rowv" else TRUE, 
  distfun = dist, hclustfun = hclust,
  dendrogram = c("both", "row", "column", "none"), 
  reorderfun = function(d, w) reorder(d, w), 
  symm = FALSE, 
  # plot 
  na.rm = TRUE, 
  rowAnnotations = NULL,
  columnAnnotations = NULL, 
  colorScheme = NULL,
  rowSize = 13,
  columnSize = 13,
  drawGrid = TRUE,
  gridColor = "#808080",
  gridThickness = 0.1,
  drawValues = FALSE,
  width = NULL, 
  height = NULL,
  ...)
}
\arguments{
\item{x}{numeric matrix of the values to be plotted.}

\item{labRow}{character vector with row labels to use (optional, defaults to 
\code{rownames(x)})}

\item{labCol}{character vector with column labels to use (optional, defaults 
to \code{colnames(x)})}

% Dendrogram Control
\item{Rowv}{determines if and how the \emph{row} dendrogram should be 
reordered.    By default, it is TRUE, which implies dendrogram is computed and
reordered based on row means. If NULL or FALSE, then no dendrogram is 
computed and no reordering is done. If a \code{\link{dendrogram}}, then it 
is used "as-is", ie without any reordering. If a vector of integers, then 
dendrogram is computed and reordered based on the order of the vector.}

\item{Colv}{determines if and how the \emph{column} dendrogram should be 
reordered. Has the options as the \code{Rowv} argument above and 
\emph{additionally} when \code{x} is a square matrix, \code{Colv="Rowv"} 
means that columns should be treated identically to the rows.}

\item{distfun}{function used to compute the distance (dissimilarity) between 
both rows and columns. Defaults to \code{\link{dist}}.}

\item{hclustfun}{function used to compute the hierarchical clustering when 
\code{Rowv} or \code{Colv} are not dendrograms. Defaults to 
\code{\link{hclust}}.}

\item{dendrogram}{character string indicating whether to draw "none", "row", 
"column" or "both" dendrograms. Defaults to "both". However, if Rowv (or 
Colv) is FALSE or NULL and dendrogram is "both", then a warning is issued 
and Rowv (or Colv) arguments are honoured.}

\item{reorderfun}{\code{function(d, w)} of dendrogram and weights for 
reordering the row and column dendrograms. The default uses 
\code{\link{stats}{reorder.dendrogram}}.}

\item{symm}{logical indicating if \code{x} should be treated 
\bold{symm}etrically; can only be true when \code{x} is a square matrix.}

\item{na.rm}{logical indicating whether \code{NA}'s should be removed.}

% Plot
\item{rowAnnotations}{Data frame of additional row annotations in same order 
as x (optional)}

\item{columnAnnotations}{Data frame of additional column annotations in same 
order as x (optional)}

\item{colorScheme}{List of scalingMode ("fixed" or "relative"), stepped 
(Whether color scheme is continuous (FALSE) or discrete (TRUE)), values 
(list of numbers corresponding to colors), colors (list of colors)}

\item{rowSize}{Heat map column size in pixels or "fit" to fit heat map to 
current height (optional, defaults to 13)}

\item{columnSize}{Heat map column size in pixels or "fit" to fit heat map to 
current width (optional, defaults to 13)}

\item{drawGrid}{Whether to draw heat map grid (optional, defaults to 
\code{TRUE})}

\item{gridColor}{Heat map grid color (optional, defaults to "#808080")}

\item{gridThickness}{Heat map grid thickness (optional, defaults to 0.1)}

\item{drawValues}{Whether to draw values in the heat map (optional, defaults 
to \code{FALSE})}

\item{width}{Heat map width (optional, defaults to available width)}

\item{height}{Heat map height (optional, defaults to available height)}

\item{...}{Additional morpheus options as documented at 
\url{https://software.broadinstitute.org/morpheus/configuration.html}}

}

\examples{
library(morpheus)
rowAnnotations <- data.frame(annotation1=1:32, annotation2=sample(LETTERS[1:3], nrow(mtcars), 
  replace = TRUE))
morpheus(mtcars, 
  colorScheme=list(scalingMode="fixed", colors=heat.colors(3)),
  rowAnnotations=rowAnnotations, 
  overrideRowDefaults=FALSE, 
  rows=list(list(field='annotation2', highlightMatchingValues=TRUE, display=list('color'))))
# Select Edit > Copy Selected Dataset in Morpheus to copy selection to clipboard in gct format
# data <- read.gct('clipboard') # read the clipboard data into R

}
\seealso{
\link{heatmap}, \link[gplots]{heatmap.2}
}
