NULL

`%||%` <- function(a, b) {
  if (!is.null(a))
  a
  else
  b
}


#' Morpheus Heatmap widget
#' 
#' Creates a morpheus.js-based heatmap widget.
#' 
#' @param x numeric matrix of the values to be plotted.
#' @param labRow character vectors with row labels to use (from top to bottom); default to rownames(x).
#' @param labCol character vectors with column labels to use (from left to right); default to colnames(x).
#' @param Rowv determines if and how the row dendrogram should be computed and reordered. Either a dendrogram or a vector of values used to reorder the row dendrogram or NA to suppress any row dendrogram (and reordering) or by default, NULL, see ‘Details’ below.
#' @param Colv determines if and how the column dendrogram should be reordered. Has the same options as the Rowv argument above and additionally when x is a square matrix, Colv = "Rowv" means that columns should be treated identically to the rows (and so if there is to be no row dendrogram there will not be a column one either).
#' @param distfun function used to compute the distance (dissimilarity) between both rows and columns. Defaults to dist.
#' @param hclustfun function used to compute the hierarchical clustering when Rowv or Colv are not dendrograms. Defaults to hclust. Should take as argument a result of distfun and return an object to which as.dendrogram can be applied.
#' @param reorderfun  function(d, w) of dendrogram and weights for reordering the row and column dendrograms. The default uses reorder.dendrogram.
#' @param rowAnnotations Data frame of additional row annotations in same order as x (optional)
#' @param columnAnnotations Data frame of additional column annotations in same order as x (optional)
#' @param colorScheme List of scalingMode ("fixed" or "relative"), stepped (Whether color scheme is continuous (FALSE) or discrete (TRUE)), values (list of numbers corresponding to colors), colors (list of colors)
#' @param rowSize Heat map column size in pixels or "fit" to fit heat map to current height (optional, defaults to 13)
#' @param columnSize Heat map column size in pixels or "fit" to fit heat map to current width (optional, defaults to 13)
#' @param drawGrid Whether to draw heat map grid (optional, defaults to \code{TRUE})
#' @param gridColor Heat map grid color (optional, defaults to "#808080")
#' @param gridThickness Heat map grid thickness (optional, defaults to 0.1)
#' @param drawValues Whether to draw values in the heat map (optional, defaults to \code{FALSE})
#' @param ... Additional morpheus options as documented at https://clue.io/morpheus/configuration.html
#' 
#' @import htmlwidgets
#'   
#' @export
#' @source 
#' 
#' @seealso 
#' \link{heatmap}
#' 
#' @examples 
#' library(morpheus)
#' rowAnnotations = data.frame(1:32)
#' morpheus(mtcars, rowAnnotations=rowAnnotations, colorScheme=list(values=list(0, 4), colors=list('green', 'black')))
#' 
morpheus <- function(x,
  labRow = rownames(x), 
  labCol = colnames(x), 
  Rowv = NULL, 
  Colv = if(symm)"Rowv" else NULL,
  distfun = dist, 
  hclustfun = hclust,
  reorderfun = function(d, w) reorder(d, w),
  rowAnnotations=NULL,
  columnAnnotations=NULL,
  symm = FALSE,
  na.rm = TRUE,
  width = NULL, height = NULL,...
) {
  
  name <- deparse(substitute(x))
  ## x is a matrix!
  if(!is.matrix(x)) {
  x <- as.matrix(x)
  }
  if(!is.matrix(x)) stop("x must be a matrix")
  
  nr <- dim(x)[1]
  nc <- dim(x)[2]
  ddc <- NULL
  ddr <- NULL
  doRdend <- !identical(Rowv, NA)
  doCdend <- !identical(Colv, NA)
  if (!doRdend && identical(Colv, "Rowv")) 
    doCdend <- FALSE
  if (is.null(Rowv)) 
    Rowv <- rowMeans(x, na.rm = na.rm)
  if (is.null(Colv)) 
    Colv <- colMeans(x, na.rm = na.rm)
  if (doRdend) {
    if (inherits(Rowv, "dendrogram")) 
      ddr <- Rowv
    else {
      hcr <- hclustfun(distfun(x))
      ddr <- as.dendrogram(hcr)
      if (!is.logical(Rowv) || Rowv) 
        ddr <- reorderfun(ddr, Rowv)
    }
    if (nr != length(rowInd <- order.dendrogram(ddr))) 
      stop("row dendrogram ordering gave index of wrong length")
  }
  else rowInd <- 1L:nr
  if (doCdend) {
    if (inherits(Colv, "dendrogram")) 
      ddc <- Colv
    else if (identical(Colv, "Rowv")) {
      if (nr != nc) 
        stop("Colv = \"Rowv\" but nrow(x) != ncol(x)")
      ddc <- ddr
    }
    else {
      hcc <- hclustfun(distfun(if (symm) 
        x
      else t(x)))
      ddc <- as.dendrogram(hcc)
      if (!is.logical(Colv) || Colv) 
        ddc <- reorderfun(ddc, Colv)
    }
    if (nc != length(colInd <- order.dendrogram(ddc))) 
      stop("column dendrogram ordering gave index of wrong length")
  }
  else colInd <- 1L:nc

  ddr <- rev(ddr)
  rowInd <- rev(rowInd) # reverse to match order of R heat maps
  x <- x[rowInd, colInd]

  ## Labels for Row/Column 
  rownames(x) <- labRow %||% paste(1:nrow(x))
  colnames(x) <- labCol %||% paste(1:ncol(x))  
  options(htmlwidgets.TOJSON_ARGS = list(dataframe="column"))  
  options <- list(...)
  columnDendrogram <- if(!is.null(ddc) && is.dendrogram(ddc)) dendToTree(ddc) else NULL
  rowDendrogram <- if(!is.null(ddr) && is.dendrogram(ddr)) dendToTree(ddr) else NULL
  payload <- list(rows = nrow(x),  rowDendrogram=rowDendrogram, columnDendrogram=columnDendrogram, columns = ncol(x), name=name, 
    array=x, rowNames=rownames(x), columnNames=colnames(x), rowAnnotations=rowAnnotations, columnAnnotations=columnAnnotations, options = options)
  # create widget
  htmlwidgets::createWidget(
  name = 'morpheus',
  payload,
  width = width,
  height = height,
  package = 'morpheus',
  sizingPolicy = htmlwidgets::sizingPolicy(browser.fill = TRUE)
  )

}



# Serialize a dendrogram object to a d3-friendly tree. The main
# requirement is that nodes are lists with child nodes in a
# field named `children`.
dendToTree <- function(dend) {
  tree <- c(
    as.list(attributes(dend)[c('height')])
  )

  # Recursively add children
  if (!is.leaf(dend)) {
    tree$children <- lapply(dend, dendToTree)
  }
  tree
}

#' Shiny bindings for morpheus
#'
#' Output and render functions for using morpheus within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a morpheus
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name morpheus-shiny
#'
#' @export
morpheusOutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'morpheus', width, height, package = 'morpheus')
}

#' @rdname morpheus-shiny
#' @export
renderMorpheus <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, morpheusOutput, env, quoted = TRUE)
}
