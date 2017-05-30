#' @import htmlwidgets
#' @importFrom grDevices col2rgb
#' @importFrom stats as.dendrogram dist hclust is.leaf order.dendrogram reorder

NULL

`%||%` <- function(a, b) {
    if (! is.null(a))
    a
    else
    b
}


#' Morpheus heat map widget
#' 
#' Creates a morpheus.js-based heat map widget.
#' 
#' @param x numeric matrix of the values to be plotted.
#' @param labRow character vector with row labels to use (optional, defaults to 
#'   \code{rownames(x)})
#' @param labCol character vector with column labels to use (optional, defaults 
#'   to \code{colnames(x)})
#'   
#' @param Rowv determines if and how the \emph{row} dendrogram should be 
#'   reordered.	By default, it is TRUE, which implies dendrogram is computed and
#'   reordered based on row means. If NULL or FALSE, then no dendrogram is 
#'   computed and no reordering is done. If a \code{\link{dendrogram}}, then it 
#'   is used "as-is", ie without any reordering. If a vector of integers, then 
#'   dendrogram is computed and reordered based on the order of the vector.
#' @param Colv  determines if and how the \emph{column} dendrogram should be 
#'   reordered. Has the options as the \code{Rowv} argument above and 
#'   \emph{additionally} when \code{x} is a square matrix, \code{Colv="Rowv"} 
#'   means that columns should be treated identically to the rows.
#' @param distfun function used to compute the distance (dissimilarity) between 
#'   both rows and columns. Defaults to \code{\link{dist}}.
#' @param hclustfun function used to compute the hierarchical clustering when 
#'   \code{Rowv} or \code{Colv} are not dendrograms. Defaults to 
#'   \code{\link{hclust}}.
#' @param dendrogram character string indicating whether to draw "none", "row", 
#'   "column" or "both" dendrograms. Defaults to "both". However, if Rowv (or 
#'   Colv) is FALSE or NULL and dendrogram is "both", then a warning is issued 
#'   and Rowv (or Colv) arguments are honoured.
#' @param reorderfun  \code{function(d, w)} of dendrogram and weights for 
#'   reordering the row and column dendrograms. The default uses 
#'   \code{\link{stats}{reorder.dendrogram}}.
#' @param symm logical indicating if \code{x} should be treated 
#'   \bold{symm}etrically; can only be true when \code{x} is a square matrix.
#'   
#' @param na.rm logical indicating whether \code{NA}'s should be removed.
#'   
#' @param rowAnnotations Data frame of additional row annotations in same order 
#'   as x (optional)
#' @param columnAnnotations Data frame of additional column annotations in same 
#'   order as x (optional)
#' @param colorScheme List of scalingMode ("fixed" or "relative"), stepped 
#'   (Whether color scheme is continuous (FALSE) or discrete (TRUE)), values 
#'   (list of numbers corresponding to colors), colors (list of colors)
#' @param rowSize Heat map column size in pixels or "fit" to fit heat map to 
#'   current height (optional, defaults to 13)
#' @param columnSize Heat map column size in pixels or "fit" to fit heat map to 
#'   current width (optional, defaults to 13)
#' @param drawGrid Whether to draw heat map grid (optional, defaults to 
#'   \code{TRUE})
#' @param gridColor Heat map grid color (optional, defaults to "#808080")
#' @param gridThickness Heat map grid thickness (optional, defaults to 0.1)
#' @param drawValues Whether to draw values in the heat map (optional, defaults 
#'   to \code{FALSE})
#' @param width Heat map width (optional, defaults to available width)
#' @param height Heat map height (optional, defaults to available height)
#' @param ... Additional morpheus options as documented at 
#'   \url{https://clue.io/morpheus/configuration.html}
#'   
#' @import htmlwidgets
#'   
#' @export
#' @source
#' 
#' @seealso \link{heatmap}, \link[gplots]{heatmap.2}
#' @examples 
#' library(morpheus)
#' rowAnnotations <- data.frame(annotation1=1:32, annotation2=sample(LETTERS[1:3], nrow(mtcars), replace = TRUE))
#' morpheus(mtcars, colorScheme=list(colors=heat.colors(3)), rowAnnotations=rowAnnotations, overrideRowDefaults=FALSE, rows=list(list(field='annotation2', highlightMatchingValues=TRUE, display=list('color'))))

morpheus <- function(x,
labRow = rownames(x),
labCol = colnames(x),

# dendrogram control
Rowv = TRUE,
Colv=if (symm)"Rowv" else TRUE,
distfun = dist,
hclustfun = hclust,
dendrogram = c("both", "row", "column", "none"),
reorderfun = function(d, w) reorder(d, w),
symm = FALSE,
rowAnnotations=NULL,
columnAnnotations=NULL,
na.rm = TRUE,
width = NULL, height = NULL, ...
) {
    name <- deparse(substitute(x))
    ## x is a matrix!
    if (! is.matrix(x)) {
        x <- as.matrix(x)
    }
    if (! is.matrix(x)) stop("x must be a matrix")

    nr <- dim(x)[1]
    nc <- dim(x)[2]
    ddc <- NULL
    ddr <- NULL
    if (! inherits(Rowv, "dendrogram")) {
        if (((is.logical(Rowv) && ! isTRUE(Rowv)) || (is.null(Rowv))) &&
        (dendrogram %in% c("both", "row"))) {
            warning("Discrepancy: Rowv is FALSE, while dendrogram is `",
            dendrogram, "'. Omitting row dendogram.")
            if (dendrogram == "both")
            dendrogram <- "column"
            else dendrogram <- "none"
        }
    }
    if (! inherits(Colv, "dendrogram")) {
        if (((is.logical(Colv) && ! isTRUE(Colv)) || (is.null(Colv))) &&
        (dendrogram %in% c("both", "column"))) {
            warning("Discrepancy: Colv is FALSE, while dendrogram is `",
            dendrogram, "'. Omitting column dendogram.")
            if (dendrogram == "both")
            dendrogram <- "row"
            else dendrogram <- "none"
        }
    }
    if (inherits(Rowv, "dendrogram")) {
        ddr <- Rowv
        rowInd <- order.dendrogram(ddr)
        if (length(rowInd) > nr || any(rowInd < 1 | rowInd >
        nr))
        stop("Rowv dendrogram doesn't match size of x")
        if (length(rowInd) < nr)
        nr <- length(rowInd)
    }
    else if (is.integer(Rowv)) {
        distr <- distfun(x)
        hcr <- hclustfun(distr)
        ddr <- as.dendrogram(hcr)
        ddr <- reorderfun(ddr, Rowv)
        rowInd <- order.dendrogram(ddr)
        if (nr != length(rowInd))
        stop("row dendrogram ordering gave index of wrong length")
    }
    else if (isTRUE(Rowv)) {
        Rowv <- rowMeans(x, na.rm = na.rm)
        distr <- distfun(x)
        hcr <- hclustfun(distr)
        ddr <- as.dendrogram(hcr)
        ddr <- reorderfun(ddr, Rowv)
        rowInd <- order.dendrogram(ddr)
        if (nr != length(rowInd))
        stop("row dendrogram ordering gave index of wrong length")
    }
    else if (! isTRUE(Rowv)) {
        rowInd <- nr : 1
        ddr <- as.dendrogram(hclust(dist(diag(nr))))
    }
    else {
        rowInd <- nr : 1
        ddr <- as.dendrogram(Rowv)
    }
    if (inherits(Colv, "dendrogram")) {
        ddc <- Colv
        colInd <- order.dendrogram(ddc)
        if (length(colInd) > nc || any(colInd < 1 | colInd >
        nc))
        stop("Colv dendrogram doesn't match size of x")
        if (length(colInd) < nc)
        nc <- length(colInd)
    }
    else if (identical(Colv, "Rowv")) {
        if (nr != nc)
        stop("Colv = \"Rowv\" but nrow(x) != ncol(x)")
        if (exists("ddr")) {
            ddc <- ddr
            colInd <- order.dendrogram(ddc)
        }
        else colInd <- rowInd
    }
    else if (is.integer(Colv)) {
        distc <- distfun(if (symm)
        x
        else t(x))
        hcc <- hclustfun(distc)
        ddc <- as.dendrogram(hcc)
        ddc <- reorderfun(ddc, Colv)
        colInd <- order.dendrogram(ddc)
        if (nc != length(colInd))
        stop("column dendrogram ordering gave index of wrong length")
    }
    else if (isTRUE(Colv)) {
        Colv <- colMeans(x, na.rm = na.rm)
        distc <- distfun(if (symm)
        x
        else t(x))
        hcc <- hclustfun(distc)
        ddc <- as.dendrogram(hcc)
        ddc <- reorderfun(ddc, Colv)
        colInd <- order.dendrogram(ddc)
        if (nc != length(colInd))
        stop("column dendrogram ordering gave index of wrong length")
    }
    else if (! isTRUE(Colv)) {
        colInd <- 1 : nc
        ddc <- as.dendrogram(hclust(dist(diag(nc))))
    }
    else {
        colInd <- 1 : nc
        ddc <- as.dendrogram(Colv)
    }

    ddr <- rev(ddr)
    rowInd <- rev(rowInd) # reverse to match order of R heat maps
    x <- x[rowInd, colInd]

    ## Labels for Row/Column
    rownames(x) <- labRow %||% paste(1 : nrow(x))
    colnames(x) <- labCol %||% paste(1 : ncol(x))
    options(htmlwidgets.TOJSON_ARGS = list(dataframe = "column"))
    morpheusOptions <- list(...)
    
    if (!is.null(morpheusOptions$colorScheme$colors)) {
      morpheusOptions$colorScheme$colors <- lapply(morpheusOptions$colorScheme$colors, function(color){
        rgb <- col2rgb(color)
        paste("rgb(", rgb[1], ",", rgb[2], ",", rgb[3], ")", sep='')
      })
    }
    
    if (!is.null(morpheusOptions$colorScheme$colors) && is.null(morpheusOptions$colorScheme$values)) { 
      rng = range(x)
      nvals <- length(morpheusOptions$colorScheme$colors)
      fractionStep <- 1/(nvals-1)
      values <- vector("list", nvals)
      dataRange <- rng[2] - rng[1]
      values[1] <- rng[1]
      values[nvals] <- rng[2]
      
      for(i in 2:nvals-1) {
        fraction <-fractionStep*(i-1)
        values[i] <- rng[1] + fraction*dataRange
      }
      morpheusOptions$colorScheme$values <- values
     
    }
    
    columnDendrogram <- if (! is.null(ddc) &&
        is.dendrogram(ddc) &&
        dendrogram %in% c("both", "row")) dendToTree(ddc) else NULL
    rowDendrogram <- if (! is.null(ddr) &&
        is.dendrogram(ddr) &&
        dendrogram %in% c("both", "column"))
    dendToTree(ddr) else NULL
    payload <- list(rows = nrow(x), rowDendrogram = rowDendrogram, columnDendrogram = columnDendrogram, columns = ncol(x), name = name,
    array = x, rowNames = rownames(x), columnNames = colnames(x), rowAnnotations = rowAnnotations, columnAnnotations = columnAnnotations, options = morpheusOptions)
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


is.dendrogram <- function (x) { inherits(x, "dendrogram")}

# Serialize a dendrogram object to a d3-friendly tree. The main
# requirement is that nodes are lists with child nodes in a
# field named `children`.
dendToTree <- function(dend) {
    tree <- c(
    as.list(attributes(dend)[c('height')])
    )

    # Recursively add children
    if (! is.leaf(dend)) {
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
    if (! quoted) { expr <- substitute(expr)} # force quoted
    htmlwidgets::shinyRenderWidget(expr, morpheusOutput, env, quoted = TRUE)
}
