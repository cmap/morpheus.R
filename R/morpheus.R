NULL

`%||%` <- function(a, b) {
    if (! is.null(a))
    a
    else
    b
}

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
na.rm = TRUE,
rowAnnotations=NULL,
columnAnnotations=NULL,
colorScheme = NULL,
rowSize = 13,
columnSize = 13,
drawGrid = TRUE,
gridColor = "#808080",
gridThickness = 0.1,
drawValues = FALSE,
width = NULL,
height = NULL,
...
) {
    payload <- create.payload(x,
        labRow = labRow,
		labCol = labCol,
		Rowv = Rowv,
		Colv=Colv,
		distfun = distfun,
		hclustfun = hclustfun,
		dendrogram = dendrogram,
		reorderfun = reorderfun,
		symm = symm,
		na.rm = na.rm,
		rowAnnotations=rowAnnotations,
		columnAnnotations=columnAnnotations,
		colorScheme = colorScheme,
		rowSize = rowSize,
		columnSize = columnSize,
		drawGrid = drawGrid,
		gridColor = gridColor,
		gridThickness = gridThickness,
		drawValues = drawValues,
		width = width,
		height = height,
		...)
    htmlwidgets::createWidget(
        name = 'morpheus',
        payload,
        width = width,
        height = height,
        package = 'morpheus',
        sizingPolicy = htmlwidgets::sizingPolicy(browser.fill = TRUE)
    )
}

morpheus.toJSON <- function(x,
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
na.rm = TRUE,
rowAnnotations=NULL,
columnAnnotations=NULL,
colorScheme = NULL,
rowSize = 13,
columnSize = 13,
drawGrid = TRUE,
gridColor = "#808080",
gridThickness = 0.1,
drawValues = FALSE,
width = NULL,
height = NULL,
...
) {

     payload <- create.payload(x,
            labRow = labRow,
    		labCol = labCol,
    		Rowv = Rowv,
    		Colv=Colv,
    		distfun = distfun,
    		hclustfun = hclustfun,
    		dendrogram = dendrogram,
    		reorderfun = reorderfun,
    		symm = symm,
    		na.rm = na.rm,
    		rowAnnotations=rowAnnotations,
    		columnAnnotations=columnAnnotations,
    		colorScheme = colorScheme,
    		rowSize = rowSize,
    		columnSize = columnSize,
    		drawGrid = drawGrid,
    		gridColor = gridColor,
    		gridThickness = gridThickness,
    		drawValues = drawValues,
    		width = width,
    		height = height,
    		...)
     jsonlite::toJSON(payload$options,dataframe = "columns", null = "null", na = "null", auto_unbox = TRUE,
                                       digits = getOption("shiny.json.digits", 16), use_signif = TRUE, force = TRUE,
                                       POSIXt = "ISO8601", UTC = TRUE, rownames = FALSE, keep_vec_names = TRUE,
                                       strict_atomic = TRUE)
}

create.payload <- function(x,
labRow = rownames(x),
labCol = colnames(x),
Rowv = TRUE,
Colv=if (symm)"Rowv" else TRUE,
distfun = dist,
hclustfun = hclust,
dendrogram = c("both", "row", "column", "none"),
reorderfun = function(d, w) reorder(d, w),
symm = FALSE,
na.rm = TRUE,
rowAnnotations=NULL,
columnAnnotations=NULL,
colorScheme = NULL,
rowSize = 13,
columnSize = 13,
drawGrid = TRUE,
gridColor = "#808080",
gridThickness = 0.1,
drawValues = FALSE,
width = NULL,
height = NULL,
...
) {
    name <- deparse(substitute(x))
    ## x is a matrix!
    if (! is.matrix(x)) {
        x <- as.matrix(x)
    }
    if (! is.matrix(x)) stop("x must be a matrix")
    options(expressions= 500000)
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
        if (length(rowInd) > nr || any(rowInd < 1 | rowInd > nr))
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
        if (length(colInd) > nc || any(colInd < 1 | colInd > nc))
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
    if(!is.null(rowAnnotations)){
        rowAnnotations <- rowAnnotations[rowInd,]
    }
    if(!is.null(columnAnnotations)){
        columnAnnotations <- columnAnnotations[colInd,]
    }

    ## Labels for Row/Column
    rownames(x) <- labRow %||% paste(1 : nrow(x))
    colnames(x) <- labCol %||% paste(1 : ncol(x))
    options(htmlwidgets.TOJSON_ARGS = list(dataframe = "column"))
    morpheusOptions <- list(...)
    morpheusOptions$colorScheme <- colorScheme
    morpheusOptions$rowSize <- rowSize
    morpheusOptions$columnSize <- columnSize
    morpheusOptions$drawGrid <- drawGrid
    morpheusOptions$gridColor <- gridColor
    morpheusOptions$gridThickness <- gridThickness
    morpheusOptions$drawValues <- drawValues

    x <- x[rowInd, colInd]
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

    columnDendrogram <- if (!is.null(ddc) &&
        is.dendrogram(ddc) &&
        dendrogram %in% c("both", "column")) dendToTree(ddc) else NULL
    rowDendrogram <- if (!is.null(ddr) &&
        is.dendrogram(ddr) &&
        dendrogram %in% c("both", "row")) dendToTree(ddr) else NULL



    rowVectors <- list()
    rowVectors[[1]] = list(name='id', array=rownames(x))

    if(!is.null(rowAnnotations)) {
        for (i in 1:ncol(rowAnnotations)) {
            rowVectors[[i+1]] = list(name= names(rowAnnotations)[i], array=rowAnnotations[,i])
        }
    }
    rowMetadataModel <- list(vectors=rowVectors)
    columnVectors <- list()
    columnVectors[[1]] = list(name='id', array=colnames(x))
    if(!is.null(columnAnnotations)) {
        for (i in 1:ncol(columnAnnotations)) {
            columnVectors[[i+1]] = list(name= names(columnAnnotations)[i], array=columnAnnotations[,i])
        }
    }
    columnMetadataModel <- list(vectors=columnVectors)
    dataset <- list(seriesNames=list(name), rows = nrow(x), columns = ncol(x),  seriesDataTypes=list('number'),
    seriesArrays=list(x), rowMetadataModel=rowMetadataModel, columnMetadataModel=columnMetadataModel)

    morpheusOptions$dataset = dataset
    morpheusOptions$name = name
    payload <- list(rowDendrogram = rowDendrogram, columnDendrogram = columnDendrogram, options=morpheusOptions)
    return(payload)
}

is.dendrogram <- function (x) { inherits(x, "dendrogram")}


read.gct <- function(file) {
  if(file=="clipboard" && .Platform$OS.type!="windows") {
    file <- pipe("pbpaste")
  }
  version = trimws(scan(
    file,
    what = "",
    nlines = 1,
    sep = "\t",
    quiet = TRUE
  )[1])
  dimensions = scan(
    file,
    what = double(0),
    nlines = 1,
    skip = 1,
    sep = "\t",
    quiet = TRUE
  )
  nrmat = dimensions[1]
  ncmat = dimensions[2]
  nrhd <- 0
  nchd <- 0
  if (version == "#1.3") {
    nrhd <- dimensions[3]
    nchd <- dimensions[4]
  }
  header = scan(
    file,
    what = "",
    nlines = 1,
    skip = 2,
    sep = "\t",
    quote = NULL,
    quiet = TRUE
  )
  # construct row header and column id's from the header line
  if (nrhd > 0) {
    rhd <- header[2:(nrhd + 1)]
    cid <- header[-(nrhd + 1):-1]
    column.offset <- 1
  }
  else {
    column.offset <- 2
    rhd = NULL
    cid = header[(1 + column.offset):length(header)]
  }
  # read in the next set of headers (column annotations) and shape into a matrix
  if (nchd > 0) {
    header = scan(
      file,
      what = "",
      nlines = nchd,
      skip = 3,
      sep = "\t",
      quote = NULL,
      quiet = TRUE
    )
    header = matrix(header,
                    nrow = nchd,
                    ncol = ncmat + nrhd + 1,
                    byrow = TRUE)
    # extract the column header and column descriptions
    chd = header[, 1]
    columnAnnotations = header[,-(nrhd + 1):-1]
    # need to transpose in the case where there's only one column annotation
    if (nchd == 1)
      columnAnnotations = t(columnAnnotations)
  }
  else {
    chd = NULL
    columnAnnotations = data.frame()
  }
  # read in the data matrix and row descriptions, shape into a matrix
  data = scan(
    file,
    what = "",
    nlines = nrmat,
    skip = 3 + nchd,
    sep = "\t",
    quote = NULL,
    quiet = TRUE
  )
  data = matrix(
    data,
    nrow = nrmat,
    ncol = ncmat + nrhd + column.offset,
    byrow = TRUE
  )
  # Extract the row id's row descriptions, and the data matrix
  rid = data[, 1]
  if (nrhd > 0) {
    # need as.matrix for the case where there's only one row annotation
    rowAnnotations = as.matrix(data[, 2:(nrhd + 1)])
    data = matrix(as.numeric(data[,-(nrhd + 1):-1]),
                 nrow = nrmat, ncol = ncmat)
  }
  else {
    rowAnnotations = data.frame()
    data = matrix(as.numeric(data[, (1 + column.offset):ncol(data)]), nrow = nrmat, ncol = ncmat)
  }
  # assign names to matrix
  dimnames(data) = list(rid, cid)
  if (nrhd > 0) {
    dimnames(rowAnnotations) = list(rid, rhd)
    rowAnnotations = as.data.frame(rowAnnotations, stringsAsFactors = FALSE)
  }
  if (nchd > 0) {
    columnAnnotations = t(columnAnnotations)
    dimnames(columnAnnotations) = list(cid, chd)
    columnAnnotations = as.data.frame(columnAnnotations, stringsAsFactors = FALSE)
  }
  return (
    list (
      data = data,
      rowAnnotations = rowAnnotations,
      columnAnnotations = columnAnnotations
    )
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
    if (! is.leaf(dend)) {
        tree$children <- lapply(dend, dendToTree)
    }
    tree
}


morpheusOutput <- function(outputId, width = '100%', height = '400px'){
    htmlwidgets::shinyWidgetOutput(outputId, 'morpheus', width, height, package = 'morpheus')
}

renderMorpheus <- function(expr, env = parent.frame(), quoted = FALSE) {
    if (! quoted) { expr <- substitute(expr)} # force quoted
    htmlwidgets::shinyRenderWidget(expr, morpheusOutput, env, quoted = TRUE)
}
