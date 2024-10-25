
# * gplotData class ####

gplotData <- setClass("gplotData",
         slots = c(
                "object" = "ANY", # giotto object
                "filters" = "list", # spat_unit, feat_type, ext defaults
                "work" = "list", # processed layers or gg objects
                "layers" = "list" # gplotLayerParam objects
             )
         )

# * gplotLayer classes ####

gplotLayerParam <- setClass("gplotLayerParam", slots = c("data" = "ANY", "param" = "list"))

## * layer types ####

setClass("gplotLayerCustom", contains = "gplotLayerParam")
setClass("gplotLayerSpat", contains = "gplotLayerParam")
setClass("gplotLayerDim", contains = "gplotLayerParam")
setClass("gplotLayerNet", contains = "gplotLayerParam")
setClass("gplotLayerPolygon", contains = "gplotLayerParam")
setClass("gplotLayerPoint", contains = "gplotLayerParam")
setClass("gplotLayerImage", contains = "gplotLayerParam")
setClass("gplotLayerHexbin", contains = "gplotLayerParam")
setClass("gplotLayerDensity", contains = "gplotLayerParam")
setClass("gplotLayerLabel", contains = "gplotLayerParam")

# * show ####

setMethod("show", signature("gplotData"), function(object) {
    cat(wrap_txtf("%s:", class(object)))
    cat("\n")

    obj <- object@object
    if (!is.null(obj)) {
        cat(sprintf("-- %s --\n", color_yellow("data")))
        cat(sprintf("  %s", class(obj)))
        cat("\n")
    }

    if (length(object@layers) == 0L) {
        return(invisible()) #
    }

    cat(sprintf("\n-- %s --\n", color_yellow("plan")))
    cat(color_blue("start\n"))

    lyr_names <- names(object@layers)
    lyr_nums <- format(
        paste("[", seq_along(object@layers), "]", sep = "")
    )
    for (lyr_i in seq_along(object@layers)) {
        lyr_name <- lyr_names[[lyr_i]]
        if (!is.null(lyr_name) && !is.na(lyr_name)) {
            lyr_name <- sprintf("\"%s\"", lyr_name)
        }
        lyr_num <- lyr_nums[[lyr_i]]
        lyr <- object@layers[[lyr_i]]
        cat(sprintf(" %s %s %s\n", lyr_num, class(lyr), lyr_name %na% ""))
    }

    cat(color_blue("end\n"))
})


setMethod("show", signature("gplotLayerParam"), function(object) {
    cat(wrap_txtf("%s:", class(object)))
    cat("\n")

    toplevel <- object@param[names(object@param) != "mapping"]
    mapping <- object@param$mapping
    data <- object@data

    # data info
    if (!is.null(data)) {
        cat("-- data --\n")
        cat(sprintf("`%s` with: \n  \"%s\"\n\n",
                      class(data),
                      paste(collapse = "\", \"", names(data))
        ))
    }

    # empty object - return early
    if (sum(length(mapping), length(toplevel)) == 0L) {
        cat("<no params>")
        return(invisible())
    }

    # print params
    print(mapping)
    cat("Toplevel params\n")
    print_list(toplevel, pre = "*  ")
})

# * length ####

setMethod("length", signature("gplotData"), function(x) {
    length(x@layers)
})

# * names ####

setMethod("names", signature("gplotData"), function(x) {
    names(x@layers)
})

setMethod("names<-", signature(x = "gplotData", value = "ANY"), function(x, value) {
    names(x@layers) <- value
    return(x)
})

# * [ ####

#' @title gIndex
#' @description
#' class for handling indices similar to `index` class from the Matrix package.
#' simple class union (setClassUnion) of "numeric", "logical" and "character".
#' @keywords internal
#' @noRd
setClassUnion("gIndex", c("numeric", "logical", "character"))

setMethod("[", signature(x = "gplotData", i = "gIndex", j = "missing", drop = "missing"), function(x, i) {
    x@layers <- x@layers[i]
    return(x)
})


# * initialize ####

setMethod("initialize", signature("gplotLayerParam"), function(.Object, ..., data = NULL) {
    input = list(...)
    if (length(input) == 0 &&
        length(.Object@param) == 0) {
        return(.Object) # no input or params - return early
    }

    input <- c(.Object@param, input)
    input$data <- data
    gpp <- do.call(gg_param, args = input, quote = TRUE)

    # data
    if (!is.null(gpp$data)) {
        .Object@data <- gpp$data
    }

    # param
    not_data <- vapply(FUN.VALUE = logical(1L), names(gpp), function(n) {
        !identical(n, "data")
    })
    .Object@param <- gpp[not_data]

    return(.Object)
})

setMethod("initialize", signature("gplotData"),
          function(.Object, ..., ext = NULL, spat_unit = NULL, feat_type = NULL) {
    .Object <- callNextMethod(.Object, ...)

    return(.Object)
})






# * + layer appending ####

setMethod("+", signature(e1 = "gplotData", e2 = "gplotLayerParam"), function(e1, e2) {
    e1@layers <- c(e1@layers, e2)
    return(e1)
})








# exported functions ####

gplot <- function(gobject, ext = NULL, spat_unit = NULL, feat_type = NULL) {
    x <- new("gplotData")

    if (missing(gobject)) {
        return(x)
    }

    x@object <- gobject
    x <- initialize(x,
        ext = ext,
        spat_unit = spat_unit,
        feat_type = feat_type
    )
    return(x)
}


gplotLayerSpat <- function(...,
    feat = NULL,
    size = 3,
    alpha = 1, # vor alpha
    shape = c("border", "no_border", "voronoi"),
    stroke = 0.1,
    stroke_color = "black", # stroke and vor border
    vor_max_radius = 200,
    color_as_factor = FALSE,
    scale_alpha_with_value = FALSE,
    color_code = NULL,
    color_gradient = NULL,
    gradient_midpoint = NULL,
    gradient_style = c("divergent", "sequential"),
    gradient_limits = NULL,
    select_cell_groups = NULL,
    select_cells = NULL,
    show_other_cells = TRUE,
    other_cell_color = "lightgrey",
    other_point_size = 1,
    other_cells_alpha = 0.1,
    spat_loc_name = NULL,
    expression_values = NULL,
    spat_enr_names = NULL,
    x = "sdimx",
    y = "sdimy",
    spat_unit = NULL,
    feat_type = NULL,
    ext = NULL,
    data = NULL,
    warn_duplicates = TRUE) {
    # init param list
    p <- list(...)

    x <- new("gplotLayerSpat")

}

gplotLayerDim <- function(..., data) {
    x <- new("gplotLayerDim")
}

# links to preceding spat or dim
gplotLayerLabel <- function(...,
    type = c("text", "label"),
    size = 4,
    fontface = "bold",
    family = NULL,
    alpha = 1,
    angle = 0,
    color = "black",
    fill = NULL,
    padding = NULL, # label only
    show_cluster_center = FALSE,
    center_point_size = 4,
    center_point_border_col = "black",
    center_point_border_stroke = 0.1,
    data = NULL,
    warn_duplicates = TRUE) {
    # init param list
    p <- list(...)

    x <- new("gplotLayerLabel")
}

# links to preceding spat or dim
gplotLayerNet <- function(...,
    color = "lightgrey",
    alpha = 1,
    network_name = NULL,
    data = NULL,
    warn_duplicates = TRUE) {
    # init param list
    p <- list(...)

    x <- new("gplotLayerNet")
}


# theme

# basic

# coord_fix_ratio = 1,
# title = NULL,
# show_legend = TRUE,
# legend_text = 8,
# legend_symbol_size = 1,
# background_color = "white",
# axis_text = 8,
# axis_title = 8,

# saving

# show_plot = NULL,
# return_plot = NULL,
# save_plot = NULL,
# verbose = FALSE,
# save_param = list(),
# default_save_name = "spatPlot2D_single"
