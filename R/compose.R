
# Composable plotting for Giotto

# composable elements
#  - Functions that define something to plot from the Giotto object. Accepts
#    params information for that plotting action. Also handles other things such
#    as dealing with the gobject for data extraction and combining based on what
#    kinds of plots are requested. Also saving, cowplot etc.
#  - Returns `gvis` object which can be combined with other `gvis` via `+`.
#  - `gvis` is a list of 2 elements
#    - 1. plist - a list of named `gvisparam` objects
#    - 2. render - closure that defines a workflow for the gobject to
#                  produce a desired plot defined by the composables.
#  - `+.gvis` is a complex function that first examines the `gvis` plist for
#    incompatibilities and then reorders the items if needed. It then generates
#    the workflow closure based on plist.
#
# General workflow:
# gvis <- comp1(param1, param2, ...) + comp2(param1 + param3, ...) + ...
# gvis$render(gobject) # to plot





gvis <- function(
    spat_unit = NULL,
    feat_type = NULL,
    ...,
    background = NULL,
    show_plot = NULL,
    save_plot = NULL,
    return_plot = NULL,
    save_param = NULL
  ) {

  gobject_params <- structure(
    list(
      spat_unit = spat_unit,
      feat_type = feat_type
    ), class = c("gvisparam")
  )

  plot_params <- structure(
    list(
      background = background,
      show_plot = show_plot,
      save_plot = save_plot,
      return_plot = return_plot,
      save_param = save_param,
      ...
    ), class = c("gvisparam")
  )




  structure(
    list(

    ),
    class("gvis")
  )

  return(gscene)
}


print.gvisparam <- function(x, ...) {
  cat("GiottoVisuals params: \n")
  if (length(x) == 0) {
    cat("<empty>\n")
  } else {
    values <- vapply(x, function(val) {
      utils::capture.output(val) %>%
        gsub(pattern = "^\\[.*\\] ", replacement = "")
    }, FUN.VALUE = character(1L))
    bullets <- sprintf("* %s -> %s\n",
                       format(sprintf("`%s`", names(x))),
                       values)
    cat(bullets, sep = "")
  }
  invisible(x)
}

`[.gvisparam` <- function(x, i, j, ..., drop = FALSE) {
  structure(NextMethod(), class = "gvisparam")
}

`[<-.gvisparam` <- function(x, i, j, ..., value) {
  structure(NextMethod(), class = "gvisparam")
}

# order matters. First item(s) are prioritized
c.gvisparam <- function(x, ...) {
  comb <- structure(NextMethod(), class = "gvisparam")
  unique_idx <- match(unique(names(comb)), names(comb))
  return(comb[unique_idx])
}



# core function in charge of curating the render function based on the gvisparams
# available in the component `gvis` objects
`+.gvis` <- function(e1, e2) {

  # checks
  combined_plist <- c(e1$plist, e2$plist)
  dup_params <- duplicated(names(combined_plist))
  if (any(dup_params)) {
    stop(sprintf(
      "More than one call to the same type of plot component found: \n%s",
      paste(unique(dup_params), sep = ", ")
    ))
  }



  # assemble data #
  # Extract and combine necessary data from Giotto object in series of closures.
  # These closures must find the needed data from the object, coerce it into a
  # format convenient for combining with other necessary information (by relying)
  # on an understood standard, and then merging them.
  # The list of closures will then be sent to .data_assembly workflow function
  # Final output should be a combined data.frame-like of the needed information to
  # plot.
  #
  # Each component closure should be designed with the params data and gobject
  # where the data is modified, but the gobject is static.
  #
  # The returned assembly function should only take the gobject as input.

  data_assembly <- function(...) {
    closures <- list(...)

    assembly_closure <- function(gobject) {
      for (closure in closures) {
        data <- closure(data, gobject)
      }
      return(data)
    }
    return(assembly_closure)
  }


  # assemble plot function #

  plot_assembly <- function(...) {
    closures <- list(...)

    assembly_closure <- function(p) {
      for (closure in closures) {
        p <- closure(p)
      }
    }
    return(assembly_closure)
  }


}

























gvis_image <- function(name = "image", type = c("largeImage", "image")) {
  type = match.arg(type, choices = c("largeImage", "image"))

  out <- list(
    name = name,
    type = type
  )
  class(out) <- c("gvis_image", "GiottoVisuals")
}

# metadata
gvis_aes <- function(...) {
  a <- list(...)


  aes_params <- list(
    color = .standardize_param(a, c("col", "colour", "color")),
    color_as_factor = a$color_as_factor %null% TRUE,
    color_code = a$color_code,
    color_gradient = a$color_gradient,

    shape = a$shape %null% c("border", "no_border", "voronoi"),
    size = a$size %null% 3,
    alpha = a$alpha %null% 1,

    border_col = a$border_col %null% "black",
    border_stroke = a$border_stroke %null% 0.1,

    label_size = a$label_size %null% 4,
    label_fontface = a$label_fontface %null% "bold",

    gradient_midpoint = a$gradient_midpoint,
    gradient_style = a$gradient_style %null% c("divergent", "sequential"),
    gradient_limits = a$gradient_limits
  )

  class(aes_params) <- c("gvis_aes")

  return(aes_params)
}

# returns NULL if none are present
.standardize_param <- function(args_list, params) {
  a <- args_list[params]

  isnull <- vapply(a, is.null, FUN.VALUE = logical(length = 1L))
  isna <- is.na(a)
  present <- !(isnull | isna)

  if (sum(present) > 1L) {
    stop(sprintf("Only one of \"%s\" can be provided", paste0(params, collapse = "\", \"")))
  }

  out <- unlist(a[present])
  names(out) <- NULL

  return(out)
}


print.gvis_scene <- function(x, ...) {
  # empty scene case
  if (length(x$layers) == 0L) {
    cat(sprintf("NULL %s", class(x)[[1]]))
    return(invisible(NULL))
  }


}


# modularly assemble and prep data for plotting ####


# modular functions to add to gg plots ####



p <- ggplot(data.table::as.data.table(mtcars), aes(x = {{ a }}, y = mpg)) + geom_point(aes())




sl <- GiottoData::loadSubObjectMini("spatLocsObj")
sn <- GiottoData::loadSubObjectMini("spatialNetworkObj")
dr <- GiottoData::loadSubObjectMini("dimObj", 2)


p <- ggplot() + geom_point(data = dr, aes_string2(x = , y = dr[][,2]))

gg_giotto_spatlocs <- function(x, mapping = NULL, ..., inherit.aes = TRUE) {
  mapping <- .append_aes(
    aes(x = sdimx, y = sdimy),
    mapping
  )

  structure(
    list(x = x[], mapping = mapping,
         params = list(..., inherit.aes = inherit.aes)),
    class = "gg_giotto_spatlocs"
  )
}

gg_giotto_dimred <- function(x, mapping = NULL, dim1 = 1L, dim2 = 2L, ..., inherit.aes = TRUE) {
  dt <- data.table::as.data.table(x[, c(dim1, dim2)][], keep.rownames = TRUE)

  # format data with colnames cell_ID, key.1, key.2
  new_colnames <- paste(objName(x), c(dim1, dim2), sep = ".")
  data.table::setnames(dt, new = c("cell_ID", new_colnames))

  mapping <- .append_aes(
      aes(x = .data[[new_colnames[1L]]], y = .data[[new_colnames[2L]]]),
      mapping
  )

  structure(
    list(x = dt, mapping = mapping,
         params = list(..., inherit.aes = inherit.aes)),
    class = "gg_giotto_dimred"
  )
}

# adds network connections to existing set of points data
  gg_giotto_network <- function(x, mapping = NULL, ..., inherit.aes = TRUE) {
    if (!is.null(mapping)) {
      if (!is.null(mapping$x) || !is.null(mapping$y)) {
        stop("aes mappings for 'x' and 'y' are not accepted for this function\n")
      }
    }

    structure(
      list(x = x[], mapping = mapping,
           params = list(..., inherit.aes = inherit.aes)),
      class = "gg_giotto_network"
    )
  }

ggplot_add.gg_giotto_network <- function(object, plot, object_name) {
  nodes <- plot$data
  # expect that spatlocs info is first available GeomPoints layer
  browser()
}

ggplot_add.gg_giotto_dimred <- function(object, plot, object_name) {
  a <- c(
    list(mapping = object$mapping, data = object$x),
    object$params
  )

  plot + do.call(geom_point, args = a)
}

ggplot_add.gg_giotto_spatlocs <- function(object, plot, object_name) {
  a <- c(
    list(mapping = object$mapping, data = object$x),
    object$params
  )
  plot + do.call(geom_point, args = a)
}



# order matters
.append_aes <- function(x, y) {
  structure(c(x, y), class = "uneval")
}

# does something similar to the function from gginnards
# match_type will pull all layers that match the provided gg type
# idx further refines those matches by pulling the nth entries
# if match_type is not provided, idx simply pulls the nth layers
.gg_extract_layers <- function(x, match_type = NULL, idx = NULL) {

  if (is.null(match_type) && is.null(idx)) {
    stop("one of `match_type` or `idx` must be provided")
  }

  if (!is.null(match_type)) {
    lyr_types <- vapply(x$layers, function(lyr) {
      class(lyr$geom)[1L]
    }, FUN.VALUE = character(1L))
    match_bool <- match_type == lyr_types
    lyrs <- x$layers[match_bool]
  } else {
    lyrs <- x$layers
  }

  if (!is.null(idx)) {
    lyrs <- lyrs[idx]
  }

  return(lyrs)
}

.gg_n_layers <- function(x) {
  length(x$layers)
}



.add_network <- function(plot, ) {
  if(is.null(edge_alpha)) {
    edge_alpha = 0.5
    pl <- pl + ggplot2::geom_segment(data = annotated_network_DT,
                                     aes_string(x = from_dim_names[1], y = from_dim_names[2],
                                                xend = to_dim_names[1], yend = to_dim_names[2]),
                                     alpha = edge_alpha, color=network_color,size = 0.1,
                                     show.legend = F)
  } else if(is.numeric(edge_alpha)) {
    pl <- pl + ggplot2::geom_segment(data = annotated_network_DT,
                                     aes_string(x = from_dim_names[1], y = from_dim_names[2],
                                                xend = to_dim_names[1], yend = to_dim_names[2]),
                                     alpha = edge_alpha, color=network_color,size = 0.1,
                                     show.legend = F)
  } else if(is.character(edge_alpha)) {

    if(edge_alpha %in% colnames(annotated_network_DT)) {
      pl <- pl + ggplot2::geom_segment(data = annotated_network_DT,
                                       aes_string(x = from_dim_names[1], y = from_dim_names[2],
                                                  xend = to_dim_names[1],
                                                  yend = to_dim_names[2], alpha = edge_alpha),
                                       color=network_color,
                                       show.legend = F)
    }
  }
}


