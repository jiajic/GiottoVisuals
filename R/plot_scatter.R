#' @name gg_simple_scatter
#' @title gg_simple_scatter
#' @param ggobject ggplot2 object
#' @param data data.frame-like object of information to plot
#' @param x col to plot as x in 'data'
#' @param y col to plot as y in 'data'
#' @param xlab a title for the x axis
#' @param ylab a title for the y axis
#' @param main an overall title for the plot
#' @inheritDotParams ggplot2::aes
#' @keywords internal
#' @returns a ggplot object
#' @examples
#' x <- data.frame(x = rnorm(10), y = rnorm(10))
#' gg_simple_scatter(data = x, x = "x", y = "y")
#'
#' @export
gg_simple_scatter <- function(
        ggobject = NULL, data, x, y,
        xlab = "x", ylab = "y", main = NULL, ...) {
    pl <- .gg_input(ggobject)

    pl <- pl +
        ggplot2::geom_point(data = data, aes_string2(x, y, ...)) +
        ggplot2::theme_classic() +
        ggplot2::labs(x = xlab, y = ylab)

    if (!is.null(main)) {
        pl <- pl + ggplot2::ggtitle(label = main)
    }

    pl
}

#' @name scatterPlot
#' @title Create a Scatter Plot
#' @description
#' Create a scatter plot from `Giotto` object values. Aesthetic parameters
#' allow usage of `svkey()` to specify any values accessible with `spatValues`.
#' The main restriction is that retrieved values must map 1-to-1 with individual
#' cell_IDs. The points plotted will be based on the intersect of available
#' cell_IDs.
#' @inheritParams data_access_params
#' @inheritParams data_output_params
#' @inheritParams plot_params
#' @inheritParams plot_cow_params
#' @param feat1 `character` or `svkey`. First feature to plot.
#' Continuous values only.
#' @param feat2 `character` or `svkey`. Second feature to plot.
#' Continuous values only.
#' @param color *aes* `character` or `svkey`. Categorical or continuous values.
#' @param size *aes* `numeric` or `svkey`. Continuous values only.
#' @param stroke *aes* `numeric` or `svkey`. Continuous values only. Only used if
#' `shape` is within 21-25
#' @param alpha *aes* `numeric` or `svkey`. Continuous values only.
#' @param shape *aes* `character`, `numeric` or `svkey`. Accepts any of the
#' `numeric` values that designate R point shapes. Passing a `character` that
#' is either `"border"`, `"no_border"`, or `"voronoi"` will use point shapes
#' 21, 19, or vornoi plotting respectively. `character` or `svkey` inputs
#' designating categorical values in the `giotto` object may be provided, but
#' there may only be up to 6 different categories within the data.
#' @param jitter *aes* `numeric` or `FALSE` (default). Add random noise to positioning
#' of points. Useful in datasets with discrete positions. Applies first value
#' as width jitter, second as height jitter. If only one value provided, the
#' same value will be used for width and height.
#' @param as_factor *aes* `character`. One or more of `"color"`, `"fill"`, `shape`.
#' Values supplied to the selected aesthetics will be made categorical via
#' `as.factor()` as the last step before plotting.
#' @param palette *aes* `character vector`. Specific colors to use when plotting
#' the data specified by `color` param (unless `color` is just a single color
#' to use already). If provided:
#'
#' * single character: must be the name of a palette understood by [getColors()]
#' * character, length > 1:
#' @param gradient_style *aes* `character`. Either `"divergent"` (1st value
#' supplied to `gradient_breaks` is used as midpoint. If not provided, midpoint
#' is calculated based on the data) or `"sequential"` (colors are scaled based
#' on data range)
#' @param gradient_breaks *aes* `numeric` vector. When
#' `gradient_style = "divergent"`, If provided, 1st value is selected as
#' gradient midpoint. When `gradient_style = "sequential"`, breaks are used
#' as color breaks for color vectors supplied to `palette`. Note that
#' `gradient_breaks` must be the same length as `palette` colors for this.
#' @param gradient_limits *aes* `numeric` vector of length 2. Upper and lower
#' limits for the color gradient.
#' @param stroke_gradient_style *aes* Same as `gradient_style`, but for the
#' stroke.
#' @param stroke_gradient_limits *aes* Same as `gradient_limits`, but for the
#' stroke.
#' @param stroke_gradient_breaks *aes* Same as `gradient_breaks`, but for the
#' stroke.
#' @param cell_ids *subset* `character` vector. If provided, only the cell_IDs
#' provided will be plotted.
#' @param group_by *subset* `character` or `svkey`. Categorical values only.
#' Create multiple plots based on a cell metadata column.
#' @param group_by_subset *subset* Only make plots for selected groups
#' designated by the data selected by `group_by`.
#' @param select *subset* `character` vector. If provided, the cell_IDs
#' provided will be selected as foreground cells.
#' @param select_groups *subset* `"character"`. If provided, designate
#' categories of cells as foreground cells when `color` is a categorical.
#' @param show_other *subset* `logical`. Whether to plot background cells. If
#' `TRUE`, recursively calls to plot the background cells with the aesthetics
#' defined in `other_aes`.
#' @param other_aes *subset* named `list` of params. Accepts the same set of
#' aesthetic parameters as those used in this function.
#' @param ext *spatial subset* `numeric` of length 4 (xmin, xmax, ymin, ymax)
#' or `SpatExtent` that can be used to define `feat1_lim` and `feat2_lim` at
#' once. Conceptually, this makes most sense when `feat1` is `"sdimx"` and
#' `feat2` is `sdimy`.
#' @param feat1_lim,feat2_lim *spatial subset* `numeric` vector of length 2.
#' Restrict plotting to be within lim defined.
#' @param stat *modifier* `character`. Statistic used when plotting. One of
#' `"point"`, `"hexbin"`, `"density"`, or `"contour"`. When not using
#' `"point"`, `shape`,
#' @param transforms *modifier* (optional) named `list` of `characters` defining the transforms
#' to perform on specific aesthetics (e.g. x = "pseudo_log"). Transforms must
#' be understandable by [ggplot2::continuous_scale(transform)]
#' @param trend *modifier* (optional). Apply a trendline if not `NULL`. This param accepts
#' any smoothing method accepted by [ggplot2::geom_smooth(method)].
#' @param rasterize *modifier* `logical`. Whether to rasterize with `scattermore`. If not
#' `TRUE`, more than 50,000 points being plotted will default to rasterization.
#' @param set_seed `logical`. whether to use a seed with random plotting
#' behaviors.
#' @param seed seed to use with random plotting behaviors e.g. `jitter`
#' @returns a {ggplot} object
#' @export
scatterPlot <- function(gobject, feat1, feat2,
    # aes params
    color = NULL,
    size = 1,
    alpha = 1,
    stroke_width = 0.1,
    stroke_color = "black",
    shape = "border",
    jitter = FALSE,
    as_factor = NULL,
    palette = NULL,
    gradient_style = c("sequential", "divergent"),
    gradient_limits = NULL,
    gradient_breaks = NULL,
    stroke_gradient_style = c("sequential", "divergent"),
    stroke_gradient_limits = NULL,
    stroke_gradient_breaks = NULL,
    # observation grouping and foreground selection
    cell_ids = NULL,
    group_by = NULL,
    group_by_subset = NULL,
    select = NULL,
    select_groups = NULL,
    show_other = TRUE,
    other_aes = list(
        color = "lightgrey",
        size = 1,
        alpha = 0.2,
        shape = "no_border"
    ),
    # spatial subsets
    ext = NULL,
    feat1_lim = NULL,
    feat2_lim = NULL,
    # plot modifiers
    stat = c("point", "hexbin", "density", "contour"),
    transforms = NULL,
    trend = NULL,
    rasterize = NULL,
    # formatting and plotting meta params
    axis_text = 8,
    axis_title = 8,
    legend_text = 6,
    coord_fix_ratio = NULL,
    background_color = "black",
    show_legend = NULL,
    theme_param = list(),
    set_seed = TRUE,
    seed = 1234,
    # plot combining
    cow_n_col = NULL,
    cow_rel_h = 1,
    cow_rel_w = 1,
    cow_align = "h",
    # plot output
    show_plot = NULL,
    return_plot = NULL,
    save_plot = NULL,
    save_param = list(),
    default_save_name = "scatterPlot",
    verbose = NULL
) {

    if (!isFALSE(jitter)) checkmate::assert_numeric(jitter)
    if (!.is_svkey(feat1)) checkmate::assert_character(feat1, len = 1L)
    if (!.is_svkey(feat2)) checkmate::assert_character(feat2, len = 1L)
    # if (!.is_svkey(color)) checkmate::assert_character(color, null.ok = TRUE)



    # initialize shared plot args
    shared_args <- list()

    # sort between static (toplevel) and dynamic (aes) params

    # update transforms for colors based on gradient type

    # handle jitter
    shared_args <- .gg_scatter_jitter(shared_args, jitter, set_seed, seed)


}





# TODO binned transforms are also possible

# internals ####

# 0. basic type checking
# 1.1. accumulate necessary table information for plotting
# 1.2. type check values for aes requirements
# 2. distribute plotting jobs to `.scatter_plot_single()`
.scatter_plot <- function(gobject, feat1, feat2,
    # aes params
    color = NULL,
    size = 1,
    alpha = 1,
    stroke_width = 0.4,
    stroke_color = "black",
    shape = "border",
    jitter = FALSE,
    as_factor = NULL,
    palette = NULL,
    gradient_style = c("sequential", "divergent"),
    gradient_limits = NULL,
    gradient_breaks = NULL,
    stroke_gradient_style = c("sequential", "divergent"),
    stroke_gradient_limits = NULL,
    stroke_gradient_breaks = NULL,
    # observation grouping and foreground selection
    cell_ids = NULL,
    group_by = NULL,
    group_by_subset = NULL,
    select = NULL,
    select_groups = NULL,
    show_other = TRUE,
    other_aes = list(
        color = "lightgrey",
        size = 1,
        alpha = 0.2,
        shape = "no_border"
    ),
    # spatial subsets
    ext = NULL,
    feat1_lim = NULL,
    feat2_lim = NULL,
    # plot modifiers
    stat = c("point", "hexbin", "density", "contour"),
    transforms = NULL,
    trend = NULL,
    rasterize = NULL,
    # formatting and plotting meta params
    axis_text = 8,
    axis_title = 8,
    legend_text = 6,
    coord_fix_ratio = NULL,
    background_color = "black",
    show_legend = NULL,
    theme_param = list(),
    set_seed = TRUE,
    seed = 1234,
    # plot combining
    cow_n_col = NULL,
    cow_rel_h = 1,
    cow_rel_w = 1,
    cow_align = "h",
    # plot output
    show_plot = NULL,
    return_plot = NULL,
    save_plot = NULL,
    save_param = list(),
    default_save_name = "scatterPlot",
    verbose = NULL
) {
    # initialize param lists
    dynamic_vals <- list() # designate aes params
    static_vals <- list() # designate toplevel `geom_point()` params
    sv_list <- list() # list of data to combine into final info table

    # get info to extract
    # xy
    xy <- .svget_aes_values(gobject, list(x = feat1, y = feat2))
    if (!.sv_inherits(xy, "x", "numeric")) {
        stop("feat1 must be numeric\n", call. = FALSE)
    }
    if (!.sv_inherits(xy, "y", "numeric")) {
        stop("feat2 must be numeric\n", call. = FALSE)
    }
    xy <- merge(xy[[1]], xy[[2]], by = "cell_ID", all.x = TRUE)

    # set of representative cell_IDs that can be used
    # (especially when an external vector is provided with no paired IDs)
    x_ids <- xy[["x"]][, "cell_ID"]

    # recurse for "other" aes param data collection

    # color
    color_res_list <- .handle_color_input(
        gobject = gobject,
        color = color,
        name = "color", # temp name to track this state as
        x_cell_ids = x_ids,
        static_vals = static_vals,
        dynamic_vals = dynamic_vals,
        sv_list = sv_list,
        verbose = verbose
    )
    color <- color_res_list$color
    static_vals <- color_res_list$static_vals
    dynamic_vals <- color_res_list$dynamic_vals
    sv_list <- color_res_list$sv_list

    # stroke (border) colors
    if (.is_strokable_shape(shape, gobject)) {
        stroke_res_list <- .handle_color_input(
            gobject = gobject,
            color = stroke_color,
            name = "stroke_color",
            x_cell_ids = x_ids,
            static_vals = static_vals,
            dynamic_vals = dynamic_vals,
            sv_list = sv_list,
            verbose = verbose
        )
    }

    # size

    # alpha

    # stroke (border) width

    # shape

    # factor coercion

    # group_by and select

    # send to .scatter_plot_single() for final processing and plot assembly


}



# intended for aes params that work with color (color and stroke_color)
# sort color input into static (toplevel) or dynamic (mapped aes)
# pull data if needed into sv_list
# * determination of continuous vs categorical happens downstream based on
# `sv_list`.
# * fill vs color aes usage depends on downstream `shape` processing
# * after cont./cat.  and fill/color decisions, gradients and color settings
#   can be finalized.
.handle_color_input <- function(
        gobject, color, name = "color", x_cell_ids, static_vals, dynamic_vals,
        sv_list, verbose = NULL
) {
    # [null]
    if (is.null(color)) color <- "#AFAFCF"
    # [external vector] (same ordering as metadata)
    if (length(color) > 1L) {
        if (is.logical(color)) color <- as.factor(color) # logical -> factor
        if (is.numeric(color) || is.factor(color)) {
            # numeric or factor (categorical)
            # setup a new table of data with cols "cell_ID" and "temp_color"
            temp_color <- data.table::data.table(
                cell_ID = x_cell_ids
            )
            if (length(color) != nrow(temp_color)) {
                stop(sprintf("`%s` external vector inputs must be the same length as number of data to plot.\n", name), call. = FALSE)
            }
            temp_color$temp_color <- color
            append_color <- list()
            append_color[[name]] <- temp_color
            sv_list <- c(sv_list, list(temp_color)) # add to data list
            dynamic_vals[[name]] <- "temp_color" # add to aes mapping
        } else if (is.character(color)) {
            # character (color_codes)
            if (!all(.is_color_code(color))) {
                stop(sprintf("`%s` is not a numeric, a factor, or vector of colors\n", name), call. = FALSE)
            }
            static_vals[[name]] <- color # add to toplevel params
        }
    } else if (is.character(color)) { # length 1 character inputs
        # these can be either a request for data from the gobject
        # or a color code to apply to all
        if (.is_color_code(color)) {
            static_vals[[name]] <- color # add to toplevel params
        } else {
            color <- svkey(color) # pass to svkey steps
        }
    }
    # process svkey requests
    if (inherits(color, "svkey")) {
        dynamic_vals[[name]] <- color@feats
        klist <- list()
        klist[[name]] <- color
        sv_list <- .svget_aes_values(gobject, keys = klist,
            verbose = verbose, add_to = sv_list
        )
        .sv_assert(sv_list, name = "color",
            what = c("logical", "numeric", "character")
        )
    }
    # return
    list(
        color = color,
        static_vals = static_vals,
        dynamic_vals = dynamic_vals,
        sv_list = sv_list
    )
}



# returns list of ggplot objects
.scatter_plot_single <- function(gobject, feat1, feat2,
        # aes params
        color = NULL,
        size = 1,
        alpha = 1,
        stroke_width = 0.1,
        stroke_color = "black",
        shape = "border",
        jitter = FALSE,
        as_factor = NULL,
        palette = NULL,
        gradient_style = c("sequential", "divergent"),
        gradient_limits = NULL,
        gradient_breaks = NULL,
        # observation grouping and foreground selection
        cell_ids = NULL,
        group_by = NULL,
        group_by_subset = NULL,
        select = NULL,
        select_groups = NULL,
        show_other = TRUE,
        other_aes = list(
            color = "lightgrey",
            size = 1,
            alpha = 0.2,
            shape = "no_border"
        ),
        # plot modifiers
        stat = c("point", "hexbin", "density", "contour"),
        transforms = NULL,
        trend = NULL,
        rasterize = NULL,
        # formatting and plotting meta params
        axis_text = 8,
        axis_title = 8,
        legend_text = 6,
        coord_fix_ratio = NULL,
        background_color = "black",
        show_legend = NULL,
        theme_param = list(),
        set_seed = TRUE,
        seed = 1234,
        # plot output
        verbose = NULL
) {
    # plotting engine (geom_point, geom_scattermore, or specific stat function)

    # gradient finalization

    # scale and transforms setup

    # apply trendline
}



# p is a param list for `geom_points()`
.gg_scatter_jitter <- function(p,
    jitter = FALSE, set_seed = TRUE, seed = 1234) {
    if (isFALSE(jitter)) return(p)
    # recycle to length 2 if needed
    if (length(jitter) == 1L) jitter <- rep.int(jitter, 2)
    j <- ggplot2::position_jitter(width = jitter[1], height = jitter[2])
    if (isTRUE(set_seed)) j$seed <- seed
    p$position <- j
    p
}


# transforms should be a named list of either the names of ggplot2 built-in
# transforms or a {scales} transform
# returns list of gg scales
.gg_scale_continuous_factory <- function(
        param_list = NULL,
        color_type = c("m", "g", "g2", "gn"),
        fill_type = c("m", "g", "g2", "gn")
) {
    checkmate::assert_list(param_list, null.ok = TRUE)
    color_type <- match.arg(color_type, c("m", "g", "g2", "gn"))
    fill_type <- match.arg(fill_type, c("m", "g", "g2", "gn"))
    scale_names <- names(param_list)
    # accept params directed to feat1 and feat2 as x and y respectively
    has_feat1 <- scale_names == "feat1"
    has_feat2 <- scale_names == "feat2"
    if (any(has_feat1)) {
        names(param_list)[has_feat1] <- "x"
        scale_names[has_feat1] <- "x"
    }
    if (any(has_feat2)) {
        names(param_list)[has_feat2] <- "y"
        scale_names[has_feat2] <- "y"
    }

    gg_tfs <- lapply(scale_names, function(sc) {
        fun_id <- sc
        if (sc == "color") fun_id <- paste(sc, color_type, sep = "_")
        if (sc == "fill") fun_id <- paste(sc, fill_type, sep = "_")
        scale_fun <- switch(fun_id,
            "x" = ggplot2::scale_x_continuous,
            "y" = ggplot2::scale_y_continuous,
            "color_m" = ggplot2::scale_color_manual,
            "color_g" = ggplot2::scale_color_gradient,
            "color_g2" = ggplot2::scale_color_gradient2,
            "color_gn" = ggplot2::scale_color_gradientn,
            "fill_m" = ggplot2::scale_fill_manual,
            "fill_g" = ggplot2::scale_fill_gradient,
            "fill_g2" = ggplot2::scale_fill_gradient2,
            "fill_gn" = ggplot2::scale_fill_gradientn,
            "size" = ggplot2::scale_size_continuous,
            "alpha" = ggplot2::scale_alpha_continuous
        )
        do.call(scale_fun, param_list[[sc]])
    })
}

# disambiguate the naming of values selected
# `keys` param accepts a list of character and `svkey.` list names are ignored.
# if identical tables found, 2nd one will be removed.
# returns a list
.disamb_value_keys <- function(keys) {

}

# use spatValues() to retrieve data based on provided list of keys
# list names should likely reflect the aes params that they are for
# keys may be character or svkey
# add_to is an optional running list of named spatValues outputs to append to
# returns list of values with list names unchanged
.svget_aes_values <- function(gobject, keys, verbose = NULL, add_to = NULL) {
    checkmate::assert_list(keys)
    checkmate::assert_list(add_to, null.ok = TRUE)
    res_list <- lapply(keys, function(key) {
        # make svkey if not
        if (!.is_svkey(key)) key <- svkey(key)
        # apply verbose param if not specified in svkey
        key@verbose <- key@verbose %null% verbose
        # get data
        key@get(gobject)
    })
    if (!is.null(add_to)) {
        res_list <- c(add_to, res_list)
    }
    res_list
}

.keylist_svkey_to_character <- function(keys) {
    # make svkey if not
    keys <- lapply(keys, function(key) {
        if (!.is_svkey(key)) key <- svkey(key)
    })
}

.combine_aes_value_tables <- function(table_list) {

}

.sv_inherits <- function(x, name, what) {
    if ("numeric" %in% what) what <- unique(c(what, "integer"))
    inherits(x[[name]][[2]], what) # check second column
}

.sv_assert <- function(x, name, what) {
    if (!.sv_inherits(x, name, what)) {
        stop(sprintf("'%s' must be one of: `%s`\nbut is actually `%s`",
                     name,
                     paste0(what, collapse = "`, `"),
                     class(x[[name]][[2]])),
             call. = FALSE)
    }
}

.is_svkey <- function(x) {
    inherits(x, "svkey")
}

# detect if shape is strokable (21:25)
# or if it is a giotto setting ("border" or "voronoi") which accepts stroking
# svkey inputs will be recursively tested
# returns a `logical`
.is_strokable_shape <- function(x, gobject) {
    if (!x[[1L]] %in% c("border", "voronoi", "no_border")) x <- svkey(x)
    if (.is_svkey(x)) return(.is_strokable_shape(x@get(gobject), NULL))
    all(x %in% seq(from = 21, to = 25)) ||
        x[[1]] %in% c("border", "voronoi")
}

