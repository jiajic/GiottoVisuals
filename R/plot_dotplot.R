




#' @name dotPlot
#' @title Create a dotplot
#' @description Visualize feature expression statistics applied across
#'   clusters/groupings of cells. The default behavior is dot size scaled by
#'   what percentage of cells within a particular cluster express the feature,
#'   and dot color scaled by mean expression of that feature within the cluster.
#' @inheritParams data_access_params
#' @inheritParams plot_params
#' @inheritParams plot_output_params
#' @param dot_size,dot_color summary function e.g. `sum`, `mean`, `var`, or
#'   other custom function. The default for `dot_size` finds the percentage of
#'   cells of a particular cluster that do not have an expression level of 0.
#' @param dot_size_threshold numeric. The minimal value at which a dot is no
#'   longer drawn.
#' @param feats character vector. Features to use
#' @param cluster_column character. Clusterings column to use (usually in cell
#'   metadata)
#' @param cluster_custom_order character vector. Specific cluster order to use
#' @param dot_scale numeric. Controls size of dots
#' @param dot_color_gradient hex codes or palette name. Color gradient to use.
#' @param gradient_limits numeric vector of length 2. Set minmax value mappings
#'   for color gradient
#' @param expression_values character. Expression values to use.
#' @param title character. title for plot
#' @param theme_param list of additional params passed to `ggplot2::theme()`
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#' feats <- head(featIDs(g), 20)
#' dotPlot(g, cluster_column = "leiden_clus", feats = feats)
#' dotPlot(g,
#'     cluster_column = "leiden_clus",
#'     feats = feats,
#'     dot_size = mean,
#'     dot_color = var
#' )
#' @export
dotPlot <- function(
        gobject,
        spat_unit = NULL,
        feat_type = NULL,
        feats,
        cluster_column,
        cluster_custom_order = NULL,
        dot_size = function(x) mean(x != 0) * 100,
        dot_size_threshold = 0,
        dot_scale = 6,
        dot_color = mean,
        dot_color_gradient = NULL,
        gradient_midpoint = NULL,
        gradient_style = "sequential",
        gradient_limits = NULL,
        expression_values = c(
            "normalized",
            "scaled",
            "custom"
        ),
        title = NULL,
        show_legend = TRUE,
        legend_text = 10,
        legend_symbol_size = 2,
        background_color = "white",
        axis_text = 10,
        axis_title = 9,
        theme_param = list(),
        show_plot = NULL,
        return_plot = NULL,
        save_plot = NULL,
        save_param = list(),
        default_save_name = "dotPlot"
) {
    checkmate::assert_character(cluster_column, len = 1L)
    checkmate::assert_character(feats)
    checkmate::assert_class(gobject, "giotto")
    if (!is.null(gradient_limits)) {
        checkmate::assert_numeric(gradient_limits, len = 2L)
    }


    spat_unit <- set_default_spat_unit(gobject = gobject, spat_unit = spat_unit)
    feat_type <- set_default_feat_type(gobject = gobject,
                                       spat_unit = spat_unit,
                                       feat_type = feat_type)

    expression_values <- match.arg(
        expression_values,
        unique(c("normalized", "scaled", "custom", expression_values))
    )

    clus <- spatValues(gobject, spat_unit = spat_unit, feat_type = feat_type,
                       feats = cluster_column, verbose = FALSE)

    expr <- spatValues(gobject, spat_unit = spat_unit, feat_type = feat_type,
                       feats = feats, expression_values = expression_values,
                       verbose = FALSE)
    ann_dt <- clus[expr, on = "cell_ID"]

    dsize <- ann_dt[, lapply(.SD, dot_size), .SDcols = feats, by = cluster_column]
    dcol <- ann_dt[, lapply(.SD, dot_color), .SDcols = feats, by = cluster_column]

    dsize <- data.table::melt(dsize,
        id.vars = cluster_column,
        measure.vars = feats,
        value.name = "size",
        variable.name = "feat"
    )
    dcol <- data.table::melt(dcol,
        id.vars = cluster_column,
        measure.vars = feats,
        value.name = "color",
        variable.name = "feat"
    )
    plot_dt <- dsize[dcol, on = c(cluster_column, "feat")]
    data.table::setnames(plot_dt, old = cluster_column, new = "cluster")

    ## dot size cutoff ##
    plot_dt <- plot_dt[size > dot_size_threshold,]

    ## set cluster order ##
    if (is.null(cluster_custom_order)) {
        plot_dt[, cluster := factor(cluster, levels = mixedsort(unique(cluster)))]
    } else {
        plot_dt[, cluster := factor(cluster, levels = cluster_custom_order)]
    }

    # apply limits
    if (!is.null(gradient_limits)) {
        plot_dt[, color := scales::oob_squish(color, gradient_limits)]
    }

    pl <- ggplot2::ggplot() +
        ggplot2::geom_point(
            data = plot_dt,
            ggplot2::aes(x = cluster, y = feat, color = color, size = size)
        )

    # apply color gradient
    if (is.null(gradient_midpoint)) {
        gradient_midpoint <-
            stats::median(plot_dt$color)
    }
    pl <- pl + set_default_color_continuous_cell(
        colors = dot_color_gradient,
        instrs = instructions(gobject),
        midpoint = gradient_midpoint,
        style = gradient_style,
        type = "color"
    )

    # size scaling
    pl <- pl + scale_size_continuous(range = c(1, dot_scale))

    ## theme ##
    gg_theme_args <- c(
        theme_param,
        legend_text = legend_text,
        axis_title = axis_title,
        axis_text = axis_text,
        background_color = background_color,
        axis.ticks = element_blank(),
        axis_text_y_angle = 0
    )
    pl <- pl + do.call(.gg_theme, args = gg_theme_args)


    plot_output_handler(
        gobject = gobject,
        plot_object = pl,
        save_plot = save_plot,
        return_plot = return_plot,
        show_plot = show_plot,
        default_save_name = default_save_name,
        save_param = save_param,
        else_return = NULL
    )
}




