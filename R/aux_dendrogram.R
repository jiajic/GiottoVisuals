#' @title showClusterDendrogram
#' @name showClusterDendrogram
#' @description Creates dendrogram for selected clusters.
#' @param gobject giotto object
#' @param spat_unit spatial unit (e.g. "cell")
#' @param feat_type feature type (e.g. "rna", "dna", "protein")
#' @param expression_values expression values to use (e.g. "normalized", "scaled", "custom")
#' @param cluster_column name of column to use for clusters (e.g. "leiden_clus")
#' @param cor correlation score to calculate distance (e.g. "pearson", "spearman")
#' @param distance distance method to use for hierarchical clustering, default to "ward.D"
#' @param h height of horizontal lines to plot
#' @param h_color color of horizontal lines
#' @param rotate rotate dendrogram 90 degrees
#' @param show_plot show plot. TRUE or FALSE
#' @param return_plot return ggplot object. TRUE or FALSE
#' @param save_plot directly save the plot. TRUE or FALSE
#' @param save_param list of saving parameters, see \code{\link{showSaveParameters}}
#' @param default_save_name default save name for saving, don't change, change save_name in save_param
#' @param ... additional parameters passed to \code{\link[ggdendro]{ggdendrogram}}
#' @return ggplot
#' @details Expression correlation dendrogram for selected clusters.
#' @export
showClusterDendrogram <- function(gobject,
                                  spat_unit = NULL,
                                  feat_type = NULL,
                                  expression_values = c('normalized', 'scaled', 'custom'),
                                  cluster_column,
                                  cor = c('pearson', 'spearman'),
                                  distance = 'ward.D',
                                  h = NULL,
                                  h_color = 'red',
                                  rotate = FALSE,
                                  show_plot = NA,
                                  return_plot = NA,
                                  save_plot = NA,
                                  save_param =  list(),
                                  default_save_name = 'showClusterDendrogram',
                                  ...) {


  # verify if optional package is installed
  package_check(pkg_name = "ggdendro", repository = "CRAN")

  cor = match.arg(cor, c('pearson', 'spearman'))
  values = match.arg(expression_values, unique(c('normalized', 'scaled', 'custom', expression_values)))

  # Set feat_type and spat_unit
  spat_unit = set_default_spat_unit(gobject = gobject,
                                    spat_unit = spat_unit)
  feat_type = set_default_feat_type(gobject = gobject,
                                    spat_unit = spat_unit,
                                    feat_type = feat_type)

  metatable = calculateMetaTable(gobject = gobject,
                                 spat_unit = spat_unit,
                                 feat_type = feat_type,
                                 expression_values = values,
                                 metadata_cols = cluster_column)
  dcast_metatable = data.table::dcast.data.table(metatable, formula = variable~uniq_ID, value.var = 'value')
  testmatrix = dt_to_matrix(x = dcast_metatable)

  # correlation
  cormatrix = cor_flex(x = testmatrix, method = cor)
  cordist = stats::as.dist(1 - cormatrix, diag = T, upper = T)
  corclus = stats::hclust(d = cordist, method = distance)

  cordend = stats::as.dendrogram(object = corclus)

  # plot dendrogram
  pl = ggdendro::ggdendrogram(cordend, rotate = rotate, ...)

  # add horizontal or vertical lines
  if(!is.null(h)) {
    pl = pl + ggplot2::geom_hline(yintercept = h, col = h_color)
  }

  # print, return and save parameters
  show_plot = ifelse(is.na(show_plot), readGiottoInstructions(gobject, param = 'show_plot'), show_plot)
  save_plot = ifelse(is.na(save_plot), readGiottoInstructions(gobject, param = 'save_plot'), save_plot)
  return_plot = ifelse(is.na(return_plot), readGiottoInstructions(gobject, param = 'return_plot'), return_plot)

  ## print plot
  if(show_plot == TRUE) {
    print(pl)
  }

  ## save plot
  if(save_plot == TRUE) {
    do.call('all_plots_save_function', c(list(gobject = gobject, plot_object = pl, default_save_name = default_save_name), save_param))
  }

  ## return plot
  if(return_plot == TRUE) {
    return(pl)
  }
}
