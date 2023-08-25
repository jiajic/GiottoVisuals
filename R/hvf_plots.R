
# Create plots for whether features can be considered highly variable.

#' @name hvf_visualizations
#' @title Highly Variable Feature plots
#' @description
#' Create plots that report the highly variable features detected
#' @keywords internal
NULL






#' @rdname hvf_visualizations
#' @export
create_cov_group_HVF_plot = function(feat_in_cells_detected, nr_expression_groups) {
  pl <- ggplot2::ggplot()
  pl <- pl + ggplot2::theme_classic() +
    ggplot2::theme(axis.title = ggplot2::element_text(size = 14), axis.text = ggplot2::element_text(size = 12))
  pl <- pl + ggplot2::geom_point(data = feat_in_cells_detected, ggplot2::aes(x = mean_expr, y = cov, color = selected))
  pl <- pl + ggplot2::scale_color_manual(values = c(no = 'lightgrey', yes = 'orange'), guide = ggplot2::guide_legend(title = 'HVF', override.aes = list(size=5)))
  pl <- pl + ggplot2::facet_wrap(~expr_groups, ncol = nr_expression_groups, scales = 'free_x')
  pl <- pl + ggplot2::theme(axis.text.x = ggplot2::element_blank(), strip.text = ggplot2::element_text(size = 4))
  pl <- pl + ggplot2::labs(x = 'expression groups', y = 'cov')
  pl
}

#' @rdname hvf_visualizations
#' @export
create_cov_loess_HVF_plot = function(feat_in_cells_detected, difference_in_cov, var_col) {
  pl <- ggplot2::ggplot()
  pl <- pl + ggplot2::theme_classic() + ggplot2::theme(axis.title = ggplot2::element_text(size = 14),
                                                       axis.text = ggplot2::element_text(size = 12))
  pl <- pl + ggplot2::geom_point(data = feat_in_cells_detected, ggplot2::aes_string(x = 'log(mean_expr)', y = var_col, color = 'selected'))
  pl <- pl + ggplot2::geom_line(data = feat_in_cells_detected, ggplot2::aes_string(x = 'log(mean_expr)', y = 'pred_cov_feats'), color = 'blue')
  hvg_line = paste0('pred_cov_feats+',difference_in_cov)
  pl <- pl + ggplot2::geom_line(data = feat_in_cells_detected, ggplot2::aes_string(x = 'log(mean_expr)', y = hvg_line), linetype = 2)
  pl <- pl + ggplot2::labs(x = 'log(mean expression)', y = var_col)
  pl <- pl + ggplot2::scale_color_manual(values = c(no = 'lightgrey', yes = 'orange'),
                                         guide = ggplot2::guide_legend(title = 'HVF',
                                                                       override.aes = list(size=5)))
  pl
}

#' @rdname hvf_visualizations
#' @export
create_calc_var_HVF_plot = function(dt_res) {
  pl = ggplot2::ggplot()
  pl = pl + ggplot2::geom_point(data = dt_res, aes(x = rank, y = var, color = selected))
  pl = pl + ggplot2::scale_x_reverse()
  pl = pl + ggplot2::theme_classic() + ggplot2::theme(axis.title = ggplot2::element_text(size = 14),
                                                      axis.text = ggplot2::element_text(size = 12))
  pl = pl + ggplot2::scale_color_manual(values = c(no = 'lightgrey', yes = 'orange'),
                                        guide = ggplot2::guide_legend(title = 'HVF',
                                                                      override.aes = list(size=5)))
  pl = pl + ggplot2::labs(x = 'feature rank', y = 'variance')
  pl
}
