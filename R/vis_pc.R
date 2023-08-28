

## * PC estimates ####
# ------------------ #

#' @title create_screeplot
#' @name create_screeplot
#' @description create screeplot with ggplot
#' @param eigs numeric. Vector of pca eigenvalues
#' @param ncp numeric. max number of principal components to plot
#' @param ylim numeric. y-axis limits on scree plot
#' @return ggplot
#' @examples
#' \dontrun{
#' dr = GiottoData::loadSubObjectMini('dimObj')
#' scree = create_screeplot(methods::slot(dr, 'misc')$eigenvalues)
#' scree
#' }
#' @export
create_screeplot = function(eigs, ncp = 20, ylim = c(0, 20)) {

  checkmate::assert_numeric(eigs)
  checkmate::assert_numeric(ncp, len = 1L)
  checkmate::assert_numeric(ylim, len = 2L)

  # DT vars
  PC = NULL

  eigs = sort(eigs, decreasing = TRUE)

  # variance explained
  var_expl = eigs/sum(eigs)*100
  var_expl_cum = cumsum(eigs)/sum(eigs)*100

  # create data.table
  screeDT = data.table::data.table('PC' = paste0('PC.', 1:length(var_expl)),
                                   'var_expl' = var_expl,
                                   'var_expl_cum' = var_expl_cum)
  screeDT[, 'PC' := factor(PC, levels = PC)]

  max_ncp = length(eigs)
  ncp = ifelse(ncp > max_ncp, max_ncp, ncp)

  pl = ggplot2::ggplot()
  pl = pl + ggplot2::theme_bw()
  pl = pl + ggplot2::geom_bar(data = screeDT[1:ncp], ggplot2::aes(x = PC, y = var_expl), stat = 'identity')
  pl = pl + ggplot2::coord_cartesian(ylim = ylim)
  pl = pl + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1))
  pl = pl + ggplot2::labs(x = '', y = '% of variance explained per PC')

  cpl = ggplot2::ggplot()
  cpl = cpl + ggplot2::theme_bw()
  cpl = cpl + ggplot2::geom_bar(data = screeDT[1:ncp], ggplot2::aes(x = PC, y = var_expl_cum), stat = 'identity')
  cpl = cpl + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1))
  cpl = cpl + ggplot2::labs(x = '', y = 'cumulative % of variance explained')

  savelist = list(pl, cpl)

  ## combine plots with cowplot
  combo_plot <- cowplot::plot_grid(plotlist = savelist,
                                   ncol = 1,
                                   rel_heights = c(1),
                                   rel_widths = c(1),
                                   align = 'v')

  return(combo_plot)
}






#' @title create_jackstrawplot
#' @name create_jackstrawplot
#' @description create jackstrawplot with ggplot
#' @param jackstraw_data result from jackstraw function (`testresult$p`)
#' @param ncp number of principal components to calculate
#' @param ylim y-axis limits on jackstraw plot
#' @param threshold p.value threshold to call a PC significant
#' @keywords internal
#' @return ggplot
#' @export
create_jackstrawplot = function(jackstraw_data,
                                ncp = 20,
                                ylim = c(0, 1),
                                threshold = 0.01) {

  checkmate::assert_numeric(ncp, len = 1L)
  checkmate::assert_numeric(ylim, len = 2L)
  checkmate::assert_numeric(threshold, len = 1L)

  # data.table variables
  PC = p.val = NULL

  testDT = data.table::data.table(
    PC = paste0('PC.', 1:length(jackstraw_data)),
    p.val = jackstraw_data
  )
  testDT[, 'PC' := factor(PC, levels = PC)]
  testDT[, 'sign' := ifelse(p.val <= threshold, 'sign', 'n.s.')]

  pl = ggplot2::ggplot()
  pl = pl + ggplot2::theme_bw()
  pl = pl + ggplot2::geom_point(data = testDT[1:ncp], ggplot2::aes(x = PC, y = p.val, fill = sign), shape = 21)
  pl = pl + ggplot2::scale_fill_manual(values  = c('n.s.' = 'lightgrey', 'sign' = 'darkorange'))
  pl = pl + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1))
  pl = pl + ggplot2::coord_cartesian(ylim = ylim)
  pl = pl + ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
  pl = pl + ggplot2::labs(x = '', y = 'p-value per PC')

  return(pl)

}





