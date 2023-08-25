


# default plot n cols ####
#' @title Set default ncols in plotting grid
#' @name set_default_cow_n_col
#' @keywords internal
#' @noRd
set_default_cow_n_col = function(cow_n_col = NULL,
                                 nr_plots) {

  if(is.null(cow_n_col)) {
    cow_n_col = ceiling(sqrt(nr_plots))
  } else {
    cow_n_col
  }
  return(cow_n_col)
}


# default plotting fill color ####

# -------------------------------------------------------------------------- #
# internal function to provide a function that will generate 'n' hexadecimal
# color codes for discrete colors
# inputs are INTENDED to be provided via instructions and options
#
# FUNCTION input to 'color' are also permitted, but REQUIRE an 'n' param
# for downstream functionality. 'giotto.color_discrete_pal' and
# 'giotto.color_discrete_strategy' are additionally ignored
# -------------------------------------------------------------------------- #

# framework function. Wrap this for specific things that need defaults setting
set_default_color_discrete = function(
    colors = NULL,
    instr_pal,
    instr_rev,
    instr_strategy,
    ...
) {

  # global giotto options
  opt_pal <- getOption('giotto.color_d_pal', 'distinct')
  opt_rev <- getOption('giotto.color_d_rev', FALSE)
  opt_strategy <- getOption('giotto.color_d_strategy', 'interpolate')

  # get 'colors' either from param input or options
  if (is.null(colors)) {
    if (is.null(instr_pal)) {
      colors <- opt_pal
    } else colors <- instr_pal
  }

  # get 'reverse' and 'strategy'
  reverse <- ifelse(is.null(instr_rev), opt_rev, instr_rev)
  strategy <- ifelse(is.null(instr_strategy), opt_strategy, instr_strategy)

  # evaluate 'colors'
  if (inherits(colors, 'function')) return(colors)
  else if (inherits(colors, 'character')) {
    if(length(colors) > 1) { # assume simple palette if multiple entries in vector
      return(simple_palette_factory(col = colors, rev = reverse, strategy = strategy))
    } else { # assume call to getColors() otherwise
      # return wrapped
      return(get_palette_factory(pal = colors, rev = reverse, strategy = strategy))
    }
  }
}

set_default_color_discrete_cell = function(
    colors = NULL,
    instrs,
    ...
) {

  # read instructions
  instr_pal = readGiottoInstructions(instrs, 'cell_color_d_pal', NULL)
  instr_rev = readGiottoInstructions(instrs, 'cell_color_d_rev', NULL)
  instr_strategy = readGiottoInstructions(instrs, 'cell_color_d_strategy', NULL)

  set_default_color_discrete(
    colors = colors,
    instr_pal = instr_pal,
    instr_rev = instr_rev,
    instr_strategy = instr_strategy,
    ...
  )
}

set_default_color_discrete_poly = function(
    colors = NULL,
    instrs,
    ...
) {

  # read instructions
  instr_pal = readGiottoInstructions(instrs, 'poly_color_d_pal', NULL)
  instr_rev = readGiottoInstructions(instrs, 'poly_color_d_rev', NULL)
  instr_strategy = readGiottoInstructions(instrs, 'poly_color_d_strategy', NULL)

  set_default_color_discrete(
    colors = colors,
    instr_pal = instr_pal,
    instr_rev = instr_rev,
    instr_strategy = instr_strategy,
    ...
  )
}

set_default_color_discrete_feat = function(
    colors = NULL,
    instrs,
    ...
) {

  # read instructions
  instr_pal = readGiottoInstructions(instrs, 'feat_color_pal', NULL)
  instr_rev = readGiottoInstructions(instrs, 'feat_color_rev', NULL)
  instr_strategy = readGiottoInstructions(instrs, 'feat_color_strategy', NULL)

  set_default_color_discrete(
    colors = colors,
    instr_pal = instr_pal,
    instr_rev = instr_rev,
    instr_strategy = instr_strategy,
    ...
  )
}

set_default_color_discrete_heatmap_clus = function(
    colors = NULL,
    instrs,
    ...
) {

  # read instructions
  instr_pal = readGiottoInstructions(instrs, 'heatmap_clus_color_pal', NULL)
  instr_rev = readGiottoInstructions(instrs, 'heatmap_clus_color_rev', NULL)
  instr_strategy = readGiottoInstructions(instrs, 'heatmap_clus_color_strategy', NULL)

  set_default_color_discrete(
    colors = colors,
    instr_pal = instr_pal,
    instr_rev = instr_rev,
    instr_strategy = instr_strategy,
    ...
  )
}









#' @name set_default_color_continuous
#' @title Set a default color for continuous values
#' @description
#' Framework function. Should not be used directly. Specific wrapper functions
#' should be used for each item that needs defaults setting.
#' midpoint param only supplied when using color scale with 3 colors.\cr
#' This function sets a default color continuous color gradient to use. It takes
#'
#' @param colors character or NULL. 2 to n number of hex color codes or 1 single
#' name of a palette to use can be passed
#' @param instr_pal instructions default: palette
#' @param instr_rev instructions default: reverse palette
#' @param midpoint midpoint value of color gradient
#' @param style scale color scale around midpoint (divergent) or starting from
#' minimum value (sequential)
#' @param type_default data type specific defaults
#' @param type whether setting is for ggplot2 'fill' or 'color' type function
#' @param \dots additional params to pass to respective ggplot fill_gradient functions
#' @keywords internal
set_default_color_continuous <- function(
    colors = NULL, # used for function inputs
    instr_pal,
    instr_rev,
    midpoint = NULL,
    style = c('divergent', 'sequential'),
    type_default = NULL,
    type = c('fill', 'color'),
    ...
) {

  if(!is.null(midpoint)) checkmate::assert_numeric(midpoint)
  if(!is.null(instr_pal)) checkmate::assert_character(instr_pal)
  if(!is.null(instr_rev)) checkmate::assert_logical(instr_rev)
  if(!is.null(type_default)) checkmate::assert_list(type_default)

  style <- g_match_arg(style[1], choices = c('divergent', 'sequential'))
  type <- g_match_arg(type[1], choices = c('fill', 'color'))

  # select gradient functions to use
  grad <- switch(
    type,
    'fill' = ggplot2::scale_fill_gradient(),
    'color' = ggplot2::scale_color_gradient()
  )
  gradn <- switch(
    type,
    'fill' = ggplot2::scale_fill_gradientn(),
    'color' = ggplot2::scale_color_gradientn()
  )
  grad2 <- switch(
    type,
    'fill' = ggplot2::scale_fill_gradient2(),
    'color' = ggplot2::scale_color_gradient2()
  )

  # global giotto options
  opt_pal <- switch(
    style,
    'divergent' = getOption('giotto.color_c_pal', c('blue', 'white', 'red')), # default diverging scale,
    'sequential' = getOption('giotto.color_c_pal', 'viridis')
  )

  opt_rev <- getOption('giotto.color_c_rev', FALSE)

  # get 'colors' either from param input or options
  if (is.null(colors)) { # function-level input
    if (is.null(instr_pal)) { # instructions-level input (data type specific)
      if (!is.null(type_default)) { # global-level input (gradient style specific)
        colors <- type_default$pal
      } else colors <- opt_pal
    } else colors <- instr_pal
  }

  # get 'reverse'
  reverse <- ifelse(is.null(instr_rev), opt_rev, instr_rev)

  # evaluate 'colors'
  switch(
    style,
    'divergent' = evaluate_color_gradient_divergent(colors = colors,
                                                    reverse = reverse,
                                                    midpoint = midpoint,
                                                    grad2 = grad2,
                                                    grad = grad,
                                                    gradn = gradn,
                                                    ...),
    'sequential' = evaluate_color_gradient_sequential(colors = colors,
                                                      reverse = reverse,
                                                      ...)
  )
}


evaluate_color_gradient_divergent = function(colors,
                                             reverse,
                                             midpoint,
                                             grad2,
                                             grad,
                                             gradn,
                                             ...) {
  if(is.null(midpoint)) midpoint = 0

  if (inherits(colors, 'character')) {
    if(length(colors) == 3L) { # assume simple palette if 3 entries in vector
      if(reverse) colors <- rev(colors)
      gradient <- grad2(low = colors[[1]],
                        mid = colors[[2]],
                        high = colors[[3]],
                        midpoint = midpoint,
                        ...)
    } else if(length(colors) == 2L) {
      if (reverse) colors <- rev(colors)
      gradient <- grad(low = colors[[1]],
                       high = colors[[2]],
                       ...)
    } else if (length(colors) == 1L) { # assume call to getColors() otherwise
      # return wrapped
      colors <- get_palette_factory(pal = colors, rev = reverse, strategy = 'cutoff')(256)
      gradient <- gradn(colors = colors, rescaler = mid_rescaler(mid = midpoint), ...)
    } else { # assume custom palette
      gradient <- gradn(colors = colors, rescaler = mid_rescaler(mid = midpoint), ...)
    }
  } else if (inherits(colors, 'ScaleContinuous')) {
    gradient <- colors
  } else {
    stop('set_default_color_continuous: unsupported \'color\' input')
  }
  gradient
}

evaluate_color_gradient_sequential = function(colors,
                                              reverse,
                                              gradn,
                                              grad,
                                              ...) {
  if (inherits(colors, 'character')) {
    if(length(colors) == 3L) { # assume simple palette if 3 entries in vector
      if(reverse) colors <- rev(colors)
      gradient <- gradn(colors = colors, ...)
    } else if(length(colors) == 2L) {
      if (reverse) colors <- rev(colors)
      gradient <- grad(low = colors[[1]],
                       high = colors[[2]],
                       ...)
    } else if (length(colors) == 1L) { # assume call to getColors() otherwise
      # return wrapped
      colors <- get_palette_factory(pal = colors, rev = reverse, strategy = 'cutoff')(256)
      gradient <- gradn(colors = colors, ...)
    } else { # assume custom palette
      gradient <- gradn(colors = colors, ...)
    }
  } else if (inherits(colors, 'ScaleContinuous')) {
    gradient <- colors
  } else {
    stop('set_default_color_continuous: unsupported \'color\' input')
  }
  gradient
}



set_default_color_continuous_cell <- function(
    colors = NULL,
    instrs,
    midpoint = NULL,
    style = 'divergent',
    type_default = NULL,
    ...
) {

  # read instructions
  instr_pal <- readGiottoInstructions(instrs, 'cell_color_c_pal', NULL)
  instr_rev <- readGiottoInstructions(instrs, 'cell_color_c_rev', NULL)

  set_default_color_continuous(
    colors = colors,
    instr_pal = instr_pal,
    instr_rev = instr_rev,
    midpoint = midpoint,
    style = style,
    type_default = type_default,
    ...
  )
}

set_default_color_continuous_poly <- function(
    colors = NULL,
    instrs,
    midpoint = NULL,
    style = 'divergent',
    ...
) {

  # read instructions
  instr_pal <- readGiottoInstructions(instrs, 'poly_color_c_pal', NULL)
  instr_rev <- readGiottoInstructions(instrs, 'poly_color_c_rev', NULL)

  set_default_color_continuous(
    colors = colors,
    instr_pal = instr_pal,
    instr_rev = instr_rev,
    midpoint = midpoint,
    style = style,
    ...
  )
}



set_default_color_continuous_heatmap = function(
    colors = NULL,
    instrs,
    midpoint = NULL,
    style = 'divergent',
    ...
) {

  # read instructions
  instr_pal = readGiottoInstructions(instrs, 'heatmap_color_pal', NULL)
  instr_rev = readGiottoInstructions(instrs, 'heatmap_color_rev', NULL)

  set_default_color_continuous(
    colors = colors,
    instr_pal = instr_pal,
    instr_rev = instr_rev,
    midpoint = midpoint,
    style = style,
    ...
  )
}

set_default_color_continuous_CCcom_heatmap = function(
    colors = NULL,
    instrs,
    midpoint = NULL,
    style = 'divergent',
    ...
) {

  # read instructions
  instr_pal = readGiottoInstructions(instrs, 'CCcom_heatmap_color_pal', NULL)
  instr_rev = readGiottoInstructions(instrs, 'CCcom_heatmap_color_rev', NULL)

  set_default_color_continuous(
    colors = colors,
    instr_pal = instr_pal,
    instr_rev = instr_rev,
    midpoint = midpoint,
    style = style,
    type_default = list(
      pal = c('darkblue', 'blue', 'white', 'red', 'darkred')
    ),
    ...
  )
}

set_default_color_continuous_CCcom_dotplot = function(
    colors = NULL,
    instrs,
    midpoint = NULL,
    style = 'divergent',
    type = c('fill', 'color'),
    type_default = list(
      pal = c('darkblue', 'blue', 'white', 'red', 'darkred')
    ),
    ...
) {

  # read instructions
  instr_pal = readGiottoInstructions(instrs, 'CCcom_dotplot_color_pal', NULL)
  instr_rev = readGiottoInstructions(instrs, 'CCcom_dotplot_color_rev', NULL)

  set_default_color_continuous(
    colors = colors,
    instr_pal = instr_pal,
    instr_rev = instr_rev,
    midpoint = midpoint,
    style = style,
    type_default = type_default,
    type = type,
    ...
  )
}





# TODO ####

# default background fill
# default point size




# info function ####

#' @name showColorInstructions
#' @title Show Giotto color instructions
#' @description Show the color instructions that are settable in Giotto. A data.table
#' of the information is invisibly returned
#' @return data.table of Giotto color option information
#' @export
showColorInstructions <- function() {

  out = giotto_color_instructions[, paste(
    '\noption      : ', color_blue(option),
    '\ndefault     : ', default,
    '\ndescription : ', description,
    '\n'
  )]

  cat(out)
  return(invisible(giotto_color_instructions))
}


# TODO update for additional CCcom and dotplots
giotto_color_instructions = data.table::data.table(
  option = c(
    '\'cell_color_d_pal\'',
    '\'poly_color_d_pal\'',
    '\'feat_color_pal\'',
    '\'heatmap_clus_color_pal\'',
    '\'cell_color_d_rev\'',
    '\'poly_color_d_rev\'',
    '\'feat_color_rev\'',
    '\'heatmap_clus_color_rev\'',
    '\'cell_color_d_strategy\'',
    '\'poly_color_d_strategy\'',
    '\'feat_color_strategy\'',
    '\'heatmap_clus_color_strategy\'',
    '\'cell_color_c_pal\'',
    '\'poly_color_c_pal\'',
    '\'cell_color_c_rev\'',
    '\'poly_color_c_rev\'',
    '\'heatmap_color_pal\'',
    '\'heatmap_color_rev\''
  ),
  default = c(
    'NULL (distinct colors)',
    'NULL (distinct colors)',
    'NULL (distinct colors)',
    'NULL (distinct colors)',
    'FALSE',
    'FALSE',
    'FALSE',
    'FALSE',
    '\'interpolate\'',
    '\'interpolate\'',
    '\'interpolate\'',
    '\'interpolate\'',
    'c(\'blue\', \'white\', \'red\')',
    'c(\'blue\', \'white\', \'red\')',
    'FALSE',
    'FALSE',
    'c(\'blue\', \'white\', \'red\')',
    'FALSE'
  ),
  description = c(
    'Cell: color palette to use for discrete color values. Can be supplied as a the name of a palette accessible by `getColors()`, custom color vector, or an external function with a \'n\' colors requested param',
    'Poly: color palette to use for discrete color values. Can be supplied as a the name of a palette accessible by `getColors()`, custom color vector, or an external function with a \'n\' colors requested param',
    'Feat: color palette to use for discrete color values. Can be supplied as a the name of a palette accessible by `getColors()`, custom color vector, or an external function with a \'n\' colors requested param',
    'Heatmap Clusters: color palette to use for discrete color values. Can be supplied as a the name of a palette accessible by `getColors()`, custom color vector, or an external function with a \'n\' colors requested param',
    'Cell: whether discrete color values should be reversed',
    'Poly: whether discrete color values should be reversed',
    'Feat: whether discrete color values should be reversed',
    'Heatmap Clusters: whether discrete color values should be reversed',
    'Cell: \'interpolate\', \'recycle\', or \'cutoff\'. Policy to use when more colors are requested than exist within palette.',
    'Poly: \'interpolate\', \'recycle\', or \'cutoff\'. Policy to use when more colors are requested than exist within palette.',
    'Feat: \'interpolate\', \'recycle\', or \'cutoff\'. Policy to use when more colors are requested than exist within palette.',
    'Heatmap Clusters: \'interpolate\', \'recycle\', or \'cutoff\'. Policy to use when more colors are requested than exist within palette.',
    'Cell: color palette to use for continuous color values. Can be supplied as a the name of a palette accessible by `getColors()`, custom color vector, or an external function with a \'n\' colors requested param',
    'Poly: color palette to use for continuous color values. Can be supplied as a the name of a palette accessible by `getColors()`, custom color vector, or an external function with a \'n\' colors requested param',
    'Cell: whether continuous color values should be reversed',
    'Poly: whether continuous color values should be reversed',
    'Heatmap: color palette to use for continuous color values. Can be supplied as a the name of a palette accessible by `getColors()`, custom color vector, or an external function with a \'n\' colors requested param',
    'Heatmap: whether continuous color values should be reversed'
  )
)



