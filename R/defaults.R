


# default plot n cols ####
#' @title Set default ncols in plotting grid
#' @name set_default_cow_n_col
#' @keywords internal
set_default_cow_n_col = function(cow_n_col = NULL,
                                 nr_plots) {

  if(is.null(cow_n_col)) {
    cow_n_col = ceiling(sqrt(nr_plots))
  } else {
    cow_n_col
  }
  return(cow_n_col)
}


# default getColors ####

# -------------------------------------------------------------------------- #
# internal function to provide a function that will generate 'n' hexadecimal
# color codes for discrete colors
# inputs are INTENDED to be provided via options
#
# FUNCTION input to 'color' are also permitted, but REQUIRE an 'n' param
# for downstream functionality. 'giotto.color_discrete_pal' and
# 'giotto.color_discrete_strategy' are additionally ignored
# -------------------------------------------------------------------------- #
set_default_color_discrete = function(colors = NULL, ...) {

  # get 'colors' either from param input or options
  if (is.null(colors)) {
    opt_pal = getOption('giotto.color_discrete_pal', NULL)
    if (is.null(opt_pal)) {
      return(getDistinctColors)
    } else colors = opt_pal
  }

  # get palette settings
  opt_rev = getOption('giotto.color_discrete_rev', FALSE)
  opt_strategy = getOption('giotto.color_discrete_strategy', 'interpolate')

  # evaluate 'colors'
  if (inherits(colors, 'function')) return(colors)
  else if (inherits(colors, 'character')) {
    if(length(colors) > 1) { # assume simple palette if multiple entries in vector
      return(simple_palette_factory(col = colors, rev = opt_rev, strategy = opt_strategy))
    } else { # assume call to getColors() otherwise
      # return wrapped
      return(get_palette_factory(pal = colors, rev = opt_rev, strategy = opt_strategy))
    }
  }
}


#' @name showColorOptions
#' @title Show Giotto color options
#' @description Show the color options that are settable in Giotto. A data.table
#' of the information is invisibly returned
#' @return data.table of Giotto color option information
#' @export
showColorOptions = function() {
  out = giotto_color_options[, paste(
    '\noption      : ', option,
    '\ndefault     : ', default,
    '\ndescription : ', description,
    '\n'
  )]
  cat(out)
  return(invisible(giotto_color_options))
}

giotto_color_options = data.table::data.table(
  option = c(
    '\'giotto.color_discrete_pal\'',
    '\'giotto.color_discrete_rev\'',
    '\'giotto.color_discrete_strategy\''
  ),
  default = c(
    'NULL (distinct colors)',
    'FALSE',
    '\'interpolate\''
  ),
  description = c(
    'color palette to use for discrete color values. Can be supplied as a the name of a palette accessible by `getColors()`, custom color vector, or an external function with a \'n\' colors requested param',
    'whether discrete color values should be reversed',
    '\'interpolate\', \'recycle\', or \'cutoff\'. Policy to use when more colors are requested than exist within palette.'
  )
)



