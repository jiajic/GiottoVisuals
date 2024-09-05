#' @title Plot saving
#' @name plot_save
#' @description
#' Functions to automatically save plots to directory of interest
#' @param gobject giotto object or giottoInstructions
#' @param plot_object ggplot object to plot
#' @param save_dir directory to save to
#' @param save_folder folder in save_dir to save to
#' @param save_name name of plot
#' @param default_save_name default name to save a plot
#' @param save_format format (e.g. png, tiff, pdf, ...)
#' @param show_saved_plot load & display the saved plot
#' @param ncol number of columns for multiplots
#' @param nrow number of rows for multiplot
#' @param scale scale of plots
#' @param base_width width of plot
#' @param base_height height of plot
#' @param base_aspect_ratio aspect ratio of plot
#' @param units plotting units (e.g. in)
#' @param dpi dpi for each plot if plot is in raster format
#' @param limitsize When TRUE (the default), ggsave will not save images larger
#' than 50x50 inches, to prevent the common error of specifying dimensions in
#' pixels.
#' @param plot_count manually set the plot count that is appended to a
#' default_save_name
#' @param gpsparam `giotto_plot_save_param` object. If provided, will be
#' used instead of most other general save params. (save_dir, save_folder,
#' save_name, default_save_name, save_format, base_width, base_height,
#' base_aspect_ratio, units, dpi, plot_count)
#' @param \dots additional parameters to pass downstream save functions.
#' [cowplot::save_plot()] is used for `ggplot2` plots. grDevices png, tiff
#' svg, pdf is used for base and general saving
#' @returns a plot file
#' @seealso \code{\link{showSaveParameters}} \code{\link[cowplot]{save_plot}}
#' \code{\link[grDevices]{png}}
#' \code{\link[grDevices]{tiff}}
#' \code{\link[grDevices]{pdf}}
#' \code{\link[grDevices]{svg}}
NULL





#' @rdname plot_save
#' @examples
#' g <- GiottoData::loadGiottoMini("vis")
#' df <- data.frame(x = rnorm(5), y = rnorm(5))
#' g_plot <- ggplot2::ggplot(df, ggplot2::aes(x,y)) + ggplot2::geom_point()
#' all_plots_save_function(g, g_plot)
#'
#' @export
all_plots_save_function <- function(gobject,
    plot_object,
    save_dir = NULL,
    save_folder = NULL,
    save_name = NULL,
    default_save_name = "giotto_plot",
    save_format = NULL,
    show_saved_plot = FALSE,
    ncol = 1,
    nrow = 1,
    scale = 1,
    base_width = NULL,
    base_height = NULL,
    base_aspect_ratio = NULL,
    units = NULL,
    dpi = NULL,
    limitsize = TRUE,
    plot_count = NULL,
    gpsparam = NULL,
    ...) {

    # get save params
    if (is.null(gpsparam)) {
        a <- .gvis_get_save_param_input(
            gobject = gobject, plot_object = plot_object
        )
        # finalize save params
        gpsparam <- do.call(.gvis_save_param, args = a)
    }

    checkmate::assert_class(gpsparam, "giotto_plot_save_param")

    if (identical(getOption("giotto.verbose"), "debug")) {
        print(gpsparam)
    }

    # perform save
    if (any("ggplot" %in% class(plot_object))) {
        .ggplot_save_function(
            gobject = gobject,
            plot_object = plot_object,
            show_saved_plot = show_saved_plot,
            ncol = ncol,
            nrow = nrow,
            scale = scale,
            limitsize = limitsize,
            gpsparam = gpsparam,
            ...
        )
    } else {
        .general_save_function(
            gobject = gobject,
            plot_object = plot_object,
            show_saved_plot = show_saved_plot,
            gpsparam = gpsparam,
            ...
        )
    }
}




#' @title showSaveParameters
#' @name showSaveParameters
#' @description Description of Giotto saving options,
#' links to \code{\link{all_plots_save_function}}
#' @returns Instructions on how to use the automatic plot saving options
#' within Giotto
#' @export
#' @examples
#' showSaveParameters()
showSaveParameters <- function() {
    message("This is a simple guide to help you automatically saving plots. \n")
    message("Importantly, defaults for all these parameters can be set at
    the beginning with createGiottoInstructions() \n")
    message("See
    https://rubd.github.io/Giotto/articles/instructions_and_plotting.html
    for more information and examples \n \n")

    message("Each plotting function in Giotto has 4 important parameters for
    showing and/or saving a plot: \n
        - show_plot: TRUE or FALSE, show the plot to the console
        - return_plot: TRUE or FALSE, return the plot to the console
        (e.g. to further modify or save the plot
        - save_plot: TRUE or FALSE, automatically save the plot
        - save_param: a list of parameters that can be set \n")

    message("The following list of parameters can be provided to save_param: \n
        - save_dir: directory to save the plot to
        - save_folder: if not NULL, a subfolder within save_dir that will
        be created to save the plot to
        - save_name: name of the plot (no extension needed, see save_format)
        - save_format: picture format to use, default is .png
        - ncol: number of columns for multiplots
        - nrow: number of rows for multiplot
        - scale: scale of plots
        - base_width: width of plot
        - base_height: height of plot
        - base_aspect_ratio: ratio of plot
        - units: plotting units (e.g. in)
        - dpi: dpi for each plot if plot is in raster format\n")

    message("Example: \n
        plotfunction(...,
                    save_plot = TRUE,
                    save_param = list(save_name = 'favorite_name',
                    units = 'png'))")
}




# internals ####

# gpsparam should be a `giotto_plot_save_param` object if provided
#' @noMd
#' @keywords internal
.ggplot_save_function <- function(
        gobject,
        plot_object,
        show_saved_plot = FALSE,
        ncol = 1,
        nrow = 1,
        scale = 1,
        limitsize = TRUE,
        gpsparam = NULL,
        ...
) {
    if (is.null(plot_object)) {
        stop("\t there is no object to plot \t")
    }

    sparam <- gpsparam

    # create saving location
    fullpath <- sparam$fullpath
    filename <- basename(fullpath)
    path <- dirname(fullpath)
    save_format <- sparam$save_format

    if (!file.exists(path)) {
        dir.create(path, recursive = TRUE)
    }


    cowplot::save_plot(
        plot = plot_object,
        filename = filename,
        path = path,
        ncol = ncol,
        nrow = nrow,
        scale = scale,
        device = save_format,
        limitsize = limitsize,
        # save param items
        dpi = sparam$dpi,
        units = sparam$units,
        base_width = sparam$base_width,
        base_height = sparam$base_height,
        base_aspect_ratio = sparam$base_aspect_ratio,
        ...
    )

    # show saved plot if requested
    if (isTRUE(show_saved_plot)) {
        if (save_format == "png") {
            if (package_check("png", optional = TRUE)) {
                img <- png::readPNG(source = fullpath)
                grid::grid.raster(img)
            }
        } else if (save_format == "tiff") {
            if (package_check("tiff", optional = TRUE)) {
                img <- tiff::readTIFF(source = fullpath)
                grid::grid.raster(img)
            }
        } else {
            warning("\t only png & tiff are currently supported \t")
        }
    }
}



# gpsparam should be a `giotto_plot_save_param` object if provided
#' @noMd
#' @keywords internal
.general_save_function <- function(
        gobject,
        plot_object,
        show_saved_plot = FALSE,
        gpsparam = NULL,
        ...
) {
    if (is.null(plot_object)) {
        stop("\t there is no object to plot \t")
    }

    sparam <- gpsparam

    fullpath <- sparam$fullpath
    save_format <- sparam$save_format
    dpi <- sparam$dpi
    units <- sparam$units
    base_width <- sparam$base_width
    base_height <- sparam$base_height


    # create saving location
    path <- dirname(fullpath)
    if (!file.exists(path)) {
        dir.create(path, recursive = TRUE)
    }

    if (any("plotly" %in% class(plot_object))) {
        htmlwidgets::saveWidget(
            plotly::as_widget(plot_object),
            file = fullpath
        )
    } else {

        switch(save_format,
            "png" = {
                grDevices::png(
                    filename = fullpath, width = base_width,
                    height = base_height, res = dpi, units = units, ...
                )
                print(plot_object)
                grDevices::dev.off()
            },
            "tiff" = {
                grDevices::tiff(
                    filename = fullpath, width = base_width,
                    height = base_height, units = units, ...
                )
                print(plot_object)
                grDevices::dev.off()
            },
            "pdf" = {
                grDevices::pdf(
                    file = fullpath, width = base_width,
                    height = base_height, useDingbats = FALSE, ...
                )
                print(plot_object)
                grDevices::dev.off()
            },
            "svg" = {
                grDevices::svg(
                    filename = fullpath, width = base_width,
                    height = base_height, ...
                )
                print(plot_object)
                grDevices::dev.off()
            }
        )

        # show saved plot if requested
        if (isTRUE(show_saved_plot)) {
            switch(save_format,
                "png" = {
                    if (package_check("png", optional = TRUE)) {
                        img <- png::readPNG(source = fullpath)
                        grid::grid.raster(img)
                    }
                },
                "tiff" = {
                    if (package_check("tiff", optional = TRUE)) {
                        img <- tiff::readTIFF(source = fullpath)
                        grid::grid.raster(img)
                    }
                },
                warning("\t only png & tiff are currently supported \t")
            )
        }
    }
}


# detect these variables from previous stack frame and provide as list
.gvis_get_save_param_input <- function(gobject, plot_object) {
    # separately provide gobject and plot_object since they may be named
    # something non-standard.

    # type of plot object
    type <- "general"
    if(any("ggplot" %in% class(plot_object))) type <- "gg"
    if (any("plotly" %in% class(plot_object))) type <- "plotly"

    data_args <- list(
        gobject = gobject,
        type = type
    )

    # args to pull from prev. stack frame
    # This is only possible because these args are standard
    expected_save_argnames <- c(
        "save_dir", "save_folder", "save_name", "default_save_name",
        "save_format", "dpi", "base_width", "base_height", "base_aspect_ratio",
        "units", "plot_count"
    )

    # pull expected args from previous stack frame
    sparam_input <- get_args_list(keep = expected_save_argnames, toplevel = 2L)
    sparam_input <- c(data_args, sparam_input)

    # report any save args not found
    missing_bool <- !expected_save_argnames %in% names(sparam_input)
    missing_args <- expected_save_argnames[missing_bool]
    if (length(missing_args) > 0L) {
        warning(sprintf(
            ".gvis_get_save_param_input did not find some args: '%s'",
            paste(missing_args, collapse = "', '")
        ))
    }

    return(sparam_input)
}


# returns `giotto_plot_save_param`
# check giotto instructions for defaults when values are not passed
# builds a full filepath to use for the plot saving
# the `type` param affects which types of outputs are possible.
.gvis_save_param <- function(
        gobject,
        type = c("gg", "plotly", "general"),
        save_dir = NULL,
        save_folder = NULL,
        save_name = NULL,
        default_save_name = "giotto_plot",
        save_format = NULL,
        dpi = NULL,
        base_width = NULL,
        base_height = NULL,
        base_aspect_ratio = NULL,
        units = NULL,
        plot_count = NULL
) {
    ## save format -------------------------------------------------------- ##

    save_format <- save_format %null%
        instructions(gobject, param = "plot_format")

    save_format <- switch(type,
        "gg" = save_format,
        "plotly" = "html",
        "general" = match.arg(save_format, c("png", "tiff", "pdf", "svg"))
    )

    ## get save information and set defaults ------------------------------ ##
    save_dir <- save_dir %null% instructions(gobject, param = "save_dir")
    plot_count <- plot_count %null% getOption("giotto.plot_count", 1)
    dpi <- dpi %null% instructions(gobject, param = "dpi")
    base_width <- base_width %null% instructions(gobject, param = "width")
    base_height <- base_height %null% instructions(gobject, param = "height")
    base_aspect_ratio <- base_aspect_ratio %null% 1.1
    units <- units %null% instructions(gobject, param = "units")


    ## checking ----------------------------------------------------------- ##
    dpi <- as.numeric(dpi)
    base_width <- as.numeric(base_width)
    base_height <- as.numeric(base_height)
    base_aspect_ratio <- as.numeric(base_aspect_ratio)
    if (is.na(save_dir)) save_dir <- getwd()

    # build filepath ------------------------------------------------------ ##

    if (is.null(save_name)) {
        save_name <- default_save_name
        save_name <- paste0(plot_count, "-", save_name)
        options("giotto.plot_count" = plot_count + 1L) # increment
    }

    if (!is.null(save_folder)) {
        file_location <- file.path(save_dir, save_folder)
    } else {
        file_location <- save_dir
    }

    filename <- paste0(save_name, ".", save_format)
    fullpath <- file.path(file_location, filename)

    # create params object ------------------------------------------------ ##
    structure(
        list(
            fullpath = fullpath,
            save_format = save_format,
            dpi = dpi,
            base_width = base_width,
            base_height = base_height,
            base_aspect_ratio = base_aspect_ratio,
            units = units
        ),
        class = "giotto_plot_save_param"
    )
}

#' @export
print.giotto_plot_save_param <- function(x, ...) {
    cat(sprintf("<%s>\n", class(x)))
    print_list(x)
}

# gpsparam should be a `giotto_plot_save_param`
.plot_px_area <- function(gpsparam) {

    dims <- c(gpsparam$base_height, gpsparam$base_width)
    pxdims <- switch(gpsparam$units,
        "in" = dims * gpsparam$dpi,
        "cm" = (dims / 2.54) * gpsparam$dpi,
        "mm" = (dims / 25.4) * gpsparam$dpi,
        "px" = dims
    )
    round(prod(pxdims))
}


