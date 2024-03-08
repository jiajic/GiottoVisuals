#' @name gg_annotation_raster
#' @keywords internal
#' @title Append image to ggplot as annotation_raster
#' @param ggobj ggplot2 `gg` object
#' @param gimage `giottoLargeImage`, `giottoImage` or `list` thereof
#' @return `gg` object with images to plot appended as annotation rasters
NULL

#' @rdname gg_annotation_raster
setMethod(
    "gg_annotation_raster",
    signature(ggobj = "gg", gimage = "list"),
    function(ggobj, gimage, ...) {
        for (i in seq_along(gimage)) {
            ggobj <- gg_annotation_raster(ggobj, gimage[[i]], ...)
        }
        return(ggobj)
    }
)

#' @rdname gg_annotation_raster
setMethod(
    "gg_annotation_raster",
    signature(ggobj = "gg", gimage = "giottoImage"),
    function(ggobj, gimage, ...) {
        # extract min and max from object
        my_xmax <- gimage@minmax[1]
        my_xmin <- gimage@minmax[2]
        my_ymax <- gimage@minmax[3]
        my_ymin <- gimage@minmax[4]

        # convert giotto image object into array
        img_array <- as.numeric(gimage@mg_object[[1]])

        # extract adjustments from object
        xmax_b <- gimage@boundaries[1]
        xmin_b <- gimage@boundaries[2]
        ymax_b <- gimage@boundaries[3]
        ymin_b <- gimage@boundaries[4]

        # append to ggobj
        ggobj <- ggobj + annotation_raster(
            img_array,
            xmin = my_xmin - xmin_b, xmax = my_xmax + xmax_b,
            ymin = my_ymin - ymin_b, ymax = my_ymax + ymax_b
        )

        # TODO geom_raster to accommodate single-channel
        return(ggobj)
    }
)

#' @rdname gg_annotation_raster
#' @import terra
setMethod(
    "gg_annotation_raster",
    signature(ggobj = "gg", gimage = "giottoLargeImage"),
    function(ggobj, gimage, ...) {
        # get plotting minmax
        extent <- terra::ext(gimage@raster_object)[seq_len(4)]
        xmin <- extent[["xmin"]]
        xmax <- extent[["xmax"]]
        ymin <- extent[["ymin"]]
        ymax <- extent[["ymax"]]

        # convert raster object into array with 3 channels
        img_array <- terra::as.array(gimage@raster_object)

        # TODO: check if required, fixes NaN values
        # replacing NA's by zero or another value directly in raster object?
        # raster[is.na(raster[])] <- 0
        if (is.nan(max(img_array[, , 1]))) {
            img_array[, , 1][is.nan(img_array[, , 1])] <- max(img_array[, , 1],
                na.rm = TRUE
            )
        }

        if (dim(img_array)[3] > 1) {
            if (is.nan(max(img_array[, , 2]))) {
                img_array[, , 2][is.nan(img_array[, , 2])] <-
                    max(img_array[, , 2], na.rm = TRUE)
            }
        }

        if (dim(img_array)[3] > 2) {
            if (is.nan(max(img_array[, , 3]))) {
                img_array[, , 3][is.nan(img_array[, , 3])] <-
                    max(img_array[, , 3], na.rm = TRUE)
            }
        }

        img_array <- img_array / max(img_array, na.rm = TRUE)
        if (dim(img_array)[3] == 1) {
            img_array_RGB <- array(NA, dim = c(dim(img_array)[seq_len(2)], 3))
            img_array_RGB[, , seq_len(3)] <- img_array
        } else {
            img_array_RGB <- img_array
        }

        # handle NA values
        img_array_RGB[is.na(img_array_RGB)] <- 0

        # append to ggobj
        ggobj <- ggobj + annotation_raster(
            img_array_RGB,
            xmin = xmin, xmax = xmax,
            ymin = ymin, ymax = ymax
        )

        # TODO geom_raster to accommodate single-channel
        return(ggobj)
    }
)
