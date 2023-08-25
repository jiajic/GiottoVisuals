## ** image object compatibility ####

#' @title Optimized largeImage resampling
#' @name plot_auto_largeImage_resample
#' @description Downsample \code{largeImage} for plotting. Uses \code{\link[terra]{spatSample}}
#' to load only a portion of the original image, speeding up plotting and lowering memory
#' footprint.
#'
#' Default behavior of \code{spatSample} is to crop if only a smaller ROI is needed for plotting
#' followed by the sampling process in order to reduce wasted sampling by focusing the
#' sample space. For very large ROIs, this crop can be time intensive and require
#' writing to disk.
#'
#' This function examines the ROI dimensions as defined through the limits of the spatial
#' locations to be plotted, and decides between the following two methods in order to
#' avoid this issue:
#' \itemize{
#'   \item{\strong{Method A.} First crop original image and then sample n values where
#'   n = 500,000 to generate final image}
#'   \item{\strong{Method B.} First oversample n values and then crop, where n = 500,000
#'   scaled by a value >1. Scaling factor increases the smaller the ROI is and
#'   is defined by: original dimensions/crop dimensions where the larger ratio between
#'   x and y dims is chosen. Scale factor is capped by \code{max_resample_scale}}
#' }
#' Control points for this function are set by \code{max_crop} which decides the max
#' ROI area after which switchover to method B happens in order to avoid laborious crops
#' and \code{max_resample_scale} which determines the maximum scale factor for number
#' of values to sample. Both values can be adjusted depending on system resources.
#' Additionally, \code{flex_resample} determines if this switching behavior happens.
#' When set to \code{FALSE}, only method A is used.
#' @param gobject \code{gobject} containing \code{largeImage} object
#' @param giottoLargeImage \code{largeImage} object to resample if not provided through
#' \code{gobject} and \code{largeImage_name}
#' @param largeImage_name name of \code{largeImage} in \code{gobject}
#' @param spat_unit spatial unit
#' @param spat_loc_name name of spatial locations to plot
#' @param polygon_feat_type name of polygon/spatial_info to plot
#' @param include_image_in_border [boolean] expand the extent sampled to also show image in
#' border regions not included in spatlocs. This prevents images in plots from
#' being sharply cut off around the furthest spatial locations. (default is \code{TRUE})
#' @param flex_resample [boolean] Whether to allow automatic selection of sampling
#' workflow as defined in details sections. (default is \code{TRUE})
#' @param max_crop maximum crop size allowed for \strong{method A} before switching to
#' \strong{method B} (see description)
#' @param max_resample_scale maximum cells allowed to resample to compensate for
#' decreased resolution when cropping after sampling
#' @return a \code{giottoLargeImage} cropped and resampled properly for plotting
#' @seealso \code{\link[terra]{spatSample}}
#' @keywords internal
plot_auto_largeImage_resample = function(gobject,
                                         giottoLargeImage = NULL,
                                         largeImage_name = NULL,
                                         spat_unit = NULL,
                                         spat_loc_name = NULL,
                                         polygon_feat_type = NULL,
                                         include_image_in_border = TRUE,
                                         flex_resample = TRUE,
                                         max_crop = 1e+08,
                                         max_resample_scale = 100) {

  # If no giottoLargeImage, select specified giottoLargeImage. If none specified, select first one.
  if(is.null(giottoLargeImage)) {
    giottoLargeImage = get_giottoLargeImage(gobject = gobject,
                                            name = largeImage_name)
  }

  # Set spat_unit
  spat_unit = set_default_spat_unit(gobject = gobject,
                                    spat_unit = spat_unit)

  # Get spatial locations
  cell_locations = get_spatial_locations(gobject = gobject,
                                         spat_unit = spat_unit,
                                         spat_loc_name = spat_loc_name,
                                         output = 'data.table',
                                         copy_obj = TRUE)

  # If no spatial locations are available, rely on first existing polygon extent
  if(is.null(cell_locations)) {
    sub_obj = get_polygon_info(gobject = gobject,
                               polygon_name = polygon_feat_type,
                               return_giottoPolygon = TRUE)

    # Find centroids then if there are more than 100, sample 30%
    sub_obj = calculate_centroids_polygons(sub_obj)
    sampleSize = ifelse(nrow(sub_obj) > 100, ceiling(0.3*nrow(sub_obj)), nrow(sub_obj))

    centroid_sample_DT = slot(sub_obj, 'spatVectorCentroids') %>%
      sample(., size = sampleSize) %>%
      terra::geom() %>%
      as.data.table()

    cell_locations = data.table::data.table(sdimx = c(centroid_sample_DT$x),
                                            sdimy = c(centroid_sample_DT$y))

    # sub_ext = terra::ext(sub_obj)[1:4]
    # cell_locations = data.table::data.table(sdimx = c(sub_ext[['xmin']], sub_ext[['xmax']]),
    #                                         sdimy = c(sub_ext[['ymin']], sub_ext[['ymax']]))
  }

  if(is.null(cell_locations)) stop('No spatial locations or polygons discovered.\n Cannot determine largeImage resample extent\n')

  # Get image extent minmax
  im_minmax = terra::ext(giottoLargeImage@raster_object)[1:4]
  # Determine crop
  if(isTRUE(include_image_in_border)) {
    # with crop padding
    x_range = range(cell_locations$sdimx)
    y_range = range(cell_locations$sdimy)
    x_halfPaddedRange = diff(x_range)*0.625
    y_halfPaddedRange = diff(y_range)*0.625
    x_midpt = mean(x_range)
    y_midpt = mean(y_range)

    xmax_crop = x_midpt + x_halfPaddedRange
    xmin_crop = x_midpt - x_halfPaddedRange
    ymax_crop = y_midpt + y_halfPaddedRange
    ymin_crop = y_midpt - y_halfPaddedRange

    if(xmin_crop < im_minmax[['xmin']]) xmin_crop = im_minmax[['xmin']]
    if(xmax_crop > im_minmax[['xmax']]) xmax_crop = im_minmax[['xmax']]
    if(ymin_crop < im_minmax[['ymin']]) ymin_crop = im_minmax[['ymin']]
    if(ymax_crop > im_minmax[['ymax']]) ymax_crop = im_minmax[['ymax']]
  } else {
    # no crop padding
    x_range = range(cell_locations$sdimx)
    y_range = range(cell_locations$sdimy)
    xmin_crop = x_range[1]
    xmax_crop = x_range[2]
    ymin_crop = y_range[1]
    ymax_crop = y_range[2]
  }

  # setup crop extent object
  crop_ext = terra::ext(xmin_crop, xmax_crop,
                        ymin_crop, ymax_crop)

  # zoom and resample giottoLargeImage
  crop_xdim = abs(xmax_crop - xmin_crop)
  crop_ydim = abs(ymax_crop - ymin_crop)
  crop_area_px = crop_xdim * giottoLargeImage@scale_factor[['x']] * giottoLargeImage@scale_factor[['y']] * crop_ydim
  im_xdim = abs(im_minmax[2] - im_minmax[1])
  im_ydim = abs(im_minmax[2] - im_minmax[1])
  crop_relative_scale = max(im_xdim/crop_xdim, im_ydim/crop_ydim)

  if(!isTRUE(flex_resample) | crop_area_px <= max_crop) {
    # METHOD A: Crop if needed then resample to final image
    if(!isTRUE(flex_resample) & crop_area_px > max_crop) {
      warning('Plotting large regions with flex_resample == FALSE will be costly in time and drive space.')
    }
    # For ROIs with area smaller than max_crop OR if flex_resample is FALSE
    crop_image = terra::crop(x = giottoLargeImage@raster_object,
                             y = crop_ext)
    giottoLargeImage@raster_object = terra::spatSample(crop_image,
                                                       size = 500000,
                                                       method = 'regular',
                                                       as.raster = TRUE)
  } else {
    # METHOD B: Resample then crop to final image
    # Sample n values where n = default val scaled by a value >1
    if(crop_relative_scale <= max_resample_scale) {
      # Scale factor is fullsize image dim/crop dim. Larger of the two ratios is chosen
      resample_image = terra::spatSample(giottoLargeImage@raster_object,
                                         size = round(500000 * crop_relative_scale),
                                         method = 'regular',
                                         as.raster = TRUE)
    } else {
      # For scale factors that are too large, scaling is capped by max_resample_scale
      resample_image = terra::spatSample(giottoLargeImage@raster_object,
                                         size = 500000 * max_resample_scale,
                                         method = 'regular',
                                         as.raster = TRUE)
    }
    giottoLargeImage@raster_object = terra::crop(x = resample_image,
                                                 y = crop_ext)
  }

  return(giottoLargeImage)
}
