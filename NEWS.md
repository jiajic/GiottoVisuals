

# GiottoVisuals 0.1.6 (2024/03/07)

## bug fixes
- fix `spatPlot2D()` and `spatFeatPlot2D()` largeImage plotting when using `group_by`
- fix `edge_alpha` param in `spatFeatPlot2D()`

## new
- `mixRGB()` vectorized additive mixing in RGB space
- `mixHSV()` vectorized color mixing in HSV space



# GiottoVisuals 0.1.4 (2024/01/25)
## bug fixes
- fix plotting color gradient when using param `point_shape = "no_border"`
- fix image NA value [#865](https://github.com/drieslab/Giotto/issues/865) by rbutleriii

## new
- `gg_annotation_raster()` internal generic for adding a `giottoImage`, `giottoLargeImage`, or `list` thereof to a `ggplot` object

# GiottoVisuals 0.1.2 (2023/01/12)

## bug fixes
- fix `ComplexHeatmap` saving size issue [#861](https://github.com/drieslab/Giotto/issues/861) by rbutleriii

# GiottoVisuals 0.1.1 (2023/12/16)

## Breaking Changes
- Removed: `getDistinctColors()` to *GiottoUtils*

## Added
- Add: `getDistinctColors()` and `getRainbowColors()` as re-exports from *GiottoUtils*

## Changes
- Changed: Package internal functions now have `.` prefix
- Changed: *GiottoUtils* and *GiottoClass* moved to Imports
- Changed: Other internal function naming changes to bring in line with updates to *GiottoUtils*


# GiottoVisuals 0.1.0 (23/11/29)

Initial release
