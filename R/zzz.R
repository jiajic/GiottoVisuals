# Run on library loading


.onAttach <- function(libname, pkgname) {
    ## print version number ##

    check_ver <- getOption("giotto.check_version", TRUE)
    if (isTRUE(check_ver)) {
        GiottoUtils::check_github_suite_ver("GiottoVisuals")
        options("giotto.check_version" = FALSE)
    }

    ## set defaults ##
    ## ------------ ##

    # colors continuous
    options("giotto.color_cd_pal" = c("blue", "white", "red"))
    options("giotto.color_cs_pal" = "viridis")
    options("giotto.color_c_rev" = FALSE)

    # colors discrete
    options("giotto.color_d_pal" = "distinct")
    options("giotto.color_d_rev" = FALSE)
    options("giotto.color_d_strategy" = "interpolate")

    # image resampling
    options("giotto.plot_img_max_sample" = 5e5)
    options("giotto.plot_img_max_crop" = 1e8)
    options("giotto.plot_img_max_resample_scale" = 100)
}



