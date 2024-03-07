# Run on library loading


.onAttach <- function(libname, pkgname) {
    ## print version number ##

    check_ver <- getOption("giotto.check_version", TRUE)
    if (isTRUE(check_ver)) {
        GiottoUtils::check_github_suite_ver("GiottoVisuals")
        options("giotto.check_version" = FALSE)
    }
}
