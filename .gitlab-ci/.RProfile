# message("Loading custom profile.")

local({
        lib_paths <- .libPaths()
        lib_paths <- c(Sys.getenv("R_LIBS_USER"), lib_paths)

        .libPaths(lib_paths)

        rep <- getOption("repos")
        rep["CRAN"] <-Sys.getenv("CRAN_REPOS")

        options(repos = rep)

        # message("lib_paths = (", paste(lib_paths, collapse = ", "), ")")
})

# message("Global: .libPaths() = (", paste(.libPaths(), collapse = ", "), ")")
