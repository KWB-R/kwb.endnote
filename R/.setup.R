# Set the name for your new package
package <- "kwb.endnote"

# Set the path to your new package
pkg_dir <- file.path(kwb.utils::get_homedir(), "RRrojects", package)

usethis::proj_set(pkg_dir)

# Create a default package structure
withr::with_dir(pkg_dir, {kwb.pkgbuild::use_pkg_skeleton(package)})

kwb.pkgbuild::use_pkg(pkg = list(name = "kwb.endnote",
title = "Helper Functions For Analysing KWB Endnote Library (Exported As .XML)" ,
desc = "Helper Functions For Analysing KWB Endnote Library (Exported As .XML)."))


### Make the repo "public" before using "autopkgdown"
kwb.pkgbuild::use_autopkgdown("kwb.endnote")
kwb.pkgbuild::add_gitlabci_to_ghpages("kwb.endnote", dest_dir = tempdir())


usethis::use_vignette("analyse-endnote")

pkg_dependencies <- c('dplyr', 'kwb.file', 'kwb.read', 'stringr')

sapply(pkg_dependencies, usethis::use_package)
desc::desc_normalize()
