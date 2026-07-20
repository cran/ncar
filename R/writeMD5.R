# writeMD5.R
# Write the standard R 'MD5' manifest into an installed package directory so that
# tools::checkMD5sums() (and ncar::pdfIQ) can verify file integrity. The manifest
# records the md5 checksum of every installed file -- the same file CRAN ships and
# that R's own installer writes (via tools:::.installMD5sums()). Packages installed
# from CRAN already contain it; for a local source install it is usually absent,
# which is why the IQ integrity check reports WARN. Run writeMD5() once, right after
# installing the package, to record the trusted baseline; thereafter checkMD5sums()
# detects any later modification of the installed files. Uses only the exported
# tools::md5sum(), so it is CRAN-policy safe.

writeMD5 = function(pkg="ncar", lib.loc=NULL)
{
  dir = find.package(pkg, lib.loc=lib.loc)
  if (file.access(dir, mode=2) != 0)
    stop(sprintf("Package directory is not writable: %s", dir))
  files = list.files(dir, recursive=TRUE, all.files=TRUE, no.. = TRUE)
  files = setdiff(files, "MD5")                 # the manifest never lists itself
  sums  = tools::md5sum(file.path(dir, files))
  ok    = !is.na(sums)
  writeLines(paste0(sums[ok], " *", files[ok]), file.path(dir, "MD5"))
  cat(sprintf("Wrote MD5 manifest for '%s' (%d files) at %s\n",
              pkg, sum(ok), file.path(dir, "MD5")))
  invisible(file.path(dir, "MD5"))
}
