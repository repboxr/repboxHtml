repbox_save_html = function(html,file, dir, ensure_shared_www = TRUE, overwrite_shared=TRUE) {
  restore.point("repbox_save_html")
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  writeUtf8(html, file.path(dir, file))
  repbox_copy_shared_www(dir, overwrite=overwrite_shared)
  invisible()
}

repbox_copy_shared_www = function(www.dir, overwrite=FALSE) {
  # Copy shared
  if (!dir.exists(www.dir)) dir.create(www.dir)
  pkg.www = system.file("www", package="repboxHtml")
  files = list.files(pkg.www,include.dirs = TRUE,recursive = FALSE, full.names=TRUE)
  all(file.copy(files, www.dir,recursive = TRUE,overwrite = overwrite))
}

writeUtf8 = function(x, file, bom=F) {
  con <- file(file, "wb")
  if(bom) writeBin(BOM, con, endian="little")
  writeBin(charToRaw(x), con, endian="little")
  close(con)
}
