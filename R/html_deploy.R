repbox_save_html = function(html,file, project_dir, html_dir=file.path(project_dir,"reports"), ensure_shared_www = TRUE, overwrite_shared=TRUE) {
  restore.point("repbox_save_html")
  if (!dir.exists(html_dir)) dir.create(html_dir, recursive = TRUE)
  writeUtf8(html, file.path(html_dir, file))
  if (ensure_shared_www) {
    repbox_copy_shared_www(project_dir, overwrite=overwrite_shared)
  }
  invisible()
}


repbox_copy_shared_www = function(project_dir, www_dir=file.path(project_dir,"reports"), overwrite=FALSE) {
  # Copy shared
  if (!dir.exists(www_dir)) dir.create(www_dir)
  pkg.www = system.file("www", package="repboxHtml")
  files = list.files(pkg.www,include.dirs = TRUE,recursive = FALSE, full.names=TRUE)
  file.copy(files, www_dir,recursive = TRUE,overwrite = overwrite)
}




writeUtf8 = function(x, file, bom=F) {
  con <- file(file, "wb")
  if(bom) writeBin(BOM, con, endian="little")
  writeBin(charToRaw(x), con, endian="little")
  close(con)
}
