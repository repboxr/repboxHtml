example = function() {
  library(repboxHtml)
  project_dir = "~/repbox/projects_reg/aejapp_3_4_9"
  project_dir = "~/repbox/projects_reg/aejapp_3_2_2"
  #art_update_project(project_dir, overwrite=FALSE)
  repbox_project_html(project_dir, opts=repbox_html_opts(log_max_char = 1e7))

  html.dir = file.path(project_dir,"reports")
  rstudioapi::filesPaneNavigate(html.dir)
  rstudioapi::filesPaneNavigate("~/repbox/repboxHtml/R")
}

repbox_project_html = function(project_dir, lang = c("stata"), opts = repbox_html_opts(), parcels=NULL) {
  restore.point("repbox_project_html")
  #parcels = html_make_parcels(project_dir)
  if (opts$copy_shared_dir) {
    repbox_copy_shared_www(project_dir,overwrite = TRUE)
  }

  if (opts$copy_image_dir) {
    img.dir = paste0(project_dir, "/repbox/www/images")
    if (dir.exists(img.dir)) {
      copy.dir(img.dir, paste0(project_dir,"/reports/images"))
    }
  }



  if ("ejd" %in% opts$make_what) {
    ejd_opts = opts
    ejd_opts$add_mapping = FALSE
    repbox_ejd_report_html(project_dir, parcels=parcels, opts=ejd_opts)
  }

  if ("general" %in% opts$make_what) {
    if ("stata" %in% lang) {
      html = html_do_and_tab(project_dir,parcels = parcels, opts=opts)
      html.dir = file.path(project_dir,"reports")
      repbox_save_html(repbox_html_page(html), "do_tab.html", html.dir)
    }
    if ("r" %in% lang) {
      html = html_all_r(project_dir)
      html.dir = file.path(project_dir,"reports")
      repbox_save_html(html %>% repbox_add_html_header(), "r_code.html", html.dir)
    }

  }

  invisible(parcels)
}



repbox_copy_shared_www = function(project_dir, www_dir=file.path(project_dir,"reports"), overwrite=FALSE) {
  # Copy shared
  if (!dir.exists(www_dir)) dir.create(www_dir)
  pkg.www = system.file("www", package="repboxHtml")
  files = list.files(pkg.www,include.dirs = TRUE,recursive = FALSE, full.names=TRUE)
  file.copy(files, www_dir,recursive = TRUE,overwrite = overwrite)
}


