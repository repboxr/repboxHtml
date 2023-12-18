example = function() {
  library(repboxHtml)
  project.dir = "~/repbox/projects_reg/aejapp_3_4_9"
  project.dir = "~/repbox/projects_reg/aejapp_3_2_2"
  #art_update_project(project.dir, overwrite=FALSE)
  repbox_project_html(project.dir, opts=repbox_html_opts(log_max_char = 1e7))

  html.dir = file.path(project.dir,"reports")
  rstudioapi::filesPaneNavigate(html.dir)
  rstudioapi::filesPaneNavigate("~/repbox/repboxHtml/R")
}

repbox_project_html = function(project.dir, opts = repbox_html_opts(), parcels=NULL) {
  #parcels = html_make_parcels(project.dir)
  html = html_do_and_tab(project.dir,parcels = parcels, opts=opts)
  html.dir = file.path(project.dir,"reports")
  repbox_save_html(repbox_html_page(html), "do_tab.html", html.dir)
  invisible(parcels)
}