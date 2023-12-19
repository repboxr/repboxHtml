# Create HTML pages that show do files on the right
# and the article tables on the left

example = function() {
  library(repboxHtml)
  project_dir = "~/repbox/projects_reg/aejapp_3_2_2"
  html = html_do_and_tab(project_dir)
  html.dir = file.path(project_dir,"reports")
  repbox_save_html(repbox_html_page(html), "do_tab.html", html.dir)

  rstudioapi::filesPaneNavigate(html.dir)
  rstudioapi::filesPaneNavigate("~/repbox/repboxHtml/R")
}

html_do_and_tab = function(project_dir, parcels=NULL,opts = repbox_html_opts(add_mapping=TRUE)) {
  do_html = html_all_do(project_dir, parcels, opts)
  tab_html = html_art_tabs(project_dir, parcels, opts)

  ui = fluidPage(
    div(class="row",style="height: 100vh;",
        div(id="do-col-div", class="col-sm-7", style="overflow-y: scroll; height:100%; padding: 5px",HTML(do_html)),
        div(id="tabs-col-div",class="col-sm-5", style="overflow-y: scroll; height:100%; padding: 5px", HTML(tab_html))
    ),
    #tags$script(src="do_and_tabs.js"),
    #tags$script(src="link_menu.js"),
    #HTML(num.search.html),
    #tags$script(src="number_marker.js")
  )
  html = as.character(ui) %>% merge.lines()
  html
}
