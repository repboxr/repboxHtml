# Report of generated output in do files:
# currently Latex tables and graphs

project.output.report.html = function(project_dir,www.dir=file.path(project_dir, "repbox/www"), su = readRDS.or.null(paste0(project_dir,"/repbox/stata/repbox_results.Rds")), standalone=FALSE) {
  restore.point("project.output.report.html")

  if (is.null(su)) return(NULL)

  run.df  = su$run.df %>%
    filter(out.ext != "")

  run.df = run.df %>%
    mutate(
      title = paste0("<h2><a href='",do.line.link(donum,orgline),"'>", run.df$donum,".do line ", run.df$orgline,"</a></h2>")
    )

  img.dir = paste0(project_dir, "/repbox/www/images")
  run.df$img.src = ""
  rows = which(run.df$out.img.file != "")
  if (length(rows)>0) {
    if (standalone==FALSE) {
      run.df$img.src[rows] = repbox.link(paste0("images/",run.df$out.img.file[rows]))
    } else {
      run.df$img.src[rows] = lapply(rows, function(row) {
        knitr::image_uri(paste0(img.dir,"/", run.df$out.img.file[row]))
      }) %>% unlist()
    }
  }
  run.df = run.df %>%
    mutate(
      img.tag = ifelse(out.img.file =="","",paste0(
        "\n<img src='",img.src,"' style='max-width: 100%'>\n"
      )),
      hidden.txt = ifelse(out.txt == "","",
        paste0("<div class='hidden-text'>",out.txt,"</div>")
      ),
      content = ifelse(img.tag == "", paste0("<pre>",out.txt,"</pre>"), paste0(hidden.txt,img.tag))
    )

  style = "
<style>
.hidden-text {
opacity: 0;
width: 100%;
height: 10;
font-size: 1;
font-color: white;
}
#main {
  font-family: Helvetica, sans-serif;
  padding-left: 3px;
}
</style>
  "

  html = paste0(style,"<div id='main'><h1>Latex Tables and Figures Generated in Do Files (", basename(project_dir),")</h1>",
    "This page shows the latex tables and graphs generated in the project's do files. Some graphs may be missing if the corresponding command is not yet part of our list of commands. Note that you can search in your web browser for numbers in Latex tables even though the tables are shown as images. Above each table the latex source code is secretely shown with font size 1 in white color.</p>",
    paste0(run.df$title, "\n", run.df$content, collapse ="\n"),
    "</div>"
  )
  file = paste0(www.dir,"/tables_and_graphs.html")
  writeLines(html, file)

  return(invisible(html))
}

