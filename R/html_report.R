example = function() {
  library(repbox)
  project.dir = "/home/rstudio/repbox/projects/testsupp"
  project.dir = "/home/rstudio/repbox/projects/aejapp_vol_6_issue_3_article_7"
  repbox.report.html(project.dir)
}

repbox.report.html = function(project.dir,  su = readRDS.or.null(paste0(project.dir,"/repbox/stata/repbox_results.Rds")), ma=readRDS.or.null(paste0(project.dir,"/repbox/matched_tabs.Rds")), show.figure=FALSE, show.unknown=TRUE
) {
  restore.point("repbox.report.html")

  prefix = paste0("")
  options(repbox.url.prefix = prefix)
  www.dir = paste0(project.dir,"/repbox/www_report")
  if (!dir.exists(www.dir))
    dir.create(www.dir)

  head = repbox.www.head()

  repbox.copy.shared.www(project.dir,www.dir = www.dir,  overwrite = TRUE)

  project = basename(project.dir)
  time = paste0(Sys.time(), " (UTC)")

  report.frag = readLines(system.file("fragments/report.html", package="repboxMain")) %>% merge.lines()

  do.err.frag = readLines(system.file("fragments/report_do_errors.html", package="repboxMain")) %>% merge.lines()

  do.tab.html = report.do.html(project.dir, su, ma)
  tab.tab.html = report.tab.html(project.dir,su, ma)

  dotab = su$dotab
  has.err = any(is.true(su$dotab$run.err | su$dotab$parse.err))

  if (has.err) {
    re_expl = do.err.frag
  } else {
    re_expl = "<p>Great, it looks as if your code runs without errors.</p>"
  }
  data.info = datasets.info.html(project.dir)
  data_sets_html = data.info$html

  content.html = glue::glue(report.frag, project=project, time=time, re_sum=do.tab.html, tab_sum=tab.tab.html, re_expl=re_expl, data_sets_html=data_sets_html)


  # overview
  body = as.character(fluidPage(HTML(content.html)))
  html = paste0("<html><title>Repbox Report for ",basename(project.dir),"</title>\n",head,"<style> p {max-width: 60em;}</style><body>",body, "</body></html>")
  writeLines(html,paste0(project.dir,"/repbox/www_report/report.html"))


  # do pages
  do.tabs.html = HTML(project.do.tabs.html(project.dir,ma = ma,su = su))

  # tab pages
  tab.tabs.html = HTML(project.tab.tabs.html(project.dir,ma = ma,su=su, show.figure = show.figure, show.unknown = show.unknown))

  # Combined page
  num.search.html = num.search.html(su, ma)

  ui = fluidPage(
    div(class="row",style="height: 100vh;",
        div(id="do-col-div", class="col-sm-7", style="overflow-y: scroll; height:100%; padding: 5px",do.tabs.html),
        div(id="tabs-col-div",class="col-sm-5", style="overflow-y: scroll; height:100%; padding: 5px", tab.tabs.html)
    ),
    tags$script(src="do_and_tabs.js"),
    tags$script(src="link_menu.js"),
    HTML(num.search.html),
    tags$script(src="number_marker.js")
  )
  body = as.character(ui) %>% merge.lines()
  html = paste0("<html><title>Tables and Do: ",basename(project.dir),"</title>\n",head,"<body>",body, "</body></html>")
  writeLines(html,paste0(www.dir,"/do_and_tabs.html"))

  img.dir = paste0(project.dir, "/repbox/www/images")
  if (dir.exists(img.dir)) {
    copy.dir(img.dir, paste0(project.dir,"/repbox/www_report/images"))
  }

  return(NULL)
}

report.do.html = function(project.dir, su, ma, link.with.tabs=TRUE, return.do.df = FALSE) {
  restore.point("report.do.html")

  sup.dir = file.path(project.dir,"mod")
  # Do files overview
  do.files = get.project.do.files(project.dir)

  do.files = do.files[!startsWith(basename(do.files),"repbox_")]
  do.df = tibble(file = do.files, doid = tools::file_path_sans_ext(basename(do.files)))

  h = ""
  if (NROW(do.df)==0) {
    h = paste0(h,"<p>The project has no do files.<p>")
    if (return.do.df) return(list(html=h, do.df=NULL))
    return(h)
  }

  dotab = su$dotab
  if (is.null(dotab)) {
    tab.html = repbox_html_table(id="do-overview-tab",
      transmute(do.df, Do=paste0(doid,".do"), Analyzed="NO")
    )
    h = paste0(h, tab.html)
    if (return.do.df) return(list(html=h, do.df=NULL))
    return(h)
  }



  do.df = left_join(select(do.df, doid),dotab, by="doid")
  do.df$analyzed = !is.na(do.df$dofile)
  do.df = arrange(do.df, desc(analyzed), desc(!is.na(runtime)))

  run.df = su$run.df

  runs.df = run.df %>%
    group_by(donum) %>%
    summarize(
      runs = n(),
      runs.with.data = sum(has.data),
      runs.no.data = sum(!has.data),
      runs.err = sum(runerr),
      runs.err.with.data = sum(has.data & runerr)
    )

  mdf = ma$mdf
  if (!is.null(mdf)) {
    mdf.has = mdf %>%
      group_by(donum, line, counter) %>%
      summarize(matched = TRUE)
  }


  do.df = do.df %>%
    left_join(runs.df, by="donum")

  if (!has.col(do.df,"is.included"))
    do.df$is.included = FALSE

  do.df = do.df %>% mutate(
      info = case_when(
        is.true(parse.err) & is.true(timeout) ~ "timeout and parsing error",
        is.true(parse.err) ~ "parsing error",
        is.true(timeout) ~ "timeout",
        TRUE ~ ""
      )) %>%
      mutate(
        info = paste0(case_when(
          !analyzed~"not analyzed",
          is.included~"called in other do",
          is.na(runtime) ~ "just parsed ",
          TRUE ~ ""
        ), info)
      )

  if (NROW(do.df)>1) {
    total = lapply(do.df, function(val) {
      if (is.numeric(val)) return(sum(val, na.rm=TRUE))
      return(NA)
    }) %>% as_tibble()
    total$analyzed = FALSE
    total$do.file = "TOTAL"
    total$info = ""
    do.df = bind_rows(total, do.df)
    do.df$is.total = c(TRUE, rep(FALSE,NROW(do.df)-1))
  } else {
    do.df$is.total = logical(NROW(do.df))
  }

  format.share = function(x) {
    x = ifelse(is.finite(x),
      paste0(round(100*x,1),"%"),
      "-"
    )
    x
  }

   head.html = '
<tr>
  <th style="border: 0; padding-bottom: 0;"></th>
  <th style="border: 0; padding-bottom: 0;">Runtime</th>
  <th colspan="3" style="border: 0; padding-bottom: 0;">All commands</th>
  <th style="border: 0; padding-bottom: 0;">Info</th>
</tr><tr>
  <th style="border: 0"></th>
  <th style="border: 0">(sec.)</th>

  <th style="border: 0">Runs</th>
  <th style="border: 0">No data</th>
  <th style="border: 0">Error</th>
  <th style="border: 0"></th>

</tr>
  '
  do.df = mutate(do.df, row.html = paste0(
    '<tr', ifelse(is.total,' class="total-row"',""),'>','<td>',ifelse(analyzed,
      paste0('<a href="',do.line.link(donum, link.with.tabs = link.with.tabs),'">',dofile,"</a>"),
      do.file),'</td>',
    '<td style="text-align: right;">',na.val(round(runtime),"-"),'</td>',

    '<td style="text-align: right;">',na.val(runs,"-"),'</td>',
    '<td style="text-align: right;">',na.val(runs.no.data,"-"),'</td>',
    '<td style="text-align: right;">',na.val(runs.err.with.data,"-"),'</td>',
    '<td>',info,'</td>',
    '</tr>'
  ))
  tbody = paste0(do.df$row.html, collapse="\n")

  tab.html = paste0('\n<table id="do-overview-tab" class="table-mini table table-striped">
  <thead>',head.html, "</thead><tbody>\n",tbody,"</tbody></table>")
  h = paste0(h, tab.html)
  if (return.do.df) return(list(html=h, do.df=do.df))
  return(h)

}

report.tab.html = function(project.dir,su=NULL, ma=NULL, show.reg = FALSE) {
  restore.point("tab.overview.html")
  return(tab.overview.html(project.dir,su=su, ma=ma, show.reg=show.reg))
}
