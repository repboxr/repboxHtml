# Create a summary over all supplements
example.summary.html = function() {
  library(repbox)
  dirs = list.dirs("~/statabox/supp",recursive = FALSE,full.names = TRUE)
  #dirs = dirs[1:110]

  agg.df = get.project.summaries(dirs=dirs, force=TRUE)
  html = summary.html(agg.df)
  writeLines(html, "~/statabox/summary.html")
}

summary.html = function(agg.df) {
  restore.point("summary.html")
  colnames(agg.df)
  url.prefix = paste0("/files/statabox/supp/")
  options(repbox.url.prefix = url.prefix)

  url.prefix = getOption("repbox.url.prefix")
  agg.df = agg.df %>%
    mutate(
      project.link = paste0(url.prefix, basename(project_dir),"/repbox/www/overview.html"),
      project.a = paste0('<a href="', project.link,'">',project,"<a>")
    )

  agg.df = agg.df %>%
    arrange(desc(tabs>0),desc(runs>0),desc(share.full.match.tabs),desc(tabs))

  tab.html = summary.html.table(agg.df)

  head = summary.www.head()


  # overview
  body = as.character(fluidPage(
    h4("Analyzed Repbox Projects"),
    HTML(tab.html)
  ))
  html = paste0("<html><title>Repbox Projects</title>\n",head,"<body>",body, "</body></html>")
  html

}

summary.html.table = function(df, id="sum-table") {
  as.perc = function(num, denom) {
    perc = paste0(round(num/denom * 100),"%")
    perc[is.na(num) | is.na(denom)] = ""
    perc[denom==0] = ""
    perc
  }

  head.html = '
<tr>
  <th>Project</th>
  <th>Do</th>
  <th>Tables /<br>Unknown</th>
  <th>Perfect<br>matches</th>
  <th>Match shares</th>
  <th>Commands</th>
  <th>with data</th>
  <th>with error</th>
  <th>Other</th>
</tr>
'
  df = df %>% mutate(
    other = paste0("", ifelse(is.true(timeout),"timeout",""), ifelse(is.true(parse.err),"parsing error","")),
    row.html = paste0(
    '<tr>',
    '<td>',project.a,'</td>',
    '<td style="text-align: right">',do.files,'</td>',
    '<td style="text-align: right">',paste0(tabs, " / ", unknown.tabs),'</td>',
    '<td>',round(share.full.match.tabs*100),'%</td>',
    '<td>',substring(tab.match.shares,1,30),'...</td>',
    '<td style="text-align: right">',na.val(runs,0),'</td>',
    '<td style="text-align: right">',as.perc(runs.with.data, runs),'</td>',
    '<td style="text-align: right">',as.perc(runs.err.with.data, runs.with.data),'</td>',
    '<td>',other,'</td>',
    '</tr>')
  )

  tbody = paste0(df$row.html, collapse="\n")

  tab.html = paste0('\n<table class="table-mini table table-striped">
  <thead>',head.html, "</thead><tbody>\n",tbody,"</tbody></table>")
  tab.html
}

summary.www.head = function() {
  paste0('
<head>
<meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>
<script src="shared/jquery.min.js"></script>
<link href="shared/bootstrap.min.css" rel="stylesheet"/>
<link href="shared/bootstrap-accessibility.min.css" rel="stylesheet" />
<script src="shared/bootstrap.min.js"></script>
<script src="shared/bootstrap-accessibility.min.js"></script>
<link href="shared/repbox.css" rel="stylesheet"/>
</head>
  ')
}
