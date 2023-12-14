
do.line.link = function(donum, orgline=NULL, prefix=getOption("repbox.url.prefix"), link.with.tabs=TRUE) {
  restore.point("do.line.link")
  if (link.with.tabs) {
    res = paste0(prefix, "do_and_tabs.html?do=",donum)
  } else {
    res = paste0(prefix, "do.html?do=",donum)
  }
  if (!is.null(orgline)) {
    rows = !is.na(orgline)
    res[rows] = paste0(res[rows], "&L=",orgline[rows])
  }
  res
}

repbox.link = function(link, prefix=getOption("repbox.url.prefix")) {
  paste0(prefix, link)
}

arttab.link = function(tabid, col=NULL, prefix=getOption("repbox.url.prefix")) {
  restore.point("art.link")
  res = paste0(prefix, "do_and_tabs.html?tabid=",tabid)
  if (!is.null(col)) {
    rows = !is.na(col)
    res[rows] = paste0(res[rows], "&col=",col)
  }
  res
}

add.donum = function(df, dotab) {
  rows = match(df$donum, dotab$donum)
  df$donum = dotab$donum[rows]
  df

}

