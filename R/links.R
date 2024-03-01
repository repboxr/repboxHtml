
do.line.link = function(script_num, orgline=NULL, prefix=getOption("repbox.url.prefix"), link_with_tabs=TRUE) {
  restore.point("do.line.link")
  if (link_with_tabs) {
    res = paste0(prefix, "do_and_tabs.html?do=",script_num)
  } else {
    res = paste0(prefix, "do.html?do=",script_num)
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

