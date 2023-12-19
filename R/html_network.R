make.data.code.network.html = function(project_dir,www.dir, ma = readRDS.or.null(paste0(project_dir,"/repbox/matched_tabs.Rds")),su = readRDS.or.null(paste0(project_dir,"/repbox/stata/repbox_results.Rds")), do.max.runs=10000, line.max.runs=50) {
  restore.point("make.data.code.network.html")

  tab = su$tab
  dotab = su$dotab



  rdf = su$run.df
  if (NROW(rdf)==0) return(NULL)

  make.cmds = c("save","export")
  need.cmds = c("use","u","merge","import", "insheet")
  cmds = c(make.cmds, need.cmds)
  rdf = rdf %>%
    filter(cmd %in% cmds) %>%
    mutate(dta.base=basename(foundfile)) %>%
    mutate(
      make = cmd %in% make.cmds,
      need= cmd %in% need.cmds,
      mode = "post"
    )

  mn = bind_rows(dotab$make.need.df)

  if (NCOL(mn)==0) return(NULL)
  mn = mn %>%
    filter(!has.substr(dta.base, "`"),
           !has.substr(dta.base, "$")) %>%
    mutate(mode="pre")

  doinfo = bind_rows(rdf, mn) %>%
    group_by(dta.base, donum, make, need) %>%
    summarize(
      post = any(mode=="post"),
      pre = any(mode=="pre")
    ) %>%
    ungroup()



  dafi = su$datatab

  mn = left_join(doinfo, dafi, by=c("dta.base"="database")) %>%
    mutate(referenced=TRUE)
  library(visNetwork)

  # Look at existing files in the project
  # Some might be there but never called in a
  # Stata do file
  sup.dir = file.path(project_dir, "mod")
  exist.file = list.project.data.files(sup.dir)
  exist.base = basename(exist.file)
  new = which(!exist.base %in% mn$dta.base)
  if (length(new)>0) {
    size = file.size(exist.file[new])
    mn = bind_rows(mn, tibble(dta.base=exist.base[new], datamb=size / 1e6, referenced=FALSE, make=FALSE, need=FALSE))
  }


  data.nodes = mn %>%
    select(label = dta.base, size=datamb, referenced) %>%
    group_by(label, size, referenced) %>%
    summarize(
      exists=any(!is.na(size))
    ) %>%
    ungroup() %>%
    mutate(
      id = 1:n(),
      size = sqrt(size),
      value = ifelse(is.na(size), 1,sqrt(size)),
      #shape = "database",
      shape = "dot",
      title = ifelse(exists,paste0(round(size,2),"MB"), "Dataset not found"),
      color = case_when(
        exists & referenced ~ "#aabbff",
        !referenced ~ "#dedede",
        !exists ~ "#ffbbbb"
      ),
      shadow= FALSE
    )


  file.nodes = dotab %>%
    select(label = dofile,runtime,donum) %>%
    mutate(
      id = (1:n())+NROW(data.nodes),
      size = ifelse(is.na(runtime), 1,sqrt(runtime*1000)),
      shape = "box",
      title = label,
      color = "#ffffcc",
      shadow= FALSE
    )

  nodes = bind_rows(data.nodes, file.nodes)

  make.edges = filter(mn, make) %>%
    mutate(
      from = match(donum, file.nodes$donum)+NROW(data.nodes),
      to = match(dta.base, data.nodes$label),
      arrows = "to"
    )
  need.edges = filter(mn, need) %>%
    mutate(
      from = match(dta.base, data.nodes$label),
      to = match(donum, file.nodes$donum)+NROW(data.nodes),
      arrows = "to"
    )

  edges = bind_rows(make.edges, need.edges) %>%
    select(from, to, arrows)

  project = basename(project_dir)
  title = paste0("Data-Code Network for ", project)
  vis = visNetwork(nodes, edges,
    main = list(text=title,style="font-family: Helvetia, sans-serif;font-weight:bold;font-size:20px; text-align: center;"),
    footer=list(text = "Notes: Yellow boxes are do files and circles are data sets (red = not existing, grey = not used, blue = exists and used). An arrow from a data set to a box means the file loads that data set. In the other direction, the do file creates or modifies that data set and saves the file.", style="font-family: Helvetia, sans-serif; text-align: left;size:12px")
  )
  vis
  file = paste0(www.dir,"/data_code_network.html")
  visNetwork::visSave(vis,file)
}

