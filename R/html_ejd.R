example = function() {
  library(repboxMain)
  project_dir = "~/repbox/projects_ejd/aejpol_12_4_10"
  project_dir = "~/repbox/projects_ejd/aejmic_14_4_17"
  project_dir = "~/repbox/projects_ejd/pandp_108_1_30"
  repbox.ejd.html(project_dir)
  rstudioapi::filesPaneNavigate(paste0(project_dir,"/repbox"))


  repbox.make.www(project_dir, just.overview=!TRUE, overwrite.shared = TRUE,for.rstudio = TRUE)

  project_dir = "~/repbox/projects_ejd/aejpol_12_4_10"
  copy.ejd.www(project_dir)
  project_dir = "~/repbox/projects_ejd/aejmic_13_2_11"
  copy.ejd.www(project_dir)


  # This only works in 8333 container. Otherwise web is not linked!
  copy.all.ejd.www()
  rstudioapi::filesPaneNavigate(paste0(project_dir,"/repbox"))
  rstudioapi::filesPaneNavigate("~/repbox/repboxMain/R")
}

make.ejd.www.index.page = function(info.file = "~/repbox/repbox_ejd_infos.Rds", www.dir = "~/web/repbox", project.dirs = NULL) {
  restore.point("make.ejd.www.index.page")

  info.df = readRDS(info.file)
  www.projects = list.dirs(www.dir, full.names=FALSE, recursive = TRUE) %>% str.between("/","/")

  if (!is.null(project.dirs)) {
    projects = basename(project.dirs)
    www.projects = intersect(www.projects, projects)
  }

  info.df = info.df %>%
    filter( id %in% www.projects) %>%
    filter( is.true(share.runs.no.error >0)) %>%
    mutate(has.all.data = is.true(data.existing > 0 & data.missing==0)) %>%
    arrange(desc(share.runs.no.error), desc(has.all.data)) %>%
    mutate(html = paste0(
      '<p .ejd_link>',1:n(),". ",round(share.runs.no.error*100),'%, ', data.missing, ' missing data, ',round(size.mb,2)," MB, ", runs,' runs, ', runtime, ' seconds ',
      '<a href="',journ,'/',id,'" target="_blank">',title,' (',journ,",",year,')</a>',
      '</p>'
    ))
  style = "<style> .ejd_link {font-family: Helvetia; padding: 5px;}</style>"


  html = paste0("<body>",style,"\n<ul class='ejd_link'>",
    "<h4>Repbox-EJD Webpages</h4>",
    paste0(info.df$html, collapse="\n"),
    "</ul></body>"
  )
  writeLines(html, file.path(www.dir,"index.html"))
}

copy.all.ejd.www = function(project.dirs, ejd.repbox.dir = "~/web/repbox", overwrite=TRUE) {
  restore.point("copy.all.ejd.www")
  project.dirs = get.ejd.projects(project.dirs)
  res = lapply(seq_along(project.dirs), function(i) {
    project_dir = project.dirs[i]
    cat("\n",i,project_dir)
    copy.ejd.www(project_dir, ejd.repbox.dir, overwrite=overwrite)
  })

}


copy.ejd.www = function(project_dir, ejd.repbox.dir = "~/web/repbox", overwrite=TRUE) {
  restore.point("copy.ejd.www")
  source.dir = file.path(project_dir, "repbox","www_ejd")
  if (!file.exists(source.dir)) return()
  art = readRDS(file.path(project_dir,"meta","ejd_art.Rds"))

  dest.dir = file.path(ejd.repbox.dir,art$journ,art$id)
  if (dir.exists(dest.dir)) {
    if (!overwrite) return()
    remove.dir(dest.dir,must.contain = "/web/repbox")
  }
  try(dir.create(dest.dir,recursive = TRUE,showWarnings = FALSE))
  copy.dir(source.dir, dest.dir)
}


make.all.ejd.html = function(project.dirs=NULL, overwrite=FALSE) {
  project.dirs = get.ejd.projects(project.dirs)
  projects = ""

  project_dir = project.dirs[1]
  project.dirs = project.dirs[114:258]
  for (project_dir in project.dirs) {
    exists = dir.exists(file.path(project_dir,"repbox","www_ejd"))
    if (!exists | overwrite) {
      cat(paste0("\n\nCreate ejd html for ", project_dir))
      try(repbox.ejd.html(project_dir))
      projects= c(projects, basename(project_dir))
    } else {
      exists = file.exists(file.path(project_dir,"repbox","www_ejd","repbox.css"))
      if (!exists) {
        res.dir = system.file("www",package="repboxMain")
        copy.dir(file.path(res.dir,"shared"), file.path(project_dir,"repbox","www_ejd","shared") )
        file.copy(file.path(res.dir,"do_bottom.js"), file.path(project_dir,"repbox","www_ejd"))
        file.copy(file.path(res.dir,"repbox.css"), file.path(project_dir,"repbox","www_ejd"))

        projects= c(projects, basename(project_dir))

      }
    }

  }
  projects
}

repbox.ejd.html = function(project_dir,  su = readRDS.or.null(paste0(project_dir,"/repbox/stata/repbox_results.Rds"))) {
  restore.point("repbox.ejd.html")

  ejd.art.file = file.path(project_dir,"meta","ejd_art.Rds")
  if (!file.exists(ejd.art.file)) {
    cat(paste0("\nCannot make EJD HTML for ", project_dir, " since ejd_art.Rds does not exist. Probably not a valid EJD project.\n"))
    return(invisible())
  }
  art = readRDS(ejd.art.file)
  art$Journ = toupper(art$journ)
  if (is.null(art$authors)) art$authors = ""
  art$authors = stringi::stri_replace_last_fixed(art$authors,", "," and " )
  art = as.list(art)

  prefix = paste0("")
  options(repbox.url.prefix = prefix)
  www.dir = paste0(project_dir,"/repbox/www")

  head = repbox.www.head("")

  project = basename(project_dir)
  time = paste0(Sys.time(), " (UTC)")



  report.frag = readLines(system.file("fragments/ejd.html", package="repboxMain")) %>% merge.lines()

  res = report.do.html(project_dir, su, ma=NULL,link.with.tabs = FALSE, return.do.df = TRUE)
  do.summary.html = res$html
  do.info = res$do.df
  if (NROW(do.info)>0) {
    sum = do.info[1,]
    noerr_share_num = ifelse(isTRUE(sum$runs > 0),(sum$runs.with.data-sum$runs.err.with.data) / sum$runs, NA)
    noerr_share = ifelse(is.na(noerr_share_num),"---",paste0(round(100*noerr_share_num),"%"))
  } else {
    noerr_share = "-- No do files found --"
  }


  data.info = datasets.info.html(project_dir)
  data.sets.html = data.info$html

  if (any(is.true(su$dotab$timeout))) {
    timeout_info = "The analyis in a do file took so long that we triggered a timeout and exited the analysis. A timeout can also cause errors in other do files. "
    has_timeout = "Yes"
  } else {
    timeout_info = ""
    has_timeout = "No"
  }

  if (!is.empty(art$readme_file)) {
    readme_a = paste0('<a target = "_blank" href="',art$readme_file,'">Readme</a>')
    readme_li = paste0('<li><a target = "_blank" href="',art$readme_file,'">Readme</a></li>')
  } else {
    readme_a = "Readme in the supplement"
    readme_li = ""
  }

  info_txt = ""
  info_txt  = paste0(info_txt, timeout_info)
  if ( isTRUE((round(noerr_share_num*100) < 100 & noerr_share_num > 0.8) | (NROW(data.info$missing.df)==0 & NROW(data.info$exist.df)>0))) {
    info_txt = paste0(info_txt,"Note that even if a human can reproduce the analysis, often less than 100% of commands run without error in the automatic reproduction. ")
  }
  if (NROW(data.info$missing.df)>0 & NROW(data.info$exist.df)>0) {
    info_txt = paste0(info_txt,"Sometimes the main analysis can be reproduced even if some data sets are missing, e.g. the missing data sets could be raw data files that are not directly needed. On the other hand, existence of a data set file does not imply that the data is indeed there: the data set could be empty or contain simulated data if the original data is confidential or proprietary. The ", readme_a," and the tables below will provide more information.  ")
  }


  num_missing_data_org = sum(!data.info$missing.df$intermediate)
  num_missing_data_all = NROW(data.info$missing.df$intermediate)
  if (num_missing_data_all != num_missing_data_org) {
    num_missing_data = paste0(num_missing_data_org, " - ",num_missing_data_all,"*" )
    info_txt = paste0(info_txt, paste0("<br>*We detected ", num_missing_data_all," missing data sets if we include missing intermediate data sets that were supposed to be generated by Stata code, otherwise just detected ", num_missing_data_org, " missing data sets."))
  } else {
    num_missing_data = num_missing_data_all
  }

  vals = c(art, list(info_txt = info_txt, time=time, re_sum = do.summary.html, data_sets_html=data.sets.html, project=project, has_timeout = has_timeout, timeout_info = timeout_info, readme_li = readme_li, noerr_share=noerr_share, num_misssing_data=num_missing_data, num_existing_data = NROW(data.info$exist.df)))
  content.html = glue::glue(report.frag, .envir=vals)


  # overview
  body = as.character(fluidPage(HTML(content.html)))
  html = paste0("<html><title>Report for ",basename(project_dir),"</title>\n",head,"<style> p {max-width: 60em;}</style><body>",body, "</body></html>")
  if (!dir.exists(paste0(project_dir,"/repbox/www_ejd")))
      dir.create(paste0(project_dir,"/repbox/www_ejd"))


  writeLines(html,paste0(project_dir,"/repbox/www_ejd/index.html"))


  res.dir = system.file("www",package="repboxMain")

  copy.dir(file.path(res.dir,"shared"), file.path(project_dir,"repbox","www_ejd","shared") )
  try(copy.dir(file.path(project_dir,"repbox","www","images"), file.path(project_dir,"repbox","www_ejd","images")))
  #file.copy(file.path(project_dir,"repbox","www","do.html"), file.path(project_dir,"repbox","www_ejd"))

  file.copy(file.path(res.dir,"do_bottom.js"), file.path(project_dir,"repbox","www_ejd"))
  file.copy(file.path(res.dir,"repbox.css"), file.path(project_dir,"repbox","www_ejd"))

  do.tabs.html = HTML(project.do.tabs.html(project_dir,ma = NULL, su=su,add.match.info=FALSE,title.html = paste0('<h3>Do files for "',art$title,'"</h3>')))

  ui = fluidPage(
    do.tabs.html,
    tags$script(src="do_bottom.js")
  )
  body = as.character(ui) %>% merge.lines()
  html = paste0("<html><title>Do files ",basename(project_dir),"</title>\n",head,"<body>",body, "</body></html>")
  writeLines(html, file.path(project_dir,"repbox","www_ejd","do.html"))

  invisible(html)
}

