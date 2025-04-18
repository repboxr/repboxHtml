example = function() {
  library(repboxHtml)
  project_dir = "~/repbox/projects_reg/aejapp_3_2_2"
  html = html_all_do(project_dir)
  html.dir = file.path(project_dir,"reports")
  repbox_save_html(html %>% repbox_add_html_header(), "do.html", html.dir)


  rstudioapi::filesPaneNavigate(html.dir)
  rstudioapi::filesPaneNavigate("~/repbox/repboxHtml/R")
}

html_just_do = function(project_dir, parcels=NULL,opts = repbox_html_opts(add_mapping=FALSE)) {
  do_html = html_all_do(project_dir, parcels, opts)
  return(do_html)

  ui = fluidPage(
    do_html
  )
  html = as.character(ui) %>% merge.lines()
  html
}

html_all_do = function(project_dir, parcels=list(), opts = repbox_html_opts(), return_fragments=FALSE, just_script_num=NULL,  just_runids=NULL) {
  restore.point("html_all_do")

  parcels = repdb_load_parcels(project_dir, c("stata_source","stata_run_cmd","stata_run_log","stata_cmd", if (opts$add_debug_info) "reg_core"), parcels = parcels)

  # script_source is script_file info + source text
  script_df = parcels$stata_source$script_source

  if (!is.null(just_script_num)) {
    script_df = script_df[script_df$script_num %in% just_script_num,]
  }

  if (NROW(script_df)==0) {
    return("<p>The supplement has no do files.</p>")
  }

  run_df = load_full_run_df(project_dir, parcels)
  if (is.null(run_df)) {
    return("<p>No do files from the supplement were run.</p>")
  }

  cmd_df = load_full_cmd_df(project_dir, parcels)

  if (!is.null(just_runids)) {
    run_df = filter(run_df, runid %in% just_runids)
    cmd_df = semi_join(cmd_df, run_df, by=c("file_path", "line"))
    script_df = semi_join(script_df, cmd_df, by=c("file_path"))
    show_just_runid_lines = TRUE
  } else {
    show_just_runid_lines = FALSE
  }

  # We may ignore some runs
  # if do file or a line has too many runs
  # This could blow up websites massively otherwise
  run_df = adapt_too_big_run_df(run_df,opts)

  log_info_html = log_info_html(run_df,opts=opts, project_dir=project_dir)

  project = basename(project_dir)

  outer.tabids = paste0("dotab_", script_df$script_num)

  contents = lapply(seq_len(NROW(script_df)), function(i) {
    restore.point("jnsdjsjd")
    script_num = script_df$script_num[[i]]
    otabid = outer.tabids[i]
    do_txt = script_df$text[i]
    do.html = do_code_html(project_dir, script_num=script_num,file_path = script_df$file_path[[i]], do_txt=do_txt,   run_df=run_df, cmd_df = cmd_df, log_info_html=log_info_html, opts=opts, parcels=parcels, show_just_runid_lines=show_just_runid_lines)
    return(do.html)
  })

  if (return_fragments) {
    return(contents)
  }

  html = repboxTabSetPanel(type="pills",id="dotabs",tabnames = paste0(basename(script_df$file_path)),tabids = outer.tabids,contents = contents)

  return(html)
}


# If we just want fragments of some do code
html_do_fragment = function(project_dir, parcels=list(), opts=repbox_html_opts(), just_script_num=NULL, just_runids=NULL) {
  html_all_do(project_dir=project_dir, parcels=parcels, opts=opts, return_fragments = TRUE, just_script_num=just_script_num, just_runids=just_runids)
}


# In this version I try not to directly add matching information.
# Instead general class info shall facilitate
# color-coded mapping
do_code_html = function(project_dir, script_num, file_path, do_txt, log_info_html, run_df, cmd_df, opts, parcels, show_just_runid_lines = FALSE) {
  restore.point("do_code_html")

  do_txt = sep.lines(do_txt)

  cmd_df = cmd_df[cmd_df$script_num == script_num,]

  run_df = run_df[run_df$script_num == script_num,]

  #if (script_num == "1 Tables") stop()

  log_info_html = log_info_html[log_info_html$script_num == script_num,]

  ldf = tibble(orgline = seq_along(do_txt), txt = htmlEscape(do_txt), comment="", title="", class="", infobtn="", infobox="")

  if (show_just_runid_lines) {
    ldf = filter(ldf, orgline %in% run_df$orgline)
  }

  ldf = left_join(ldf, select(cmd_df, orgline, line, is_reg, cmd), by=c("orgline"))

  # For delimit ; the orglines often don't match
  # the actual command line but a comment or empty line further above
  # Try to repair it heuristically

  # Update: No longer necessary. Problem is tackled in repboxStata parser
  #ldf = correct_delimit_semi_orglines(ldf)

  # Aggregate run error info on orgline level
  #re = run_df[run_df$script_num==script_num,]
  re = run_df

  rel = re %>%
    group_by(line) %>%
    summarize(
      cmdline = first(cmdline),
      runerr = any(is.true(errcode != 0)),
      errmsg = trimws(paste0(unique(trimws(errmsg)), collapse=" ")),
      missing_data = any(is.true(missing_data))
    ) %>%
    arrange(line) %>%
    mutate(
      firsterr = is.true(runerr & (seq_len(n())==1 | !lag(runerr)))
    )

  ldf = left_join(ldf, rel, by="line")




  # Set commands that were not run to error
  # This is usually the case due to an earlier error
  # in a loop
  #ldf$actual_orgline = ldf$orgline
  #ldf$orgline = ldf$first_orgline

  ldf$is.cmd = !ldf$cmd %in% c("}","end","program","if","else")
  ldf$not.run = ldf$is.cmd & is.na(ldf$runerr)

  rows = match(log_info_html$line, ldf$line)
  ldf$infobtn[rows] = log_info_html$log.info.btn
  ldf$infobox[rows] = log_info_html$log.info.div

  if (opts$add_debug_info) {
    reg_df = parcels$reg_core$reg %>%
      repdb_null_to_empty("reg")
    reg_check = parcels$reg_core$regcheck %>%
      repdb_null_to_empty("regcheck")
    run_df = re %>% left_join(select(reg_df, runid, step), by="runid") %>%
      left_join(select(reg_check, step, reg_did_run=did_run, reg_problem=problem, reg_deviation = deviation), by="step") %>%
      mutate(
        reg_problem_debug_txt = case_when(
          is.true(!reg_did_run) ~ paste0(" Regression base run failed: ", reg_problem,"."),
          TRUE ~ ""
        ),
        reg_deviation_debug_txt = ifelse(is.true(reg_deviation > 1e-12)," Base run results deviate from original replication.",""),
        debug_problem_txt = paste0(reg_problem_debug_txt, reg_deviation_debug_txt)
      ) %>%
      mutate(
        debug_problem_txt = paste0(debug_problem_txt, ifelse(missing_data," missing data", ""))
      )


  line_debug_txt = run_df %>%
    group_by(line) %>%
    summarize(
      debug_txt = ifelse(any(is_reg),
        paste0("runid: ",runid, ifelse(is.na(step), " ", paste0(" step: ", step)), debug_problem_txt, collapse="\n"),
        paste0("runid: ", paste0(runid, collapse=", "), " ", paste0(unique(debug_problem_txt), collapse=" "))
      ),
      has_reg_problem = any(!is.na(reg_problem) & reg_problem != "")
    )
    ldf = left_join(ldf, line_debug_txt, by="line")
    ldf$debug_title = paste0(' title="', ldf$debug_txt,'"')
  } else {
    ldf$debug_txt = ldf$debug_title = ""
    ldf$has_reg_problem = FALSE
  }


  # Commented out: We don't show error messages in code
  # Users are asked to open log
  if (opts$show_first_err_msg) {
    rows = which(ldf$firsterr)
    ldf$comment[rows] = paste0(ldf$comment[rows],'\n<code class="error-comment">// Error: ',shorten.str(ldf$errmsg[rows],200),'</code>\n')
  }

  ldf$class = case_when(
    is.true(ldf$runerr) ~ "err-line",
    is.true(ldf$not.run) ~ "norun-line",
    is.true(ldf$missing_data) ~ "mida-line",
    TRUE ~ "noerr-line"
  )

  rows = is.true(ldf$is_reg)
  ldf$class[rows] = paste0(ldf$class[rows]," ","reg-cmd")
  rows = is.true(ldf$has_reg_problem)
  ldf$class[rows] = paste0(ldf$class[rows]," ","reg-prob")


  if (opts$unfold_do_log_btns) {
    unfold.all.code = paste0('<tr><td colspan="1"><button class="toogle-all-results btn btn-xs" title="Show or hide all results" style="color: #880000" onclick="$(\'#dotab_',script_num,' .collapse\').collapse(\'toggle\');">&#x25BC;</button></td></tr>')
  } else {
    unfold.all.code = ""
  }

  if (opts$add_do_mapping)  {
    new_parcels = c("map_cell","stata_run_cmd", "art_tab")
    parcels = repdb_load_parcels(project_dir, new_parcels, parcels)
    do.map = !is.null(parcels$map_cell$map_cell)
  } else {
    do.map = FALSE
  }

  if (do.map)  {
    # We will add lines above commands that map to particular
    # tables
    run_df = parcels$stata_run_cmd$stata_run_cmd
    run_df = run_df[run_df$file_path == file_path,]

    tab_df = parcels$art_tab$art_tab

    map_df = parcels$map_cell$map_cell %>%
      filter(match_type > 0) %>%
      left_join(select(run_df, runid, line), by="runid") %>%
      filter(!is.na(line)) %>%
      left_join(select(tab_df, tabid, tabpos), by="tabid")

    agg_map = map_df %>%
      group_by(tabpos,tabid, line) %>%
      summarize(
        num_cells = n_distinct(cellid),
        link_txt = paste0('Table ', first(tabid),' (', num_cells,' cells)')
        #link_txt = paste0('<span data="to_tab-', first(tabpos),'">Table ', first(tabid),' (', num_cells," cells)</span>")
      ) %>%
      arrange(line, desc(num_cells)) %>%
      group_by(line) %>%
      summarize(
        link_html = paste0("<span class='code2tab'>Used in ",paste0(link_txt, collapse=", "),"\n</span>")
      ) %>%
      ungroup()

    ldf = left_join(ldf, agg_map, by="line") %>%
      mutate(link_html = na.val(link_html,""))

  } else {
    ldf$link_html = ""
  }

  ldf = ldf %>% mutate(
    html.txt = paste0(
      '<tr><td id="B',orgline,'___',script_num,'">',infobtn,'</td><td class="code-line-td">',orgline,'</td><td><pre class = "do-pre">',comment,
      link_html,
      #'<code id="cb_',orgline,'_',script_num,'" class="colorbar"></code>',
      '<code id="L',orgline,'___',script_num,'" class="',class,'"', debug_title, '>',txt,'</code></pre>',
      infobox,'</td></tr>')
    )
  inner = paste0(ldf$html.txt, collapse="\n")
  code.html = paste0(unfold.all.code,"<table class='code-tab'>\n",inner,"</table>")

  return(code.html)
}


adapt_too_big_run_df = function(run_df, opts) {
  restore.point("adapt_too_big_run_df")
  run_df = run_df %>%
    group_by(script_num, line) %>%
    mutate(line.run.count = 1:n()) %>%
    ungroup() %>%
    filter(line.run.count <= opts$line_max_runs) %>%
    group_by(script_num) %>%
    mutate(do.run.count = 1:n()) %>%
    ungroup() %>%
    filter(do.run.count <= opts$do_max_runs)

  run_df
}

log_info_html = function(run_df, project_dir,opts) {
  restore.point("log_info_html")
  i = 1

  # Update: Some run_df entries may be incomplete
  # and not have an orgline reference. Remove
  # to prevent later errors
  run_df = run_df[!is.na(run_df$orgline),]

  n = NROW(run_df)
  if (n==0) return(list())

  if (any(is.na(run_df$script_num))) {
    run_df = filter(run_df,!is.na(script_num))
    repbox_problem(type="na_run_df",msg = "run_df has rows in which the original scriptfile (script_num, orgline) are not given. No log_info_html will be generated for those calls.", fail_action = "msg", project_dir=project_dir)

  }

  # include images, possibly img_inline with bas64 encoding
  img.dir = paste0(project_dir, "/repbox/www/images")
  run_df$img.src = ""
  rows = which(run_df$out_img_file != "")
  if (length(rows)>0) {
    if (opts$img_inline==FALSE) {
      run_df$img.src[rows] = repbox.link(paste0("images/",run_df$out_img_file[rows]))
    } else {
      run_df$img.src[rows] = lapply(rows, function(row) {
        knitr::image_uri(paste0(img.dir,"/", run_df$out_img_file[row]))
      }) %>% unlist()
    }
  }

  #repbox.link(paste0("images/",run_df$out_img_file))
  log_info_html = run_df %>%
    mutate(
      logtxt = ifelse(endsWith(trimws(cmdline),"{"), "", logtxt),
      len.logtxt = nchar(logtxt)
    ) %>%
    mutate(
      logtxt = ifelse(is.true(len.logtxt > opts$log_max_char), paste0(substring(logtxt,1, opts$log_max_char), "\n... further output omitted ..."), logtxt)
    ) %>%
    mutate(
      img.tag = ifelse(out_img_file =="","",paste0(
"\n<img src='",img.src,"' style='max-width: 100%'>\n"
      )),
      logtxt.html = paste0(img.tag,'
<pre id="runid-',runid,'" class="logtxt-pre"><code class="logtxt-code">',
        # We might get a warning in has.substr if cmdline = ""
        ifelse(suppressWarnings(has.substr(logtxt, cmdline)), "",paste0(cmdline,"\n")),
        logtxt,'</code></pre>'
      )
    ) %>%
    group_by(script_num, orgline, line) %>%
    summarize(
      is_reg = first(is_reg),
      runs = n(),
      log.info.div = paste0(
        '<div class="collapse"  id = "loginfo-',orgline,'-', script_num,'">',
        #'<div class="collapse"  id = "runid-', runid,'">',
        ifelse(n()==1,
          logtxt.html,
          repboxTabSetPanel(tabnames = paste0("Run ",1:n()),contents = logtxt.html, ul.class="small-tab-ul")
        ),
        '</div>'
      ),
      log.info.btn = paste0('<a class="btn btn-xs" role="button" data-toggle="collapse" href="#loginfo-',orgline,'-', script_num,'" aria-expanded="false">&#x25BC;</a>')
    )

  log_info_html

}

#
# correct_delimit_semi_orglines = function(ldf) {
#   restore.point("correct_delimit_semi_orglines")
#   x = ldf$cmd
#   not_na <- !is.na(x)
#   ldf$cmd_group <- cumsum(not_na)
#   ldf2 = ldf %>%
#     group_by(cmd_group) %>%
#     mutate(
#       #cmd = first(cmd),
#       #line = first(line),
#       #is_reg = first(is_reg),
#       first_orgline = min(orgline),
#       cmd_points = has.substr(txt, first(cmd)) + startsWith(trimws(txt),first(cmd)) + (1:n()) / 1e6,
#       is_org_line = cmd_points == max(cmd_points)
#     ) %>%
#     mutate(
#       cmd = ifelse(is_org_line, first(cmd), NA_character_),
#       line = ifelse(is_org_line, first(line), NA_integer_),
#       is_reg = ifelse(is_org_line, first(is_reg), NA)
#     ) %>%
#     ungroup()
#   ldf2
# }
