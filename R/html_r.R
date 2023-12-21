example = function() {
  library(repboxHtml)
  project_dir = "~/repbox/projects_reg/testr"
  project_dir = "C:/libraries/repbox/projects_reg/testr"
  html = html_all_r(project_dir)
  html.dir = file.path(project_dir,"reports")
  repbox_save_html(html %>% repbox_add_html_header(), "r_code.html", html.dir)


  rstudioapi::filesPaneNavigate(html.dir)
  rstudioapi::filesPaneNavigate(project_dir)
  rstudioapi::filesPaneNavigate("~/repbox/repboxHtml/R")
}



html_all_r = function(project_dir, parcels=NULL, opts = repbox_html_opts()) {
  restore.point("html_all_r")

  parcels = repdb_load_parcels(project_dir, c("r_source","r_chunk","r_chunk_out"),parcels = parcels)

  # script_source is script_file info + source text
  script_df = parcels$r_source$script_source

  if (NROW(script_df)==0) {
    return("<p>The supplement has no R files</p>")
  }

  chunk_df = parcels$r_chunk$r_chunk %>%
    repdb_null_to_empty("r_chunk")

  out_df = parcels$r_chunk_out$r_chunk_out %>%
    repdb_null_to_empty("r_chunk_out") %>%
    left_join(select(chunk_df, chunkid,  line2), by="chunkid")

  line_out_df = r_make_out_lines_html(project_dir, out_df, opts=opts)


  project = basename(project_dir)
  outer.tabids = paste0("rtab_", script_df$script_num)

  contents = lapply(seq_len(NROW(script_df)), function(i) {
    restore.point("jnsdjsjd")
    script_num = script_df$script_num[[i]]
    otabid = outer.tabids[i]
    code = script_df$text[i]
    r_html = r_code_html(project_dir, script_num=script_num,file_path = script_df$file_path[[i]], code=code,line_out_df, out_df, opts=opts, parcels=parcels)
    return(r_html)
  })

  html = repboxTabSetPanel(type="pills",id="rtabs",tabnames = paste0(basename(script_df$file_path)),tabids = outer.tabids,contents = contents)

  return(html)
}


r_make_out_lines_html = function(project_dir, out_df,opts) {
  restore.point("r_make_out_lines_html")
  if (NROW(out_df)==0) {
    return(NULL)
  }

  # include images, possibly img_inline with bas64 encoding
  img.dir = paste0(project_dir, "/repbox/r/figure")
  out_df$img.src = ""
  rows = which(out_df$img_file!="")
  if (length(rows)>0) {
    if (opts$img_inline==FALSE) {
      out_df$img.src[rows] = repbox.link(paste0("images/",out_df$img_file[rows]))
    } else {
      out_df$img.src[rows] = lapply(rows, function(row) {
        img.file = paste0(img.dir,"/", out_df$img_file[row])
        knitr::image_uri(img.file)
      }) %>% unlist()
    }
  }

  out_df = out_df %>%
    mutate(
      out_html = case_when(
        is_html ~ out_text,
        img.src != "" ~ paste0("<img src='",img.src,"' style='max-width: 100%'>\n"),
        is.na(out_text) | is.true(out_text=="") ~ "",
        TRUE ~ paste0("<pre>", htmlEscape(out_text), "</pre>")
      )
    )


  # We further aggregate on script_num, line2 level
  line_out_df = out_df %>%
    group_by(script_num, line2) %>%
    summarize(out_html = trimws(paste0(out_html, collapse="\n"))) %>%
    mutate(
      div_html = ifelse(out_html == "", "",
        paste0('<div class="collapse"  id = "loginfo-',line2,'-', script_num,'">',out_html,"</div>")
      ),
      btn_html = ifelse(div_html == "", "", paste0('<a class="btn btn-xs" role="button" data-toggle="collapse" href="#loginfo-',line2,'-', script_num,'" aria-expanded="false">&#x25BC;</a>'))
    ) %>%
    rename(line = line2)
  line_out_df
}

# In this version I try not to directly add matching information.
# Instead general class info shall facilitate
# color-coded mapping
r_code_html = function(project_dir, script_num, file_path, code, line_out_df,out_df, opts, parcels) {
  restore.point("r_code_html")

  #stop()
  line_out_df = line_out_df[line_out_df$script_num == script_num,]
  out_df = out_df[out_df$script_num == script_num,] %>%
    rename(line = line2)

  code = sep.lines(code)

  ldf = tibble(line = seq_along(code), txt = htmlEscape(code), comment="", title="", class="", infobtn="", infobox="")

  err_df = out_df %>%
    filter(out_type == "error") %>%
    group_by(line) %>%
    summarize(
      num_err = n()
      #err_msg = first(out_text)
    )
  ldf = left_join(ldf, err_df, by="line") %>%
    mutate(num_err = na.val(num_err,0))

  ldf$class = case_when(
    ldf$num_err > 0 ~ "err-line",
    TRUE ~ "noerr-line"
  )

  ldf = left_join(ldf,line_out_df, by = c("line")) %>%
    mutate(
      btn_html = na.val(btn_html, ""),
      div_html = na.val(div_html, "")
    )

  ldf = ldf %>% mutate(
    html.txt = paste0(
      '<tr><td id="B',line,'___',script_num,'">',btn_html,'</td><td class="code-line-td">',line,'</td><td><pre class = "do-pre">',
      '<code id="L',line,'___',script_num,'" class="',class,'"', '>',txt,'</code></pre>',
      div_html,'</td></tr>')
    )
  inner = paste0(ldf$html.txt, collapse="\n")
  code.html = paste0("<table class='code-tab'>\n",inner,"</table>")
  return(code.html)
}

