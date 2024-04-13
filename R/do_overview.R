


report_do_overview = function(project_dir, parcels = list(), link_with_tabs=TRUE, return_do_df = FALSE) {
  restore.point("report_do_overview")

  do_df = parcels$stata_do_run_info$stata_do_run_info


  h = ""
  if (NROW(do_df)==0) {
    h = paste0(h,"<p>The project has no do files.<p>")
    if (return_do_df) return(list(html=h, do_df=NULL))
    return(h)
  }

  do_df = do_df %>%
    mutate(
      file = basename(file_path)
    ) %>%
    mutate(
      info = case_when(
        is.true(has_parse_err) & is.true(timeout) ~ "timeout and parsing error",
        is.true(has_parse_err) ~ "parsing error",
        is.true(timeout) ~ "timeout",
        TRUE ~ ""
    )) %>%
    mutate(
      info = paste0(case_when(
          !was_run~"not run",
          is_included~"called in other do",
          is.na(runtime) ~ "just parsed ",
          TRUE ~ ""
        ),
        ifelse(info=="","",paste0(" ", info))
      )
    )


  if (NROW(do_df)>1) {
    total = lapply(do_df, function(val) {
      if (is.numeric(val)) return(sum(val, na.rm=TRUE))
      return(NA)
    }) %>% as_tibble()
    total$was_run = FALSE
    total$file = "TOTAL"
    total$info = ""
    do_df = bind_rows(total, do_df)
    do_df$is.total = c(TRUE, rep(FALSE,NROW(do_df)-1))
  } else {
    do_df$is.total = logical(NROW(do_df))
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
  <th colspan="4" style="border: 0; padding-bottom: 0;">Commands</th>
  <th style="border: 0; padding-bottom: 0;">Info</th>
</tr><tr>
  <th style="border: 0"></th>
  <th style="border: 0">(sec.)</th>
  <th style="border: 0">Runs</th>
  <th style="border: 0">OK</th>
  <th style="border: 0">Missing Data</th>
  <th style="border: 0">Error</th>
  <th style="border: 0"></th>

</tr>
  '
  do_df = mutate(do_df, row.html = paste0(
    '<tr', ifelse(is.total,' class="total-row"',""),'>','<td>',
    ifelse(analyzed & !is.total,                                                                    paste0('<a href="',do.line.link(script_num, link_with_tabs = link_with_tabs),'">',file,"</a>"),
                                                                      file),'</td>',
    '<td style="text-align: right;">',na.val(round(runtime),"-"),'</td>',

    '<td style="text-align: right;">',na.val(num_runs,"-"),'</td>',
    '<td style="text-align: right;">',na.val(num_runs_ok,"-"),'</td>',
    '<td style="text-align: right;">',na.val(num_runs_no_dat,"-"),'</td>',
    '<td style="text-align: right;">',na.val(num_runs_err,"-"),'</td>',
    '<td>',info,'</td>',
    '</tr>'
  ))

  tbody = paste0(do_df$row.html, collapse="\n")

  tab.html = paste0('\n<table id="do-overview-tab" class="table-mini table table-striped">
  <thead>',head.html, "</thead><tbody>\n",tbody,"</tbody></table>")
  h = paste0(h, tab.html)
  return(h)

}
