

make_parcel_stata_do_run_info = function(project_dir, parcels = list()) {
  restore.point("make_parcel_stata_do_run_info")
  library(repboxDB)
  parcels = repdb_load_parcels(project_dir, c("stata_file","stata_run_cmd"), parcels=parcels)

  do_df = parcels$stata_file$script_file

  # dotab contains some information that we have not yet nicely stored
  # in a repdb table
  dotab_file = file.path(project_dir, "/repbox/stata/dotab.Rds")
  dotab = readRDS.or.null(dotab_file)
  if (is.null(dotab)) {
    dotab = data.frame(file_path=character(0), timeout=logical(0), runtime=numeric(0),is.included=logical(0), parse.err = logical(0))
  }
  #cat(paste0('"', names(dotab),'"', collapse=", "))
  old_cols = c("num.reg.lines", "parse.err",  "is.included", "does.include", "timeout", "runtime")
  new_cols = c("num_reg_lines", "has_parse_err",  "is_included", "does_include", "timeout", "runtime")
  dotab = rename.cols(dotab, old_cols, new_cols)

  dotab$file_path = str.right.of(dotab$file,paste0(dotab$project_dir,"/mod/"))
  dotab$analyzed = rep(TRUE, NROW(dotab))
  do_df = left_join(do_df, dotab, by="file_path") %>%
    mutate(analyzed = na.val(analyzed, FALSE))

  #cmd_df = parcels$stata_cmd$stata_cmd
  run_df = parcels$stata_run_cmd$stata_run_cmd

  run_info_df = run_df %>%
    mutate(
      loads_data = cmd %in% c("use","u","us","import","guse","gzuse","insheet"),
      has_error = is.true(errcode != 0)
    ) %>%
    group_by(file_path) %>%
    summarize(
      was_run = TRUE,
      num_runs = n(),
      num_runs_err = sum(has_error),
      num_load_data = sum(loads_data),
      num_load_data_err = sum(has_error & loads_data)
    )

  do_df = do_df %>%
    left_join(run_info_df, by="file_path") %>%
    mutate(
      was_run = na.val(was_run,FALSE),
      has_parse_err = na.val(has_parse_err, FALSE)
    )

  do_df = repdb_select_fields(do_df, "stata_do_run_info")
  parcels$stata_do_run_info = list(stata_do_run_info = do_df)
  repboxDB::repdb_save_parcels(parcels["stata_do_run_info"],file.path(project_dir, "repdb"))

  parcels
}


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
      ), info)
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
  <th colspan="2" style="border: 0; padding-bottom: 0;">All commands</th>
  <th colspan="2" style="border: 0; padding-bottom: 0;">Data load</th>
  <th style="border: 0; padding-bottom: 0;">Info</th>
</tr><tr>
  <th style="border: 0"></th>
  <th style="border: 0">(sec.)</th>
  <th style="border: 0">Runs</th>
  <th style="border: 0">Error</th>
  <th style="border: 0">Success</th>
  <th style="border: 0">Failure</th>
  <th style="border: 0"></th>

</tr>
  '
  do_df = mutate(do_df, row.html = paste0(
    '<tr', ifelse(is.total,' class="total-row"',""),'>','<td>',ifelse(analyzed,
                                                                      paste0('<a href="',do.line.link(script_num, link_with_tabs = link_with_tabs),'">',file,"</a>"),
                                                                      file),'</td>',
    '<td style="text-align: right;">',na.val(round(runtime),"-"),'</td>',

    '<td style="text-align: right;">',na.val(num_runs,"-"),'</td>',
    '<td style="text-align: right;">',na.val(num_runs_err,"-"),'</td>',
    '<td style="text-align: right;">',na.val(num_load_data-num_load_data_err,"-"),'</td>',
    '<td style="text-align: right;">',na.val(num_load_data_err,"-"),'</td>',
    '<td>',info,'</td>',
    '</tr>'
  ))

  tbody = paste0(do_df$row.html, collapse="\n")

  tab.html = paste0('\n<table id="do-overview-tab" class="table-mini table table-striped">
  <thead>',head.html, "</thead><tbody>\n",tbody,"</tbody></table>")
  h = paste0(h, tab.html)
  return(h)

}
