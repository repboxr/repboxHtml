# Chunk output for regression commands
# We will replace the standard Stata log with our custom
# regression output. This facilitates tagging and mapping.

example = function() {
  library(repboxHtml)
  project_dir = "~/repbox/projects_reg/aejapp_3_2_2"

  parcels = html_make_parcels(project_dir)

  rstudioapi::filesPaneNavigate(project_dir)
  rstudioapi::filesPaneNavigate("~/repbox/repboxHtml/R")
}

html_make_parcels = function(project_dir, parcels = list()) {
  restore.point("html_make_parcels")
  repdb_load_specs(libs="repboxHtml")

  parcels$html = list()
  # creates html_do_chunk, html_cmd and html_regcmd
  parcels = html_parcel_do_chunk(project_dir, parcels)
  # creates html_do_num
  parcels = html_parcel_do_num(project_dir, parcels)
  # creates html_reg_map
  parcels = html_parcel_reg_map(project_dir, parcels)

  # creates html_reg_num_coef  to html_reg_num_p
  parcels = html_parcel_reg_num(project_dir, parcels)

  # creates html_reg_num_coef  to html_reg_num_p
  parcels = html_parcel_reg_stat_map(project_dir, parcels)


  # creates html_regout
  parcels = html_parcel_do_regout(project_dir, parcels)
  # creates html_tab_num_cell
  parcels = html_parcel_tab_num_cell(project_dir, parcels)





  dir = file.path(project_dir,"reports","repdb")
  if (!dir.exists(dir)) dir.create(dir)
  repdb_save_parcels(parcels["html"],dir)
  parcels
}

html_parcel_reg_stat_map = function(project_dir, parcels=list()) {
  parcels = repdb_load_parcels(project_dir, c("html_do_chunk", "map_reg", "map_regstat", "art_reg"), parcels)

  # html_do_stat_num
  stat_df = parcels$art_reg$art_regstat
  chunk_df = parcels$html$html_do_chunk

  map_df = parcels$map_regstat$map_regstat %>%
    left_join(select(chunk_df, step, chunkid), by="step") %>%
    mutate(
      # See db specifications
      reg_num_type = case_when(
        stat_name == "nobs" ~ 11,
        stat_name == "r2" ~ 12,
        TRUE ~ 10
      )
    )

  repdb_check_data(map_df, "html_reg_stat_map")
  parcels$html$html_reg_stat_map = map_df
  parcels
}

#
html_parcel_reg_num = function(project_dir, parcels=list()) {
  restore.point("html_parcel_reg_num")
  parcels = repdb_load_parcels(project_dir, c("html_do_chunk", "map_reg", "map_regstat", "base_core","base_regcoef", "art_reg"), parcels)

  chunk_df = parcels$html$html_do_chunk

  reg_df = parcels$base_core$reg
  map_df = parcels$map_reg$map_reg

  coef_df = parcels$base_regcoef$regcoef %>%
    left_join(select(chunk_df, step, chunkid), by="step")

  # html_do_coef_num
  c_df = semi_join(coef_df, map_df, by="step")
  c_df$num = c_df[["coef"]]
  c_df = c_df[,c("chunkid","num")]
  c_df = c_df %>%
    unique() %>%
    na.omit() %>%
    arrange(num)

  parcels$html$html_reg_num_coef = c_df


  # html_do_se_num, html_do_t_num, html_do_p_num
  paren_types = c("se","t","p")

  paren_type = "se"
  paren_tabs = lapply(paren_types, function(paren_type) {
    m_df = map_df[map_df$paren_type == paren_type, ]
    if (NROW(m_df)==0) return(NULL)
    c_df = semi_join(coef_df, m_df, by="step")

    c_df$num = c_df[[paren_type]]
    c_df = c_df[,c("chunkid","num")]
    c_df %>%
      unique() %>%
      na.omit() %>%
      arrange(num)

  })
  names(paren_tabs) = paste0("html_reg_num_", paren_types)
  parcels$html[names(paren_tabs)] = paren_tabs

  parcels
}


html_parcel_tab_num_cell = function(project_dir, parcels=list()) {
  restore.point("html_parcel_tab_num_cell")
  parcels = repdb_load_parcels(project_dir, c("art_tab_cell","art_tab","art_reg","html"), parcels)

  tab_df = parcels$art_tab$art_tab
  reg_df = parcels$art_reg$art_reg

  stat_df = parcels$art_reg$art_regstat %>%
    left_join(select(reg_df, regid, tabid), by="regid")

  coef_df = parcels$art_reg$art_regcoef %>%
    left_join(select(reg_df, regid, tabid), by="regid")

  num_df = parcels$html$html_do_num

  cell_df = parcels$art_tab_cell$art_tab_cell %>%
    filter(type=="num") %>%
    left_join(select(tab_df, tabid, tabpos), by="tabid") %>%
    # Match regid and determine
    left_join( select(coef_df,tabid, row=coef_cell_row, col=coef_cell_col, coef_regid=regid), by=c("tabid","row","col")) %>%
    left_join( select(coef_df,tabid, row=paren_cell_row, col=paren_cell_col, paren_regid=regid), by=c("tabid","row","col")) %>%
    left_join( select(stat_df,tabid, row, col, stat_regid=regid, stat_name), by=c("tabid","row","col")) %>%
    mutate(
      regid = coalesce(coef_regid, paren_regid, stat_regid, 0),
      reg_num_type = case_when(
        !is.na(coef_regid) ~ 1,
        !is.na(paren_regid) ~ 2,
        is.true(stat_name=="nobs") ~ 11,
        is.true(stat_name=="r2") ~ 12,
        !is.na(stat_regid) ~ 10,
        TRUE ~ 0
      )
    )

  do_nums = c(-Inf,num_df$num,Inf)

  # To do: need to check with an article where we do actually have matches
  cell_df = cell_df %>% mutate(
    # Special rule for integers
    use_deci = case_when(
      num_deci == 0 & abs(num) <= 20 ~ 10L,
      #num_deci == 0 & abs(num) <= 10000 ~ 2L,
      TRUE ~ num_deci
    ),
    num_low = num - 0.51 * 10^-use_deci,
    num_high = num + 0.51 * 10^-use_deci,
    do_num_start_ind = findInterval(num_low,do_nums)+1,
    do_num_end_ind = findInterval(num_high, do_nums),
    has_match = do_num_end_ind >= do_num_start_ind
  ) %>% mutate(
    do_num_start_ind = ifelse(has_match, do_num_start_ind, -1),
    do_num_end_ind = ifelse(has_match, do_num_end_ind, -2)
  )

  cell_df = cell_df_find_best_match_chunks(cell_df, parcels)

  repdb_check_data(cell_df, "html_tab_num_cell")
  parcels$html$html_tab_num_cell = cell_df
  parcels
}



html_parcel_do_num = function(project_dir, parcels=list()) {
  restore.point("html_parcel_do_num")
  parcels = repdb_load_parcels(project_dir, c("stata_log_num"), parcels)

  chunk_df = parcels$html$html_do_chunk

  num_df = parcels$stata_log_num$stata_log_num

  if (NROW(num_df)==0) return(parcels)

  num_df = num_df %>%
    left_join(select(chunk_df, runid, chunkid), by="runid") %>%
    arrange(num) %>%
    select(num, chunkid) %>%
    unique()



  repdb_check_data(num_df, "html_do_num")
  parcels$html$html_do_num = num_df
  parcels
}


html_parcel_reg_map = function(project_dir, parcels=list()) {
  restore.point("html_parcel_reg_map")
  parcels = repdb_load_parcels(project_dir, c("map_reg"), parcels)

  chunk_df = parcels$html$html_do_chunk

  map_df = parcels$map_reg$map_reg

  if (NROW(map_df)==0) return(NULL)


  map_df = map_df %>%
    left_join(select(chunk_df, step, runid, chunkid), by="step") %>%
    mutate(paren_num_type = paren_type_to_paren_num_type(paren_type))

  repdb_check_data(map_df, "html_reg_map")

  parcels$html$html_reg_map = map_df
  parcels
}

# See html_tab_num_cell.yml
paren_type_to_paren_num_type = function(paren_type) {
  num_vec = c("se"=3, "t"=4, "p"=5)
  num_vec[paren_type]
}

html_parcel_do_chunk = function(project_dir, parcels=NULL) {
  restore.point("html_parcel_do_chunk")
  parcels = repdb_load_parcels(project_dir, c("stata_run_cmd","stata_cmd","stata_file","base_core"), parcels)

  ignore_cmds = repboxMap::ignore_log_stata_commands()

  reg_df = parcels$base_core$reg
  cmd_df = parcels$stata_cmd$stata_cmd
  run_df = parcels$stata_run_cmd$stata_run_cmd
  script_df = parcels$stata_file$script_file

  chunk_df = run_df %>%
    filter(!cmd %in% ignore_cmds) %>%
    left_join(select(cmd_df, file_path,line, is_reg, orgline), by=c("file_path","line")) %>%
    left_join(select(script_df, file_path, script_num), by="file_path") %>%
    mutate(
      chunkid = seq_len(n()),
      regchunkid = cumsum(is_reg)
    ) %>%
    left_join(select(reg_df, step, runid), by="runid")

  html_cmd = chunk_df %>%
    filter(!is_reg) %>%
    group_by(cmd) %>%
    summarize(
      cmd_count = n()
    ) %>%
    arrange(desc(cmd_count)) %>%
    mutate(cmd_ind = seq_len(n())) %>%
    # TO DO: We won't store bg_color but hue instead
    #mutate(bg_color = cmd_ind_colors(n()))
    mutate(bg_color = "#bbbbff")

  html_regcmd = chunk_df %>%
    filter(is_reg) %>%
    group_by(cmd) %>%
    summarize(
      cmd_count = n()
    ) %>%
    arrange(desc(cmd_count)) %>%
    select(-cmd_count) %>%
    mutate(regcmd_ind = seq_len(n()))

  chunk_df = chunk_df %>%
    left_join(html_cmd, by = "cmd") %>%
    left_join(html_regcmd, by = "cmd") %>%
    mutate(
      cmd_ind = ifelse(is.na(cmd_ind), -regcmd_ind, cmd_ind)
    )


  parcels$html$html_cmd = html_cmd
  parcels$html$html_regcmd = html_regcmd
  parcels$html$html_do_chunk = chunk_df



  repdb_check_data(chunk_df,"html_do_chunk")
  repdb_check_data(html_regcmd,"html_regcmd")
  repdb_check_data(html_cmd,"html_cmd")

  parcels

}

html_parcel_do_regout = function(project_dir, parcels=NULL) {
  restore.point("html_make_regout")
  parcels = repdb_load_parcels(project_dir, c("base_core","base_regcoef"), parcels)
  reg_df = parcels$base_core$reg
  coef_df = parcels$base_regcoef$regcoef

  chunk_df = parcels$html$html_do_chunk

  # TO DO: Derive num_deci
  coef_df$digits = 4
  tr_df = coef_df %>%
    group_by(step) %>%
    summarize(
      tr_html = paste0("<tr>",
      "<td>", shown_term, "</td>",
      "<td class='coef'>", signif(coef, digits), "</td>",
      "<td class='se'>", signif(se, digits), "</td>",
      "<td class='t'>", signif(t, digits), "</td>",
      "<td class='p'>", signif(p, digits), "</td>",
      "<td>", signif(ci_low, digits), "</td>",
      "<td>", signif(ci_up, digits), "</td>",
      "</tr>")
    )

  out_df = tr_df %>%
    group_by(step) %>%
    summarize(
      regout = paste0(
        "<table class='table table-mini'>\n",
        "<tr><th>coef</th><th>se</th><th>t</th><th>p</th><th colspan=2>95% CI</th></tr>\n",
        paste0(tr_html, collapse="\n"),
        "\n</table>"
      )
    )

  out_df = left_join(out_df, select(reg_df,runid, step), by="step") %>%
    left_join(select(chunk_df, runid, chunkid), by="runid")

  repdb_check_data(out_df, "html_regout")

  parcels$html$html_regout = out_df

  parcels

}
