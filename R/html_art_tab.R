example = function() {
  library(repboxHtml)
  project.dir = "~/repbox/projects_reg/aejapp_3_2_2"
  html = html_art_tabs(project.dir)
  html.dir = file.path(project.dir,"reports")
  repbox_save_html(repbox_html_page(html), "art_tab.html", html.dir)

  rstudioapi::filesPaneNavigate(html.dir)
  rstudioapi::filesPaneNavigate("~/repbox/repboxHtml/R")
}

html_art_tabs = function(project.dir, parcels=NULL, opts = repbox_html_opts(add_mapping=add_mapping), add_mapping=TRUE) {
  restore.point("html_art_tabs")

  parcels = regdb_load_parcels(project.dir, c("art_tab","art_tab_cell"), parcels)

  tab_df = parcels$art_tab$art_tab
  if (is.null(tab_df)) {
    parcels = regdb_load_parcels(project.dir,"art")
    art = parcels$art$art[1,]
    return(paste0('<h5>No tables detected in the article "', art$title,'" <a href="', art$pdf_url,'" target="_blank">🡥</a></h5>'))
  }
  cell_df = parcels$art_tab_cell$art_tab_cell

  new_parcels = NULL
  if (opts$add_tab_indicators) new_parcels = "ind_tab_type"
  if (opts$add_art_source_tab) new_parcels = c(new_parcels, "art_tab_source")

  if (opts$add_mapping) {
    new_parcels = c(new_parcels, "map_cell", if (opts$add_tab_report) c("stata_cmd_tab_fig_ref","stata_run_cmd"))
    parcels = regdb_load_parcels(project.dir, new_parcels, parcels)
    map_df = parcels$map_cell$map_cell
    add_mapping = !is.null(map_df)
  } else {
    add_mapping = FALSE
  }

  if (add_mapping) {
    new_parcels = c(new_parcels, "map_cell", if (opts$add_tab_report) c("stata_cmd_tab_fig_ref","stata_run_cmd"))
    parcels = regdb_load_parcels(project.dir, new_parcels, parcels)
    map_df = parcels$map_cell$map_cell

    cell_df = cell_df %>% left_join(select(map_df, cellid, block, regid, num_type,match_type, runid, variant, block, cmd), by=c("cellid"))


    cell_df = cell_df %>% mutate(
      id_code = ifelse(is.true(cellid > 0), paste0(' id="cell_', cellid,'"'),""),
      class_code = ifelse(is.true(match_type > 0),' class="tabnum"',""),
      debug_txt = ifelse(cellid==0, "",paste0("cellid: ", cellid,"\nrunid: ", runid, "\nregid: ", regid, "\nvariant: ", variant,"\nblock: ", block)),
      debug_title = ifelse(debug_txt == "","",paste0(' title="', debug_txt,'"') )
    )

    no_match_color = "#dddddd"
    reg_best_color = hsl_to_rgb(0, 0.7, 0.8)
    reg_best_approx_color = paste0("linear-gradient(45deg, ", reg_best_color,",",no_match_color,");")
    reg_color = hsl_to_rgb(0, 0.3, 0.8)
    reg_approx_color = reg_color
    reg_approx_color = paste0("linear-gradient(45deg, ", reg_color,",",no_match_color,");")

    # noreg_best_color = hsl_to_rgb(270, 0.7, 0.8)
    # noreg_best_approx_color = paste0("linear-gradient(45deg, ", noreg_best_color,",",no_match_color,");")
    # noreg_color = hsl_to_rgb(180, 0.7, 0.8)
    # noreg_approx_color = paste0("linear-gradient(45deg, ", noreg_color,",",no_match_color,");")


    cell_df = compute_cell_df_noreg_hue(cell_df)

    cell_df$noreg_color = "#dddddd"
    rows = !is.na(cell_df$noreg_hue)
    cell_df$noreg_color[rows] = hsl_to_rgb(cell_df$noreg_hue[rows], 0.6, 0.8)

    cell_df = cell_df %>% mutate(
      bg_color = case_when(
        is.na(cellid) ~ "",
        match_type == 1 ~ reg_best_color,
        match_type == 2 ~ reg_best_approx_color,
        match_type == 3 ~ reg_color,
        match_type == 4 ~ reg_approx_color,
        match_type == 5 ~ noreg_color,
        match_type == 6 ~ paste0("linear-gradient(45deg, ", noreg_color,",",no_match_color,");"),
        match_type == 7 ~ noreg_color,
        regid > 0 ~ no_match_color,
        match_type == 0 ~ no_match_color,
        TRUE ~ "#ffffff"
      ),
      style_code = ifelse(is.na(cellid),"",
        paste0(' style="background: ', bg_color,'"')
      )
    )


    cellid2runid = rep(0,max(cell_df$cellid))
    rows = is.true(cell_df$cellid>0)
    cellid2runid[cell_df$cellid[rows]] = cell_df$runid[rows] %>% na.val(0)

    js_vars = paste0('<script>const cellid_to_runid = [', paste0(cellid2runid, collapse=","),"];</script>")

  } else {
    cell_df$cellid = NA_integer_
    cell_df$id_code = cell_df$style_code = cell_df$class_code = cell_df$debug_title = js_vars =""
  }

  tabids = tab_df$tabid
  tabid = 1

  if (opts$add_art_source_tab & !is.null(tab_df)) {
    tab_df = left_join(tab_df, parcels$art_tab_source$art_tab_source, by="tabid")
  }

  contents = lapply(tabids, function(tabid) {
    tab = tab_df[tab_df$tabid == tabid, ]
    cells = cell_df[cell_df$tabid == tabid, ]
    cell_html = html_cell_tab(tab,cells, opts)
    # We add source view only for tables that were extracted from a PDF
    #if (tab$pdf_file == "" | !opts$add_art_source_tab) return(cell_html)

    source_html = report_html = ind_html = ""
    if (tab$pdf_file != "" & opts$add_art_source_tab) source_html = html_source_tab(tab)
    if (opts$add_tab_report & add_mapping) report_html = html_tab_report(tab, cell_df, parcels)
    if (opts$add_tab_indicators) ind_html = html_tab_indicators(tab, parcels)

    paste0(
      '<div id="art-tab-div_', tab$tabid,'" class="art-tab-div">',
      '<div class="art-cell-tab-div">',cell_html,'</div>',
      '<div>',report_html,'</div>',
      '<div>',ind_html,'</div>',
      '<div class="art-source-tab-div">', source_html,'</div>',
      '</div>'
    )
  })

  html = paste0(
    repboxTabSetPanel(
      id=paste0("tabtabs"),
      tabnames = paste0("Tab ", tab_df$tabid),
      tabids = paste0("tabtab",tabids),
      contents = contents, type="pills"
    ),"\n",
    js_vars
    #'<div id="link-menu"></div>'
  )
  html
  return(html)
}

html_cell_tab = function(tab, cells, opts) {
  restore.point("html_cell_tab")

  tr_df = cells %>%
    group_by(tabid,row) %>%
    summarize(html = paste0(
'<td ', id_code, class_code, style_code,
ifelse(colspan > 1,paste0(' colspan="',colspan,'"'),""),
if (opts$add_debug_info) debug_title,
">", text, "</td>", collapse="")
    )

  body_html = tr_df %>% summarize(
    html = paste0('<tr class="tabrow-', row,'">', html, "</tr>", collapse="\n")
  ) %>%
    pull(html)

  org_link = ""
  if (opts$add_org_tab_link & tab$url_org_tab != "") {
    org_link = paste0(' <a href="', tab$url_org_tab,'" target="_blank">🡥</a>')
  }

  html = paste0("<h5>",tab$tabtitle, org_link,"</h5>
<table class='arttab table-mini table '><tbody>\n",
  body_html,
  "</tbody></table>")
  html
}


html_source_tab = function(tab) {
  restore.point("html_source_tab")
  html = paste0(
    "<pre id='source-tab-",tab$tabid,"' style='background-color: #fff'>",tab$tabsource,"</pre>"
  )
}


html_tab_report = function(tab, cell_df, parcels) {
  restore.point("html_tab_report")
  #if (tab$tabpos == 1) stop()
  c_df = cell_df[cell_df$tabid == tab$tabid & cell_df$type=="num",]
  #art_small_reg = parcels$art_reg$art_small_reg
  #start_small_regid = first(art_small_reg$regid)

  #is_small_reg = any(is.true(c_df$regid >= start_small_regid))
  run_df = parcels$stata_run_cmd$stata_run_cmd

  # Determine which cell maps are to a runid whose code line has a reference
  # (by a comment above or file name) to the current table
  c_df = left_join(c_df, select(run_df, file_path, line, runid), by="runid")

  .tabid = tab$tabid
  cmd_ref = parcels$stata_cmd_tab_fig_ref$stata_cmd_tab_fig_ref %>%
    regdb_null_to_empty("stata_cmd_tab_fig_ref") %>%
    mutate(is_cur_tab = ref_type=="tab" & ref_id==.tabid)

  cur_ref = filter(cmd_ref,is_cur_tab) %>%
    mutate(has_cur_ref = TRUE)

  other_ref = filter(cmd_ref,!is_cur_tab) %>%
    mutate(has_other_ref = TRUE,
           other_ref_name = ifelse(ref_type=="tab", paste0("Table ", ref_id), paste0("Figure ", ref_id)))

  c_df = c_df %>%
    left_join( select(cur_ref, file_path, line, has_cur_ref), by = c("file_path","line")) %>%
    left_join( select(other_ref, file_path, line, has_other_ref, other_ref_name), by = c("file_path","line")) %>%
    mutate(
      has_cur_ref = na.val(has_cur_ref, FALSE),
      has_other_ref = na.val(has_other_ref, FALSE),
      other_ref_name = na.val(other_ref_name, "")
    )

  # c_df = c_df %>% mutate(
  #   ref_class = case_when(
  #     match_type == 0 ~ "",
  #     has_cur_ref & match_type >= 1 & match_type <= 2 ~ "cur_best_reg",
  #     has_cur_ref & match_type >= 3 & match_type <= 4 ~ "cur_reg",
  #     has_cur_ref & match_type >= 5 & match_type <= 6 ~ "cur_best_noreg",
  #     has_cur_ref & match_type == 7 ~ "cur_noreg",
  #
  #     has_other_ref & !has_cur_ref & match_type >= 1 & match_type <= 2~ "other_best_reg",
  #     has_other_ref & !has_cur_ref & match_type >= 3 & match_type <= 4~ "other_reg",
  #     has_other_ref & !has_cur_ref & match_type >= 5 & match_type <= 6~ "other_best_noreg",
  #     has_other_ref & !has_cur_ref & match_type == 7 ~ "other_noreg",
  #
  #     match_type >= 1 & match_type <= 2~ "none_best_reg",
  #     match_type >= 3 & match_type <= 4~ "none_reg",
  #     match_type >= 5 & match_type <= 6~ "none_best_noreg",
  #     match_type == 7 ~ "none_noreg"
  #   )
  # )
  #
  # c_df %>%
  #   group_by(has_cur_ref, ref_class) %>%
  #   summarize(
  #     num_cells = n(),
  #     other_ref_name = paste0(unique(other_ref_name), collapse=", ")
  #   )

  num_cells = NROW(c_df)
  num_perfect_reg = sum(c_df$has_cur_ref & c_df$match_type == 1)
  num_perfect_noreg = sum(c_df$has_cur_ref & c_df$match_type == 5)
  num_perfect = num_perfect_reg + num_perfect_noreg

  perc = function(x) {
    paste0(round(100* (x / num_cells),1),"%")
  }
  li_fun = function(num, str) {
    if (num == 0) return("")
    paste0("<li>",num," ", str,"</li>")
  }


  txt = paste0("<h5><b>Matching Report</b></h5>",
    "<ul>",
    #if (is_small_reg) "<li>Regressions are mapped assuming each row corresponds to a different regression</li>",
    "<li>", perc(num_perfect), " of the ", num_cells, " numbers are nicely matched (regression: ", num_perfect_reg, ", other: ", num_perfect_noreg,") </li></ul>"
  )


  if (num_perfect < num_cells) {
    txt = paste0(txt, "\n", num_cells - num_perfect, " numbers have problems: <ul>",
    li_fun(sum(c_df$match_type==0), "numbers are not mapped (grey cells)."),
    li_fun(sum(c_df$has_other_ref & !c_df$has_cur_ref & c_df$match_type > 0), paste0("numbers are mapped to commands for which the comments or do file name suggest that they belong to a different table / figure.</li>")),
    li_fun(sum(!c_df$has_other_ref & !c_df$has_cur_ref & c_df$match_type > 0), paste0("numbers are mapped to commands for which comments in the code provide no reference to any table or figure.</li>")),

    li_fun(sum(c_df$match_type %in% c(2,4,6)), paste0("numbers are only approximately matched (cell color fades to grey).</li>")),
    li_fun(sum(c_df$match_type %in% c(3,4,7)), paste0("numbers map to a command or regression but not to a unique best command for this block (cell color less intense).</li>")),
    "</ul>"
    )
  }

  txt
}

html_tab_indicators = function(tab, parcels) {
  restore.point("html_tab_indicators")
  p_def = repboxArt::get_phrases_def()
  ind_df = parcels$ind_tab_type$ind_tab_type
  if (NROW(ind_df)==0) {
    return("<p>No keyword indicators for the table type detected in the article text or table content.</p>")
  }

  ind_df = ind_df[ind_df$tabid == tab$tabid,]
  agg = ind_df %>%
    group_by(tabid, tab_type) %>%
    # Will only keep same sentence references
    filter(ind_val >= 0.9) %>%
    summarize(
      points = sum(ind_val * ind_counts)
    ) %>%
    arrange(desc(points)) %>%
    mutate(
      label = p_def$tt_labels[tab_type]
    )

  html = paste0("<h5><b>Table type indicators from text</b></h5>",
    '<table class="table table-mini">\n',
    # "<thead><tr><td>Indicator</td><td>Points</td></tr></thead>\n",
    "\n<tbody>\n",
    paste0("<tr><td>",agg$label,":</td><td>",agg$points,"</td></tr>", collapse="\n"),
    '\n</tbody></table>'
  )

  html
}
