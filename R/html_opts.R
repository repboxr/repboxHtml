repbox_html_opts_just_ejd = function(make_what="ejd",img_inline=FALSE, standalone=FALSE, copy_image_dir = TRUE, ...) {
  repbox_html_opts(make_what = make_what,img_inline=img_inline, standalone=standalone, ...)
}

repbox_html_opts = function(make_what = c("ejd","general")[2],
  standalone = FALSE, add_art_source_tab = FALSE,do_max_runs=8000,line_max_runs=25,log_max_char=50000,add_match_info=TRUE, img_inline=!standalone, unfold_do_log_btns = TRUE, show_first_err_msg = TRUE, add_org_tab_link = TRUE, add_mapping=TRUE, add_tab_report = TRUE, add_do_mapping = add_mapping, add_tab_indicators = TRUE, add_debug_info = TRUE, copy_image_dir = !img_inline, copy_shared_dir = TRUE,...) {
  list(
    make_what = make_what,
    standalone = standalone,
    add_art_source_tab = add_art_source_tab,
    do_max_runs= do_max_runs,
    line_max_runs= line_max_runs,
    add_match_info=add_match_info,
    img_inline=img_inline,
    log_max_char = log_max_char,
    unfold_do_log_btns=unfold_do_log_btns,
    show_first_err_msg = show_first_err_msg,
    add_org_tab_link = add_org_tab_link,
    add_mapping = add_mapping,
    add_tab_report = add_tab_report,
    add_do_mapping = add_do_mapping,
    add_tab_indicators = add_tab_indicators,
    add_debug_info = add_debug_info,
    copy_image_dir = copy_image_dir,
    copy_shared_dir = copy_shared_dir,
    ...
  )
}


