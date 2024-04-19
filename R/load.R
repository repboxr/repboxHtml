# Load run_df and merge log and script_num
load_full_run_df = function(project_dir, parcels=NULL) {
  restore.point("load_full_run_df")
  parcels = repdb_load_parcels(project_dir, c("script_source","stata_run_cmd","stata_run_log","stata_file","stata_cmd"))

  run_df = parcels$stata_run_cmd$stata_run_cmd
  if (is.null(run_df)) return(NULL)
  cmd_df = parcels$stata_cmd$stata_cmd
  log_df = parcels$stata_run_log$stata_run_log
  #file_df = parcels$stata_file$script_file



  run_df = run_df %>%
    left_join(select(cmd_df, file_path, line, orgline, is_reg), by=c("file_path","line")) %>%
    #left_join(select(file_df,file_path, script_num), by="file_path") %>%
    left_join(log_df, by=c("artid", "runid"))


  # Correct old versions of run_df
  if (!has.col(run_df, "missing_data")) {
    run_df$missing_data = rep(FALSE, NROW(run_df))
  }

  run_df
}

# Load run_df and merge log and script_num
load_full_cmd_df = function(project_dir, parcels=NULL) {
  parcels = repdb_load_parcels(project_dir, c("stata_file","stata_cmd"))

  cmd_df = parcels$stata_cmd$stata_cmd
  if (is.null(cmd_df)) return(NULL)
  file_df = parcels$stata_file$script_file

  cmd_df = cmd_df %>%
    left_join(select(file_df,file_path, script_num), by="file_path")
  cmd_df
}

