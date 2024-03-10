example = function() {
  library(repboxMain)
  project_dir = "~/repbox/projects2/testsupp"
  rstudioapi::filesPaneNavigate(paste0(project_dir,"/repbox"))

}

data_set_extensions = function() {
  c("dta","csv","rds","tsv","tab","xlx","xlsx")
}

make_parcel_data_file_info = function(project_dir, parcels=list()) {
  restore.point("make_parcel_data_set_info")
  parcels = repdb_load_parcels(project_dir, "file_info", parcels = parcels)
  exts = data_set_extensions()

  org_files = repboxRun::repbox_get_org_sup_files(project_dir) %>%
    filter(tolower(file_type) %in% exts)
  mod_files = parcels$file_info$file_info %>%
    filter(tolower(file_type) %in% exts)

  fi = bind_rows(
    org_files,
    mod_files
  ) %>%
    filter(!duplicated(file_path)) %>%
    mutate(
      exists_org = file_path %in% org_files$file_path,
      exists_final = file_path %in% mod_files$file_path,
      artid = basename(project_dir),
      file_type = tolower(file_type)
    )

  parcels$data_file = list(data_file = fi)

  repdb_save_parcels(parcels["data_file"], file.path(project_dir,"repdb"))

  data_read = repdb_null_to_empty(NULL, "data_read")
  data_write = repdb_null_to_empty(NULL, "data_write")
  if (NROW(fi)==0) {
    parcels$data_read = list(data_read = data_read)
    parcels$data_write = list(data_write = data_write)
    repdb_save_parcels(parcels[c("data_read","data_write")], file.path(project_dir,"repdb"))
    return(parcels)
  }

  res = readRDS.or.null(file.path(project_dir,"repbox/stata/do_data_use.Rds"))
  do_data_load = res$do_data_load
  do_data_save = res$do_data_save

  if (NROW(do_data_load)>0) {
    old_cols = c("base","donum","from.parse","from.run","runs.noerr","runs.err")
    new_cols = c("file_base","script_num","from_parse","from_run","times_read_ok","times_read_err")
    data_read = rename.cols(do_data_load, old_cols, new_cols) %>%
      mutate(
        artid = basename(project_dir),
        file_type = tolower(tools::file_ext(file_base))
      )
  }
  if (NROW(do_data_save)>0) {
    old_cols = c("base","donum","from.parse","from.run","runs.noerr","runs.err")
    new_cols = c("file_base","script_num","from_parse","from_run","times_write_ok","times_write_err")
    data_write = rename.cols(do_data_save, old_cols, new_cols) %>%
      mutate(
        artid = basename(project_dir),
        file_type = tolower(tools::file_ext(file_base))
      )
  }


  parcels$data_read = list(data_read = data_read)
  parcels$data_write = list(data_write = data_write)
  repdb_save_parcels(parcels[c("data_read","data_write")], file.path(project_dir,"repdb"))
  return(parcels)
}

datasets_info_html = function(project_dir, parcels=list()) {
  restore.point("datasets_info_html")

  do_files = parcels$stata_file$script_file
  data_file = parcels$data_file$data_file %>% mutate(file_base = basename(file_path))
  data_read = parcels$data_read$data_read
  data_write = parcels$data_write$data_write
  exts = data_set_extensions()

  data_load = data_read %>%
    filter(tolower(file_type) %in% exts) %>%
    left_join(select(do_files, do_file = file_name, script_num), by="script_num") %>%
    group_by(file_base) %>%
    summarize(
      do_str = paste0(unique(do_file), collapse=", "),
      across(from_parse:times_read_ok, ~sum(.))
    )

  data_save = data_write %>%
    filter(tolower(file_type) %in% exts) %>%
    left_join(select(do_files, do_file = file_name, script_num), by="script_num") %>%
    group_by(file_base) %>%
    summarize(
      do_str = paste0(unique(do_file), collapse=", "),
      across(from_parse:times_write_ok, ~sum(.)),
      intermediate = TRUE
    )

  # Table for existing data sets
  exist.df = data_file %>%
    left_join(select(data_load,file_base, loaded_by = do_str), by="file_base") %>%
    left_join(select(data_save,file_base, saved_by = do_str), by="file_base") %>%
    mutate(
      loaded_by = na.val(loaded_by,""),
      saved_by = na.val(saved_by,"")
    ) %>%
    arrange(desc(mb))


  if (NROW(exist.df)>0) {
    table.head = "<tr><th>Data set</th><th>Size</th><th>Loaded in</th><th>Created in</th></tr>"

    exist.df = exist.df %>% mutate(
      tr_str = paste0("<tr><td>",file_base,"</td><td>", data.size.str(mb),"</td><td>",loaded_by,"</td><td>", saved_by, "</td></tr>")
    )
    tbody = paste0(exist.df$tr_str, collapse="\n")

    exists.tab.html = paste0('\n<table class="table-mini table table-striped">
  <thead>',table.head, "</thead><tbody>\n",tbody,"</tbody></table>")
  } else {
    exists.tab.html = "<p>--- No data sets found in the supplement ---"
  }

  missing.df = data_load %>%
    filter(times_read_ok==0 & (times_read_err > 0 & (!file_base %in% data_file$file_base)) ) %>%
    left_join(select(data_save,file_base, intermediate), by="file_base") %>%
    mutate(
      intermediate = na.val(intermediate, FALSE),
      loaded_by = na.val(do_str,"")
    ) %>%
    filter(!is.na(file_base) & is.true(tools::file_path_sans_ext(file_base) !="")) %>%
    arrange(desc(intermediate),file_base)


  if (NROW(missing.df)>0) {
    table.head = "<tr><th>Data set</th><th>Intermediate</th><th>Supposed to be loaded in</th></tr>"

    missing.df = missing.df %>% mutate(
      tr_str = paste0("<tr><td>",file_base,"</td><td>",ifelse(intermediate,"Yes","No"),"</td><td>",loaded_by,"</td></tr>")
    )
    tbody = paste0(missing.df$tr_str, collapse="\n")
    missing.tab.html = paste0('\n<table class="table-mini table table-striped">
  <thead>',table.head, "</thead><tbody>\n",tbody,"</tbody></table>")
  } else {
    missing.tab.html = "<p>--- No missing data sets detected ---"
  }

  # Temporary data sets
  temp.df = data_save %>%
    mutate(
      saved_by = na.val(do_str,""),
      ext = str.right.of(file_base,"."),
      exists = file_base %in% exist.df$file_base,
    ) %>%
    # Remove files generated with Stata tempfile like
    # St1117213.000001
    filter(nchar(ext <= 4) | ext %in% data_set_extensions())

  if (NROW(temp.df)>0) {
    table.head = "<tr><th>Data set</th><th>Exists</th><th>Created in</th></tr>"

    temp.df = temp.df %>% mutate(
      tr_str = paste0("<tr><td>",file_base,"</td><td>",ifelse(exists,"Yes","No"),"</td><td>",saved_by,"</td></tr>")
    )
    tbody = paste0(temp.df$tr_str, collapse="\n")
    temp.tab.html = paste0('\n<table class="table-mini table table-striped">
  <thead>',table.head, "</thead><tbody>\n",tbody,"</tbody></table>")
  }

  html = paste0(
    "<h4>Missing Data Sets (",NROW(missing.df),")</h4>",
    if (NROW(missing.df)>0) "<p>There is Stata code that attempts but fails to load the following data sets, but those data sets are  not found in the data and code supplement. Note that sometimes the main analysis can still be replicated even if some data sets are missing.</p>",
    missing.tab.html,
    "<h4>Existing Data Sets (",NROW(exist.df),")</h4>",
    if (NROW(exist.df)>0) "<p>Below are existing data set files. Note that sometimes a file for confidential or proprietary data set may exist, but it is empty or has simulated fake data.</p>",
    exists.tab.html,
    if (NROW(temp.df)>0) paste0(
      "<h4>Intermediate Data Sets (", NROW(temp.df),")</h4>",
      "<p>The following data sets are generated or supposed to be generated by Stata code. They might only exist temporarily.",
      "</p>",
      temp.tab.html
    )
  )
  return(list(html=html, missing.df=missing.df, exist.df=exist.df, temp.df = temp.df))
}

data.size.str = function(mb) {
  str = case_when(
    mb > 1000 ~ paste0( round(mb / 1000,1), " GB"),
    mb > 10 ~ paste0(round(mb), " MB"),
    mb > 0.1 ~ paste0(round(mb,1), " MB"),
    TRUE ~ paste0(round(mb * 1000), " KB")
  )
  str
}
