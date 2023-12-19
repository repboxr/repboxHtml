# Determine initial matching of cells in article table
# with chunks of do files



# A first heuristic (can be improved)
# 1. If a regression is mapped for a cell
#    then regression mappings have priority
# 2. Otherwise map remaining cells by most command command in the column
cell_df_find_best_match_chunks = function(cell_df, parcels) {
  restore.point("find_best_match_chunks")
  parcels = regdb_load_parcels(project_dir, c("html","art_reg"), parcels)

  cell_df$cellid = seq_len(NROW(cell_df))

  # TO DO: Sensibly define cell blocks
  cell_df$block_str = paste0(cell_df$tabid,";", cell_df$panel_num, ";", cell_df$num_row_block, ";", cell_df$col)
  cell_df$block = match(cell_df$block_str, unique(cell_df$block_str))
  cell_df %>%
    group_by(block) %>%
    summarize(n())

  cell_df = find_best_reg_match_chunks(cell_df, parcels)
  cell_df = find_best_noreg_match_chunks(cell_df, parcels)

  # Let us now determine best_match_type and best_match_chunkind
  # from reg and noreg matches

  # Currently we will always prefer reg matches, but that should
  # be adapted later.
  cell_df = cell_df %>%
    group_by(block) %>%
    mutate(
      block_num_best_reg_match = sum(is_best_reg_match),
      block_num_reg_match = sum(is_reg_match),
      block_num_noreg_match = sum(!is.na(noreg_match_chunkid)),
      block_type = case_when(
        block_num_reg_match >= block_num_noreg_match ~ "reg",
        TRUE ~ "noreg"
      )
    )


  # See html_tab_num_cell.yml
  cell_df = cell_df %>% mutate(
    best_match_type = case_when(
      block_type == "reg" & is_best_reg_match ~ 1L,
      block_type == "reg" & is_reg_match ~ 2L,
      block_type == "reg" ~ 0L,
      block_type == "noreg" & is.true(noreg_match_chunk_rank==1) ~ 4L,
      block_type == "noreg" & !is.na(noreg_match_chunkid) ~ 5L,
      TRUE ~ 0L
    ),
    best_match_chunkid = case_when(
      best_match_type == 0 ~ 0L,
      best_match_type <= 3 ~ best_reg_match_chunkid,
      best_match_type <= 5 ~ noreg_match_chunkid
    )
  )
  cell_df
}

# Idea: For each block find command with most matches
find_best_noreg_match_chunks = function(cell_df, parcels) {
  restore.point("find_best_noreg_match_chunks")

  cell_df$noreg_match_chunk_rank = NA_integer_
  cell_df$noreg_match_chunkid = NA_integer_

  c_df = filter(cell_df,do_num_start_ind>0)

  if (NROW(c_df)==0) return(cell_df)

  #stop("Please check find_best_noreg_match_chunks code for this example.")

  num_df = parcels$html$html_do_num
  chunk_df = parcels$html$html_do_chunk

  # To match c_df with num_df we use match_all_rounded even
  # though we know do_num_start_ind and do_num_end_ind.
  # That is because I don't know a quick way to expand them
  # and probably the C code data.table code is faster even
  # though the algo is complexer.
  ind_df = match_all_rounded(c_df$num, c_df$num_deci, num_df$num, ydeci = 10)
  numa = tibble(cellid = c_df$cellid[ind_df[[1]]], chunkid=num_df$chunkid[ind_df[[2]]])

  # Determine how often each chunkid is found per block
  numa = left_join(numa, select(c_df, cellid, block), by="cellid")

  block_df = numa %>%
    group_by(block, chunkid) %>%
    summarize(
      n_chunk = n()
    ) %>%
    left_join(select(chunk_df, chunkid, cmd)) %>%
    group_by(block, cmd) %>%
    mutate(
      n_cmd = n()
    ) %>%
    group_by(block) %>%
    arrange(desc(n_cmd), desc(n_chunk)) %>%
    mutate(
      chunk_rank = seq_len(n())
    ) %>%
    ungroup()

  numa = left_join(numa, select(block_df, block, chunkid, chunk_rank), by=c("block","chunkid"))

  # Only pick for every cell the "best chunk"
  numa = numa %>%
    group_by(cellid) %>%
    arrange(chunk_rank, chunkid) %>%
    slice(1)

  rows = numa$cellid
  cell_df$noreg_match_chunk_rank[rows] = numa$chunk_rank
  cell_df$noreg_match_chunkid[rows] = numa$chunkid
  cell_df
}


# To do: match regstats
find_best_reg_match_chunks = function(cell_df, parcels) {
  restore.point("find_best_reg_match_chunks")

  cell_df$is_best_reg_match = FALSE
  cell_df$is_reg_match = FALSE
  cell_df$best_reg_match_chunkid = NA_integer_


  # Match regression cells
  reg_map = parcels$html$html_reg_map
  #art_coef = parcels$art_reg$art_regcoef %>%
  #  mutate(art.coef.row = seq_len(n()))

  breg_map = reg_map %>%
    group_by(chunkid, regid) %>%
    top_n(match_score,1) %>%
    ungroup()

  reg_cell_df = cell_df %>%
    filter( regid > 0) %>%
    left_join(select(breg_map, regid, best_chunk=chunkid, paren_num_type), by="regid")

  # coef, se, t, p
  reg_num_types = c(coef = 1,se=3,t=4,p=5)

  i = 2
  for (i in seq_along(reg_num_types)) {
    type_str = names(reg_num_types)[i]
    num_df = parcels$html[[paste0("html_reg_num_", type_str)]]
    if (NROW(num_df)==0) next

    filter.type = if (i == 1) 1 else 2
    c_df = reg_cell_df %>%
      filter(reg_num_type == filter.type)


    ind_df = match_all_rounded(c_df$num, c_df$num_deci, num_df$num, ydeci = 14)
    numa = tibble(cellid = c_df$cellid[ind_df[[1]]], chunkid=num_df$chunkid[ind_df[[2]]])
    numa = left_join(numa, select(c_df, cellid, best_chunk), by="cellid")
    best_numa = numa[is.true(numa$chunkid==numa$best_chunk),]

    rows = c_df$cellid
    cell_df$best_reg_match_chunkid[rows] = best_numa$chunkid[match(cell_df$cellid[rows],best_numa$cellid)]
    cell_df$is_best_reg_match[rows] = !is.na(cell_df$best_reg_match_chunkid[rows])
    cell_df$is_reg_match[rows] = cell_df$cellid[rows] %in% numa$cellid
  }


  # Now also store info for regstat
  c_df = filter(cell_df, !is.na(stat_name))

  rows = c_df$cellid
  cell_df$is_reg_match[rows] = TRUE

  stat_map = parcels$html$html_reg_stat_map
  best_stat_map = semi_join(stat_map, breg_map, by=c("step","regid"))
  best_c_df = c_df %>%
    left_join(select(best_stat_map, stat_name, regid, chunkid), by=c("stat_name","regid")) %>%
    filter(!is.na(chunkid))
  rows = best_c_df$cellid
  cell_df$is_best_reg_match[rows] = TRUE
  cell_df$best_reg_match_chunkid[rows] = best_c_df$chunkid

  cell_df
}
