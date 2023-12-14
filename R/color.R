example = function() {

  df = expand.grid(x=1:6, y=1:6)

  df$hue = seq(10,360,by=10)

  df$colors = hsl_to_rgb(df$hue,0.9,0.85)

  library(ggplot2)
  ggplot(df, aes(x=x,y=y, fill=colors, label=hue)) +
    geom_tile(fill=df$colors) +
    geom_text() +
    theme_minimal()

  df$colors = hsl_to_rgb(df$hue,1,0.8)
  library(ggplot2)
  ggplot(df, aes(x=x,y=y, fill=colors, label=hue)) +
    geom_tile(fill=df$colors) +
    geom_text() +
    theme_minimal()

  df$colors = hsl_to_rgb(df$hue,0.6,0.7)
  library(ggplot2)
  ggplot(df, aes(x=x,y=y, fill=colors, label=hue)) +
    geom_tile(fill=df$colors) +
    geom_text() +
    theme_minimal()

  df$colors = hsl_to_rgb(df$hue,0.7,0.75)
  library(ggplot2)
  ggplot(df, aes(x=x,y=y, fill=colors, label=hue)) +
    geom_tile(fill=df$colors) +
    geom_text() +
    theme_minimal()

}


compute_cell_df_noreg_hue = function(cell_df) {
  restore.point("compute_cell_df_noreg_hue")
  tabids = unique(cell_df$tabid)

  cell_df$.row.ind = seq_len(NROW(cell_df))
  cmd_dist = 10
  tabid = "2"
  cell_df$noreg_hue = NA_integer_
  for (tabid in tabids) {
    rows = which(cell_df$tabid==tabid & cell_df$match_type >= 5)
    if (length(rows)==0) next
    if (length(rows)==1) {
      cell_df$noreg_hue[rows] = 270
      next
    }
    start = 50; end=310
    col_df = cell_df[rows, c(".row.ind","runid","cmd")] %>%
      arrange(cmd, runid) %>%
      mutate(
        ind = (cumsum(is.true(cmd!=lag(cmd))))*10+
            cumsum(is.true(runid!=lag(runid)))+1,
        max_ind = max(ind),
        hue_pos = ifelse(max_ind>1,((ind-1) / max(ind-1)),0.75),
        hue = end*hue_pos + start*(1-hue_pos)
      )
    cell_df$noreg_hue[col_df$.row.ind] = col_df$hue
  }
  #temp = cell_df %>% select(tabid, text, cellid, match_type, noreg_hue) %>% filter(tabid == "2")

  cell_df

}

explore_reg_num_type_colors = function() {
  df = bind_rows(
    tibble(type="coef",h=360,s=0.5,l=0.7),
    tibble(type="paren", h=360,s=0.8,l=0.8),
    tibble(type="stat", h=360,s=0.8,l=0.7)
  )
  df$color = hsl_to_rgb(df$h,df$s, df$l)
  df$y = 1
  df$x = 1:3

  library(ggplot2)
  ggplot(df, aes(x=x,y=y, label=type)) +
    geom_tile(fill=df$color) +
    geom_text() +
    theme_minimal()

  df$color
}

reg_type_colors = function() {
  c(coef="#D98C8C", paren="#F5A3A3", stat="#F07575")
}

cmd_ind_colors = function(n) {
  if (n==0) return(character(0))
  halton = randtoolbox::halton(n+2, dim=2)
  halton = halton[-c(1,2),,drop=FALSE]
  h = (1-halton[,1])*15 + halton[,1] * 345

  s = 0.9*(1-halton[,2]) + 0.6*(halton[,2])
  #l = 0.5*(0.85*(1-halton[,2]) + 0.7*(halton[,2]))+0.5*(0.85*(1-halton[,3]) + 0.7*(halton[,3]))
  l = 0.85*(1-halton[,2]) + 0.7*(halton[,2])

  hsl_to_rgb(h,s,l)

}

explore_cmd_ind_colors = function() {
  m = 7
  n = m*m
  df = expand.grid(x=1:m, y=1:m)
  df$color = c(cmd_ind_colors(n-3), reg_type_colors())
  df$ind = 1:NROW(df)

  library(ggplot2)
  ggplot(df, aes(x=x,y=y, label=ind)) +
    geom_tile(fill=df$color) +
    geom_text() +
    theme_minimal()

  df$color
}


hsl_to_rgb <- function(h, s, l) {
  restore.point("hsl_to_rgb")
  h <- h / 360
  r <- g <- b <- 0.0
  hue_to_rgb <- function(p, q, t) {
    t[t<0] = t[t<0] + 1.0
    t[t>1] = t[t>1] - 1.0

    res = case_when(
      t<1/6 ~ p + (q - p) * 6.0 * t,
      t<1/2 ~ q,
      t<2/3 ~ p + ((q - p) * ((2/3) - t) * 6),
      TRUE ~ p
    )
    res
  }
  q <- ifelse(l < 0.5, l * (1.0 + s), l + s - (l*s))
  p <- 2.0 * l - q
  r <- hue_to_rgb(p, q, h + 1/3)
  g <- hue_to_rgb(p, q, h)
  b <- hue_to_rgb(p, q, h - 1/3)


  return(rgb(r,g,b))
}
