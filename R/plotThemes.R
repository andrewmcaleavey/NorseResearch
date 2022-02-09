## Plot themes

# This is a basic theme for plotting on computers
# Based on theme_bw(), but without gridlines
norse_theme_bw <- ggplot2::theme_bw() +
  ggplot2::theme(panel.grid = ggplot2::element_blank(),
                 panel.grid.minor = ggplot2::element_blank())
theme_bw_norse <- norse_theme_bw
