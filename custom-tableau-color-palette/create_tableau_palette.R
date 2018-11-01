create_tableau_palette <- function(palette_name,
                                   palette_colors,
                                   palette_type) {
  # check palette type
  p_type = match.arg(palette_type,
                     choices = c('ordered-diverging',
                                 'ordered-sequential',
                                 'regular'))
  # starting line
  line_start <- paste0('<color-palette name="',
                       palette_name,
                       '" type="',
                       p_type,
                       '">\n')
  # define colors
  colors <- paste0('<color>',
                   palette_colors,
                   '</color>\n')
  # ending line
  line_end <- "</color-palette>\n"
  # push together
  cat(paste0(c(line_start, colors, line_end)))
}


# short example
# brewer_6 <- RColorBrewer::brewer.pal(6, 'Set1')
# create_tableau_palette(palette_name = "Color Brewer Set1 6",
#                        palette_colors = brewer_6,
#                        palette_type = 'regular')


# longer example
# mag_6 <- viridis::magma(6)
# mag_9 <- viridis::magma(9)
# mag_12 <- viridis::magma(12)
# 
# plas_6 <- viridis::plasma(6)
# plas_9 <- viridis::plasma(9)
# plas_12 <- viridis::plasma(12)
# 
# vir_6 <- viridis::viridis(6)
# vir_9 <- viridis::viridis(9)
# vir_12 <- viridis::viridis(12)
# 
# mapply(function(x, y) 
#   create_tableau_palette(x, y, palette_type = 'ordered-sequential'),
#   x = paste(rep('Viridis', 9), 
#             rep(c('magma', 'plasma', 'viridis'), each = 3),
#             rep(c(6, 9, 12), 3)),
#   y = list(mag_6, mag_9, mag_12,
#            plas_6, plas_9, plas_12,
#            vir_6, vir_9, vir_12))