graph_template_fleet_f <- function (input_data, x_axis, y_axis, col, title_x, title_y, title_graph, color_palette, colorCount) {
  #getPalette = colorRampPalette(brewer.pal(8, color_palette))
  ggplot(input_data, aes_(as.name(x_axis), as.name(y_axis), col = as.name(col))) + geom_line(size = 2)+
    labs(x = title_x, 
         y = title_y,
         color = "Legend")+
    #scale_colour=getPalette(colorCount)+
    scale_color_manual(values = colorRampPalette(brewer.pal(8, color_palette))(colorCount))+
    #scale_colour_brewer(palette = color_palette)+
    theme(text = element_text(size = 30))+
    theme(panel.background = element_rect(fill = 'white', color = 'purple'),
          panel.grid.major = element_line(color = 'black', linetype = 'dotted'),
          panel.grid.minor = element_line(color = 'black', size = 0.5, linetype = 'dotted'),
          panel.border = element_rect(colour = "black", fill=NA, size=2))+
    scale_x_continuous( 
                       #limits = c(1970, 2050), 
                       expand = c(0, 0))+
                       #breaks = seq(1970, 2050, by = 10))+
    scale_y_continuous( 
                       #limits = c(0, 1.1*round(max(input_data))), 
                       expand = c(0, 0))+
                       #breaks = seq(0, 300, by = 50))+
    ggtitle(title_graph)
}

