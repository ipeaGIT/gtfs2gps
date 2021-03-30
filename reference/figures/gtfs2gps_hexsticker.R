library(hexSticker) # https://github.com/GuangchuangYu/hexSticker
library(ggplot2)
library(gridExtra)
library(grid)
library(gtable)
library(sysfonts)


# add special text font
sysfonts::font_add_google(name = "Roboto", family = "Roboto")



### create empty df: 3 x 4
col1 <- c(rep("       ", 4))
df <- data.frame(col1, col1, col1, col1)
print(df)

# remove col names
colnames(df) <- NULL
print(df)



### Table theme paramters
myt <- ttheme_default(
  # Rows: fill and colors
  core = list(bg_params=list(fill = NA, col="gray99" ) ),
  
  # Column header
  colhead = list(bg_params=list(fill = NA, col="gray99"))
)


### Edit table

# create base table
g <- tableGrob(df, rows = NULL, theme = myt)

# add rectangles to emphasize border
# geral
g <- gtable_add_grob(g, grobs = rectGrob(gp = gpar(fill = NA, col="gray99",lwd = 9)),
                     t = 1, b = nrow(g), l = 1, r = ncol(g))
# header
g <- gtable_add_grob(g, grobs = rectGrob(gp = gpar(fill = NA, col="gray99", lwd = 8)),
                     t = 1, l = 1, r = ncol(g))

grid.draw(g)



### save table
png(filename = "./man/figures/table.png", width=400, height=260, bg = "transparent", res=200)
grid.draw(g)
dev.off()



### Create Hex sticker


img_address <- "./man/figures/table.png"

sticker( img_address,
        package="gtfs2gps", p_color="gray99", p_size=22, p_family= "Roboto",
        s_x=1, s_y=.8, s_width=.6, s_height=.6, # ggplot image size and position
        h_fill="#009a9a", h_color="#006767", h_size=2, # hexagon #283748 
        
        spotlight=TRUE, l_x=20, l_y=4, l_width=2, l_height=2,
        filename="./man/figures/gtfs2gps_logo.png", dpi=300)  # output name and resolution

