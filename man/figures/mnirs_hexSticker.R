library(hexSticker)
library(sysfonts)
library(showtext)

font_families_google()[which(grepl("Merriweather", font_families_google()))]
font_add_google("Merriweather Sans", "merriweather_sans")
font_add_google("Merriweather", "merriweather")

showtext_auto()

hexSticker::sticker(
    ## imported image
    subplot = r"(C:\Users\Jem\Downloads\mnirs-transparent.png)",
    s_x = 1, # centred horizontally
    s_y = 1, # slightly below centre to leave room for text
    s_width = 1.0, # fill most of the hex width
    s_height = 0.7, # constrain height proportionally

    ## package name parameters
    package = "mnirs",
    p_x = 1,
    p_y = 1.5,
    p_size = 25,
    p_color = "#1a284f",
    p_family = "merriweather_sans",
    p_fontface = 2, # "plain", "bold", "italic", "bolditalic"

    ## hex geometry
    h_fill = "#bcced7", # deep navy — contrasts orange muscle
    h_color = "#fb653a",
    # h_color = "#1a284f",
    h_size = 1.5,
    white_around_sticker = FALSE,

    ## spotlight / white lines echo — leave off unless you want glow
    # spotlight = TRUE,
    # l_x = 1,
    # l_y = 0.35,

    ## ggsave
    ## `*mnirs-hex*.png` will be gitignored
    filename = "man/figures/mnirs-hex.svg",
    dpi = 300
)
