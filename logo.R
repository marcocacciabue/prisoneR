library("hexSticker")



s<-sticker("logo/logo.svg",
        package="prisoneR", 
        p_size=50, 
        s_x=1, 
        s_y=0.9,
        s_width=0.5, 
        s_height=0.5,
        h_fill="#f7f7f7",
        h_color="black",
        p_color="black",
        url="https://github.com/marcocacciabue/prisoneR",
        u_color="black",
        p_y=1.55,
        u_size=8,
        filename="logo/hex.png",
        dpi= 800)
plot(s)

