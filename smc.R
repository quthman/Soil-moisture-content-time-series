setwd("C:/Users/uthma/Dropbox (UFL)/PhD/Wave/January-2023-FAWN")
dat = read.csv("smc.csv")
dat = data.frame(dat)
dat$doy=as.Date(dat$doy,"%m/%d/%Y")

## Visualization
## load the theme
library(extrafont)
library(ggplot2)
#loadfonts(device = "win")
#fonts() ### See the names of fonts available
#names(pdfFonts())
#https://github.com/wch/extrafont/issues/88 (issues to fix fonts)
science_theme <- theme(plot.background = element_rect(fill = "white"), 
											 panel.background = element_rect(fill="white"), 
											 panel.grid.major = element_blank(),
											 panel.grid.minor = element_blank(), 
											 axis.line = element_line(size = 0.7, color = "black"), 
																				 axis.text=element_text(size = 12, 
											 											 family="Microsoft Sans Serif",
											 											 color = "black"),
											 text = element_text(size = 12, 
											 										family="Microsoft Sans Serif",
											 										color = "black"),
											 strip.background.x = element_rect("white"),
											 strip.background.y = element_rect("white"))

datn=reshape2::melt(dat, id.var="doy")
library(ggplot2)
utils::View(datn)
levels(datn$variable)
levels(datn$variable)=c("0-15","15-30","30-45","45-60")
A=ggplot(datn,aes(x=doy, y=value, col=variable))+
	geom_line()+
	geom_hline(yintercept = 0.09, color="black",linetype="dashed")+
	geom_hline(yintercept = 0.38, color="black", linetype="dashed")+
	facet_grid(variable~.)+
	scale_color_discrete(name="Soil depth, cm",
											 labels=c("0-15","15-30","30-45","45-60"))+
	labs(y=expression(Volumetric~water~content ~ theta[v] ~~ cm^3~cm^-3), 
			 x = "Day of the year")+
	scale_x_date(date_labels = "%m-%Y")+theme_bw()+science_theme
ggsave(A,file=paste0("smc.png"), width = 6, height = 4, dpi=500)
