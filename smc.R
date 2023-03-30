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

# Reshaping the 'dat' data frame into long format, with 'doy' as the identifier variable
datn=reshape2::melt(dat, id.var="doy")
# Loading the ggplot2 package 
library(ggplot2)
# Viewing the reshaped data in a spreadsheet-like interface
utils::View(datn)
# Retrieving the unique levels of the 'variable' column in the 'datn' data frame
levels(datn$variable)
# Renaming the levels of the 'variable' column to more meaningful soil depths
levels(datn$variable)=c("0-15","15-30","30-45","45-60")
# Creating a line plot of volumetric water content against time for each soil depth
A=ggplot(datn,aes(x=doy, y=value, col=variable))+
	geom_line()+
	# Adding two horizontal dashed lines to represent the critical soil moisture levels
	geom_hline(yintercept = 0.09, color="black",linetype="dashed")+
	geom_hline(yintercept = 0.38, color="black", linetype="dashed")+
	# Faceting the plot by soil depth
	facet_grid(variable~.)+
	# Add a discrete color scale with a custom name and labels for soil depth
	scale_color_discrete(name="Soil depth, cm",
											 labels=c("0-15","15-30","30-45","45-60"))+
	# Set y-axis label with units and variable name in math format
	labs(y=expression(Volumetric~water~content ~ theta[v] ~~ cm^3~cm^-3), 
			 x = "Day of the year")+
	# Set date format for x-axis
	scale_x_date(date_labels = "%m-%Y")+
	# Set plot theme to black and white
	theme_bw()+
	# Set scientific theme for better visualization
	science_theme
# Save the plot with specified filename, size, and resolution
ggsave(A,file=paste0("smc.png"), width = 6, height = 4, dpi=500)
