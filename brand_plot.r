brand_plot <-
function(myplot,
         logo_png_path=NULL,
         color_scheme=c("#0a5096", "#b4290c", "#594522"),
         mytheme=theme_classic()) {
# Adopted from https://stackoverflow.com/a/12486301/3423480
    require(grid)
    if (!is.null(logo_png_path)) {
        require(png)
        img <- readPNG(logo_png_path)
        logo <- rasterGrob(img, interpolate=TRUE)
    }
    myplot <-
        myplot +
        mytheme +
        scale_color_manual(values=color_scheme) +
        scale_fill_manual(values=color_scheme)
   
    # Set the size of the viewport to contain the logo
    size <- unit(0.1, "npc")
    
    # Set up the layout for grid 
    # heights = unit.c(size, unit(1, "npc") - size)
    heights <- unit.c(unit(1, "npc") - size, size)
    widths <- unit.c(unit(1, "npc") - size * 1.25, size)
    lo <- grid.layout(2, 2, widths = widths, heights = heights)
    # Show the layout
    grid.show.layout(lo)
    
    # Position the elements within the viewports
    grid.newpage()
    pushViewport(viewport(layout = lo))
    
    # The plot
    pushViewport(viewport(layout.pos.row=1:2, layout.pos.col = 1:2))
    print(myplot, newpage=FALSE)
    popViewport()
    
    # The logo
    if (!is.null(logo_png_path)) {
        pushViewport(viewport(layout.pos.row=2, layout.pos.col = 2))
        print(grid.draw(logo), newpage=FALSE)
        popViewport()
        popViewport()
    }
    
    # To save the object
    g <- grid.grab()
   
    return(g) 
}

brand_plot.test_map <- function()
{
    require(dplyr)
    require(ggplot2)  # Must be 2.2.1+
    require(maps)
    require(gridExtra)
   
    us_map <- map_data("state")
    maptheme <-
        theme_classic() +
        theme(line=element_blank(),
              axis.text=element_blank(),
              axis.title=element_blank())
    
    logo_png_path <- "./anelen-logo.png"  # Replace this with your file
    
    # Source:
    # https://github.com/vega/vega-tutorials/blob/master/tutorials/airports/Tutorial.md
    flights <- read.csv("http://vega.github.io/vega-tutorials/airports/data/flights-airport.csv")
    airports <- read.csv("http://vega.github.io/vega-tutorials/airports/data/airports.csv")

   
    flight_paths <-
        merge(flights, airports, by.x="origin", by.y="iata")
    colNames <- colnames(flight_paths)
    colNames[8] <- "y"
    colNames[9] <- "x"
    colnames(flight_paths) <- colNames
    flight_paths <-
        merge(flight_paths, airports, by.x="destination", by.y="iata")
    colNames <- colnames(flight_paths)
    colNames[14] <- "yend"
    colNames[15] <- "xend"       
    colnames(flight_paths) <- colNames
  
    flight_paths$bound <-
        ifelse(flight_paths$x - flight_paths$xend > 0,
               "west",
               "east")
        
    flight_map <-
        ggplot() +
        geom_polygon(data=us_map, aes(x=long, y=lat, group=group),
                     color="gray",
                     fill=NA,
                     size=0.5,
                     alpha=.35) +
        geom_curve(data=filter(flight_paths, count > quantile(flight_paths$count, 0.95)),
                   aes(x=x, y=y, xend=xend, yend=yend, size=count, color=bound),
                   alpha=0.25,
                   arrow=arrow(length=unit(0.01,"npc"))) +
        xlim(-125,-68) + ylim(24, 55) +
        # guides(color=FALSE) +
        scale_size_continuous(range=c(0.1, 2.0)) +
        ggtitle(label="Top 5% flights by direction and count",
                subtitle="Data: United States Air Traffic System (2008)\n(https://github.com/vega/vega-tutorials/blob/master/tutorials/airports/Tutorial.md)")

    flight_hist =
        ggplot(data=filter(flight_paths, count > quantile(flight_paths$count, 0.95))) +
        geom_histogram(aes(x=count, fill=bound), alpha=0.75, position="stack") +
        labs(x="# of flights of each course", y="count (west + east bounds)")
    
    maptheme =
        theme_classic() +
        theme(line=element_blank(),
              axis.text=element_blank(),
              axis.title=element_blank())
    g1 <- brand_plot(flight_map, mytheme=maptheme)
    g2 <- brand_plot(flight_hist, logo_png_path=logo_png_path) 
    grid.newpage()
    grid.arrange(g1, g2, nrow=2)
}
