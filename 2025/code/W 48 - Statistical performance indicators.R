# ------------------------------ 48 WEEK TIDYTUESDAY'S SUBMISSION ----------------------------------
# The World Bank has developed Statistical Performance Indicators (SPI) to monitor the statistical
# performance of countries. The SPI focuses on five key dimensions of a country’s statistical
# performance:
#     (i) data use
#     (ii) data services
#     (iii) data products
#     (iv) data sources
#     (v) data infrastructure
# This set of countries covers 99 percent of the world population. The data extend from 2016-2023, 
# with some indicators going back to 2004.
# 
# The data was curated by Nicola Rennie (https://github.com/nrennie)
# 
# The questions are: 
#   - How has the statistical performance of a country changed over time?
#   - Is statistical performance related to a country's income level or population?
#   - Which pillar do countries score lowest in?
# 
# The dataset is composed by this columns: 
# 
# variable	                      class	                                description
# iso3c	                        character	                            ISO3 country code.
# country	                      character                               Country name.
# region	                      character	                              Region name.
# income	                      character                          Income level of country.
# year	                         integer	                                 Year.
# population	                    double	                          Population of the country.
# overall_score	                  double	                     Overall statistical performance score.
# data_use_score	                double	                     Score relating to Pillar 1 - Data use.
# data_services_score	            double                    Score relating to Pillar 2 - Data services.
# data_products_score	            double	                   Score relating to Pillar 3 - Data products.
# data_sources_score	            double                     Score relating to Pillar 4 - Data sources.
# data_infrastructure_score	      double	               Score relating to Pillar 5 - Data infrastructure.

# SETTING WORKING SPACE ----------------------------------------------------------------------------
# Necesito aprender a usar la librería here para hacer este tipo de cosas :D

save.pdf = T                                                  # Save pdf and dont plot in rstudio?

library(extrafont)                                            # Charge installed fonts
# font_import()
# loadfonts(device = "win")
par(family = 'Fira Code')

wd <- paste0(Sys.getenv('HOME'),'/TidyTuesday/2025')          # Working directory
setwd(wd)
dat <- tidytuesdayR::tt_load(2025, week = 48)                  # Load data
dat <- dat$spi_indicators

# Simplify names
colnames(dat) <- c('iso', 'country', 'region', 'income', 'year', 'population', 'overall', 'data use',
                  'data services', 'data products', 'data sources', 'data infraestructure')

# SETTING PLOTTING GRID AND SAVING METHOD ----------------------------------------------------------
# All plots are organized as sort of a poster and saved in a pdf file. The plot is arranged in vertical 
# sections where each section pretends to answer one of the week's questions:
#   * How has the statistical performance of a country changed over time?
#   * Is statistical performance related to a country's income level or population?
#   * Which pillar do countries score lowest in?

l = rbind(# Main title and data description
          rep(1, 4),
          rep(1, 4),
          
          # Boxplot/Barplot: Statistical performance related to income level?
          c(rep(2, 3), 3), c(rep(2, 3), 3), c(rep(2, 3), 3), c(rep(2, 3), 3), c(rep(2, 3), 3),
          c(rep(2, 3), 3), c(rep(2, 3), 3), c(rep(2, 3), 3), c(rep(2, 3), 3), c(rep(2, 3), 3),
          
          # Conclusions
          rep(4, 4),
          rep(4, 4), 
          
          # Scatter plot: Statistical performance related to population level? 
          c(rep(5, 4)),     
          6:9, 6:9, 6:9, 6:9, 6:9,
          6:9, 6:9, 6:9, 
          
          # Conclusions
          rep(10, 4),
          rep(10, 4), 
          
          # Time series plots: Comparison between pillars
          rep(11,4), rep(11,4), rep(11,4), rep(11,4), rep(11,4),
          rep(11,4), rep(11,4), rep(11,4), rep(11,4), rep(11,4),
          
          # Conclusions
          rep(12, 4),
          rep(12, 4), 
          
          # Colombian case: 
          c(rep(13, 4)),     
          c(rep(14, 3), 15), c(rep(14, 3), 15), c(rep(14, 3), 15), c(rep(14, 3), 15), c(rep(14, 3), 15),
          c(rep(14, 3), 15), c(rep(14, 3), 15), c(rep(14, 3), 15), c(rep(14, 3), 15), c(rep(14, 3), 15),
          
          # Signature and data information
          16:19)

if (save.pdf) pdf('plots/W 48 - Statistical Performance Indicators.pdf', width = 14, height = (4/10) * nrow(l), family = 'Fira Code')
# png('plots/last week.png', width = 14, height = (4/10) * nrow(l), family = 'Fira Code', units = 'in', res = 180)

layout(l)

# GENERAL TITLE AND DESCRIPTION OF DATA ------------------------------------------------------------

par(mar = c(0,0,0,0))
plot.new()
text(x = 0.5, y = 0.8, labels = 'Statistical Performance Indicators (SPI)',
     font = 4, col = 'darkred', cex = 2.5, xpd = T)
text(x = 0.5, y = 0.3, cex = 1.6, font = 2,
     labels = paste('The World Bank has developed Statistical Performance Indicators (SPI) to monitor the statistical performance of countries.',
                    'The SPI monitors statistical performance across five dimensions: data use, services, products, sources, and infrastructure.', sep = '\n'))


# STATISTICAL PERFORMANCE AND INCOME LEVEL ---------------------------------------------------------
# Two plots are drawn: Boxplot of overall score grouped by level income and organized by year in
# addition to a line representing the mean of overall score in each  group as a time serie

# ---> BOXPLOT: The No classified category has only two countries and the income classification does
#               not change over time.

# Preparing the data
temp <- dat[dat$income != 'Not classified',]
temp$income <- factor(x = temp$income, levels = rev(c('High income', 'Upper middle income', 
                                                      'Lower middle income', 'Low income')))

# Draw plotting area
par(mar = c(4.5,2.5,5.5,0))
bp <- boxplot(overall ~ year + income, data = temp,  
              frame.plot = F, xaxt = 'n', yaxt = 'n', ylab = '', xlab = '', main = '',
              xlim = c(0.5, 32))

# Gray background
rec <- par()$usr 
rect(xleft = rec[1], ybottom = rec[3], xright = rec[2], ytop = rec[4], 
     col = 'gray92', border = 'gray92')

# Axis' names and title 
title(xlab = 'Country\'s income', font.lab = 2, cex.lab = 1, line = 3)
title(ylab = 'Overall Score', font.lab = 2, cex.lab = 1, line = 1.2)
title(main = 'Statistical Performance Score by Income Level and Year', line = 3, cex.main = 1.9, col.main = 'darkblue')

# Coordinates for auxiliary lines and labels positions
x.coord <- strsplit(bp$names, '\\.') |> do.call(what = rbind)
x.coord <- table(x.coord[,2]) |>  cumsum() - 3.5
names(x.coord) = strsplit(bp$names, '\\.') |> 
  do.call(what = rbind) |> 
  (\(x) x[,2])() |> 
  unique() |> 
  gsub(pattern = ' ', replacement = '\n')

z.coord <- 1:length(bp$n)
names(z.coord) <- strsplit(bp$names, '\\.') |> 
  do.call(what = rbind) |> 
  (\(x) x[,1])()

# Axis' labels
axis(1, at = x.coord, labels = names(x.coord), tick = F, line = -1.3, cex.axis = 1, padj = 1)
axis(2, tick = F, line = -0.8, las = 2, cex.axis = 1)
axis(3, at = z.coord, labels = names(z.coord), tick = F, line = -0.6, cex.axis = 0.8, las = 2)
axis(3, at = z.coord[1] - 1, labels = 'Year', font = 2, cex.axis = 1, tick = F, las = 2, line = -0.6)

# Guide/Auxiliary lines
abline(h = 0:10 * 10, col = 'white', lwd = 1.5)
abline(v = c(0.5,x.coord + 4), col = 'black', lwd = 1.5, lty = 'dashed')
abline(v = 1:length(bp$n), col = 'white', lwd = 1.2, lty = 'solid')

# Color palette
colores = RColorBrewer::brewer.pal(n = length(unique(temp$income)), name = 'Set1') |>
  rep(each = strsplit(bp$names, '\\.') |> 
        do.call(what = rbind) |> 
        (\(x) x[,1])() |> 
        unique() |> 
        length())

# Actual boxplot
boxplot(overall ~ year + income, data = temp, add = T, 
        col = adjustcolor(colores, 0.8), border = 'black',
        lwd = 1.5, 
        xaxt = 'n', yaxt = 'n', ylab = '', xlab = '', frame.plot = F, staplewex = 0, main = '', 
        lty = 'solid', pch = 16, cex = 0.8)

# Mean time series:
names(x.coord) <- names(x.coord) |> gsub(pattern = '\n', replacement = ' ')
x.coord <- x.coord + 3.5

init <- 1
colores <- unique(colores)
names(colores) <- names(x.coord)

for (i in names(x.coord)){
  y.coord <- temp[temp$income == i,] |> 
    dplyr::group_by(year) |> 
    dplyr::summarise(mean = mean(overall, na.rm = T)) |> 
    dplyr::select(mean) |> 
    unlist()  |> 
    (\(x) x[!is.na(x)])()
  
  # A contour of the line is plotted for readability
  lines(x = init:(init + 7), y = y.coord,
        lwd = 3, col = 'gray92')
  lines(x = init:(init + 7), y = y.coord,
        lwd = 1, col = colores[i])
  init <- init + 8
}

# Legend
legend('topleft', cex = 0.9, inset = c(0.03, -0.01), bty = 'n',
       # bg = 'gray92', box.col = 'gray92', text.width = -1,
       legend = 'Each line represents the average\noverall score per year for an\nincome category.')


# ---> BARPLOT: Just as auxiliary information a barplot of number of countries by income level is plotted. 

# Preparing the data
dat$income <- factor(x = dat$income,  levels = rev(c('Not classified','High income', 'Upper middle income', 
                                                     'Lower middle income', 'Low income')))
# Color palette
colores <- RColorBrewer::brewer.pal(n = length(unique(dat$income)), name = 'Set1')

# Draw the plotting area
par(mar = c(4.5,1,5.5,5))
bp <- barplot(table(dat$income[dat$year == '2023']), 
              space = 0.08, horiz = T, las = 2, yaxt = 'n', xaxt = 'n',
              xlim = c(max(table(dat$income[dat$year == '2023'])) + 5, 0))

# Gray background
rec <- par()$usr 
rect(xleft = rec[1], ybottom = rec[3], xright = rec[2], ytop = rec[4], 
     col = 'gray92', border = 'gray92')

# Axis' labels and names; and title 
title(main = 'Distribution of countries\nby income level',
      adj = 1, line = 1.5, cex.main = 1.68, col.main = 'darkblue')
title(xlab = 'No. of countries', font.lab = 2, cex.lab = 1, line = 3)

axis(4, at = bp, labels = levels(dat$income) |>  gsub(pattern = ' ', replacement = '\n'), 
     las = 2,  cex.axis = 0.9, tick = F, line = -0.8)
axis(1, cex.axis = 1, tick = F, line = -0.2)

# Guide/Auxiliary lines 
abline(v = 0:10 * 10, col = 'white', lwd = 1.5)

# The actual barplot
par(lwd = 1.5)
barplot(table(dat$income[dat$year == '2023']), col = adjustcolor(colores, 0.8), border = 'black', 
        space = 0.08, horiz = T, las = 2, yaxt = 'n', xaxt = 'n',
        add = T)
par(lwd = 1)

# Conclusion: Is statistical performance related to a country's income level?
par(mar = c(0,0,0,0))
plot.new()
text(x = 0.5, y = 0.45, cex = 1.5, font = 1,
     labels = paste("Income and performance are closely linked, and all income groups show improvement over time. The weakest-performing years",
                    "of high-income countries still tend to outperform the best years of low-income countries as a group. However, some",
                    "high-income countries record overall scores closer to the median of low-income countries than to that of their own income group.",
                    sep = '\n'))


# STATISTICAL PERFORMANCE AND POPULATION -----------------------------------------------------------
# In this case, the population varies across countries each year. Therefore, a simple scatter plot 
# is proposed for each of the last four recorded years. A logarithmic transformation is applied to 
# the population variable to account for the extremely large populations of China and India, which 
# would otherwise distort the plot’s scale and reduce its readability.

# Title
par(mar = c(0,0,0,0))
plot.new()
text(x = 0.5, y = 0.5, cex = 1.9, labels = 'Population x overall score (By year 2019-2023)', 
     font = 2, col = 'darkblue')

# Preparing the data
temp <- dat[!is.na(dat$overall),]
years <- sort(unique(temp$year)) |>  rev() |> (\(x) x[1:4])() |> rev()

# Color palette
color = RColorBrewer::brewer.pal(n = 9, name = 'Set1')[5]


for (i in years){
  
  # Draw plotting area
  par(mar = c(4,4,3,1))
  plot(overall ~ log(population), data = temp[temp$year == i,],
       bty = 'n', xaxt = 'n', yaxt = 'n', 
       ylab = 'Overall Score', xlab = 'Log(Population)', 
       main = i, cex.main = 1.5, font.lab = 2, cex.lab = 1)
  
  # Axis' labels
  axis(1, tick = F, line = -0.5); axis(2, tick = F, line = -0.5)
  
  # Gray background
  rec <- par()$usr 
  rect(xleft = rec[1], ybottom = rec[3], xright = rec[2], ytop = rec[4], 
       col = 'gray92', border = 'gray92') 
  grid(col = 'white', lwd = 2, lty = 'solid')
  
  # The actual scatter plot
  points(overall ~ log(population), data = temp[temp$year == i,],
         pch = 16, cex =1.2,
         col = ifelse(temp[temp$year == i,]$country %in% c('China', 'India'), 'blue',color))
  
  # India and China labels
  text(x = temp[temp$year == i & temp$country %in% c('China', 'India'),]$population |> log() - 0.2, 
       y = temp[temp$year == i & temp$country %in% c('China', 'India'),]$overall - 2,
       labels = temp[temp$year == i & temp$country %in% c('China', 'India'),]$country, 
       col = 'blue', font = 2, cex = 0.8) 
}

# Conclusion
par(mar = c(0,0,0,0))
plot.new()
text(x = 0.5, y = 0.48, cex = 1.5, font = 1,
     labels = paste("Over the last four years, a positive correlation between population size and statistical performance appears to emerge.", 
                    "However, due to the high dispersion of data around the middle of the population distribution, it is difficult to draw a",
                    "definitive conclusion. The logarithmic transformation was applied to prevent the population levels of India and China from",
                    "distorting the plot.",
                    sep = '\n'))

# PERFORMANCE FOR EACH PILLAR: ---------------------------------------------------------------------
# One could argue that the ts.category object is unnecesary but filtering each time is required to
# draw a line takes so much time :D

# Preparing the data
categories <- c('data use', 'data products', 'data sources', 'data services', 'data infraestructure')
years_by_cat <- list()
for (category in categories){
  years_by_cat[[category]] <- dat$year[!is.na(dat[[category]])] |>  unique() |> sort()
}
no_years <- c(0,sapply(years_by_cat, length))


# Base plot
par(mar = c(5,4,5,1) + 0.1, font.lab = 2, cex.lab = 1.2, cex.axis = 1.2)
plot(0,0, type = 'n', xlim = c(1, rev(cumsum(no_years))[1]), ylim = c(0, 100), 
     xlab = 'Time', ylab = 'Score', bty = 'n', xaxt = 'n', yaxt = 'n')

# Title
title(main = 'Pillar\'s performance over time', line = 3.6, col.main = 'darkblue', cex.main = 1.9)

# Axis' labels
axis(2, tick = 'F', line = -0.5)
axis(1, tick = F, at = 1:(rev(cumsum(no_years))[1]), labels = unlist(years_by_cat), las = 2, 
     line = -0.5, cex.axis = 1.05)
axis(3, tick = F, at = (cumsum(no_years) - no_years/2)[-1], line = -0.5,
     labels = gsub(pattern = ' ', replacement = '\n', categories) |> stringr::str_to_title(), font = 2) 

# Gray background
rec <- par()$usr 
rect(xleft = rec[1], ybottom = rec[3], xright = rec[2], ytop = rec[4], 
     col = 'gray92', border = 'gray92') 


# Guide lines
abline(h = 0:10 * 10, col = 'white', lwd = 1.5)
abline(v = 1:(rev(cumsum(no_years))[1]), col = 'white', lwd = 1.5)
for (i in cumsum(no_years) + 0.5){
  lines(x = c(i,i), y = c(-12, rec[4]), lty = 'solid', xpd = T, lwd = 1.5)
}

# Color palette
colores = RColorBrewer::brewer.pal(n = length(categories), name = 'Dark2')
colores2 <- c("#006400", "#8B4513","#4B0082", "#8B0A50","#228B22")
names(colores) <- categories
for (i in 1:length(categories)){
  category <- categories[i]
  temp <- dat[,c("year", "iso", category)]
  temp <- temp[order(temp$year),]
  temp <- temp[!is.na(temp[[category]]),]
  iso <- temp$iso |> unique()
  years <- years_by_cat[[category]]
  
  ts.category <- c()
  for (id in iso){
    aux <- temp[temp$iso == id, c('year', category)] |> as.data.frame()
    nombres <- aux[['year']]
    aux <- aux[[category]]
    names(aux) <- nombres
    
    ts.category <- cbind(ts.category, aux[years |> as.character()])
  }
  
  x.coord <- seq(rev(cumsum(no_years[1:i]))[1] + 1, rev(cumsum(no_years[1:(i + 1)]))[1])
  for (country in 1:ncol(ts.category)){
    lines(x = x.coord, y = ts.category[,country],
          col = adjustcolor(colores[i], 0.1), lwd = 1.8)
  }
  
  mu <- apply(ts.category, MARGIN = 1, \(x) mean(x, na.rm = T))
  lines(x = x.coord, y = mu, col = colores2[i], lwd = 2.5)
}

# Conclusions
par(mar = c(0,0,0,0))
plot.new()
text(x = 0.5, y = 0.48, cex = 1.5, font = 1,
     labels = paste(
       "The darker line shows the average score for each year and pillar.",
       "Overall, all pillars exhibit an upward trend over time, particularly the data use index. But again, the wide",
       "dispersion within each pillar makes it difficult to draw firm conclusions. Country-level analysis could",
       "provide more meaningful insights of the performance over time for each country.",
       sep = '\n'))

# THE COLOMBIAN CASE -------------------------------------------------------------------------------
# IN consequence with the conclusions of the latter plot, a plot for the colombian case :D 

# Title
par(mar = c(0,0,0,0))
plot.new()
text(x = 0.5, y = 0.5, cex = 1.9, labels = 'The colombian case', 
     font = 2, col = 'darkblue')

# Plot area
par(mar = c(5,4,1,1) + 0.1)
temp <- dat[dat$country == 'Colombia',]
plot(0,0, ylim = c(0,100), xlim = range(temp$year), bty = 'n', xaxt = 'n', yaxt = 'n', 
     xlab = 'Time', ylab = 'Score')
axis(2, tick = F, line = -0.5)
axis(1, at = temp$year |> unique() |> sort() |>  rev(), labels = temp$year |> unique(), 
     tick = F, line = -0.5)

# grau background
rec <- par()$usr 
rect(xleft = rec[1], ybottom = rec[3], xright = rec[2], ytop = rec[4], 
     col = 'gray92', border = 'gray92') 

# Guide lines
abline(h = 0:10 * 10, col = 'white', lwd = 1.5)
abline(v = temp$year |> unique(), col = 'white', lwd = 1.5)

# Actual plot
for (category in categories){
  lines(x = temp$year, y = temp[[category]], col = colores[category], lwd = 2)
}

# Leyenda
legend('bottomright', inset = c(0.05, 0.05), legend = categories |> stringr::str_to_title(), 
       ncol = 2,  lty = 'solid', lwd = 2.5, col = colores, bg = 'gray92')

# Conclusions
par(mar = c(0,0,0,0))
plot.new()
text(x = 0.5, y = 0.55, cex = 1.5, font = 1,
     labels = paste(
       "In the case of Colombia, there is",
       "a clear upward trend in overall",
       "statistical performance",
       "particularly in the Data Use and",
       "Data Infrastructure pillars, which",
       "show the most notable gains.",
       "However, progress has been slower",
       "in the areas of Data Products and",
       "Data Services, suggesting that",
       "improvements in data quality and",
       "service delivery have not kept",
       "pace with infrastructure growth.",
       sep = '\n'))


# DATA INFORMATION AND SIGNATURE -------------------------------------------------------------------
par(mar = c(0,0,0,0))
plot.new()
text(x = 0, y = 0.5, cex = 1.2, labels = 'Week 48/52 - TidyTuesday submission', font = 2, adj = 0,
     col = rgb(0.5, 0.5, 0.5, alpha = 0.7))

plot.new()
text(x = 0.5, y = 0.5, cex = 1.2, labels = 'Data curated by Nicola Rennie', font = 2, 
     adj = 0.5, col = rgb(0.5, 0.5, 0.5, alpha = 0.7))

plot.new()
text(x = 0.5, y = 0.5, cex = 1.2, labels = 'Data published by World Bank ', font = 2, 
     adj = 0.5, col = rgb(0.5, 0.5, 0.5, alpha = 0.7))

plot.new()
text(x = 1, y = 0.5, cex = 1.2, labels = 'By Mendivenson Barragán', font = 2,
     adj = 1, col = rgb(0.5, 0.5, 0.5, alpha = 0.7))

if (save.pdf) dev.off()