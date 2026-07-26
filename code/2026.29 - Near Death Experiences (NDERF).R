# --------------------------------------- 2026-29 WEEK TIDYTUESDAY SUBMISSION ----------------------
# This week we're exploring near-death experiences (NDEs) reported to the Near Death Experience
# Research Foundation (NDERF). The dataset contains 589 individual NDE records scraped from the
# NDERF Search site, which embeds structured JSON metadata for each experience. Each record includes
# \demographics, a Greyson NDE Scale score, and AI-detected experience features. No narrative text
# is included in the extracted dataset, respecting NDERF's copyright.
#
# Near-death experiences are reported by 10–23% of cardiac arrest survivors in prospective studies.
# They typically involve out-of-body perception, a feeling of peace, seeing a bright light, and
# encountering deceased relatives. The Greyson NDE Scale (0–32) is the standard validated instrument
# for measuring NDE depth. Score of 7 or higher indicates a genuine NDE.
#
# VARIABLES:
# - entry_id          : Unique NDERF experience identifier (numeric page ID on search.nderf.org).
# - gender	          : 	Gender of the experiencer (M or F).
# - classification    : 	NDERF classification of the experience (NDE, Probable NDE, Possible NDE,
#                         etc.). Multiple values separated by semicolons.
# - country	          : 	Country of the experiencer, detected by AI from the narrative text.
# - category	        : 	Experience category assigned by NDERF (e.g., NDE, FDE, STE, OBE).
# - language	        : 	Language of the submitted narrative (e.g., english, french, spanish).
# - greyson_score	    :  	Score on the Greyson NDE Scale (0–32). A score of 7 or higher indicates a
#                         validated near-death experience.
# - post_date	        : 	Date the experience was submitted to NDERF.
# - exp_date	        : 	Date the near-death experience occurred (self-reported).
# - narrative_length	: 	Character count of the narrative text (proxy for level of detail in the
#                         account).
# - ai_obe	          : 	Whether AI detected an out-of-body experience in the narrative.-
# - ai_unity	        : 	Whether AI detected a feeling of unity or oneness in the narrative.
# - ai_hellish	      : 	Whether AI detected distressing or hellish imagery in the narrative.
# - ai_clinical	      : 	Whether AI detected confirmed clinical death in the narrative.
# - ai_esp	          : 	Whether AI detected extrasensory perception or seeing distant events in the
#                         narrative.
# - ai_past_lives	    : 	Whether AI detected past life recall in the narrative.
# - ai_world_future	  : 	Whether AI detected visions of the world's future in the narrative.
# - ai_aliens	        : 	Whether AI detected alien or extraterrestrial encounters in the narrative.
#
# WEEK'S QUESTIONS:
# * What features most commonly co-occur in NDEs? Are out-of-body experiences correlated with ESP or unity?
# * How does the Greyson score distribution differ between genders or countries?
# * Are distressing NDEs more common in certain demographics or time periods?
# * How has the rate of NDERF submissions changed over time (1999–2025)?
# * Do deeper NDEs (higher Greyson scores) tend to have longer narratives?

setwd(this.path::this.dir())

# FUNCIONES ----------------------------------------------------------------------------------------
# Al igual que la semana pasada se generan funciones para tratar sobre todo con coordenadas polares.

# Pasar de coordenadas polares a cartesianas:
tocartesians = function(r, theta){
  x = cos(theta)*r
  y = sin(theta)*r
  return(c('x' = x, 'y' = y))
}

# Prototipo texto alrededor de un círculo: Seguramente en el futuro lo cambie a alinear la parte de
# abajo de cada una de las cajas que contienen las letras con el radio dado. Al principio estaba
# diseñado para tomar el ancho de cada letra, pero las letras se renderizaban de forma bastante
# extraña. Por eso es que se hacen operaciones innecesarias
polar_text <- function(r = 1, theta = 6 * pi/4, lab = 'text', cex = 1, ...){

  lab <- strsplit(lab, '')[[1]]

  xy <- graphics::strwidth('m') * cex / 2 |>  rep(length = length(lab))
  xy <- cbind('x' = xy,
              'y' = sqrt(r^2 - xy^2))

  lab <- rev(lab)

  angle <- apply(xy, MARGIN = 1,
                 FUN = function(x){
                   pi/2 - atan2(x = x[1], y = x[2])
                 })
  angle <- cumsum(rep(angle, each = 2))
  angle <- angle[1:length(angle) %% 2 == 1]

  theta <- theta - rev(angle)[1]/2

  angle  <- theta + angle
  polars <- cbind(r, angle)
  coords <- apply(X = polars,
                  MARGIN = 1,
                  FUN = \(x) tocartesians(x['r'], x['angle'])) |>
    t()

  for (letter in 1:length(lab)){
    text(x = coords[letter,1], y = coords[letter,2], labels = lab[letter],
         srt = (angle[letter] * 180/pi) + 270,
         cex = cex, ...)
  }

}


# Función para barplot circular desde (0,0):
circ.barplot <- function(start = 0, end = pi/2, r = c(0.5, 0.6, 0.7), r_int = 0.5,
                         labels = c(), collab = 'black', cexlab = 0.2,
                         fontlab = 2, ...){

  # Cálculo de los ángulos en que dividir el círculo
  segs <- length(r)
  theta <- seq(start, end, length.out = segs+1)

  # Completando los polígonos (Intersección entre las barras)
  theta <- c(theta[1], rep(theta[-c(1,segs+1)], each=2), theta[segs+1])
  r <- rep(r, each = 2)
  r <- ifelse(r == 0, 0, r + r_int)

  # Pasando de cartesianas a polares.
  polares <- cbind(r = r, theta = theta)
  breaks <- apply(polares,
                  MARGIN = 1,
                  (\(x) tocartesians(r = x['r'], theta = x['theta']))) |>
    t()

  # Agregar los segmentos de regreso al origen
  breaks <- lapply(seq(2, nrow(breaks), by = 2),
                   (\(x) breaks[(x-1):x,]))
  breaks_origin <- c()
  for (section in breaks){
    breaks_origin <- rbind(breaks_origin, section, c(0,0))
  }
  breaks_origin <- rbind(c(0,0), breaks_origin)

  # Dibujar el polígono
  polygon(x = breaks_origin[,1], y = breaks_origin[,2], ...)

  if (length(labels) > 0){
    theta <- c()
    for (i in seq(2, nrow(polares), by=2)){
      theta <- rbind(theta, polares[(i-1),"theta"] + (polares[i,"theta"] - polares[(i-1),"theta"])/2)
    }
    polares <- cbind(unique(r+0.05),theta)
    coords  <- apply(polares,
                     MARGIN = 1,
                     (\(x) tocartesians(r = x[1], theta = x[2]))) |>
      t()
    for (i in 1:length(labels)){
      def <- par()$lheight
      par(lheight = 0.75)
      text(x = coords[i,1], y = coords[i,2], labels = labels[i],
           srt = (theta[i] * 180/pi) + 270, xpd = NA,
           col = collab, cex=cexlab, font = fontlab)
      par(lheight = def)
    }
  }
}

# Función para mapear cualquier polígono alrededor de un círculo
polar_polygon <- function(start = pi, end = 2*pi, x = c(1,2,3,4,5), y = c(.5,.6,.7,.8,1.2),
                          x.max = max(x), x.min = min(x), y.max = max(y), y.min = min(y), r = 3, r.int = 2, ...){

  # Para encontrar las posiciones de x en el círculo
  fc <- end-start

  # Mapear x al rango (0,1)
  x <- (x - x.min)/x.max

  # Breaks del polígono
  theta <- start + x*fc

  # Mapear y al rango de r
  r <- r.int + r*(y - y.min)/y.max

  # Generar las coordenadas cartesianas
  coords <- cbind(r, theta)
  coords <- apply(coords, MARGIN = 1,
                  FUN = function(x){
                    tocartesians(r = x[1], theta = x[2])
                  }) |>  t()
  coords <- rbind(c(0,0), coords)

  # Dibujar el polígono
  polygon(coords[,1], coords[,2], ...)
}

# DATOS --------------------------------------------------------------------------------------------
dat <- tidytuesdayR::tt_load(2026, week = 29)$nde_experiences
dat$greyson_score <- ifelse(dat$greyson_score >= 7, 'Validated', 'Non-validated')

# Validación del Grayson Score por GÉNERO:
genre <- table(dat[,c("greyson_score", "gender")])
genre["Non-validated",] <- colSums(genre)
genre <- genre[c('Validated', 'Non-validated'),]


# Validación del Grayson Score po PAÍS:
# Debido a la cantidad de países con un solo caso se genera la categoría otros y USA.
dat$country <- ifelse(dat$country == 'United States', 'United States', 'Other')
country <- table(dat[,c("greyson_score", "country")])
country["Non-validated",] <- colSums(genre)
country <- country[c('Validated', 'Non-validated'),]


# Validación del Grayson Score por TIPO:
# Aún cuando un solo caso puede tener más de un tipo se separan para facilidad de graficación
# (i.e. La suma de todos los tipos puede ser mayor al total de casos)
types <- c("out of\nbody" = "ai_obe",
           "unity" = "ai_unity",
           "infernal" = "ai_hellish",
           "clinical\ndeath" = "ai_clinical",
           "extrasensory\nperception" = "ai_esp",
           "past life\nrecall" ="ai_past_lives",
           "visions of\nfuture" = "ai_world_future",
           "alien\nencounters" = "ai_aliens")

types <- lapply(types,
       FUN = function(x) {
         dat[dat[[x]] == TRUE,]$greyson_score |>
           factor(levels = c('Validated','Non-validated')) |>
           table()
       })
types <- do.call(cbind, types)
types["Non-validated",] <- colSums(types)
types <- types[,types["Non-validated",] |> sort(decreasing = F) |> names()]

# Validación del Grayson Score relacionado con narrativa:
narrative <- lapply(c('Validated' = 'Validated',
                      'Non-validated' = 'Non-validated'),
       (\(x) density(dat[dat$greyson_score == x,]$narrative_length, na.rm = T, from=0, )))

# PLOTTING -----------------------------------------------------------------------------------------
# Al contrario de la división usual de los gráficos, el gráfico deseado en este oportunidad sigue una
# disposición no usual por lo que la disposición se maneja directamente en el código de cada una de
# las gráficas que componen la infografía.

col.pal <- c(
  'bg'     = 'black',
  'val'    = '#8FA831',
  'no.val' = '#8B3A2B',
  'border' = '#D4C08C',
  'light'  = 'lightgoldenrod1',
  'light2' = '#C9A227')


pdf('../plots/2026.29 - Near Death Experiences (NDERF).pdf', width=8, height = 8)
# png(file = '../plots/last week.png', width = 8, height = 8, units = 'in', res = 150)
# Fondo negro para dejar la temática de luz al final de l túnel :D
par(bg = 'black', mar = c(0,0,0,0))
plot(0, xlim = c(-1.8,1.8), ylim = c(-1.8,1.8), type='n')

# BARPLOTS:
# Tipo de experiencia cerca a la muerte detectada por IA
circ.barplot(start= pi/18, end= 17*pi/18, col = col.pal["no.val"], lwd=1., border= col.pal["no.val"],
             r=types["Non-validated",]/nrow(dat),r_int = 0.75,
             collab='white')  # WTF
circ.barplot(start= pi/18, end= 17*pi/18, col = col.pal["val"], lwd = 1.5, border=col.pal["val"],
             r=types["Validated",]/nrow(dat),r_int = 0.75,
             collab='white')

# Género reportado
circ.barplot(start= 19*pi/18, end= 23*pi/18,
             r=genre["Non-validated",]/nrow(dat),r_int = 0.75,
             col = col.pal["no.val"], lwd=1., border=col.pal["no.val"],
             collab='white')
circ.barplot(start= 19*pi/18, end= 23*pi/18,
             r=genre["Validated",]/nrow(dat),r_int = 0.75,
             col = col.pal["val"], lwd = 1.5, border=col.pal["val"],
             collab='white')

# País reportado
circ.barplot(start= 31*pi/18, end= 35*pi/18,
             r=country["Non-validated",]/nrow(dat),r_int = 0.75,
             col = col.pal["no.val"], lwd=1., border=col.pal["no.val"],
             collab='white')
circ.barplot(start= 31*pi/18, end= 35*pi/18,
             r=country["Validated",]/nrow(dat),r_int = 0.75,
             col = col.pal["val"], lwd = 1.5, border=col.pal["val"],
             collab='white')

# Bordes y labels en blanco para que no se pierda en el fondo negro
circ.barplot(start= pi/18, end= 17*pi/18,
             r=types["Non-validated",]/nrow(dat),r_int = 0.75,
             lwd=1, border=col.pal["border"],
             labels=colnames(types), collab = col.pal["border"], cexlab = 0.7)

circ.barplot(start= 19*pi/18, end= 23*pi/18,
             r=genre["Non-validated",]/nrow(dat),r_int = 0.75,
             lwd=1., border=col.pal["border"],
             labels=colnames(genre), collab = col.pal["border"], cexlab = 0.7)


circ.barplot(start= 31*pi/18, end= 35*pi/18,
             r=country["Non-validated",]/nrow(dat),r_int = 0.75,
             lwd=1., border=col.pal["border"],
             labels=colnames(country), collab = col.pal["border"], cexlab = 0.7)


# HISTOGRAMA CONTEO DE CARÁCTERES EN NARRATIVA:
polar_polygon(start = 25*pi/18, end = 30*pi/18,
              x = narrative$Validated$x, y = narrative$Validated$y,
              x.min = 0, y.min = 0,
              x.max = max(c(narrative$`Non-validated`$x,
                            narrative$Validated$x)),
              y.max = max(c(narrative$`Non-validated`$y,
                            narrative$Validated$y)),
              col = adjustcolor(col.pal["val"], 0.5),
              border = col.pal["val"], lwd = 1.2,
              r.int = 0.72, r =0.6)

polar_polygon(start = 25*pi/18, end = 30*pi/18,
              x = narrative$`Non-validated`$x, y = narrative$`Non-validated`$y,
              x.min = 0, y.min = 0,
              x.max = max(c(narrative$`Non-validated`$x,
                            narrative$Validated$x)),
              y.max = max(c(narrative$`Non-validated`$y,
                            narrative$Validated$y)),
              col = adjustcolor(col.pal["no.val"], 0.5),
              border = col.pal["no.val"], lwd = 1.2,
              r.int = 0.72, r =0.6)


# TÍTULO CENTRAL:

# Difuminado
symbols(0, 0, circles = 0.7, add=T, bg=col.pal['bg'],fg=col.pal["bg"], inches = F)
for(i in seq(0.1,0.575,length.out=50)) {
  symbols(0, 0, circles = i, add=T, bg=adjustcolor(col.pal["light"],1/(20*i)),
          fg=adjustcolor(col.pal["light"],1/(20*i)),
          inches = F)
}
# Texto
text(x = 0, y = 0.1, 'NEAR DEATH\nEXPERIENCES', col = '#2E1D0F', font=2, cex=1.3)
text(x = 0, y = -0.1, 'Based on the Greyson Scale each\ncase is classified as:',
     col = '#2E1D0F', font=1, cex=0.75)
legend(x=0,y=-0.15,legend = c('Validated (> 6)', expression("Non-validated ("<="7)")),
       pt.bg = col.pal[c("val", "no.val")], col = col.pal["border"],pch=22, x.intersp = 0.7, y.intersp = 1,
       bty ='n', xjust = 0.5, text.font = 1, cex=0.7, text.col = '#2E1D0F', pt.cex = 1.7)


# CÍRCULO EXTERNO (Total de reportes)
symbols(0, 0, circles = 1.75, add=T,
        fg=col.pal["light2"], lty = 'dashed', inches = F, lwd = 1.5)

cases_rect <- rbind(
  tocartesians(1.7, 5.8 * pi/4),tocartesians(1.8, 5.8 * pi/4),
  tocartesians(1.8, 6.2 * pi/4),tocartesians(1.7, 6.2 * pi/4))
polygon(x = cases_rect[,1], y = cases_rect[,2], col = col.pal["bg"], border = col.pal["bg"])
cases_rect <- colMeans(cases_rect)
text(x = cases_rect["x"], y = cases_rect["y"], label = paste0(nrow(dat), ' reports'),
     col = col.pal["light2"], font=2)


# TÍTULO DE CADA SECCIÓN:
polar_text(r = .64, theta = pi/2, lab = 'TYPE OF NARRATIVE (detected by AI)',
           cex = 0.8, col=col.pal["light2"], font=2)

polar_text(r = .64, theta = 3.5 * pi/3, lab = 'GENDER',
           cex = 0.8, col=col.pal["light2"], font=2)

polar_text(r = .64, theta = 3 * pi/2, lab = 'NARRATIVE LENGTH',
           cex = 0.75, col=col.pal["light2"], font=2)

polar_text(r = .64, theta =(5.5*pi)/3, lab = 'COUNTRY',
           cex = 0.8, col=col.pal["light2"], font=2)

# Texto :D
rect(ytop = 2, ybottom = 1.62, xright = -2, xleft = 2, col = 'black')
mtext(side=3,
      text = '   Data from 589 NDERF accounts (1999–2025). Gender, country, narrative length, and type\nshow no distinct validated vs. non-validated pattern.',
      col = col.pal["light2"], line = -2, adj = 0.05, font = 2)
mtext(side = 3, adj = 0.065, line = -3, font = 1, cex = 0.85,
      text = ' Visualization: Mendivenson Barragán • Data curation: Anthony Galvan • Data: NDERF',
      col = adjustcolor(col.pal["light"], 0.5))

dev.off()
