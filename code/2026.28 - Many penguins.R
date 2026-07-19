# --------------------------- 2026-28 WEEK TIDYTUESDAY SUUBMISSION ------------------------
# This dataset gives morphometric data for 93 penguins from 18 species within 6 genera.
# It was inspired by the now-classic "Palmer penguins data set".
#
# VARIABLES:
# - species:            Penguin species name.
# - genus:              Penguin genus name.
# - shortname:          Abbreviated species name.
# - sex:	              Sex of the individual sampled: M = Male; F = Female; U = Unknown.
# - beak.length_culmen: Length from the tip of the beak to the base of the skull (mm).
# - beak.length_nares:	Length from the anterior edge of the nostrils to the tip of the beak (mm).
# - beak.width:	        Width of the beak at the anterior edge of the nostrils (mm).
# - beak.depth:	        Depth of the beak at the anterior edge of the nostrils (mm).
# - tarsus.length:    	Length of the tarsus from the posterior notch between tibia and tarsus (mm).
# - wing.length:	      Length from the carpal joint (bend of the wing) to the tip of the longest
#                       primary on the unflattened wing (mm).
# - kipps.distance:	    Length from the tip of the first secondary feather to the tip of the longest
#                       primary (mm).
# - secondary1:	        Length from the carpal joint (bend of the wing) to the tip of the first
#                       secondary (mm).
# - hand-wing.index:	  100*DK/Lw, where DK is Kipp’s distance and Lw is wing length (i.e., Kipp’s
#                       distance corrected for wing size). Species average HWI differ from estimates
#                       in Sheard et al. (2020) because of much higher sampling of individuals in
#                       some species, as well as taxonomic effects in the BirdLife list (mm).
# - tail.length:	      Distance between the tip of the longest rectrix and the point at which the
#                       two central rectrices protrude from the skin, typically measured using a
#                       ruler inserted between the two central rectrices (mm).
#
# WEEK'S QUESTIONS:
# - How do trait values covary within/across species and genera?
# - Is there a good way to do ordination/visualization that handles the missingness of some of the
#   traits nicely?
# - Are there interesting ways to visualize these data in >2 dimensions?

setwd(this.path::this.dir())
current = T

# FUNCIONES ----------------------------------------------------------------------------------------
# Pasar de polar (En términos de radianes) a cartesianos
# Aunque creo que en R es posible graficar usando números complejos :D
tocartesians = function(r, theta){
  x = cos(theta)*r
  y = sin(theta)*r
  return(c('x' = x, 'y' = y))
}

# Crear gráficos de radar en términos de proporción:
radarplot <- function(prop, start = 1/4, add = F, max.axis = 1, ...){
  # Cálculo del polígono interior
  axis        <- seq(from = start,to = start+2,length.out = length(prop) + 1) * pi
  axis        <- axis[-c(length(prop) + 1)]
  coordinates <- cbind(prop,axis)
  coordinates <- apply(X = coordinates, MARGIN = 1, (\(x) tocartesians(x[1], x[2]))) |> t()

  # Posición de etiquetas
  labels           <- cbind(max.axis*11/10,axis)
  labels           <- apply(X = labels, MARGIN = 1,
                            (\(x) tocartesians(x[1], x[2]))) |> t()
  rownames(labels) <- names(prop)

  # Cálculo de los ejes
  axis <- cbind(max.axis, axis)
  axis <- apply(X = axis, MARGIN = 1, (\(x) tocartesians(x[1], x[2]))) |> t()

  # Dibujar área gráfica
  if (!add){
    plot(NA,
         xlim = c(-max.axis,max.axis), ylim = c(-max.axis,max.axis),
         bty = 'n', xaxt = 'n', yaxt = 'n',
         xlab = '', ylab = '')
    apply(axis, MARGIN = 1,
          (\(x) lines(rbind(c(0,0), x))))

    # Etiquetas de eje:
    # Las etiquetas siempre son las más dificíles de dibujar XD
    text(x = labels[,1], y = labels[,2], labels = rownames(labels),
         xpd=NA, cex=0.7, font=2)
  }

  # Polígono radar
  polygon(x = coordinates[, 1], y = coordinates[,2], ...)
}


# DATOS: -------------------------------------------------------------------------------------------
dat <- tidytuesdayR::tt_load(2026, week = 28)
dat <- dat$many_penguins


# PLOTTING: ----------------------------------------------------------------------------------------

# SETUP:
# - Debido a la cantidad de datos y de NAS todos los gráficos se discriminarán por género en cambio
#   de por especie
# - Se hará un radarplot de las medidas del pico.
# - Se hará un radarplot de las medidas del cuerpo.
# - Se hará un boxplot del hand wing index.

genus          <- unique(dat$genus)                                            # Género
col.pal        <- RColorBrewer::brewer.pal(n = length(genus), name = 'Dark2')  # Color para cada género
names(col.pal) <- genus

beak.measures <- c('culmen\nlength\n(10cm)'    = 'beak.length_culmen',         # Medidas del pico
                   'nares\nlength\n(10cm)'     = 'beak.length_nares',
                   'width\n(5cm)'              = 'beak.width',
                   'depth\n(5cm)'              = 'beak.depth')
body.measures <- c('tarsus\nlength\n(5cm)'     = 'tarsus.length',              # Medidas del cuerpo
                   'wing\nlength\n(20cm)'      = 'wing.length',
                   'tail\nlength\n(15cm)'      = 'tail.length',
                   'kipps\ndistance\n(1cm)'    = 'kipps.distance',
                   'secondary\nkipps\n(20cm )' = 'secondary1')

beak.means <- sapply(genus,                                           # Medias de los picos
                     FUN = function(x){
                       medias <- dat[dat$genus == x, beak.measures] |>
                         colMeans(na.rm = T)
                       names(medias) <- names(beak.measures)
                       return(medias)
                     }, simplify = T) |> t()
beak.means <- t(t(beak.means)/c(100,100,50,50))

body.means <- sapply(genus,                                           # Medias de los cuerpos
                     FUN = function(x){
                       medias <- dat[dat$genus == x, body.measures] |>
                         colMeans(na.rm = T)
                       names(medias) <- names(body.measures)
                       return(medias)
                     }, simplify = T) |> t()
body.means <- t(t(body.means)/c(50,200,150,10,200))

if (current){
  png(file = '../plots/last week.png', width = 8, height = 6, units = 'in', res = 120)
} else {
  pdf(file = '../plots/2025.28 - Many penguins.pdf', width = 8, height = 6)
}


# DISTRIBUCIÓN DEL ÁREA GRÁFICA:
# A pesar de que esté haciendo todo esto en Rbase, ggplot seguramente sería mejor xd
#
# De arriba hacia abajo el gráfico debería leerse así:
# - Título
# - Descripción
# - A la izquierda los dos gráficos de radar, a la derecha los boxplot
# Un pie de página a la derecha relacionando los géneros con los colores.

l <- rbind(
  rep(1,5),
  c(rep(2,2), rep(4,3)),
  c(rep(2,2), rep(4,3)),
  c(rep(3,2), rep(4,3)),
  c(rep(3,2), rep(4,3))
)

layout(l)

# Título y descripción
par(mar=c(0,0,0,0), oma=c(0,0,0,0))
plot(c(0,0), type='n',
     bty = 'n', xaxt = 'n', yaxt = 'n',
     xlab = '', ylab = '')
mtext(text = 'MANY PENGUINS', side = 3, adj = 0.01, line = -2, font = 2, cex = 1.2)
mtext(text = 'By Mendivenson Barragán', side = 3, adj = 0.99, line = -1.1, font = 2, cex = 0.8,
      col = 'darkgray')
mtext(
  text = "This dataset provides morphometric data for 93 penguins across 18 species and 6 genera.
 Despite missing data, Aptenodytes stands out as having the largest body measurements overall.
 However, Megadyptes shows the highest mean values for Hand-Wing Index and Kipps Distance.",
  side = 3, adj = 0.05, line = -6.5, font = 1, cex = 0.9)

# GRÁFICOS DE RADAR:
par(mar = c(1,5,0,0))

# Medida de los picos
for (gen in genus){
  radarplot(beak.means[gen,],
            add = (gen != genus[1]),
            border = col.pal[gen],
            col = adjustcolor(col.pal[gen], 0.1),
            start = 0,
            lwd = 1.5)
}

mtext(text = 'Beak measurements', side = 2, font=2, line = 3, cex = 0.8)
mtext(text = 'mean by genus', side = 2, font=2, col='gray40',line = 2, cex = 0.7)

# Medida de los cuerpos
for (gen in genus){
  radarplot(body.means[gen,],
            add = (gen != genus[1]),
            border = col.pal[gen],
            col = adjustcolor(col.pal[gen], 0.1),
            lwd = 1.5)
}
mtext(text = 'Body measurements', side = 2, font=2, line = 3, cex = 0.8)
mtext(text = 'mean by genus', side = 2, font=2, col='gray40',line = 2, cex = 0.7)


# Box plot hand wing index
par(mar=c(4.5,2.5,0,0))
boxplot(`hand-wing.index`~genus,data = dat, horizontal = T, ann = F, yaxt='n',
        frame='F', col=adjustcolor(col.pal, alpha.f = 0.4),
        border=col.pal, lwd=2, bg='gray80')
legend('bottom', legend = genus, pt.bg = adjustcolor(col.pal, 0.4), col = col.pal, pt.lwd = 2,
       xpd = NA, pch=22, horiz=T, bg='gray90', box.col = 'gray90',
       pt.cex = 2, inset = c(0,-0.15),
       text.width=graphics::strwidth(genus)+0.05,
       x.intersp = 0.65)

mtext(side=3,text = 'Hand-Wing index', font=2, cex= 0.8)
mtext(text = 'by genus', side = 3, font=2, col='gray40',line =-1, cex = 0.7,xpd=NA)
dev.off()
