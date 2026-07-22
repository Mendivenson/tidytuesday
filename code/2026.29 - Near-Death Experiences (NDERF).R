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

# Función para barplot circular desde (0,0):
circ.barplot <- function(start = 0, end = pi/2, r = c(0.5, 0.6, 0.7), r_int = 0.5, ...){

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

# Validación del Grayson Score relacionado con narrativa:
narrative <- lapply(c('Validated' = 'Validated',
                      'Non-validated' = 'Non-validated'),
       (\(x) density(dat[dat$greyson_score == x,]$narrative_length, na.rm = T)))

# PLOTTING -----------------------------------------------------------------------------------------
# Al contrario de la división usual de los gráficos, el gráfico deseado en este oportunidad sigue una
# disposición no usual por lo que la disposición se maneja directamente en el código de cada una de
# las gráficas que componen la infografía.

pdf('../plots/2026.29 - Near-Death Experiences (NDERF).pdf', width=8, height = 8)
par(bg = 'black', mar = c(0,0,0,0))
plot(0, xlim = c(-1.8,1.8), ylim = c(-1.8,1.8), type='n')

circ.barplot(start= pi/18, end= 17*pi/18,
             r=types["Non-validated",]/nrow(dat),r_int = 0.75,
             col = "darkred", lwd=1., border="white")
circ.barplot(start= pi/18, end= 17*pi/18,
             r=types["Validated",]/nrow(dat),r_int = 0.75,
             col = "darkgreen", lwd = 1.5, border="white")

circ.barplot(start= 19*pi/18, end= 23*pi/18,
             r=genre["Non-validated",]/nrow(dat),r_int = 0.75,
             col = 'darkred', lwd=1., border='white')
circ.barplot(start= 19*pi/18, end= 23*pi/18,
             r=genre["Validated",]/nrow(dat),r_int = 0.75,
             col = 'darkgreen', lwd = 1.5, border='white')

circ.barplot(start= 31*pi/18, end= 35*pi/18,
             r=country["Non-validated",]/nrow(dat),r_int = 0.75,
             col = 'darkred', lwd=1., border='white')
circ.barplot(start= 31*pi/18, end= 35*pi/18,
             r=country["Validated",]/nrow(dat),r_int = 0.75,
             col = 'darkgreen', lwd = 1.5, border='white')


symbols(0, 0, circles = 0.7, add=T, bg='black',
        fg='black', inches = F)

for(i in seq(0.1,0.6,length.out=50)) {
  symbols(0, 0, circles = i, add=T, bg=adjustcolor('white',1/(20*i)),
          fg=adjustcolor('white',1/(20*i)), inches = F)
}
# points(x = 0, y = 0, col=adjustcolor('white', 1/i), cex=i/0.3, pch = 16)
text(x = 0, y = 0.1, 'NEAR DEATH\nEXPERIENCES', col = 'black', font=2, cex=1.3)
text(x = 0, y = -0.1, 'Based on the Greyson Scale each\ncase is classified as:',
     col = 'black', font=1, cex=0.75)
legend(x=0,y=-0.15,legend = c('Validated (>6)', 'Non-validated (<7)'),
       pt.bg = c('darkgreen', 'darkred'), pch=22, x.intersp = 0.7,
       bty ='n', xjust = 0.5, text.font = 1, cex=0.7, pt.cex = 1.6)


symbols(0, 0, circles = 1.75, add=T,
        fg='gold', lty = 'solid', inches = F, lwd = 1)

cases_rect <- rbind(
  tocartesians(1.7, 5.8 * pi/4),
  tocartesians(1.8, 5.8 * pi/4),
  tocartesians(1.8, 6.2 * pi/4),
  tocartesians(1.7, 6.2 * pi/4)
)

polygon(x = cases_rect[,1], y = cases_rect[,2], col = 'black', border = 'black')

cases_rect <- colMeans(cases_rect)
text(x = cases_rect["x"], y = cases_rect["y"], label = paste0(nrow(dat), ' reports'),
     col = 'gold', font=2)

dev.off()
