# =============================================================================
#  Módulo de reporte EDA - Compras públicas de la región Puno (Perú Compras)
#  Curso: Business Intelligence & Data Analytics - Actividad semana 2
#  Ejecutar:  shiny::runApp("shiny")   (desde la carpeta ACTIVIDAD SEMANA 2)
# =============================================================================
library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(readr)
library(plotly)
library(DT)
library(scales)
library(sf)
library(leaflet)

# ---- Datos ------------------------------------------------------------------
ruta <- if (file.exists("../data/compras_puno_2025_2026.csv")) {
  "../data/compras_puno_2025_2026.csv"
} else {
  "data/compras_puno_2025_2026.csv"
}
compras <- read_csv(ruta, show_col_types = FALSE,
                    col_types = cols(ruc_entidad = "c", ruc_proveedor = "c", ubigeo_entidad = "c",
                                     fecha = col_date()))

# Límites distritales de Puno (INEI). San Miguel (211105) se creó en 2016 a partir de
# Juliaca y no está en ese mapa: en el mapa se dibuja sumado a Juliaca.
ruta_geo <- sub("compras_puno_2025_2026.csv", "ref/geo/puno_distritos.geojson", ruta, fixed = TRUE)
mapa_puno <- st_read(ruta_geo, quiet = TRUE)
ubigeo_mapa <- function(u) ifelse(u == "211105", "211101", u)
titulo <- function(x) tools::toTitleCase(tolower(x))

# ---- Paleta -----------------------------------------------------------------
# Ocho tonos categóricos en orden fijo (probados para daltonismo). Cada sector y
# cada origen de empresa tiene SIEMPRE el mismo color, aunque se filtre.
AZUL <- "#2a78d6"; NARANJA <- "#eb6834"; AQUA <- "#1baf7a"; AMARILLO <- "#eda100"
MAGENTA <- "#e87ba4"; VERDE <- "#008300"; VIOLETA <- "#6250d6"; ROJO <- "#e34948"
CATEG <- c(AZUL, NARANJA, AQUA, AMARILLO, MAGENTA, VERDE, VIOLETA)
GRIS  <- "#9a9893"
TINTA <- "#1f2340"
TINTA2 <- "#5b5f77"
REJILLA <- "#e9ecf3"

# Escalas secuenciales (un solo tono, de claro a oscuro)
ESC_AZUL <- list(c(0, "#eef5fe"), c(0.15, "#cde2fb"), c(0.35, "#86b6ef"),
                 c(0.6, "#3987e5"), c(0.8, "#1c5cab"), c(1, "#0d366b"))
# Para barras (cada barra debe verse sobre el fondo blanco: empieza en un azul medio)
ESC_AZUL_BARRAS <- list(c(0, "#86b6ef"), c(0.4, "#3987e5"), c(0.75, "#1c5cab"), c(1, "#0d366b"))
ESC_NARANJA <- list(c(0, "#fff4ee"), c(0.2, "#fcd5c0"), c(0.45, "#f5a47c"),
                    c(0.7, "#eb6834"), c(0.88, "#bf4a1b"), c(1, "#83300f"))

sectores <- names(sort(table(compras$sector), decreasing = TRUE))
sectores <- c(setdiff(sectores, "Otras entidades"), intersect("Otras entidades", sectores))
COL_SECTOR <- setNames(c(CATEG, GRIS)[seq_along(sectores)], sectores)
COL_SECTOR[names(COL_SECTOR) == "Otras entidades"] <- GRIS

COL_ORIGEN <- c("Lima y Callao" = AZUL, "Otras regiones" = NARANJA, "Puno" = AQUA)
otros_origen <- setdiff(unique(compras$origen_proveedor), names(COL_ORIGEN))
COL_ORIGEN <- c(COL_ORIGEN, setNames(rep(GRIS, length(otros_origen)), otros_origen))

rgba <- function(hex, a) {
  v <- col2rgb(hex)
  sprintf("rgba(%d,%d,%d,%.2f)", v[1], v[2], v[3], a)
}

fmt_soles <- function(x) {
  ifelse(abs(x) >= 1e6, paste0("S/ ", number(x / 1e6, accuracy = 0.01, big.mark = " ", decimal.mark = ","), " M"),
         ifelse(abs(x) >= 1e3, paste0("S/ ", number(x / 1e3, accuracy = 0.1, big.mark = " ", decimal.mark = ","), " mil"),
                paste0("S/ ", number(x, accuracy = 1, big.mark = " "))))
}
fmt_n <- function(x) number(x, big.mark = " ", accuracy = 1)
pct <- function(x, acc = 0.1) percent(x, accuracy = acc, decimal.mark = ",")
corta <- function(x, n) ifelse(nchar(x) > n, paste0(substr(x, 1, n - 2), "…"), x)

# ---- Estilo común de los gráficos -------------------------------------------
estilo <- function(p, ytitulo = NULL, xtitulo = NULL, leyenda = TRUE) {
  p |>
    layout(
      font = list(family = "Inter, Segoe UI, sans-serif", size = 12, color = TINTA2),
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
      xaxis = list(title = list(text = if (is.null(xtitulo)) "" else xtitulo, font = list(size = 11)),
                   gridcolor = REJILLA, zeroline = FALSE, linecolor = "#d5d9e4", tickfont = list(color = TINTA2)),
      yaxis = list(title = list(text = if (is.null(ytitulo)) "" else ytitulo, font = list(size = 11)),
                   gridcolor = REJILLA, zeroline = FALSE, tickfont = list(color = TINTA2)),
      margin = list(l = 10, r = 10, t = 10, b = 10),
      hoverlabel = list(bgcolor = "white", bordercolor = "#d5d9e4",
                        font = list(color = TINTA, family = "Inter, Segoe UI, sans-serif")),
      legend = list(orientation = "h", x = 0, y = 1.12, font = list(size = 11, color = TINTA2)),
      showlegend = leyenda,
      separators = ", "
    ) |>
    config(displayModeBar = FALSE, locale = "es")
}

# Barras horizontales; 'color' puede ser un color o un vector de valores para escala
barras_h <- function(df, etiqueta, valor, texto_hover, color = AZUL, escala = NULL) {
  df$.et <- factor(df[[etiqueta]], levels = rev(unique(df[[etiqueta]])))
  df$.v <- df[[valor]]
  marcador <- if (is.null(escala)) list(color = color) else
    list(color = df$.v, colorscale = escala, cmin = 0, cmax = max(df$.v), showscale = FALSE)
  plot_ly(df, x = ~.v, y = ~.et, type = "bar", orientation = "h",
          marker = marcador, text = texto_hover, hoverinfo = "text", textposition = "none") |>
    estilo(leyenda = FALSE) |> layout(bargap = 0.28)
}

# Gráfico de anillo con un texto en el centro
dona <- function(etiquetas, valores, colores, centro, hover) {
  plot_ly(labels = etiquetas, values = valores, type = "pie", hole = 0.66, sort = FALSE,
          direction = "clockwise", marker = list(colors = colores, line = list(color = "#ffffff", width = 2)),
          textinfo = "percent", textposition = "inside", insidetextfont = list(color = "#ffffff", size = 11),
          text = hover, hoverinfo = "text") |>
    estilo() |>
    layout(legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.05),
           annotations = list(list(text = centro, showarrow = FALSE, x = 0.5, y = 0.5,
                                   font = list(size = 14, color = TINTA))))
}

# Mini gráfico de tendencia dentro de las tarjetas KPI
mini_tendencia <- function(x, y) {
  plot_ly(x = x, y = y, type = "scatter", mode = "lines", fill = "tozeroy",
          line = list(color = "rgba(255,255,255,0.95)", width = 2, shape = "spline"),
          fillcolor = "rgba(255,255,255,0.18)", hoverinfo = "none") |>
    layout(xaxis = list(visible = FALSE, fixedrange = TRUE), yaxis = list(visible = FALSE, fixedrange = TRUE),
           margin = list(l = 0, r = 0, t = 0, b = 0), paper_bgcolor = "rgba(0,0,0,0)",
           plot_bgcolor = "rgba(0,0,0,0)", hovermode = FALSE, showlegend = FALSE) |>
    config(displayModeBar = FALSE, staticPlot = TRUE)
}

# ---- Tema y CSS ---------------------------------------------------------------
# Contorno de la región Puno (límites INEI, simplificado) para el logo del encabezado
SILUETA_PUNO <- HTML(paste0(
  '<svg viewBox="0 0 100 100" width="42" height="42" aria-label="Región Puno">',
  '<path d="', "M38.7,68.9L37.9,68.0L36.2,68.1L34.9,69.0L32.6,67.9L30.1,63.8L30.8,62.2L30.5,60.8L28.7,59.3L28.4,58.3L28.8,56.7L27.9,55.7L28.4,54.3L29.5,53.8L29.7,53.2L29.0,52.3L29.5,51.6L28.5,46.4L29.1,41.9L28.3,41.4L27.5,41.7L26.1,39.5L28.5,37.8L29.0,38.0L28.6,36.4L29.3,35.1L29.0,34.5L29.3,33.3L31.0,31.3L30.6,30.4L30.8,29.3L32.5,28.2L32.9,27.2L32.0,27.0L31.3,26.1L31.9,22.5L30.9,21.9L31.3,21.2L32.0,21.8L33.3,21.5L34.2,21.8L34.7,21.3L33.9,19.8L34.3,18.2L35.6,18.1L36.4,17.1L38.0,16.3L38.1,15.3L39.8,13.6L40.0,12.4L40.9,12.0L41.1,10.7L42.0,10.3L40.5,9.5L41.1,9.0L40.8,6.4L56.7,11.3L73.0,4.0L73.4,5.2L72.9,6.4L73.5,9.2L72.5,13.8L70.4,17.8L69.0,18.0L68.9,18.7L70.8,20.2L70.7,20.7L72.0,21.4L71.3,22.8L70.7,23.1L70.8,24.9L72.5,26.2L73.9,30.0L70.9,30.2L70.5,30.6L70.0,32.1L70.6,33.4L67.4,36.4L67.2,37.8L65.7,37.8L65.5,39.4L65.0,39.8L65.6,41.2L65.3,41.6L64.0,41.8L62.9,42.5L63.1,44.3L62.8,45.7L64.4,48.8L65.4,49.4L67.6,51.8L66.6,52.4L64.9,54.8L65.1,55.9L64.3,56.1L63.4,58.3L62.2,57.7L62.9,57.1L61.3,57.1L58.5,54.4L57.9,54.7L56.2,52.0L55.4,52.0L54.2,53.4L53.3,53.4L53.0,52.8L51.7,53.1L51.5,54.4L50.7,55.3L52.4,58.8L54.3,60.9L54.7,62.4L53.8,62.4L52.7,61.5L52.4,60.1L51.8,59.5L51.0,60.1L51.3,60.2L50.4,61.5L49.2,61.6L48.8,62.3L49.0,63.4L50.3,65.1L52.0,65.9L52.4,66.8L53.4,65.5L52.7,65.3L52.8,64.1L53.6,63.6L54.3,66.0L56.4,67.3L58.6,67.4L61.0,68.2L61.5,69.2L59.9,71.4L59.7,72.1L60.1,72.7L61.5,72.1L62.9,73.6L64.2,73.7L64.5,74.4L67.3,73.6L69.2,72.4L70.2,72.9L70.6,72.3L71.1,72.6L70.3,74.1L69.7,73.9L69.3,74.3L69.8,75.9L69.4,77.2L68.6,78.1L68.6,78.9L70.4,82.3L66.8,83.8L65.8,86.1L62.2,90.2L62.6,91.2L61.9,91.8L61.0,91.8L58.7,93.4L57.1,95.8L56.0,95.6L55.2,96.0L53.9,94.7L52.8,94.9L48.9,92.4L48.7,90.9L46.6,88.4L47.7,87.4L47.8,85.7L49.7,83.9L49.1,81.8L47.8,82.3L46.1,81.3L45.8,79.8L43.1,78.2L42.7,77.1L42.1,77.1L41.7,76.6L42.6,75.2L42.7,72.6L42.1,72.8L41.3,71.5L40.4,69.1L40.0,69.4L50.9,50.1L49.7,50.4L48.5,49.2L47.1,50.4L50.5,51.5L51.8,51.3L45.2,62.1L45.4,63.4L46.4,63.0Z", '" fill="#fff" stroke="rgba(255,255,255,.55)" ',
  'stroke-width="1.5" stroke-linejoin="round"/></svg>'))

# Fondo «de agua»: olas suaves en capas, fijas al pie de la pantalla
OLAS <- paste0(
  "data:image/svg+xml;utf8,",
  "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 1440 320' preserveAspectRatio='none'>",
  "<path fill='%232a78d6' fill-opacity='.20' d='M0,192L60,176C120,160,240,128,360,138.7C480,149,600,203,720,208C840,213,960,171,1080,149.3C1200,128,1320,128,1380,128L1440,128L1440,320L0,320Z'/>",
  "<path fill='%232fb7a7' fill-opacity='.20' d='M0,256L80,240C160,224,320,192,480,197.3C640,203,800,245,960,250.7C1120,256,1280,224,1360,208L1440,192L1440,320L0,320Z'/>",
  "<path fill='%231f2470' fill-opacity='.14' d='M0,288L90,277.3C180,267,360,245,540,250.7C720,256,900,288,1080,293.3C1260,299,1350,277,1395,266.7L1440,256L1440,320L0,320Z'/>",
  "</svg>")
css <- "
body { background-color: #e9f2fb;
       background-image: url(\"OLAS_URL\"),
         radial-gradient(ellipse at 12% 0%, rgba(42,120,214,.16) 0, transparent 45%),
         radial-gradient(ellipse at 95% 25%, rgba(47,183,167,.14) 0, transparent 40%),
         radial-gradient(ellipse at 50% 100%, rgba(98,80,214,.10) 0, transparent 55%),
         linear-gradient(180deg, #f1f6fc 0%, #e3eef9 100%);
       background-repeat: no-repeat; background-attachment: fixed;
       background-position: bottom center, 0 0, 0 0, 0 0, 0 0;
       background-size: 100% 380px, 100% 100%, 100% 100%, 100% 100%, 100% 100%; }
.bslib-page-main, .bslib-sidebar-layout > .main { background: transparent !important; }
/* Encabezado */
.navbar { background: linear-gradient(100deg, #10173a 0%, #1f2470 45%, #2b2a8c 70%, #2a6fd0 100%) !important;
          box-shadow: 0 4px 18px rgba(20,27,61,.28); padding-top: 10px; padding-bottom: 10px;
          border-bottom: 1px solid rgba(255,255,255,.08); }
.navbar > .container-fluid { align-items: center; flex-wrap: nowrap; }
.navbar-brand { margin-right: 16px; padding: 0; display: flex; align-items: center; }
.navbar-collapse { align-items: center; }
.selectize-control.multi .selectize-input > .item { background: #eef3fd !important; color: #1c5cab !important;
          border-radius: 6px; border: 0 !important; font-weight: 500; }
.marca { display: flex; align-items: center; gap: 12px; }
.marca-logo { width: 54px; height: 54px; border-radius: 15px; display: inline-flex; align-items: center;
              justify-content: center; font-size: 1.15rem; color: #fff;
              background: linear-gradient(135deg, #3d8bff 0%, #8a6cff 100%);
              box-shadow: 0 4px 14px rgba(61,139,255,.45), inset 0 1px 0 rgba(255,255,255,.35); }
.marca-tit { font-size: 1.22rem; font-weight: 700; color: #fff; letter-spacing: .6px; white-space: nowrap; line-height: 1.2; }
.marca-tit em { font-style: normal; color: #9cc4ff; }
.marca-sub { font-size: .74rem; white-space: nowrap; line-height: 1.3; color: rgba(255,255,255,.62); font-weight: 400; letter-spacing: .2px; margin-top: 2px; }
.navbar .navbar-nav { background: rgba(255,255,255,.07); border: 1px solid rgba(255,255,255,.1);
                      border-radius: 14px; padding: 4px; gap: 2px; }
.navbar .nav-link { font-weight: 500; font-size: .8rem; border-radius: 10px; padding: 6px 10px !important;
                    color: rgba(255,255,255,.72) !important; transition: background .15s, color .15s; }
.navbar .nav-link:hover { background: rgba(255,255,255,.1); color: #fff !important; }
.navbar .nav-link.active { background: #fff !important; color: #1f2470 !important; font-weight: 600;
                           box-shadow: 0 2px 8px rgba(0,0,0,.18); }
.navbar .nav-link.active i, .navbar .nav-link.active svg { color: #2a78d6; }
.navbar .navbar-nav .nav-item.corte .nav-link, .corte-badge { background: transparent; }
.corte-badge { display: inline-flex; align-items: center; gap: 7px; font-size: .75rem; color: rgba(255,255,255,.85);
               border: 1px solid rgba(255,255,255,.22); border-radius: 999px; padding: 5px 12px; white-space: nowrap; }
.corte-badge .punto { width: 7px; height: 7px; border-radius: 50%; background: #36d39a;
                      box-shadow: 0 0 0 3px rgba(54,211,154,.25); }
/* Lateral */
.bslib-sidebar-layout > .sidebar { background: rgba(255,255,255,.72); backdrop-filter: blur(10px); border-right: 1px solid rgba(42,120,214,.12); }
.bslib-sidebar-layout > .sidebar > .sidebar-content { padding-top: 14px; gap: 12px; }
.sidebar .control-label { font-weight: 600; color: #3a3f5c; font-size: .8rem; margin-bottom: 4px; }
.sidebar .form-group { margin-bottom: 10px; }
.filtros-cab { display: flex; align-items: center; justify-content: space-between; }
.filtros-cab h6 { margin: 0; font-weight: 700; color: #1f2340; font-size: .95rem; display: flex; align-items: center; gap: 8px; }
.filtros-cab h6 i, .filtros-cab h6 svg { color: #2a78d6; }
.n-activos { font-size: .7rem; font-weight: 600; padding: 3px 9px; border-radius: 999px; }
.n-activos.on { background: #2a78d6; color: #fff; }
.n-activos.off { background: #eef1f8; color: #5b5f77; }
.grupo-filtro { background: #fff; border: 1px solid #dbe3f1; border-radius: 12px; overflow: hidden;
                box-shadow: 0 1px 3px rgba(16,24,40,.06); }
.grupo-tit { display: flex; align-items: center; gap: 9px; font-size: .74rem; font-weight: 700; color: #fff;
             text-transform: uppercase; letter-spacing: .8px; padding: 9px 14px;
             background: linear-gradient(100deg, #1f2470 0%, #2a6fd0 100%); }
.grupo-tit .gi { width: 22px; height: 22px; border-radius: 6px; display: inline-flex; align-items: center;
                 justify-content: center; font-size: .68rem; background: rgba(255,255,255,.18); color: #fff; }
.grupo-cuerpo { padding: 12px 14px 4px; }
.atajos { display: flex; gap: 6px; margin: -2px 0 10px; }
.atajos .btn { flex: 1; font-size: .74rem; padding: 3px 0; border-radius: 8px; border: 1px solid #dfe4ee;
               background: #f6f8fc; color: #3a3f5c; font-weight: 500; }
.atajos .btn:hover { background: #e7f0fd; border-color: #b9d3f6; color: #1c5cab; }
.sidebar .input-daterange input { font-size: .82rem; border-radius: 10px; }
.sidebar .input-daterange .input-group-addon, .sidebar .input-daterange .input-group-text {
               font-size: .75rem; background: #f6f8fc; }
.sidebar .selectize-input { border-radius: 10px; border-color: #dfe4ee; font-size: .84rem; min-height: 38px; box-shadow: none; }
.sidebar .selectize-input.focus { border-color: #86b6ef; box-shadow: 0 0 0 3px rgba(42,120,214,.12); }
/* Estados como chips */
#estado .shiny-options-group { display: flex; flex-wrap: wrap; gap: 6px; }
#estado .checkbox, #estado .form-check { margin: 0; padding: 0; }
#estado label { margin: 0; padding: 0; cursor: pointer; }
#estado input[type=checkbox] { position: absolute; opacity: 0; pointer-events: none; }
#estado label span { display: inline-flex; align-items: center; gap: 5px; padding: 4px 11px; border-radius: 999px;
                     font-size: .76rem; border: 1px solid #dfe4ee; background: #fff; color: #5b5f77; transition: all .15s; }
#estado label span::before { content: ''; width: 7px; height: 7px; border-radius: 50%; background: #c9cfdc; }
#estado input:checked + span { background: #eef3fd; border-color: #a9c8f3; color: #1c5cab; font-weight: 600; }
#estado input:checked + span::before { background: #2a78d6; }
.nota-filtro { font-size: .72rem; color: #7a7f96; line-height: 1.35; margin: 4px 0 10px; }
.fuente { display: flex; gap: 9px; font-size: .7rem; color: #7a7f96; line-height: 1.4;
          border-top: 1px dashed #dfe4ee; padding-top: 12px; }
.fuente i, .fuente svg { color: #9aa1b8; margin-top: 2px; flex-shrink: 0; }
.selectize-control.multi .selectize-input > div { background: #eef3fd; color: #1c5cab;
          border-radius: 6px; border: 0; padding: 1px 7px; }
.card { border: 0; border-radius: 16px;
        box-shadow: 0 1px 2px rgba(16,24,40,.05), 0 6px 22px rgba(16,24,40,.06); }
.card-header { background: transparent; border-bottom: 0; font-weight: 600; color: #1f2340;
               font-size: .95rem; padding-top: 14px; }
.card-header::before { content: ''; display: inline-block; width: 4px; height: 15px; border-radius: 2px;
               background: linear-gradient(#2a78d6, #6250d6); margin-right: 9px; vertical-align: -2px; }
.subtitulo { font-size: .78rem; color: #5b5f77; font-weight: 400; margin-left: 13px; display: block; }
.kpi { border: 0 !important; border-radius: 16px !important; color: #fff !important;
       box-shadow: 0 8px 22px rgba(42,120,214,.18) !important; }
.kpi .value-box-title { color: rgba(255,255,255,.92) !important; font-size: .9rem; font-weight: 600; }
.kpi .value-box-value { color: #fff !important; font-weight: 800 !important; letter-spacing: -1px; font-size: clamp(1.45rem, 2.1vw, 2.35rem) !important; white-space: nowrap;
                        line-height: 1.05; text-shadow: 0 2px 10px rgba(0,0,0,.15); }
.kpi { cursor: default; transition: transform .2s ease, box-shadow .2s ease; }
.kpi:hover { transform: translateY(-3px); box-shadow: 0 14px 30px rgba(31,36,112,.28) !important; }
.kpi .value-box-showcase, .kpi .value-box-area { color: #fff !important; }
.kpi-1 { background: linear-gradient(135deg, #2a78d6 0%, #6250d6 100%) !important; }
.kpi-2 { background: linear-gradient(135deg, #eb6834 0%, #d23d5b 100%) !important; }
.kpi-3 { background: linear-gradient(135deg, #0d8a63 0%, #1baf7a 60%, #2fb7a7 100%) !important; }
.kpi-4 { background: linear-gradient(135deg, #6250d6 0%, #b8509a 100%) !important; }
.kpi-tit { display: flex; align-items: center; gap: 9px; }
.kpi-ico { display: inline-flex; align-items: center; justify-content: center; width: 30px; height: 30px;
           border-radius: 9px; background: rgba(255,255,255,.2); font-size: .85rem; flex-shrink: 0; }
.table { font-size: .88rem; }
/* Tablas DT */
.dataTables_wrapper { font-size: .82rem; }
table.dataTable thead th { font-weight: 600; color: #5b5f77; text-transform: uppercase; font-size: .72rem;
                           letter-spacing: .4px; border-bottom: 1px solid #e6e9f2 !important; white-space: nowrap; }
table.dataTable tbody td { vertical-align: middle; border-top: 1px solid #f0f2f7 !important; }
table.dataTable td.recorta { max-width: 150px; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
table.dataTable td.nowrap, table.dataTable td.monto { white-space: nowrap; }
#t_distritos td.recorta { max-width: 210px; }
table.dataTable td.monto { font-variant-numeric: tabular-nums; font-weight: 600; color: #1f2340; }
.dataTables_wrapper .pagination .page-link { border: 0; border-radius: 8px !important; margin: 0 2px; color: #5b5f77; }
.dataTables_wrapper .pagination .active .page-link { background: #2a78d6; color: #fff; }
.dataTables_wrapper .form-control, .dataTables_wrapper .form-select { border-radius: 10px; }
.pastilla { display: inline-block; padding: 2px 10px; border-radius: 999px; font-size: .75rem; font-weight: 600; }
.est-ACEPTADA { background: #ece9fb; color: #4a3bb0; }
.est-PAGADA { background: #e3f6ee; color: #0d7a55; }
.est-otro { background: #fdecec; color: #b3302f; }
/* Filtros */
.btn-limpiar { border-radius: 10px; font-weight: 600; font-size: .82rem; padding: 7px 0; }
.botones-filtro { display: flex; flex-direction: column; gap: 8px; }
.btn-aplicar { border-radius: 10px; font-weight: 700; font-size: .86rem; padding: 9px 0; border: 0;
               background: linear-gradient(100deg, #1f2470 0%, #2a6fd0 100%); color: #fff;
               box-shadow: 0 4px 12px rgba(42,111,208,.28); transition: transform .15s, box-shadow .15s; }
.btn-aplicar:hover { color: #fff; transform: translateY(-1px); box-shadow: 0 6px 16px rgba(42,111,208,.38); }
.btn-aplicar:disabled, .btn-aplicar.disabled { background: #c9d3e6; color: #fff; box-shadow: none; opacity: 1; }
.btn-aplicar.pendiente { animation: latido 1.6s ease-in-out infinite; }
@keyframes latido { 0%, 100% { box-shadow: 0 0 0 0 rgba(42,120,214,.45); } 50% { box-shadow: 0 0 0 7px rgba(42,120,214,0); } }
.aviso-pendiente { font-size: .72rem; color: #c2410c; text-align: center; margin-top: -2px; display: none; }
.aviso-pendiente.visible { display: block; }
.card-header.cab-flex { display: flex; align-items: center; }
.mini-kpi { background: #f3f6fd; border-radius: 12px; padding: 10px 14px; }
.mini-kpi > span { display: block; font-size: .75rem; color: #5b5f77; text-transform: uppercase; letter-spacing: .4px; }
.mini-kpi strong { font-size: 1.3rem; color: #1f2340; font-weight: 700; display: block; margin-top: 2px; }
.seccion { font-size: .78rem; font-weight: 600; color: #5b5f77; text-transform: uppercase; letter-spacing: .4px;
           margin: 6px 0 -4px; }
#metrica_mapa, #modo_anual { margin: 0; }
#metrica_mapa .shiny-options-group, #modo_anual .shiny-options-group { display: flex; background: #eef1f8; border-radius: 10px; padding: 3px; }
#metrica_mapa .form-check, #modo_anual .form-check { padding: 0; margin: 0; }
#metrica_mapa input[type=radio], #modo_anual input[type=radio] { position: absolute; opacity: 0; pointer-events: none; }
#metrica_mapa label, #modo_anual label { margin: 0; padding: 0; }
#metrica_mapa label span, #modo_anual label span { display: inline-block; padding: 4px 12px; border-radius: 8px; font-size: .82rem;
           cursor: pointer; color: #5b5f77; }
#metrica_mapa input:checked + span, #modo_anual input:checked + span { background: #fff; color: #1c5cab; font-weight: 600;
           box-shadow: 0 1px 3px rgba(16,24,40,.12); }
.leaflet-container { background: #f7f9fc !important; border-radius: 0 0 16px 16px; font-family: Inter, 'Segoe UI', sans-serif; }
.leaflet-tooltip { border-radius: 10px; border: 1px solid #d5d9e4; box-shadow: 0 6px 18px rgba(16,24,40,.12); }
.info.legend { border-radius: 10px; box-shadow: 0 4px 14px rgba(16,24,40,.12); font-size: .75rem; }
.card-header.cab-flex > div { flex: 1; }
/* Variación frente al año anterior dentro de los KPI */
.kpi-var { display: inline-flex; align-items: center; gap: 5px; font-size: .74rem; font-weight: 600;
           background: rgba(255,255,255,.2); color: #fff; border-radius: 999px; padding: 2px 10px; margin-top: 2px; }
.kpi-var small { font-weight: 500; opacity: .85; }
.kpi-var { font-size: .78rem; font-weight: 800; padding: 3px 11px; backdrop-filter: blur(4px); }
.kpi-tit { width: 100%; font-size: .88rem; white-space: nowrap; }
.kpi .value-box-area { justify-content: flex-start !important; }
.kpi-info { margin-left: auto; font-size: .75rem; opacity: .6; transition: opacity .2s; }
.kpi:hover .kpi-info { opacity: 1; }
/* Panel explicativo: se abre debajo del recuadro al pasar el mouse */
.kpi, .kpi .card-body, .kpi .value-box-grid, .kpi .value-box-area { overflow: visible !important; }
.kpi:hover { z-index: 30; }
.kpi .value-box-area { padding-bottom: 18px !important; }
.kpi-det { position: absolute; top: calc(100% + 12px); left: -4px; right: -4px; z-index: 1000;
           border-radius: 14px; padding: 16px 18px 14px; background: rgba(16,23,58,.96); color: #fff;
           box-shadow: 0 18px 40px rgba(16,23,58,.35); backdrop-filter: blur(8px);
           opacity: 0; transform: translateY(-6px); pointer-events: none; transition: opacity .2s ease, transform .2s ease; }
.kpi-det::before { content: ''; position: absolute; top: -7px; left: 34px; width: 14px; height: 14px;
                   background: rgba(16,23,58,.96); transform: rotate(45deg); border-radius: 3px; }
.kpi:hover .kpi-det { opacity: 1; transform: none; }
.kpi-det-sec + .kpi-det-sec { margin-top: 10px; padding-top: 10px; border-top: 1px solid rgba(255,255,255,.12); }
.kpi-det-sec h6 { font-size: .68rem; font-weight: 700; text-transform: uppercase; letter-spacing: .8px;
                  color: #9cc4ff; margin: 0 0 4px; display: flex; align-items: center; gap: 6px; }
.kpi-det-sec p { font-size: .8rem; line-height: 1.45; margin: 0; color: rgba(255,255,255,.88); font-weight: 400; }
.kpi-det-sec p b { color: #fff; font-weight: 700; }
.kpi-det-sec .sube { color: #7ee2b8; } .kpi-det-sec .baja { color: #ffb4a8; }
.sel-sector .selectize-input { min-height: 32px; padding: 4px 10px; font-size: .82rem; border-radius: 10px; }
.sel-sector .form-group { margin: 0; min-width: 210px; width: 460px !important; max-width: 100%; }
.sel-sector .selectize-input > .item { background: #eef3fd; color: #1c5cab; border-radius: 6px; }
/* Listas de los filtros: se abren sobre todo, sin cortarse en el borde del lateral */
body > .selectize-dropdown { z-index: 2000; border: 1px solid #dfe4ee; border-radius: 10px; font-size: .84rem;
          box-shadow: 0 10px 28px rgba(16,24,40,.16); margin-top: 4px; overflow: hidden; }
body > .selectize-dropdown .selectize-dropdown-content { max-height: 300px; }
body > .selectize-dropdown .option { padding: 6px 12px; }
body > .selectize-dropdown .option.active { background: #eef3fd; color: #1c5cab; }
/* Pestañas del encabezado en una sola línea */
.navbar .navbar-nav { flex-wrap: nowrap; }
.navbar .nav-link { white-space: nowrap; }
@media (min-width: 992px) {
  .navbar-collapse { flex-wrap: nowrap; flex: 1; }
  .navbar .navbar-nav { margin-left: auto; flex: 0 0 auto !important; width: auto !important; }
  .navbar .navbar-nav .nav-item { flex: 0 0 auto; width: 124px; }
  .navbar .navbar-nav .nav-link { display: flex; align-items: center; justify-content: center; gap: 6px; }
}
/* Membrete institucional (franja blanca sobre el encabezado) */
.membrete { position: relative; display: flex; align-items: center; justify-content: space-between; gap: 24px;
            padding: 10px 28px 12px; background: linear-gradient(180deg, #ffffff 0%, #f7f9fd 100%);
            flex: 0 0 auto; z-index: 1031; }
.membrete::after { content: ''; position: absolute; left: 0; right: 0; bottom: 0; height: 3px;
            background: linear-gradient(90deg, #2fb7a7 0%, #2a78d6 40%, #1f2470 75%, #10173a 100%); }
.membrete-lado { display: flex; align-items: center; gap: 16px; min-width: 0; }
.membrete-epg { height: 66px; width: auto; }
.membrete-fac { height: 58px; width: auto; }
.membrete-sep { width: 1px; height: 46px; background: #d5dbe8; flex: 0 0 auto; }
.membrete-inst { font-size: .68rem; font-weight: 600; letter-spacing: 1.6px; color: #6b7590; text-transform: uppercase;
                 white-space: nowrap; }
.membrete-curso { font-size: 1.28rem; font-weight: 800; color: #1f2470; letter-spacing: .2px; line-height: 1.2;
                  white-space: nowrap; margin-top: 1px; }
.membrete-act { font-size: .76rem; color: #5b6478; white-space: nowrap; margin-top: 1px; }
.membrete-autor { text-align: right; line-height: 1.25; }
.membrete-autor .etq { font-size: .66rem; font-weight: 600; letter-spacing: 1.6px; text-transform: uppercase; color: #6b7590; }
.membrete-autor .nom { font-size: 1rem; font-weight: 700; color: #1f2340; white-space: nowrap; margin-top: 2px; }
@media (max-width: 991px) {
  .membrete { padding: 8px 16px 10px; gap: 6px 12px; flex-wrap: wrap; }
  .membrete-inst, .membrete-act, .membrete-lado .membrete-sep { display: none; }
  .membrete-epg { height: 46px; }
  .membrete-fac { height: 34px; }
  .membrete-curso { font-size: .95rem; white-space: normal; }
  .membrete > .membrete-lado:last-child { width: 100%; justify-content: space-between; }
  .membrete-autor { text-align: left; }
  .membrete-autor .etq, .membrete-autor .nom { display: inline; }
  .membrete-autor .etq { margin-right: 6px; }
  .membrete-autor .nom { font-size: .85rem; }
}
"
tema <- bs_theme(version = 5, primary = AZUL, secondary = VIOLETA, bg = "#ffffff", fg = TINTA,
                 base_font = font_google("Inter", wght = "400..800"), "border-radius" = "12px") |>
  bs_add_rules(sub("OLAS_URL", OLAS, css, fixed = TRUE))

titulo_card <- function(titulo, subtitulo = NULL) {
  card_header(titulo, if (!is.null(subtitulo)) span(class = "subtitulo", subtitulo))
}

kpi <- function(titulo, icono, salida, n, mini = NULL) {
  tit <- div(class = "kpi-tit", span(class = "kpi-ico", icon(icono)), titulo,
             span(class = "kpi-info", icon("circle-info")))
  if (is.null(mini)) {
    value_box(tit, textOutput(salida), class = paste0("kpi kpi-", n))
  } else {
    value_box(tit, textOutput(salida), uiOutput(paste0(salida, "_var")), uiOutput(paste0(salida, "_det")),
              class = paste0("kpi kpi-", n))
  }
}

# ---- Interfaz ---------------------------------------------------------------
MESES <- c("ene", "feb", "mar", "abr", "may", "jun", "jul", "ago", "sep", "oct", "nov", "dic")
mes_anio <- function(f) paste(MESES[as.integer(format(f, "%m"))], format(f, "%Y"))
ESTADOS <- sort(unique(compras$estado))
SECTORES_ORDEN <- names(sort(table(compras$sector), decreasing = TRUE))
ESTADOS_DEF <- setdiff(ESTADOS, c("RESUELTA", "ORDEN DE COMPRA NULA"))

grupo <- function(titulo, icono, n, ...) {
  div(class = "grupo-filtro",
      div(class = "grupo-tit", span(class = "gi", icon(icono)), titulo),
      div(class = "grupo-cuerpo", ...))
}

filtros <- sidebar(
  width = 300,
  div(class = "filtros-cab", h6(icon("sliders"), "Filtros"), uiOutput("n_filtros", inline = TRUE)),
  grupo("Periodo", "calendar-days", 1,
        dateRangeInput("fechas", NULL,
                       start = min(compras$fecha), end = max(compras$fecha),
                       min = min(compras$fecha), max = max(compras$fecha),
                       language = "es", separator = "a", format = "dd/mm/yyyy"),
        div(class = "atajos",
            actionButton("rango_todo", "Todo", class = "btn-sm"),
            actionButton("rango_2025", "2025", class = "btn-sm"),
            actionButton("rango_2026", "2026", class = "btn-sm"))),
  grupo("Quién compra", "landmark", 2,
        selectizeInput("sector", "Sector", choices = sort(unique(compras$sector)),
                       multiple = TRUE, options = list(placeholder = "Todos los sectores", dropdownParent = "body")),
        selectizeInput("provincia", "Provincia",
                       choices = sort(unique(na.omit(compras$provincia_entidad))),
                       multiple = TRUE, options = list(dropdownParent = "body", placeholder = paste(
                         "Las", length(unique(na.omit(compras$provincia_entidad))), "provincias")))),
  grupo("Qué se compra", "cart-shopping", 3,
        selectizeInput("rubro", "Rubro (Acuerdo Marco)", choices = sort(unique(compras$acuerdo_marco)),
                       multiple = TRUE, options = list(placeholder = "Todos los rubros", dropdownParent = "body")),
        checkboxGroupInput("estado", "Estado de la orden",
                           choices = setNames(ESTADOS, titulo(ESTADOS)), selected = ESTADOS_DEF),
        div(class = "nota-filtro", "Por defecto se dejan fuera las órdenes resueltas o nulas: no llegaron a ejecutarse.")),
  div(class = "botones-filtro",
      actionButton("aplicar", "Aplicar filtros", icon = icon("check"), class = "w-100 btn-aplicar", disabled = NA),
      div(id = "aviso_pendiente", class = "aviso-pendiente", "Tienes cambios sin aplicar"),
      actionButton("limpiar", "Restablecer filtros", icon = icon("rotate-left"),
                   class = "btn-outline-primary w-100 btn-limpiar")),
  tags$script(HTML("Shiny.addCustomMessageHandler('pendiente', function(x) {
    var b = document.getElementById('aplicar'), a = document.getElementById('aviso_pendiente');
    b.disabled = !x; b.classList.toggle('pendiente', x); a.classList.toggle('visible', x);
  });")),
  div(class = "fuente", icon("database"),
      div("Fuente: Perú Compras, órdenes de compra de Catálogos Electrónicos (datosabiertos.gob.pe).",
          "Región asignada con el padrón RUC de SUNAT."))
)

pagina_resumen <- nav_panel(
  "Resumen", icon = icon("chart-line"),
  layout_columns(
    fill = FALSE, col_widths = c(3, 3, 3, 3),
    kpi("Monto total", "coins", "kpi_monto", 1, "sp_monto"),
    kpi("Órdenes", "file-invoice", "kpi_ordenes", 2, "sp_ordenes"),
    kpi("Ticket promedio", "receipt", "kpi_ticket", 3, "sp_ticket"),
    kpi("Proveedores", "building", "kpi_prov", 4, "sp_prov")
  ),
  layout_columns(
    col_widths = c(8, 4),
    card(full_screen = TRUE,
         titulo_card("¿Cómo evoluciona el gasto?", "Monto mensual en soles y su media móvil de 3 meses"),
         plotlyOutput("g_mensual", height = 320)),
    card(full_screen = TRUE,
         titulo_card("¿Cómo se compra?", "Monto por tipo de procedimiento y órdenes por estado"),
         layout_columns(col_widths = c(6, 6),
                        plotlyOutput("g_proc", height = 290),
                        plotlyOutput("g_estado", height = 290)))
  ),
  layout_columns(
    col_widths = c(6, 6),
    card(full_screen = TRUE,
         card_header(class = "cab-flex",
                     div(class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
                         div(span("¿2026 va por encima de 2025?"),
                             span(class = "subtitulo ms-0", "Monto comprado por mes, un año frente al otro")),
                         radioButtons("modo_anual", NULL, inline = TRUE,
                                      choices = c("Acumulado" = "acum", "Mensual" = "mes")))),
         plotlyOutput("g_anual", height = 400)),
    card(full_screen = TRUE,
         titulo_card("¿En qué se gasta?", "Los 10 rubros (Acuerdos Marco) con más monto y el resto agrupado"),
         plotlyOutput("g_rubro", height = 400))
  ),
  card(full_screen = TRUE,
       card_header(class = "cab-flex",
                   div(class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
                       div(span("¿Quién emite las órdenes cada mes?"),
                           span(class = "subtitulo ms-0", "Elige hasta 5 sectores para compararlos; en gris, el resto de órdenes del mes")),
                       div(class = "sel-sector",
                           selectizeInput("sector_resalte", NULL, choices = SECTORES_ORDEN,
                                          selected = SECTORES_ORDEN[1:3], multiple = TRUE,
                                          options = list(maxItems = 5, dropdownParent = "body",
                                                         plugins = list("remove_button"),
                                                         placeholder = "Elige hasta 5 sectores"))))),
       plotlyOutput("g_mensual_n", height = 340))
)

pagina_proveedores <- nav_panel(
  "Proveedores", icon = icon("building"),
  layout_columns(
    fill = FALSE, col_widths = c(4, 4, 4),
    kpi("Concentración: top 10 empresas", "ranking-star", "kpi_top10", 1),
    kpi("Monto que queda en empresas de Puno", "location-dot", "kpi_local", 3),
    kpi("Empresas que hacen el 80 % del monto", "chart-pie", "kpi_pareto", 4)
  ),
  layout_columns(
    col_widths = c(7, 5),
    card(full_screen = TRUE,
         titulo_card("Top 15 empresas por monto vendido", "El color indica de dónde es la empresa"),
         plotlyOutput("g_top_prov", height = 440)),
    card(full_screen = TRUE,
         titulo_card("Curva de Pareto", "% acumulado del monto frente al % de empresas"),
         plotlyOutput("g_pareto", height = 440))
  ),
  layout_columns(
    col_widths = c(8, 4),
    card(full_screen = TRUE,
         titulo_card("Mapa de empresas: frecuencia frente a monto",
                     "Cada burbuja es una empresa; su tamaño es el número de entidades a las que vendió (ejes en escala log)"),
         plotlyOutput("g_burbujas", height = 430)),
    card(full_screen = TRUE,
         titulo_card("¿De dónde son las empresas?", "Monto según domicilio fiscal (SUNAT)"),
         plotlyOutput("g_origen", height = 250),
         titulo_card("Tipo de empresa"),
         plotlyOutput("g_tipo_emp", height = 170))
  ),
  card(full_screen = TRUE, titulo_card("Detalle por empresa"), card_body(fillable = FALSE, DTOutput("t_prov")))
)

pagina_entidades <- nav_panel(
  "Entidades", icon = icon("landmark"),
  layout_columns(
    col_widths = c(7, 5),
    card(full_screen = TRUE,
         titulo_card("Top 15 entidades por monto comprado", "El color indica el sector de la entidad"),
         plotlyOutput("g_top_ent", height = 450)),
    card(full_screen = TRUE,
         titulo_card("Sectores y sus entidades", "Clic en un sector para abrirlo; clic en el centro para volver"),
         plotlyOutput("g_sunburst", height = 450))
  ),
  layout_columns(
    col_widths = c(5, 7),
    card(full_screen = TRUE,
         titulo_card("Monto por sector"),
         plotlyOutput("g_sector", height = 360)),
    card(full_screen = TRUE,
         titulo_card("Monto por provincia de la entidad", "Más oscuro = más monto"),
         plotlyOutput("g_prov_ent", height = 360))
  ),
  card(full_screen = TRUE,
       titulo_card("¿Qué compra cada sector?", "Monto por sector y rubro; el color usa escala logarítmica para que se vean también los montos chicos"),
       plotlyOutput("g_calor", height = 450))
)

pagina_mapa <- nav_panel(
  "Mapa distrital", icon = icon("map-location-dot"),
  layout_columns(
    col_widths = c(8, 4),
    card(full_screen = TRUE,
         card_header(class = "cab-flex",
                     div(class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
                         div(span("¿Dónde se compra?"),
                             span(class = "subtitulo ms-0", "Distrito del domicilio fiscal de la entidad. Clic en un distrito para ver su detalle")),
                         radioButtons("metrica_mapa", NULL, inline = TRUE,
                                      choices = c("Monto" = "monto", "Órdenes" = "ordenes", "Entidades" = "entidades")))),
         card_body(padding = 0, leafletOutput("mapa", height = 560)),
         div(class = "small text-muted px-3 py-2",
             "Escala logarítmica: el distrito de Puno concentra a casi todas las entidades regionales. ",
             "San Miguel (creado en 2016) aparece sumado a Juliaca porque el mapa del INEI es anterior.")),
    card(full_screen = TRUE,
         card_header(class = "cab-flex", div(uiOutput("titulo_distrito"))),
         layout_columns(fill = FALSE, col_widths = c(6, 6),
                        div(class = "mini-kpi", span("Monto"), strong(textOutput("d_monto", inline = TRUE))),
                        div(class = "mini-kpi", span("Órdenes"), strong(textOutput("d_ordenes", inline = TRUE)))),
         div(class = "seccion", "¿Quién compra?"),
         plotlyOutput("g_dist_ent", height = 210),
         div(class = "seccion", "¿En qué gasta?"),
         plotlyOutput("g_dist_rubro", height = 210))
  ),
  layout_columns(
    col_widths = c(4, 8),
    card(full_screen = TRUE,
         titulo_card("Ranking de distritos", "Top 15 por monto comprado (escala log)"),
         plotlyOutput("g_rank_dist", height = 430)),
    card(full_screen = TRUE,
         titulo_card("Distrito, entidad líder y rubro principal", "Quién gasta más en cada distrito y en qué (entre paréntesis, su peso en el monto del distrito)"),
         card_body(fillable = FALSE, DTOutput("t_distritos")))
  )
)

pagina_eda <- nav_panel(
  "Exploración", icon = icon("magnifying-glass-chart"),
  layout_columns(
    col_widths = c(4, 8),
    card(titulo_card("Estadísticos descriptivos", "Monto total por orden (S/)"), tableOutput("t_desc"),
         div(class = "small text-muted px-3 pb-2", textOutput("txt_sesgo"))),
    card(full_screen = TRUE,
         titulo_card("Distribución del monto por orden", "Histograma en escala logarítmica; el color se oscurece con el monto"),
         plotlyOutput("g_hist", height = 330))
  ),
  layout_columns(
    col_widths = c(7, 5),
    card(full_screen = TRUE,
         titulo_card("Forma de la distribución por sector", "Curvas de densidad del monto (escala log); la línea marca la mediana"),
         plotlyOutput("g_caja", height = 420)),
    card(full_screen = TRUE,
         titulo_card("Órdenes frente a monto por rango", "Muchas órdenes chicas, pocas que concentran el dinero"),
         plotlyOutput("g_rango", height = 420))
  ),
  card(full_screen = TRUE,
       titulo_card("¿Qué días se emiten las órdenes?", "Número de órdenes por día de la semana y mes"),
       plotlyOutput("g_semana", height = 300)),
  layout_columns(
    col_widths = c(4, 8),
    card(titulo_card("Calidad de datos"), tableOutput("t_calidad")),
    card(full_screen = TRUE, titulo_card("Órdenes más grandes", "Posibles valores atípicos"), card_body(fillable = FALSE, DTOutput("t_atipicos")))
  )
)

pagina_datos <- nav_panel(
  "Datos", icon = icon("table"),
  card(full_screen = TRUE, fill = FALSE,
       card_header(class = "cab-flex", div(class = "d-flex justify-content-between align-items-center",
                       div(span("Tabla de órdenes filtradas"),
                           span(class = "subtitulo ms-0", textOutput("txt_n_datos", inline = TRUE))),
                       downloadButton("descargar", "Descargar CSV", class = "btn-sm btn-primary"))),
       card_body(fillable = FALSE, DTOutput("t_datos")))
)

# Membrete institucional: se crea dentro de la página y se lleva al inicio del <body>,
# encima de la barra azul
membrete <- tagList(
  div(id = "membrete", class = "membrete",
      div(class = "membrete-lado",
          img(src = "logo_epg.png", class = "membrete-epg", alt = "Escuela de Posgrado UNA Puno"),
          span(class = "membrete-sep"),
          div(div(class = "membrete-inst", "Universidad Nacional del Altiplano · Escuela de Posgrado"),
              div(class = "membrete-curso", "Business Intelligence and Data Analytics"),
              div(class = "membrete-act", "Maestría · Actividad Semana 2: Herramientas de analítica y reporting"))),
      div(class = "membrete-lado",
          div(class = "membrete-autor", div(class = "etq", "Elaborado por"),
              div(class = "nom", "Mary Luz Nina Palacios")),
          span(class = "membrete-sep"),
          img(src = "logo_facultad.png", class = "membrete-fac",
              alt = "Facultad de Ingeniería Estadística e Informática"))),
  tags$script(HTML("document.addEventListener('DOMContentLoaded', function () {
    var m = document.getElementById('membrete'); if (m) document.body.insertBefore(m, document.body.firstChild);
  });"))
)

ui <- page_navbar(
  header = membrete,
  title = div(class = "marca",
              span(class = "marca-logo", SILUETA_PUNO),
              div(div(class = "marca-tit", "COMPRAS PÚBLICAS · REGIÓN PUNO"),
                  div(class = "marca-sub", "Catálogos Electrónicos de Perú Compras"),
                  div(class = "marca-sub", paste("Periodo:", mes_anio(min(compras$fecha)), "–",
                                                 mes_anio(max(compras$fecha)))))),
  window_title = "Reporte de compras públicas · Región Puno",
  theme = tema, fillable = FALSE, sidebar = filtros, inverse = TRUE,
  pagina_resumen, pagina_proveedores, pagina_entidades, pagina_mapa, pagina_eda, pagina_datos
)

# ---- Servidor ---------------------------------------------------------------
server <- function(input, output, session) {

  idioma_dt <- list(url = "https://cdn.datatables.net/plug-ins/1.13.6/i18n/es-ES.json")

  # Los filtros solo se aplican al pulsar «Aplicar filtros» (o «Restablecer»)
  FILTROS_DEF <- list(fechas = as.character(range(compras$fecha)), sector = character(0),
                      provincia = character(0), rubro = character(0), estado = sort(ESTADOS_DEF))
  leer_filtros <- function() {
    norm <- function(x) sort(as.character(if (is.null(x)) character(0) else x))
    list(fechas = as.character(input$fechas), sector = norm(input$sector), provincia = norm(input$provincia),
         rubro = norm(input$rubro), estado = norm(input$estado))
  }
  aplicados <- reactiveVal(FILTROS_DEF)
  observeEvent(input$aplicar, {
    f <- leer_filtros()
    if (length(f$fechas) == 2 && !anyNA(f$fechas)) aplicados(f)
  })
  observe({
    req(input$fechas)
    session$sendCustomMessage("pendiente", !identical(leer_filtros(), aplicados()))
  })

  filtrados <- reactive({
    f <- aplicados()
    d <- compras |> filter(fecha >= f$fechas[1], fecha <= f$fechas[2])
    if (length(f$sector))    d <- d |> filter(sector %in% f$sector)
    if (length(f$provincia)) d <- d |> filter(provincia_entidad %in% f$provincia)
    if (length(f$rubro))     d <- d |> filter(acuerdo_marco %in% f$rubro)
    d |> filter(estado %in% f$estado)
  })
  datos <- reactive({
    d <- filtrados()
    validate(need(nrow(d) > 0, "No hay órdenes con esos filtros."))
    d
  })

  # ---- Lateral: filtros activos y atajos ----
  output$n_filtros <- renderUI({
    f <- aplicados()
    n <- sum(length(f$sector) > 0, length(f$provincia) > 0, length(f$rubro) > 0,
             !setequal(f$estado, ESTADOS_DEF), !identical(f$fechas, FILTROS_DEF$fechas))
    if (n == 0) span(class = "n-activos off", "Sin filtros")
    else span(class = "n-activos on", paste(n, if (n == 1) "activo" else "activos"))
  })
  rango <- function(ini, fin) {
    updateDateRangeInput(session, "fechas", start = max(ini, min(compras$fecha)), end = min(fin, max(compras$fecha)))
  }
  observeEvent(input$rango_todo, rango(min(compras$fecha), max(compras$fecha)))
  observeEvent(input$rango_2025, rango(as.Date("2025-01-01"), as.Date("2025-12-31")))
  observeEvent(input$rango_2026, rango(as.Date("2026-01-01"), as.Date("2026-12-31")))

  mensual <- reactive({
    datos() |> group_by(periodo) |>
      summarise(monto = sum(total), ordenes = n(), empresas = n_distinct(ruc_proveedor), .groups = "drop") |>
      mutate(fecha = as.Date(paste0(periodo, "-01")), ticket = monto / ordenes,
             media3 = as.numeric(stats::filter(monto, rep(1 / 3, 3), sides = 1)))
  })

  # ---- KPIs ----
  output$kpi_monto   <- renderText(fmt_soles(sum(datos()$total)))
  output$kpi_ordenes <- renderText(fmt_n(nrow(datos())))
  output$kpi_ticket  <- renderText(fmt_soles(mean(datos()$total)))
  output$kpi_prov    <- renderText(fmt_n(n_distinct(datos()$ruc_proveedor)))
  # Variación frente a los mismos meses del año anterior (ej.: ene–sep 2026 vs ene–sep 2025)
  comparacion <- reactive({
    d <- datos() |> mutate(anio = as.integer(format(fecha, "%Y")), mes = as.integer(format(fecha, "%m")))
    ult <- max(d$anio)
    meses <- sort(unique(d$mes[d$anio == ult]))
    ant <- d |> filter(anio == ult - 1, mes %in% meses)
    act <- d |> filter(anio == ult)
    if (nrow(ant) == 0 || nrow(act) == 0) return(NULL)
    resumen <- function(x) c(monto = sum(x$total), ordenes = nrow(x), ticket = mean(x$total),
                             prov = n_distinct(x$ruc_proveedor))
    list(act = resumen(act), ant = resumen(ant), ult = ult,
         etiqueta = paste0(MESES[min(meses)], "–", MESES[max(meses)], " ", ult, " vs ", ult - 1))
  })
  insignia_var <- function(clave) renderUI({
    cmp <- comparacion()
    if (is.null(cmp)) return(NULL)
    v <- cmp$act[[clave]] / cmp$ant[[clave]] - 1
    flecha <- if (v >= 0) "▲" else "▼"
    span(class = "kpi-var", paste0(flecha, " ", pct(abs(v))), tags$small(paste("vs", cmp$ult - 1)))
  })
  output$kpi_monto_var   <- insignia_var("monto")
  output$kpi_ordenes_var <- insignia_var("ordenes")
  output$kpi_ticket_var  <- insignia_var("ticket")
  output$kpi_prov_var    <- insignia_var("prov")

  # Paneles explicativos (aparecen al pasar el mouse): qué es la cifra, cómo leerla y cómo va frente al año anterior
  MESES_L <- c("enero", "febrero", "marzo", "abril", "mayo", "junio", "julio", "agosto", "septiembre",
               "octubre", "noviembre", "diciembre")
  panel <- function(que_es, como_leer, frente = NULL) {
    sec <- function(ico, tit, txt) div(class = "kpi-det-sec", h6(icon(ico), tit), p(HTML(txt)))
    div(class = "kpi-det",
        sec("circle-question", "¿Qué es?", que_es),
        sec("lightbulb", "¿Cómo leerlo?", como_leer),
        if (!is.null(frente)) sec("arrow-trend-up", "Frente al año anterior", frente))
  }
  b <- function(x) paste0("<b>", x, "</b>")
  # frase del tipo "22,1 % menos" con color según suba o baje
  cambio <- function(act, ant, mas = "más", menos = "menos") {
    v <- act / ant - 1
    paste0("<b class='", if (v >= 0) "sube" else "baja", "'>", pct(abs(v)), " ", if (v >= 0) mas else menos, "</b>")
  }
  tramo <- function(cmp) {
    meses <- sort(unique(as.integer(format(datos()$fecha[format(datos()$fecha, "%Y") == cmp$ult], "%m"))))
    if (length(meses) == 1) MESES_L[meses] else paste(MESES_L[min(meses)], "a", MESES_L[max(meses)])
  }
  output$kpi_monto_det <- renderUI({
    m <- mensual(); cmp <- comparacion()
    top <- m[which.max(m$monto), ]
    frente <- if (!is.null(cmp)) paste0(
      "De ", tramo(cmp), " de ", cmp$ult, " se compró ", cambio(cmp$act[["monto"]], cmp$ant[["monto"]]),
      " que en los mismos meses de ", cmp$ult - 1, " (", b(fmt_soles(cmp$act[["monto"]])), " frente a ",
      fmt_soles(cmp$ant[["monto"]]), ").")
    panel("Todo el dinero que pagaron las entidades públicas de Puno por sus compras en los Catálogos Electrónicos de Perú Compras, con IGV incluido.",
          paste0("Equivale a unos ", b(fmt_soles(mean(m$monto))), " al mes. El mes de más gasto fue ",
                 MESES_L[as.integer(format(top$fecha, "%m"))], " de ", format(top$fecha, "%Y"),
                 " (", b(fmt_soles(top$monto)), ")."),
          frente)
  })
  output$kpi_ordenes_det <- renderUI({
    d <- datos(); m <- mensual(); cmp <- comparacion()
    frente <- if (!is.null(cmp)) paste0(
      "De ", tramo(cmp), " de ", cmp$ult, " se emitieron ", b(fmt_n(cmp$act[["ordenes"]])), " órdenes, ",
      cambio(cmp$act[["ordenes"]], cmp$ant[["ordenes"]]), " que en ", cmp$ult - 1, " (", fmt_n(cmp$ant[["ordenes"]]), ").")
    panel("Cada orden es una compra concreta: una entidad (municipalidad, hospital, colegio…) le pide a una empresa productos del catálogo.",
          paste0("Son unas ", b(fmt_n(mean(m$ordenes))), " órdenes al mes, emitidas por ", b(fmt_n(n_distinct(d$ruc_entidad))),
                 " entidades. Más órdenes no siempre es más gasto: pueden ser muchas compras pequeñas."),
          frente)
  })
  output$kpi_ticket_det <- renderUI({
    d <- datos(); cmp <- comparacion()
    frente <- if (!is.null(cmp)) paste0(
      "En ", cmp$ult, " una orden costó en promedio ", b(fmt_soles(cmp$act[["ticket"]])), ", ",
      cambio(cmp$act[["ticket"]], cmp$ant[["ticket"]]), " que en ", cmp$ult - 1, " (", fmt_soles(cmp$ant[["ticket"]]), ").")
    panel("Cuánto cuesta, en promedio, una orden de compra: el monto total dividido entre el número de órdenes.",
          paste0("Unas pocas compras muy grandes suben el promedio. La orden típica es de ", b(fmt_soles(median(d$total))),
                 ": la mitad de las órdenes cuesta menos que eso. La más grande llegó a ", b(fmt_soles(max(d$total))), "."),
          frente)
  })
  output$kpi_prov_det <- renderUI({
    d <- datos(); pp <- por_prov(); cmp <- comparacion()
    puno <- sum(pp$origen_proveedor == "Puno")
    monto_puno <- sum(d$total[d$origen_proveedor == "Puno"]) / sum(d$total)
    frente <- if (!is.null(cmp)) {
      ult <- as.character(cmp$ult)
      antes <- unique(d$ruc_proveedor[format(d$fecha, "%Y") < ult])
      nuevas <- setdiff(unique(d$ruc_proveedor[format(d$fecha, "%Y") == ult]), antes)
      paste0("De ", tramo(cmp), " de ", cmp$ult, " vendieron ", b(fmt_n(cmp$act[["prov"]])), " empresas, ",
             cambio(cmp$act[["prov"]], cmp$ant[["prov"]]), " que en ", cmp$ult - 1, ". ",
             b(fmt_n(length(nuevas))), " le vendieron al Estado en Puno por primera vez.")
    }
    panel("Cuántas empresas distintas recibieron al menos una orden de las entidades de Puno.",
          paste0("Solo ", b(fmt_n(puno)), " (", pct(puno / nrow(pp)), ") tienen domicilio en Puno y se llevan el ",
                 b(pct(monto_puno)), " del dinero: la mayor parte se le compra a empresas de Lima y de otras regiones."),
          frente)
  })


  por_prov <- reactive({
    datos() |>
      group_by(ruc_proveedor, proveedor, departamento_proveedor, origen_proveedor, tipo_empresa) |>
      summarise(monto = sum(total), ordenes = n(), entidades = n_distinct(ruc_entidad), .groups = "drop") |>
      arrange(desc(monto)) |>
      mutate(pct = monto / sum(monto), pct_acum = cumsum(pct), rango = row_number())
  })
  output$kpi_top10  <- renderText(pct(sum(head(por_prov()$pct, 10))))
  output$kpi_local  <- renderText({
    d <- datos(); pct(sum(d$total[d$origen_proveedor == "Puno"]) / sum(d$total))
  })
  output$kpi_pareto <- renderText({
    p <- por_prov(); k <- which(p$pct_acum >= 0.8)[1]
    paste0(fmt_n(k), " de ", fmt_n(nrow(p)), " (", pct(k / nrow(p)), ")")
  })

  # ---- Resumen ----
  output$g_mensual <- renderPlotly({
    m <- mensual()
    pico <- m[which.max(m$monto), ]
    notas <- list(list(x = pico$fecha, y = pico$monto, text = paste0("<b>Pico</b><br>", fmt_soles(pico$monto)),
                       showarrow = TRUE, arrowhead = 0, arrowcolor = GRIS, ax = 0, ay = -38,
                       font = list(size = 11, color = TINTA), bgcolor = "rgba(255,255,255,0.85)"))
    # Enero casi no tiene órdenes en todo el país: es el arranque del año fiscal
    ene <- m |> filter(format(fecha, "%m") == "01", fecha > min(m$fecha))
    for (i in seq_len(nrow(ene))) {
      notas[[length(notas) + 1]] <- list(x = ene$fecha[i], y = ene$monto[i],
        text = "Enero: arranque del<br>año fiscal, casi sin órdenes", showarrow = TRUE, arrowhead = 0,
        arrowcolor = GRIS, ax = 0, ay = -60, font = list(size = 10, color = TINTA2), bgcolor = "rgba(255,255,255,0.85)")
    }
    plot_ly(m, x = ~fecha) |>
      add_trace(y = ~monto, type = "scatter", mode = "lines+markers", name = "Monto mensual",
                fill = "tozeroy", fillcolor = rgba(AZUL, 0.14),
                line = list(color = AZUL, width = 2.5, shape = "spline", smoothing = 0.6),
                marker = list(color = AZUL, size = 8, line = list(color = "#ffffff", width = 2)),
                text = ~paste0("<b>", format(fecha, "%b %Y"), "</b><br>", fmt_soles(monto), " · ", fmt_n(ordenes), " órdenes"),
                hoverinfo = "text") |>
      add_trace(y = ~media3, type = "scatter", mode = "lines", name = "Media móvil 3 meses",
                line = list(color = NARANJA, width = 2, dash = "dot", shape = "spline"),
                text = ~ifelse(is.na(media3), "", paste0("Media 3 meses: ", fmt_soles(media3))), hoverinfo = "text") |>
      estilo() |>
      layout(hovermode = "x unified", xaxis = list(tickformat = "%b\n%Y", dtick = "M2"),
             yaxis = list(tickformat = "~s", ticksuffix = ""), annotations = notas)
  })
  output$g_proc <- renderPlotly({
    p <- datos() |> group_by(tipo_procedimiento) |> summarise(monto = sum(total), n = n(), .groups = "drop") |> arrange(desc(monto))
    cols <- c(AZUL, NARANJA, AQUA, AMARILLO)[seq_len(nrow(p))]
    dona(p$tipo_procedimiento, p$monto, cols, paste0("<b>", fmt_soles(sum(p$monto)), "</b>"),
         paste0(p$tipo_procedimiento, "<br>", fmt_soles(p$monto), " · ", fmt_n(p$n), " órdenes"))
  })
  output$g_estado <- renderPlotly({
    e <- datos() |> count(estado) |> arrange(desc(n))
    col_estado <- c("ACEPTADA" = VIOLETA, "PAGADA" = AQUA, "RESUELTA" = ROJO, "ORDEN DE COMPRA NULA" = GRIS)
    cols <- unname(ifelse(e$estado %in% names(col_estado), col_estado[e$estado], GRIS))
    dona(e$estado, e$n, cols, paste0("<b>", fmt_n(sum(e$n)), "</b><br>órdenes"),
         paste0(e$estado, ": ", fmt_n(e$n), " órdenes"))
  })
  output$g_mensual_n <- renderPlotly({
    sel <- input$sector_resalte
    validate(need(length(sel) > 0, "Elige al menos un sector en el selector."))
    # colores por posición, bien distintos entre sí
    paleta <- setNames(c(AZUL, NARANJA, VIOLETA, AQUA, AMARILLO)[seq_along(sel)], sel)
    d <- datos()
    tot <- d |> count(periodo, name = "total")
    m <- d |> mutate(grupo = ifelse(sector %in% sel, sector, "Demás sectores")) |>
      count(periodo, grupo) |> complete(periodo = tot$periodo, grupo = c(sel, "Demás sectores"), fill = list(n = 0)) |>
      left_join(tot, by = "periodo") |>
      mutate(fecha = as.Date(paste0(periodo, "-01")), part = ifelse(total > 0, n / total, 0))
    total_gral <- nrow(d)
    p <- plot_ly()
    for (g in c(sel, "Demás sectores")) {
      dg <- m |> filter(grupo == g)
      col <- if (g == "Demás sectores") "#dfe4ee" else paleta[[g]]
      nombre <- paste0(g, " (", pct(sum(dg$n) / total_gral), ")")
      dg$texto <- paste0("<b>", g, "</b>: ", fmt_n(dg$n), " órdenes (", pct(dg$part), ")")
      p <- p |> add_bars(data = dg, x = ~fecha, y = ~n, name = nombre, marker = list(color = col),
                         text = ~texto,
                         hoverinfo = "text", textposition = "none")
    }
    p |> estilo() |>
      layout(barmode = "stack", bargap = 0.22, hovermode = "x unified",
             xaxis = list(tickformat = "%b\n%Y", dtick = "M1", hoverformat = "%b %Y"),
             legend = list(orientation = "h", x = 0, y = 1.14, traceorder = "normal"))
  })
  output$g_anual <- renderPlotly({
    m <- datos() |> mutate(anio = format(fecha, "%Y"), mes = as.integer(format(fecha, "%m"))) |>
      group_by(anio, mes) |> summarise(monto = sum(total), .groups = "drop") |>
      complete(anio, mes = 1:12) |> arrange(anio, mes)
    # los meses posteriores al último con compras quedan vacíos (no son cero)
    m <- m |> group_by(anio) |>
      mutate(ultimo = max(mes[!is.na(monto)]), monto = ifelse(mes <= ultimo & is.na(monto), 0, monto),
             acum = ifelse(is.na(monto), NA, cumsum(ifelse(is.na(monto), 0, monto)))) |> ungroup()
    acum <- isTRUE(input$modo_anual == "acum")
    m$y <- if (acum) m$acum else m$monto
    anios <- sort(unique(m$anio))
    previo <- list(color = "#9aa1b8", dash = "dot")
    actual <- list(color = AZUL, dash = "solid")
    p <- plot_ly()
    for (k in seq_along(anios)) {
      e <- if (k == length(anios)) actual else previo
      dk <- m |> filter(anio == anios[k], !is.na(y))
      p <- p |> add_trace(data = dk, x = ~MESES[mes], y = ~y, type = "scatter", mode = "lines+markers",
                          name = anios[k], fill = if (k == length(anios)) "tozeroy" else "none",
                          fillcolor = rgba(AZUL, 0.12),
                          line = list(color = e$color, width = 3, dash = e$dash, shape = "spline", smoothing = 0.5),
                          marker = list(color = e$color, size = 7, line = list(color = "#fff", width = 1.5)),
                          text = ~paste0("<b>", MESES[mes], " ", anio, "</b>: ", fmt_soles(y)),
                          hoverinfo = "text")
    }
    notas <- list()
    if (acum && length(anios) >= 2) {
      u <- m |> filter(anio == anios[length(anios)], !is.na(acum)) |> slice_tail(n = 1)
      a <- m |> filter(anio == anios[length(anios) - 1], mes == u$mes)
      if (nrow(a) == 1 && isTRUE(a$acum > 0)) {
        v <- u$acum / a$acum - 1
        notas <- list(list(x = MESES[u$mes], y = u$acum, xanchor = "left", ax = 25, ay = 45,
                           text = paste0("<b>", if (v >= 0) "▲ " else "▼ ", pct(abs(v)), "</b> frente a<br>ene–",
                                         MESES[u$mes], " ", anios[length(anios) - 1]),
                           showarrow = TRUE, arrowhead = 0, arrowcolor = GRIS, bgcolor = "rgba(255,255,255,.92)",
                           bordercolor = "#d5d9e4", borderpad = 4, font = list(size = 11, color = TINTA)))
      }
    }
    p |> estilo() |>
      layout(hovermode = "x unified", annotations = notas,
             xaxis = list(categoryorder = "array", categoryarray = MESES, type = "category"),
             yaxis = list(tickformat = "~s"), legend = list(orientation = "h", x = 0, y = 1.1))
  })
  output$g_rubro <- renderPlotly({
    r <- datos() |> group_by(acuerdo_marco) |> summarise(monto = sum(total), ordenes = n(), .groups = "drop") |>
      arrange(desc(monto)) |> mutate(part = monto / sum(monto), otros = FALSE)
    top <- head(r, 10)
    if (nrow(r) > 10) {
      resto <- r[-(1:10), ]
      top <- bind_rows(top, tibble(acuerdo_marco = paste0("Otros (", nrow(resto), " rubros)"),
                                   monto = sum(resto$monto), ordenes = sum(resto$ordenes),
                                   part = sum(resto$part), otros = TRUE))
    }
    tono <- col_numeric(c("#86b6ef", "#2a78d6", "#0d366b"), domain = range(top$monto[!top$otros]))
    top <- top |> mutate(et = ifelse(otros, acuerdo_marco, corta(titulo(acuerdo_marco), 40)),
                         et = factor(et, levels = rev(et)),
                         color = ifelse(otros, "#c9cfdc", tono(pmin(monto, max(monto[!otros])))),
                         etiqueta = paste0(fmt_soles(monto), " · ", pct(part)))
    plot_ly(top, x = ~monto, y = ~et, type = "bar", orientation = "h",
            marker = list(color = ~color), text = ~etiqueta, textposition = "outside", cliponaxis = FALSE,
            textfont = list(size = 11, color = TINTA2),
            hovertext = ~paste0("<b>", acuerdo_marco, "</b><br>", fmt_soles(monto), " (", pct(part), ")<br>",
                                fmt_n(ordenes), " órdenes"), hoverinfo = "text") |>
      estilo(leyenda = FALSE) |>
      layout(bargap = 0.3, xaxis = list(showgrid = FALSE, showticklabels = FALSE, range = c(0, max(top$monto) * 1.6)),
             yaxis = list(tickfont = list(size = 11, color = TINTA)), margin = list(l = 10, r = 10, t = 5, b = 5))
  })

  # ---- Proveedores ----
  output$g_top_prov <- renderPlotly({
    t <- por_prov() |> slice_head(n = 15) |>
      mutate(et = corta(proveedor, 38), origen = factor(origen_proveedor, levels = names(COL_ORIGEN)))
    orden <- rev(t$et)
    plot_ly(t) |>
      add_segments(x = 0, xend = ~monto, y = ~et, yend = ~et, color = ~origen, colors = COL_ORIGEN,
                   line = list(width = 3), hoverinfo = "none", showlegend = FALSE) |>
      add_markers(x = ~monto, y = ~et, color = ~origen, colors = COL_ORIGEN,
                  marker = list(size = 14, line = list(color = "#ffffff", width = 2)),
                  text = ~paste0("<b>", proveedor, "</b><br>", fmt_soles(monto), " · ", ordenes, " órdenes<br>",
                                 entidades, " entidades · domicilio: ", departamento_proveedor),
                  hoverinfo = "text") |>
      estilo() |>
      layout(yaxis = list(categoryorder = "array", categoryarray = orden, gridcolor = "rgba(0,0,0,0)"),
             xaxis = list(tickformat = "~s"), legend = list(orientation = "h", x = 0, y = -0.08, yanchor = "top"))
  })
  output$g_pareto <- renderPlotly({
    p <- por_prov() |> mutate(pct_emp = rango / n())
    p <- bind_rows(tibble(pct_emp = 0, pct_acum = 0, rango = 0), p)
    k <- which(p$pct_acum >= 0.8)[1]
    plot_ly(p, x = ~pct_emp) |>
      add_trace(y = ~pct_acum, type = "scatter", mode = "lines", name = "Curva real",
                fill = "tozeroy", fillcolor = rgba(VIOLETA, 0.16), line = list(color = VIOLETA, width = 2.5),
                text = ~paste0(rango, " empresas (", pct(pct_emp), ")<br>concentran ", pct(pct_acum), " del monto"),
                hoverinfo = "text") |>
      add_trace(x = c(0, 1), y = c(0, 1), type = "scatter", mode = "lines", name = "Reparto igualitario",
                line = list(color = GRIS, width = 1.5, dash = "dot"), hoverinfo = "none") |>
      estilo(ytitulo = "% acumulado del monto", xtitulo = "% de empresas (de mayor a menor monto)") |>
      layout(xaxis = list(tickformat = ".0%"), yaxis = list(tickformat = ".0%", range = c(0, 1.03)),
             shapes = list(list(type = "line", x0 = 0, x1 = p$pct_emp[k], y0 = 0.8, y1 = 0.8,
                                line = list(color = NARANJA, dash = "dash", width = 1.5)),
                           list(type = "line", x0 = p$pct_emp[k], x1 = p$pct_emp[k], y0 = 0, y1 = 0.8,
                                line = list(color = NARANJA, dash = "dash", width = 1.5))),
             annotations = list(list(x = p$pct_emp[k], y = 0.8,
                                     text = paste0("<b>", pct(p$pct_emp[k]), "</b> de las empresas<br>hacen el 80 % del monto"),
                                     showarrow = TRUE, arrowhead = 0, arrowcolor = NARANJA, ax = 80, ay = 45,
                                     font = list(size = 11, color = TINTA), bgcolor = "rgba(255,255,255,0.9)")))
  })
  output$g_burbujas <- renderPlotly({
    b <- por_prov() |> mutate(origen = factor(origen_proveedor, levels = names(COL_ORIGEN)))
    plot_ly(b, x = ~ordenes, y = ~monto, size = ~entidades, sizes = c(25, 700),
            color = ~origen, colors = COL_ORIGEN, type = "scatter", mode = "markers",
            marker = list(opacity = 0.7, sizemode = "area", line = list(color = "#ffffff", width = 1.2)),
            text = ~paste0("<b>", proveedor, "</b><br>", fmt_soles(monto), "<br>", ordenes, " órdenes · ",
                           entidades, " entidades<br>", origen_proveedor),
            hoverinfo = "text") |>
      estilo(xtitulo = "N.º de órdenes (log)", ytitulo = "Monto vendido (S/, log)") |>
      layout(xaxis = list(type = "log", tickvals = c(1, 2, 5, 10, 20, 50, 100, 200, 500)),
             yaxis = list(type = "log", tickformat = "~s"))
  })
  output$g_origen <- renderPlotly({
    o <- datos() |> group_by(origen_proveedor) |> summarise(monto = sum(total), emp = n_distinct(ruc_proveedor), .groups = "drop") |>
      arrange(desc(monto))
    dona(o$origen_proveedor, o$monto, unname(COL_ORIGEN[o$origen_proveedor]),
         paste0("<b>", fmt_soles(sum(o$monto)), "</b>"),
         paste0(o$origen_proveedor, "<br>", fmt_soles(o$monto), " · ", o$emp, " empresas")) |>
      layout(legend = list(font = list(size = 10)))
  })
  output$g_tipo_emp <- renderPlotly({
    t <- datos() |> group_by(tipo_empresa) |> summarise(monto = sum(total), emp = n_distinct(ruc_proveedor), .groups = "drop") |>
      arrange(desc(monto))
    barras_h(t, "tipo_empresa", "monto", paste0(t$tipo_empresa, "<br>", fmt_soles(t$monto), " · ", t$emp, " empresas"),
             color = VIOLETA) |>
      layout(xaxis = list(tickformat = "~s"))
  })
  output$t_prov <- renderDT({
    t <- por_prov() |>
      transmute(`#` = rango, RUC = ruc_proveedor, Empresa = proveedor, Domicilio = departamento_proveedor,
                `Órdenes` = ordenes, Entidades = entidades, `Monto (S/)` = monto, `% del total` = pct)
    datatable(t, rownames = FALSE, style = "bootstrap5", class = "table table-hover",
              options = list(pageLength = 8, scrollX = TRUE, language = idioma_dt, dom = "ftp",
                             columnDefs = list(list(className = "recorta", targets = 2),
                                               list(className = "monto", targets = 6)))) |>
      formatCurrency("Monto (S/)", "S/ ", mark = " ", digits = 0) |>
      formatPercentage("% del total", 2) |>
      formatStyle("% del total", background = styleColorBar(c(0, max(t$`% del total`)), "#cde2fb"),
                  backgroundSize = "100% 6px", backgroundRepeat = "no-repeat", backgroundPosition = "bottom")
  })

  # ---- Entidades ----
  output$g_top_ent <- renderPlotly({
    e <- datos() |> group_by(entidad, sector) |> summarise(monto = sum(total), n = n(), .groups = "drop") |>
      arrange(desc(monto)) |> slice_head(n = 15) |>
      mutate(et = corta(entidad, 42), sector = factor(sector, levels = names(COL_SECTOR)))
    plot_ly(e, x = ~monto, y = ~et, color = ~sector, colors = COL_SECTOR, type = "bar", orientation = "h",
            text = ~paste0("<b>", entidad, "</b><br>", sector, "<br>", fmt_soles(monto), " · ", n, " órdenes"),
            hoverinfo = "text", textposition = "none") |>
      estilo() |>
      layout(barmode = "stack", bargap = 0.25, xaxis = list(tickformat = "~s"),
             yaxis = list(categoryorder = "array", categoryarray = rev(e$et)),
             legend = list(orientation = "h", x = 0, y = -0.08, yanchor = "top"))
  })
  output$g_sunburst <- renderPlotly({
    d <- datos() |> group_by(sector, entidad) |> summarise(monto = sum(total), .groups = "drop") |>
      group_by(sector) |> arrange(desc(monto), .by_group = TRUE) |>
      mutate(entidad = ifelse(row_number() <= 4, entidad, "Otras entidades del sector")) |>
      group_by(sector, entidad) |> summarise(monto = sum(monto), .groups = "drop")
    s <- d |> group_by(sector) |> summarise(monto = sum(monto), .groups = "drop")
    nodos <- bind_rows(
      tibble(id = s$sector, padre = "", etiqueta = s$sector, monto = s$monto,
             color = unname(COL_SECTOR[s$sector])),
      tibble(id = paste(d$sector, d$entidad, sep = " / "), padre = d$sector, etiqueta = corta(d$entidad, 28),
             monto = d$monto, color = sapply(unname(COL_SECTOR[d$sector]), rgba, a = 0.55))
    )
    plot_ly(nodos, type = "sunburst", ids = ~id, parents = ~padre, labels = ~etiqueta, values = ~monto,
            branchvalues = "total", maxdepth = 2, insidetextorientation = "radial",
            marker = list(colors = ~color, line = list(color = "#ffffff", width = 1.5)),
            hovertext = ~paste0("<b>", id, "</b><br>", fmt_soles(monto)), hoverinfo = "text") |>
      estilo(leyenda = FALSE) |>
      layout(margin = list(l = 0, r = 0, t = 0, b = 0))
  })
  output$g_sector <- renderPlotly({
    s <- datos() |> group_by(sector) |> summarise(monto = sum(total), n = n(), .groups = "drop") |> arrange(desc(monto))
    dona(s$sector, s$monto, unname(COL_SECTOR[s$sector]), paste0("<b>", fmt_soles(sum(s$monto)), "</b><br>en total"),
         paste0("<b>", s$sector, "</b><br>", fmt_soles(s$monto), " · ", fmt_n(s$n), " órdenes"))
  })
  output$g_prov_ent <- renderPlotly({
    s <- datos() |> filter(!is.na(provincia_entidad)) |> group_by(provincia_entidad) |>
      summarise(monto = sum(total), n = n(), ent = n_distinct(ruc_entidad), .groups = "drop") |> arrange(desc(monto))
    barras_h(s, "provincia_entidad", "monto",
             paste0("<b>", s$provincia_entidad, "</b><br>", fmt_soles(s$monto), " · ", fmt_n(s$n), " órdenes · ", s$ent, " entidades"),
             escala = ESC_AZUL_BARRAS) |>
      layout(xaxis = list(tickformat = "~s"))
  })
  output$g_calor <- renderPlotly({
    top_rubros <- datos() |> group_by(acuerdo_marco) |> summarise(m = sum(total)) |> arrange(desc(m)) |> slice_head(n = 12) |> pull(acuerdo_marco)
    c <- datos() |> filter(acuerdo_marco %in% top_rubros) |>
      group_by(sector, acuerdo_marco) |> summarise(monto = sum(total), .groups = "drop") |>
      complete(sector, acuerdo_marco, fill = list(monto = 0)) |>
      mutate(rubro = corta(acuerdo_marco, 36), z = ifelse(monto > 0, log10(monto), NA))
    plot_ly(c, x = ~sector, y = ~rubro, z = ~z, type = "heatmap", colorscale = ESC_AZUL,
            text = ~paste0("<b>", sector, "</b><br>", acuerdo_marco, "<br>", ifelse(monto > 0, fmt_soles(monto), "sin compras")),
            hoverinfo = "text", xgap = 3, ygap = 3,
            colorbar = list(title = list(text = "S/", font = list(size = 11)), thickness = 12, outlinewidth = 0,
                            tickvals = 3:7, ticktext = c("1 mil", "10 mil", "100 mil", "1 M", "10 M"))) |>
      estilo(leyenda = FALSE) |>
      layout(yaxis = list(autorange = "reversed", gridcolor = "rgba(0,0,0,0)"),
             xaxis = list(gridcolor = "rgba(0,0,0,0)", side = "top"))
  })

  # ---- Mapa por distritos ----
  por_distrito <- reactive({
    d <- datos() |> mutate(ub = ubigeo_mapa(ubigeo_entidad))
    top_ent <- d |> group_by(ub, entidad) |> summarise(m = sum(total), .groups = "drop") |>
      group_by(ub) |> slice_max(m, n = 1, with_ties = FALSE) |> ungroup() |> select(ub, entidad_top = entidad)
    top_rub <- d |> group_by(ub, acuerdo_marco) |> summarise(m = sum(total), .groups = "drop") |>
      group_by(ub) |> slice_max(m, n = 1, with_ties = FALSE) |> ungroup() |> select(ub, rubro_top = acuerdo_marco, m_rub = m)
    d |> group_by(ub) |>
      summarise(nombre = paste(sort(unique(distrito_entidad)), collapse = " + "),
                prov = first(provincia_entidad), monto = sum(total), ordenes = n(),
                entidades = n_distinct(ruc_entidad), .groups = "drop") |>
      left_join(top_ent, by = "ub") |> left_join(top_rub, by = "ub") |>
      arrange(desc(monto))
  })
  distrito_sel <- reactiveVal("210101")
  observeEvent(input$mapa_shape_click, { req(input$mapa_shape_click$id); distrito_sel(input$mapa_shape_click$id) })

  output$mapa <- renderLeaflet({
    leaflet(options = leafletOptions(attributionControl = FALSE, zoomSnap = 0.25, zoomDelta = 0.5)) |>
      addPolygons(data = st_union(mapa_puno), fill = FALSE, color = "#1f2340", weight = 2, opacity = 0.5,
                  options = pathOptions(interactive = FALSE)) |>
      fitBounds(-70.9, -17.2, -68.9, -13.2)
  })
  observe({
    req(input$mapa_zoom)   # espera a que el mapa exista (la pestaña arranca oculta)
    met <- input$metrica_mapa
    pd <- por_distrito()
    m <- mapa_puno |> left_join(pd, by = c("ubigeo" = "ub"))
    v <- m[[met]]
    lv <- log10(pmax(v, 1))
    pal <- colorNumeric(c("#cde2fb", "#86b6ef", "#3987e5", "#1c5cab", "#0d366b"),
                        domain = range(lv, na.rm = TRUE), na.color = "#e4e7ef")
    fmt <- switch(met, monto = fmt_soles, ordenes = fmt_n, entidades = fmt_n)
    etiqueta <- ifelse(is.na(v),
      sprintf("<b>%s</b><br><span style='color:#5b5f77'>Provincia %s</span><br>Sin órdenes con estos filtros",
              titulo(m$distrito), titulo(m$provincia)),
      sprintf("<b>%s</b><br><span style='color:#5b5f77'>Provincia %s</span><br>%s · %s órdenes · %s entidades<br><span style='color:#5b5f77'>Compra más:</span> %s<br><span style='color:#5b5f77'>Gasta más en:</span> %s",
              titulo(m$nombre), titulo(m$provincia), fmt_soles(m$monto), fmt_n(m$ordenes), m$entidades,
              corta(m$entidad_top, 40), corta(titulo(m$rubro_top), 40))) |> lapply(HTML)
    sel <- distrito_sel()
    marcas <- if (met == "monto") c(1e3, 1e4, 1e5, 1e6, 1e7) else c(1, 3, 10, 30, 100, 300, 1000, 3000)
    rng <- range(lv, na.rm = TRUE)
    marcas <- marcas[log10(marcas) >= rng[1] - 1e-9 & log10(marcas) <= rng[2] + 1e-9]
    es_sel <- m$ubigeo == sel
    leafletProxy("mapa", data = m) |>
      clearGroup("distritos") |> clearControls() |>
      addPolygons(group = "distritos", layerId = ~ubigeo, fillColor = pal(lv), fillOpacity = 0.88,
                  color = ifelse(es_sel, "#eb6834", "#ffffff"), weight = ifelse(es_sel, 3.5, 1),
                  label = etiqueta, labelOptions = labelOptions(textsize = "12px", direction = "auto"),
                  highlightOptions = highlightOptions(weight = 3, color = "#1f2340", bringToFront = TRUE)) |>
      addLegend("bottomleft", colors = pal(log10(marcas)), labels = fmt(marcas), opacity = 0.9,
                title = switch(met, monto = "Monto", ordenes = "Órdenes", entidades = "Entidades"))
  })

  datos_dist <- reactive({
    sel <- distrito_sel()
    datos() |> filter(ubigeo_mapa(ubigeo_entidad) == sel)
  })
  output$titulo_distrito <- renderUI({
    d <- datos_dist(); sel <- distrito_sel()
    nom <- if (sel == "211101") "Juliaca + San Miguel" else titulo(mapa_puno$distrito[mapa_puno$ubigeo == sel])
    prov <- titulo(mapa_puno$provincia[mapa_puno$ubigeo == sel])
    tagList(span(paste("Distrito", nom)),
            span(class = "subtitulo ms-0", paste0("Provincia ", prov, " · ",
                                                  n_distinct(d$ruc_entidad), " entidades compradoras")))
  })
  output$d_monto   <- renderText(if (nrow(datos_dist())) fmt_soles(sum(datos_dist()$total)) else "–")
  output$d_ordenes <- renderText(fmt_n(nrow(datos_dist())))
  output$g_dist_ent <- renderPlotly({
    d <- datos_dist()
    validate(need(nrow(d) > 0, "Este distrito no tiene órdenes con los filtros actuales."))
    e <- d |> group_by(entidad, sector) |> summarise(monto = sum(total), n = n(), .groups = "drop") |>
      arrange(desc(monto)) |> slice_head(n = 5) |> mutate(et = corta(entidad, 30))
    plot_ly(e, x = ~monto, y = ~et, type = "bar", orientation = "h",
            marker = list(color = unname(COL_SECTOR[e$sector])),
            text = ~paste0("<b>", entidad, "</b><br>", sector, "<br>", fmt_soles(monto), " · ", n, " órdenes"),
            hoverinfo = "text", textposition = "none") |>
      estilo(leyenda = FALSE) |>
      layout(xaxis = list(tickformat = "~s"), bargap = 0.3,
             yaxis = list(categoryorder = "array", categoryarray = rev(e$et), gridcolor = "rgba(0,0,0,0)",
                          tickfont = list(size = 10)))
  })
  output$g_dist_rubro <- renderPlotly({
    d <- datos_dist()
    validate(need(nrow(d) > 0, ""))
    r <- d |> group_by(acuerdo_marco) |> summarise(monto = sum(total), n = n(), .groups = "drop") |>
      arrange(desc(monto)) |> slice_head(n = 5) |> mutate(et = corta(titulo(acuerdo_marco), 30))
    barras_h(r, "et", "monto", paste0("<b>", r$acuerdo_marco, "</b><br>", fmt_soles(r$monto), " · ", r$n, " órdenes"),
             escala = ESC_AZUL_BARRAS) |>
      layout(xaxis = list(tickformat = "~s"), bargap = 0.3, yaxis = list(tickfont = list(size = 10)))
  })
  output$g_rank_dist <- renderPlotly({
    r <- por_distrito() |> slice_head(n = 15) |> mutate(et = titulo(nombre), provincia = titulo(prov))
    plot_ly(r, x = ~monto, y = ~et, type = "bar", orientation = "h",
            marker = list(color = log10(r$monto), colorscale = ESC_AZUL_BARRAS, showscale = FALSE),
            text = ~paste0("<b>", et, "</b> (", provincia, ")<br>", fmt_soles(monto), " · ", fmt_n(ordenes),
                           " órdenes · ", entidades, " entidades"),
            hoverinfo = "text", textposition = "none") |>
      estilo(xtitulo = "Monto (S/, escala log)", leyenda = FALSE) |>
      layout(xaxis = list(type = "log", tickvals = c(1e4, 1e5, 1e6, 1e7, 1e8),
                          ticktext = c("10 mil", "100 mil", "1 M", "10 M", "100 M")),
             yaxis = list(categoryorder = "array", categoryarray = rev(r$et), gridcolor = "rgba(0,0,0,0)"),
             bargap = 0.28)
  })
  output$t_distritos <- renderDT({
    t <- por_distrito() |>
      transmute(Distrito = titulo(nombre), `Monto (S/)` = monto, `Órdenes` = ordenes,
                `Entidad que más compra` = entidad_top,
                `Rubro con más gasto` = paste0(titulo(rubro_top), " (", pct(m_rub / monto, 1), ")"))
    datatable(t, rownames = FALSE, style = "bootstrap5", class = "table table-hover", selection = "none",
              options = list(pageLength = 10, scrollX = TRUE, language = idioma_dt, dom = "ftp",
                             columnDefs = list(list(className = "nowrap", targets = c(0, 2)),
                                               list(className = "recorta", targets = 3:4),
                                               list(className = "monto dt-right", targets = 1)))) |>
      formatCurrency("Monto (S/)", "S/ ", mark = " ", digits = 0)
  })

  # ---- EDA ----
  output$t_desc <- renderTable({
    x <- datos()$total
    q <- quantile(x, c(.25, .5, .75))
    asim <- mean((x - mean(x))^3) / sd(x)^3
    num <- function(v) comma(v, 0.01, big.mark = " ", decimal.mark = ",")
    data.frame(
      Estadístico = c("N.º de órdenes", "Media", "Mediana", "Desviación estándar", "Mínimo",
                      "Percentil 25", "Percentil 75", "Máximo", "Rango intercuartílico", "Coef. de asimetría", "Coef. de variación"),
      Valor = c(fmt_n(length(x)), num(mean(x)), num(q[2]), num(sd(x)), num(min(x)), num(q[1]), num(q[3]),
                num(max(x)), num(q[3] - q[1]), number(asim, 0.01, decimal.mark = ","), pct(sd(x) / mean(x)))
    )
  }, striped = TRUE, hover = TRUE, width = "100%", align = "lr")
  output$txt_sesgo <- renderText({
    x <- datos()$total
    paste0("La media es ", number(mean(x) / median(x), 0.1, decimal.mark = ","),
           " veces la mediana: la distribución tiene sesgo a la derecha (muchas compras pequeñas y pocas muy grandes).")
  })
  output$g_hist <- renderPlotly({
    d <- datos()
    lx <- log10(pmax(d$total, 1))
    cortes <- seq(floor(min(lx) * 10) / 10, ceiling(max(lx) * 10) / 10 + 0.1, by = 0.1)
    h <- tibble(b = cut(lx, cortes, right = FALSE, labels = FALSE)) |> count(b) |>
      mutate(centro = cortes[b] + 0.05, desde = 10^cortes[b], hasta = 10^(cortes[b] + 0.1))
    med <- log10(median(d$total))
    plot_ly(h, x = ~centro, y = ~n, type = "bar", width = 0.088,
            marker = list(color = ~centro, colorscale = ESC_AZUL_BARRAS, cmin = min(h$centro), cmax = max(h$centro),
                          showscale = FALSE),
            text = ~paste0(fmt_soles(desde), " a ", fmt_soles(hasta), "<br><b>", n, " órdenes</b>"),
            hoverinfo = "text", textposition = "none") |>
      estilo(ytitulo = "N.º de órdenes", xtitulo = "Monto de la orden (S/, escala log)", leyenda = FALSE) |>
      layout(xaxis = list(tickvals = 1:7, ticktext = c("10", "100", "1 mil", "10 mil", "100 mil", "1 M", "10 M")),
             shapes = list(list(type = "line", x0 = med, x1 = med, y0 = 0, y1 = 1, yref = "paper",
                                line = list(color = NARANJA, dash = "dash", width = 2))),
             annotations = list(list(x = med, y = 0.98, yref = "paper", showarrow = FALSE, xanchor = "left",
                                     text = paste0("  <b>mediana</b> ", fmt_soles(median(d$total))),
                                     font = list(size = 11, color = TINTA))))
  })
  output$g_caja <- renderPlotly({
    d <- datos()
    orden <- d |> group_by(sector) |> summarise(m = median(total)) |> arrange(m) |> pull(sector)
    lx_todo <- log10(pmax(d$total, 1))
    rango_x <- c(floor(min(lx_todo)), ceiling(max(lx_todo)))
    # Gráfico de crestas: una curva de densidad por sector, apiladas en vertical
    p <- plot_ly()
    for (i in rev(seq_along(orden))) {
      s <- orden[i]
      x <- log10(pmax(d$total[d$sector == s], 1))
      if (length(x) < 3) next
      k <- density(x, from = rango_x[1], to = rango_x[2], n = 200, adjust = 1.1)
      alto <- k$y / max(k$y) * 1.35
      col <- COL_SECTOR[[s]]
      med <- median(x)
      p <- p |>
        add_trace(x = c(k$x, rev(k$x)), y = c(i + alto, rep(i, length(k$x))), type = "scatter", mode = "lines",
                  fill = "toself", fillcolor = rgba(col, 0.55), line = list(color = col, width = 2),
                  text = paste0("<b>", s, "</b><br>", length(x), " órdenes · mediana ", fmt_soles(10^med)),
                  hoverinfo = "text", hoveron = "fills", showlegend = FALSE) |>
        add_trace(x = c(med, med), y = c(i, i + approx(k$x, alto, med)$y), type = "scatter", mode = "lines",
                  line = list(color = TINTA, width = 2), hoverinfo = "none", showlegend = FALSE)
    }
    p |>
      estilo(xtitulo = "Monto de la orden (S/, escala log)", leyenda = FALSE) |>
      layout(xaxis = list(tickvals = 1:7, ticktext = c("10", "100", "1 mil", "10 mil", "100 mil", "1 M", "10 M"),
                          range = rango_x),
             yaxis = list(tickvals = seq_along(orden), ticktext = orden, gridcolor = REJILLA,
                          range = c(0.8, length(orden) + 1.6)))
  })
  output$g_rango <- renderPlotly({
    r <- datos() |> group_by(rango_monto) |> summarise(n = n(), monto = sum(total), .groups = "drop") |>
      mutate(pn = n / sum(n), pm = monto / sum(monto),
             et = sub("^\\d\\. ", "", rango_monto) |> sub(pattern = "Menos de ", replacement = "< ") |>
               sub(pattern = "Más de ", replacement = "> ") |> sub(pattern = " - ", replacement = "<br>a "))
    plot_ly(r, x = ~et, y = ~pn, type = "bar", name = "% de órdenes",
            marker = list(color = AZUL), texttemplate = "%{y:.0%}", textposition = "outside",
            textfont = list(size = 10, color = TINTA2),
            hovertext = ~paste0(et, "<br>", fmt_n(n), " órdenes (", pct(pn), ")"), hoverinfo = "text") |>
      add_trace(y = ~pm, name = "% del monto", marker = list(color = NARANJA),
                hovertext = ~paste0(et, "<br>", fmt_soles(monto), " (", pct(pm), " del monto)")) |>
      estilo() |>
      layout(yaxis = list(tickformat = ".0%", range = c(0, max(c(r$pn, r$pm)) * 1.15)),
             xaxis = list(categoryorder = "array", categoryarray = r$et[order(r$rango_monto)], tickangle = 0),
             barmode = "group", bargap = 0.25, bargroupgap = 0.08)
  })
  output$g_semana <- renderPlotly({
    dias <- c("Dom", "Lun", "Mar", "Mié", "Jue", "Vie", "Sáb")
    s <- datos() |> mutate(dia = factor(dias[as.POSIXlt(fecha)$wday + 1], levels = c(dias[-1], dias[1]))) |>
      count(periodo, dia) |> complete(periodo, dia, fill = list(n = 0)) |>
      mutate(mes = format(as.Date(paste0(periodo, "-01")), "%b %Y"), fmes = as.Date(paste0(periodo, "-01")))
    plot_ly(s, x = ~fmes, y = ~dia, z = ~n, type = "heatmap", colorscale = ESC_NARANJA,
            text = ~paste0("<b>", dia, "</b> · ", mes, "<br>", n, " órdenes"), hoverinfo = "text",
            xgap = 3, ygap = 3, colorbar = list(title = list(text = "órdenes", font = list(size = 11)),
                                                 thickness = 12, outlinewidth = 0)) |>
      estilo(leyenda = FALSE) |>
      layout(yaxis = list(autorange = "reversed", gridcolor = "rgba(0,0,0,0)"),
             xaxis = list(gridcolor = "rgba(0,0,0,0)", tickangle = 0, dtick = "M2", tickformat = "%b
%Y"))
  })
  output$t_calidad <- renderTable({
    todo <- compras
    data.frame(
      Revisión = c("Órdenes en el dataset", "Órdenes duplicadas", "Valores vacíos en columnas clave",
                   "Montos menores o iguales a 0", "Órdenes resueltas o nulas", "Entidades sin provincia",
                   "Proveedores sin domicilio SUNAT"),
      Resultado = c(fmt_n(nrow(todo)), fmt_n(sum(duplicated(todo$orden))),
                    fmt_n(sum(is.na(todo[, c("fecha", "ruc_entidad", "ruc_proveedor", "total", "acuerdo_marco")]))),
                    fmt_n(sum(todo$total <= 0)),
                    fmt_n(sum(todo$estado %in% c("RESUELTA", "ORDEN DE COMPRA NULA"))),
                    fmt_n(sum(is.na(todo$provincia_entidad))),
                    fmt_n(n_distinct(todo$ruc_proveedor[todo$departamento_proveedor == "SIN DATO"])))
    )
  }, striped = TRUE, width = "100%", align = "lr")
  output$t_atipicos <- renderDT({
    x <- datos()$total
    lim <- quantile(x, .75) + 3 * IQR(x)
    t <- datos() |> filter(total > lim) |> arrange(desc(total)) |>
      transmute(Fecha = fecha, Entidad = entidad, Empresa = proveedor, Rubro = acuerdo_marco, `Monto (S/)` = total)
    t$Fecha <- format(t$Fecha, "%d/%m/%Y")
    datatable(t, rownames = FALSE, style = "bootstrap5", class = "table table-hover",
              caption = paste0("Atípicos extremos: monto > Q3 + 3·RIC (", fmt_soles(lim), ")"),
              options = list(pageLength = 6, scrollX = TRUE, language = idioma_dt, dom = "tp",
                             columnDefs = list(list(className = "nowrap", targets = 0),
                                               list(className = "recorta", targets = 1:3),
                                               list(className = "monto", targets = 4)))) |>
      formatCurrency("Monto (S/)", "S/ ", mark = " ", digits = 0)
  })

  # ---- Datos ----
  output$t_datos <- renderDT({
    clase <- ifelse(datos()$estado %in% c("ACEPTADA", "PAGADA"), paste0("est-", datos()$estado), "est-otro")
    datos() |>
      transmute(Fecha = format(fecha, "%d/%m/%Y"), Entidad = entidad, Sector = sector,
                Empresa = proveedor, Rubro = acuerdo_marco, `Monto (S/)` = total,
                Estado = paste0('<span class="pastilla ', clase, '">', tolower(estado), '</span>'),
                Provincia = provincia_entidad, `Domicilio empresa` = departamento_proveedor) |>
      datatable(rownames = FALSE, escape = FALSE, style = "bootstrap5", class = "table table-hover",
                options = list(pageLength = 15, scrollX = TRUE, language = idioma_dt,
                               columnDefs = list(list(className = "nowrap", targets = c(0, 2, 6, 7, 8)),
                                                 list(className = "recorta", targets = c(1, 3, 4)),
                                                 list(className = "monto dt-right", targets = 5)))) |>
      formatCurrency("Monto (S/)", "S/ ", mark = " ", dec.mark = ",", digits = 2)
  })
  output$txt_n_datos <- renderText(paste0(fmt_n(nrow(datos())), " órdenes · ", fmt_soles(sum(datos()$total))))
  observeEvent(input$limpiar, {
    updateDateRangeInput(session, "fechas", start = min(compras$fecha), end = max(compras$fecha))
    updateSelectizeInput(session, "sector", selected = character(0))
    updateSelectizeInput(session, "provincia", selected = character(0))
    updateSelectizeInput(session, "rubro", selected = character(0))
    updateCheckboxGroupInput(session, "estado", selected = ESTADOS_DEF)
    aplicados(FILTROS_DEF)
  })
  output$descargar <- downloadHandler(
    filename = function() paste0("compras_puno_filtrado_", Sys.Date(), ".csv"),
    content = function(file) write_csv(datos(), file)
  )
}

shinyApp(ui, server)
