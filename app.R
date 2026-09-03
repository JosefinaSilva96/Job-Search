# ============================================================
# Dashboard de Matching CV-Trabajo (enfocado en Chile)
#
# Flujo:
#   1. Subes tu CV (PDF)
#   2. Vas agregando ofertas manualmente (copiando el link y la
#      descripción desde LinkedIn u otro sitio - ver mensaje anterior
#      sobre por qué no se hace scraping automático)
#   3. La tabla muestra el % de match de tu CV contra cada oferta,
#      más sugerencias de qué palabras clave le faltan a tu CV
#   4. Puedes pedirle a la IA (Gemini, de Google, vía su API gratuita)
#      que analice tu CV contra una oferta específica y te sugiera cómo
#      reformular tu experiencia para esa postulación - esto va más
#      allá de palabras clave sueltas, porque entiende el contexto real
#   5. Descargas todo en Excel o PDF para revisarlo con calma
#
# NOTA: los datos viven solo mientras la sesión está abierta (no hay
# base de datos detrás). Si cierras la app, se pierde lo que agregaste,
# así que conviene descargar el Excel/PDF antes de cerrar. Si más
# adelante quieres que varias personas compartan la misma tabla de
# ofertas entre sesiones, se puede sumar persistencia (ej. Google
# Sheets o una base de datos simple) - lo dejamos para otro paso.
#
# PRIVACIDAD: la función de sugerencias con IA envía el texto completo
# del CV y de la oferta a la API de Google (Gemini) para poder
# analizarlos. Cada persona necesita su propia API key GRATIS
# (consíguela en https://aistudio.google.com/apikey - no pide tarjeta
# en la mayoría de los países) - se pide en un campo tipo contraseña
# que solo vive en la memoria de esa sesión, nunca se guarda en disco
# ni se comparte entre usuarias. El tier gratis tiene un límite de
# usos por día (no es para uso masivo, pero alcanza bien para un grupo
# chico de amigas probando el dashboard).
# ============================================================


library(shiny)
library(pdftools)
library(stringr)
library(stringi)
library(dplyr)
library(purrr)
library(DT)
library(openxlsx)
library(httr)
library(jsonlite)
library(gridExtra)
library(grid)
library(bslib)

# Correo de contacto que se muestra en el header y en la pestaña de
# instrucciones - cámbialo aquí si alguna vez migra a otra persona.
CONTACTO_EMAIL <- "josefinasilvafuente@gmail.com"

# ------------------------------------------------------------
# 1. REGIONES DE CHILE
# ------------------------------------------------------------
regiones_chile <- c(
  "Arica y Parinacota", "Tarapacá", "Antofagasta", "Atacama", "Coquimbo",
  "Valparaíso", "Metropolitana de Santiago", "Libertador Gral. Bernardo O'Higgins",
  "Maule", "Ñuble", "Biobío", "La Araucanía", "Los Ríos", "Los Lagos",
  "Aysén", "Magallanes y de la Antártica Chilena", "Internacional / Remoto"
)

# ------------------------------------------------------------
# 2. TEXTO, MATCHING Y SUGERENCIAS
# ------------------------------------------------------------

# Stopwords básicas ES/EN para no sugerir palabras sin valor ("para", "with", etc.)
stopwords_es_en <- c(
  "de", "la", "el", "en", "y", "a", "los", "las", "un", "una", "para",
  "con", "por", "que", "se", "su", "sus", "del", "al", "es", "como",
  "más", "mas", "o", "u", "e", "tu", "les", "lo", "le", "nos", "muy",
  "the", "and", "of", "to", "in", "for", "with", "on", "at", "by", "is",
  "are", "this", "that", "an", "be", "will", "we", "you", "your", "our",
  "or", "as", "from", "have", "has", "not", "all", "can", "job", "work"
)

extraer_texto_pdf <- function(filepath) {
  resultado <- tryCatch(
    list(ok = TRUE, texto = paste(pdftools::pdf_text(filepath), collapse = " ")),
    error = function(e) list(ok = FALSE, texto = "", error = conditionMessage(e))
  )
  resultado
}

tokenizar <- function(texto) {
  # Normaliza acentos (algunos PDFs devuelven letras + tilde combinada en
  # vez de la letra acentuada ya compuesta, lo que rompe las comparaciones)
  texto <- stringi::stri_trans_general(texto, "NFC")
  texto <- str_to_lower(texto)
  texto <- str_replace_all(texto, "[^a-záéíóúñ ]", " ")
  tokens <- str_split(texto, "\\s+")[[1]]
  tokens[nchar(tokens) > 2]
}

# Igual que tokenizar(), pero además saca conectores y palabras muy
# cortas - esta es la versión que se usa para calcular el match, así
# no se diluye con "de", "la", "en", etc.
tokenizar_significativas <- function(texto) {
  tokens <- tokenizar(texto)
  tokens <- setdiff(tokens, stopwords_es_en)
  tokens[nchar(tokens) > 3]
}

# Similitud coseno sobre frecuencia de palabras significativas.
# Mucho más representativa que Jaccard puro para textos largos como un
# CV: en vez de dividir por TODO el vocabulario combinado (que hace que
# el % caiga casi siempre a 0 cuando el CV es largo), pondera cuánto se
# repiten las palabras que sí se comparten. Para más precisión aún, el
# siguiente paso sería usar embeddings semánticos (text2vec o
# sentence-transformers vía reticulate) que también capten sinónimos.
similitud_coseno <- function(tokens_cv, tokens_oferta) {
  if (length(tokens_cv) == 0 || length(tokens_oferta) == 0) return(0)
  vocab <- union(unique(tokens_cv), unique(tokens_oferta))
  if (length(vocab) == 0) return(0)
  vec_cv <- as.numeric(table(factor(tokens_cv, levels = vocab)))
  vec_of <- as.numeric(table(factor(tokens_oferta, levels = vocab)))
  num <- sum(vec_cv * vec_of)
  denom <- sqrt(sum(vec_cv^2)) * sqrt(sum(vec_of^2))
  if (denom == 0) return(0)
  round(100 * num / denom, 1)
}

# Sugerencias: palabras que aparecen seguido en la oferta pero no están
# en el CV. Es una heurística simple (frecuencia de palabras), no un
# análisis semántico real - úsalo como pista, no como verdad absoluta.
# Para cada palabra que falta, se agrega la frase de la oferta donde
# aparece, así das el contexto tú misma en vez de ver la palabra sola
# (una lista de palabras sueltas no dice nada; "gestión de liquidez y
# sistemas de pagos" sí orienta hacia qué destacar).
extraer_frase_contexto <- function(texto_original, palabra, maximo = 140) {
  oraciones <- str_split(texto_original, "(?<=[.!?])\\s+")[[1]]
  idx <- which(str_detect(str_to_lower(oraciones), fixed(str_to_lower(palabra))))
  if (length(idx) == 0) return("")
  frase <- str_trim(oraciones[idx[1]])
  if (nchar(frase) > maximo) frase <- paste0(str_sub(frase, 1, maximo), "...")
  frase
}

sugerir_mejoras_cv <- function(tokens_cv, tokens_oferta, texto_oferta_original = "", top_n = 4) {
  if (length(tokens_oferta) == 0) return("")
  tabla <- table(tokens_oferta)
  candidatos <- setdiff(names(tabla), unique(tokens_cv))
  candidatos <- setdiff(candidatos, stopwords_es_en)
  candidatos <- candidatos[nchar(candidatos) > 3]
  if (length(candidatos) == 0) {
    return("Tu CV ya cubre las palabras clave principales de esta oferta.")
  }
  candidatos_ordenados <- names(sort(tabla[candidatos], decreasing = TRUE))
  top <- head(candidatos_ordenados, top_n)

  if (nchar(texto_oferta_original) == 0) {
    return(paste("Considera mencionar:", paste(top, collapse = ", ")))
  }

  lineas <- vapply(top, function(palabra) {
    contexto <- extraer_frase_contexto(texto_oferta_original, palabra)
    if (nchar(contexto) == 0) {
      paste0("- '", palabra, "'")
    } else {
      paste0("- '", palabra, "' (la oferta dice: \"", contexto, "\")")
    }
  }, character(1))

  paste0("Palabras que no aparecen en tu CV, con el contexto de la oferta:\n",
         paste(lineas, collapse = "\n"))
}

# ------------------------------------------------------------
# 2b. SUGERENCIAS CON IA (Gemini de Google, o Groq como alternativa)
# ------------------------------------------------------------
# A diferencia de sugerir_mejoras_cv() (que solo compara palabras
# sueltas), esto le manda el CV completo y la oferta completa a un
# modelo de lenguaje para que dé consejos de verdad: qué experiencia
# destacar, cómo reformular logros, y qué le falta. No reescribe el CV
# por ti automáticamente (eso implicaría regenerar el PDF/Word con su
# formato original, que es un paso aparte) - te da el texto para que
# tú edites tu CV con criterio.
#
# Hay DOS proveedores gratuitos disponibles, porque cada uno tiene sus
# propios límites de uso diario y conviene tener un plan B si uno se
# queda sin cupo:
#   - Gemini (Google): consíguela en https://aistudio.google.com/apikey
#     Límite: bastante bajo por día en el tier gratis (variable según
#     modelo, ronda las 20-50 consultas/día en modelos "flash").
#   - Groq (sirve modelos open-source como Llama, muy rápido):
#     consíguela en https://console.groq.com/keys
#     Límite: mucho más generoso, ronda las 14.000+ consultas/día.
#     La calidad de las respuestas es buena pero algo distinta a
#     Gemini/Claude, al ser un modelo abierto en vez de uno propietario.

construir_prompt_cv <- function(cv_texto, oferta_titulo, oferta_empresa, oferta_texto) {
  paste0(
    "Eres un asesor de carrera experto ayudando a una persona a preparar ",
    "su CV para postular a un cargo específico en Chile. ",
    "A continuación te doy el texto completo de su CV y el texto completo ",
    "de la oferta de trabajo.\n\n",
    "=== CARGO ===\n", oferta_titulo, " en ", oferta_empresa, "\n\n",
    "=== TEXTO DE LA OFERTA ===\n", oferta_texto, "\n\n",
    "=== TEXTO DEL CV ===\n", cv_texto, "\n\n",
    "Dame una respuesta en español, organizada así:\n",
    "1. FORTALEZAS A DESTACAR: 3-5 puntos concretos de su experiencia/CV ",
    "que calzan bien con este cargo y cómo redactarlos mejor para esta oferta.\n",
    "2. BRECHAS O DEBILIDADES: qué pide la oferta que no se ve reflejado ",
    "claramente en el CV, y cómo mitigarlo (con experiencia que sí tenga, ",
    "aunque no lo haya conectado explícitamente).\n",
    "3. SUGERENCIA DE RESUMEN/PERFIL: un párrafo corto (3-4 líneas) que ",
    "podría usar como resumen profesional al inicio del CV, adaptado a ",
    "este cargo específico.\n",
    "Sé concreto y específico al CV y la oferta reales, no genérico."
  )
}

# Si el nombre del modelo por defecto ya no existe (Google los renueva
# seguido), revisa el nombre vigente en
# https://ai.google.dev/gemini-api/docs/models y cámbialo en el campo
# "Modelo" de la app.
generar_sugerencias_ia_gemini <- function(api_key, modelo, cv_texto, oferta_titulo,
                                           oferta_empresa, oferta_texto) {
  if (nchar(api_key) == 0) {
    return(list(ok = FALSE, mensaje = "Falta la API key de Google Gemini."))
  }

  prompt <- construir_prompt_cv(cv_texto, oferta_titulo, oferta_empresa, oferta_texto)

  url <- paste0(
    "https://generativelanguage.googleapis.com/v1beta/models/",
    modelo, ":generateContent?key=", api_key
  )

  # Reintenta un par de veces si el modelo gratuito viene sobrecargado
  # (error 503 "UNAVAILABLE") - es algo temporal del lado de Google, no
  # de la app, así que un pequeño reintento con espera suele resolverlo.
  intentos <- 3
  for (intento in seq_len(intentos)) {
    res <- tryCatch(
      POST(
        url = url,
        add_headers("content-type" = "application/json"),
        body = toJSON(list(
          contents = list(list(parts = list(list(text = prompt))))
        ), auto_unbox = TRUE),
        encode = "raw"
      ),
      error = function(e) e
    )

    if (!inherits(res, "error") && !inherits(res, "simpleError") &&
        httr::status_code(res) != 503) {
      break
    }
    if (intento < intentos) Sys.sleep(3)
  }

  if (inherits(res, "error") || inherits(res, "simpleError")) {
    return(list(ok = FALSE, mensaje = paste("Error de conexión:", conditionMessage(res))))
  }

  if (httr::status_code(res) != 200) {
    cuerpo <- tryCatch(content(res, "text", encoding = "UTF-8"), error = function(e) "")
    if (httr::status_code(res) == 503) {
      return(list(ok = FALSE, mensaje = paste0(
        "Los servidores de Gemini están saturados ahora mismo (probamos 3 veces). ",
        "Espera un minuto y prueba de nuevo, o cambia de proveedor a Groq mientras tanto."
      )))
    }
    if (httr::status_code(res) == 429) {
      return(list(ok = FALSE, mensaje = paste0(
        "Llegaste al límite diario gratuito de Gemini para este modelo (el tier ",
        "gratis permite un número limitado de consultas por día, no ilimitadas). ",
        "Espera a que se resetee (usualmente a medianoche), activa facturación, ",
        "o cambia de proveedor a Groq mientras tanto - tiene un cupo diario mucho mayor."
      )))
    }
    return(list(ok = FALSE, mensaje = paste0("Error de la API (código ",
                                              httr::status_code(res), "): ", cuerpo)))
  }

  parsed <- tryCatch(fromJSON(content(res, "text", encoding = "UTF-8")),
                      error = function(e) NULL)
  texto_respuesta <- tryCatch(parsed$candidates$content$parts[[1]]$text[1],
                               error = function(e) NULL)

  if (is.null(texto_respuesta) || is.na(texto_respuesta)) {
    return(list(ok = FALSE, mensaje = "La API respondió pero no se pudo leer el texto. Puede que el modelo indicado ya no exista - revisa https://ai.google.dev/gemini-api/docs/models"))
  }

  list(ok = TRUE, mensaje = texto_respuesta)
}

# Groq usa una API compatible con el formato de OpenAI (chat completions).
# Consigue tu key gratis en https://console.groq.com/keys - no pide
# tarjeta. Si el modelo por defecto deja de existir, la lista vigente
# está en https://console.groq.com/docs/models.
generar_sugerencias_ia_groq <- function(api_key, modelo, cv_texto, oferta_titulo,
                                         oferta_empresa, oferta_texto) {
  if (nchar(api_key) == 0) {
    return(list(ok = FALSE, mensaje = "Falta la API key de Groq."))
  }

  prompt <- construir_prompt_cv(cv_texto, oferta_titulo, oferta_empresa, oferta_texto)

  url <- "https://api.groq.com/openai/v1/chat/completions"

  intentos <- 3
  for (intento in seq_len(intentos)) {
    res <- tryCatch(
      POST(
        url = url,
        add_headers(
          "Authorization" = paste("Bearer", api_key),
          "content-type" = "application/json"
        ),
        body = toJSON(list(
          model = modelo,
          messages = list(list(role = "user", content = prompt))
        ), auto_unbox = TRUE),
        encode = "raw"
      ),
      error = function(e) e
    )

    if (!inherits(res, "error") && !inherits(res, "simpleError") &&
        httr::status_code(res) != 503) {
      break
    }
    if (intento < intentos) Sys.sleep(3)
  }

  if (inherits(res, "error") || inherits(res, "simpleError")) {
    return(list(ok = FALSE, mensaje = paste("Error de conexión:", conditionMessage(res))))
  }

  if (httr::status_code(res) != 200) {
    cuerpo <- tryCatch(content(res, "text", encoding = "UTF-8"), error = function(e) "")
    if (httr::status_code(res) == 429) {
      return(list(ok = FALSE, mensaje = paste0(
        "Llegaste al límite de uso de Groq por ahora (por minuto o por día). ",
        "Espera un momento y prueba de nuevo, o cambia de proveedor a Gemini mientras tanto."
      )))
    }
    return(list(ok = FALSE, mensaje = paste0("Error de la API de Groq (código ",
                                              httr::status_code(res), "): ", cuerpo)))
  }

  parsed <- tryCatch(fromJSON(content(res, "text", encoding = "UTF-8")),
                      error = function(e) NULL)
  texto_respuesta <- tryCatch(parsed$choices$message$content[1], error = function(e) NULL)

  if (is.null(texto_respuesta) || is.na(texto_respuesta)) {
    return(list(ok = FALSE, mensaje = "La API de Groq respondió pero no se pudo leer el texto. Puede que el modelo indicado ya no exista - revisa https://console.groq.com/docs/models"))
  }

  list(ok = TRUE, mensaje = texto_respuesta)
}

# Despachador: llama a la función del proveedor elegido en la app.
generar_sugerencias_ia <- function(proveedor, api_key, modelo, cv_texto,
                                    oferta_titulo, oferta_empresa, oferta_texto) {
  if (proveedor == "groq") {
    generar_sugerencias_ia_groq(api_key, modelo, cv_texto, oferta_titulo,
                                 oferta_empresa, oferta_texto)
  } else {
    generar_sugerencias_ia_gemini(api_key, modelo, cv_texto, oferta_titulo,
                                   oferta_empresa, oferta_texto)
  }
}

# ------------------------------------------------------------
# 3. UI
# ------------------------------------------------------------
ui <- fluidPage(
  theme = bslib::bs_theme(
    version = 5,
    bg = "#f7f6fb",
    fg = "#3f3a52",
    primary = "#a89ce8",
    success = "#8fd3b6",
    danger = "#e8a5a5",
    base_font = bslib::font_google("Inter"),
    heading_font = bslib::font_google("Inter"),
    "border-radius" = "0.6rem"
  ),
  # Truco para que un fileInput dispare el evento de carga incluso si
  # se vuelve a elegir el MISMO archivo dos veces seguidas (por defecto
  # los navegadores no avisan si "no cambió nada" desde su punto de
  # vista, así que la app nunca se entera de la segunda subida).
  tags$head(tags$script(HTML("
    Shiny.addCustomMessageHandler('resetFileInput', function(id) {
      var el = document.getElementById(id);
      if (el) { el.value = ''; }
    });
  "))),
  tags$head(tags$style(HTML("
    .well { background-color: #ffffff; border: none; border-radius: 14px;
            box-shadow: 0 2px 14px rgba(63,58,82,0.06); padding: 22px; }
    .tab-content { background-color: #ffffff; border-radius: 0 14px 14px 14px;
                   padding: 24px; box-shadow: 0 2px 14px rgba(63,58,82,0.06); }
    .nav-tabs { border-bottom: none; margin-bottom: -1px; }
    .nav-tabs > li > a { border-radius: 10px 10px 0 0; font-weight: 600; color: #9c94b8; }
    .nav-tabs > li.active > a, .nav-tabs > li.active > a:focus, .nav-tabs > li.active > a:hover {
      color: #8a7cc9; background-color: #ffffff; border-color: transparent;
    }
    h3, h4 { color: #8a7cc9; font-weight: 700; }
    .app-header {
      background: linear-gradient(135deg, #d8cff2 0%, #f0d9e6 100%);
      color: #4a4166; padding: 26px 32px; border-radius: 16px; margin-bottom: 22px;
      box-shadow: 0 4px 18px rgba(168,156,232,0.20);
    }
    .app-header h1 { margin: 0; font-size: 26px; font-weight: 800; color: #5b4e8c; }
    .app-header p.subtitle { margin: 6px 0 0 0; opacity: 0.85; font-size: 14.5px; }
    .app-header p.contacto { margin: 10px 0 0 0; font-size: 13px; opacity: 0.8; }
    .app-header p.contacto a { color: #5b4e8c; text-decoration: underline; font-weight: 600; }
    .btn-primary { border-radius: 8px; font-weight: 600; color: #4a4166; border-color: #a89ce8; }
    .btn-success { border-radius: 8px; font-weight: 600; }
    .btn-danger { border-radius: 8px; font-weight: 600; }
    table.dataTable { border-radius: 10px; overflow: hidden; }
    .contacto-footer { text-align: center; color: #b3abc9; font-size: 12.5px; margin-top: 30px; }
    .contacto-footer a { color: #a89ce8; }
  "))),
  div(
    class = "app-header",
    h1("🔎 Dashboard de Matching CV - Ofertas de Trabajo"),
    p(class = "subtitle",
      "Sube tu CV, agrega ofertas, revisa el % de match y pide sugerencias con IA."),
    p(class = "contacto",
      "¿Algún problema o sugerencia con el dashboard? Escríbeme a ",
      tags$a(href = paste0("mailto:", CONTACTO_EMAIL), CONTACTO_EMAIL))
  ),
  sidebarLayout(
    sidebarPanel(
      fileInput("cv_file", "1) Sube tu CV (PDF)", accept = ".pdf"),
      hr(),
      h4("2) Agrega una oferta"),
      helpText("Copia el link y la descripción desde LinkedIn u otro sitio",
               "y pégala aquí."),
      textInput("manual_titulo", "Título del puesto"),
      textInput("manual_empresa", "Empresa"),
      selectInput("manual_region", "Región", choices = regiones_chile,
                  selected = "Metropolitana de Santiago"),
      selectInput("manual_modalidad", "Modalidad",
                  choices = c("Presencial", "Híbrido", "Remoto")),
      textInput("manual_url", "Link de la oferta"),
      fileInput("oferta_pdf", "O sube el PDF de la oferta (opcional)",
                accept = ".pdf"),
      textOutput("estado_pdf_oferta"),
      br(),
      verbatimTextOutput("vista_previa_pdf"),
      textAreaInput("manual_descripcion", "Descripción / requisitos (se llena sola si subes un PDF, o pega el texto tú)",
                     rows = 5),
      textOutput("contador_descripcion"),
      br(),
      actionButton("agregar_manual", "➕ Agregar oferta", class = "btn-success"),
      hr(),
      actionButton("eliminar_seleccionadas", "🗑️ Eliminar seleccionadas",
                   class = "btn-danger"),
      hr(),
      downloadButton("descargar_excel", "⬇️ Descargar Excel", class = "btn-primary"),
      downloadButton("descargar_pdf", "⬇️ Descargar PDF", class = "btn-primary")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "📋 Ofertas y Match",
          br(),
          textOutput("estado"),
          br(),
          DTOutput("tabla_ofertas"),
          hr(),
          h4("🪄 Sugerencias de IA para tu CV"),
          helpText("Elige una de las ofertas que ya agregaste y la IA te",
                   "sugiere qué destacar de tu experiencia, qué brechas",
                   "mitigar y un resumen profesional adaptado a ese cargo.",
                   "Hay dos proveedores gratuitos disponibles - si uno se",
                   "queda sin cupo por hoy, prueba con el otro. Ninguna key",
                   "se guarda en ningún lado, solo se usan en esta sesión."),
          radioButtons("proveedor_ia", "Proveedor de IA",
                       choices = c("Google Gemini" = "gemini", "Groq (Llama)" = "groq"),
                       selected = "gemini", inline = TRUE),
          conditionalPanel(
            condition = "input.proveedor_ia == 'gemini'",
            passwordInput("api_key_gemini", "API key de Google Gemini", value = ""),
            textInput("modelo_gemini", "Modelo",
                      value = "gemini-3.6-flash",
                      placeholder = "revisa ai.google.dev/gemini-api/docs/models si da error"),
            helpText("Gratis en aistudio.google.com/apikey - no pide tarjeta en la",
                     "mayoría de países, pero el cupo diario gratis es bajo (ronda",
                     "las 20-50 consultas/día según el modelo).")
          ),
          conditionalPanel(
            condition = "input.proveedor_ia == 'groq'",
            passwordInput("api_key_groq", "API key de Groq", value = ""),
            textInput("modelo_groq", "Modelo",
                      value = "openai/gpt-oss-120b",
                      placeholder = "revisa console.groq.com/docs/models si da error"),
            helpText("Gratis en console.groq.com/keys - no pide tarjeta. Cupo diario",
                     "mucho más generoso (miles de consultas/día), buena opción si",
                     "Gemini se queda sin cupo. Es un modelo open-source (Llama), la",
                     "calidad es buena pero algo distinta a Gemini.")
          ),
          uiOutput("selector_oferta_ia"),
          actionButton("generar_ia", "🪄 Generar sugerencias", class = "btn-primary"),
          br(), br(),
          uiOutput("resultado_ia")
        ),
        tabPanel(
          "❓ Instrucciones",
          br(),
          div(
            style = "max-width: 800px;",
            h3("Cómo usar este dashboard"),
            p("Guía rápida para postular de forma más ordenada, con matching",
              "automático entre tu CV y las ofertas que vayas encontrando."),

            h4("1) Sube tu CV"),
            p("En el panel de la izquierda, arriba de todo, sube tu CV en PDF.",
              "La app lee el texto automáticamente - no hace falta que hagas nada más.",
              "Si el PDF es una imagen escaneada (sin texto seleccionable), no va a",
              "poder leerlo; en ese caso necesitas un PDF con texto real."),

            h4("2) Agrega ofertas de trabajo"),
            p(strong("Importante:"), " la app no hace scraping automático de LinkedIn ni de",
              "otros sitios (va contra sus términos de uso y arriesga que te bloqueen la cuenta).",
              "En vez de eso, agregas las ofertas tú misma, de dos formas:"),
            tags$ul(
              tags$li(strong("Copiar y pegar: "), "copia el link de la oferta y pega el texto de la",
                      "descripción/requisitos en el cuadro correspondiente."),
              tags$li(strong("Subir el PDF de la oferta: "), "si la tienes en PDF (por ejemplo una circular",
                      "o un aviso descargado), súbela directo y la app extrae el texto sola. Espera",
                      "a ver el mensaje verde ✅ confirmando cuántos caracteres se extrajeron antes de",
                      "seguir - si le das a \"Agregar oferta\" muy rápido, puede quedar incompleta.")
            ),
            p("Completa también el título del puesto, la empresa, la región y la modalidad,",
              "y dale a \"➕ Agregar oferta\". Se va sumando a la tabla de la derecha."),

            h4("3) Revisa el % de Match y las sugerencias"),
            p("Una vez que subiste tu CV y agregaste al menos una oferta, la tabla calcula",
              "automáticamente qué tan bien calzan (columna \"Match\") y te sugiere palabras",
              "clave de la oferta que no aparecen en tu CV, con el contexto donde aparecen."),
            p(em("Este cálculo es una heurística simple (compara palabras), no un análisis",
                 "semántico real - úsalo como pista general, no como verdad absoluta.")),

            h4("4) Pide sugerencias con IA (opcional, pero recomendado)"),
            p("Más abajo en esta misma pestaña hay una sección para pedirle a la IA (Gemini,",
              "de Google) un análisis mucho más completo: qué destacar de tu experiencia real,",
              "qué brechas tiene tu CV frente a esa oferta y cómo mitigarlas, y una sugerencia",
              "de resumen profesional para el encabezado de tu CV."),
            tags$ul(
              tags$li("Necesitas tu propia API key gratuita de Google - consíguela en ",
                      tags$a(href = "https://aistudio.google.com/apikey", target = "_blank",
                             "aistudio.google.com/apikey"),
                      ". No pide tarjeta de crédito en la mayoría de los países."),
              tags$li("Pégala en el campo \"API key de Google Gemini\" - no se guarda en ningún",
                      "lado, solo se usa mientras tienes la app abierta."),
              tags$li("Elige la oferta que quieres analizar y dale a \"🪄 Generar sugerencias\"."),
              tags$li("Si te sale un error de \"modelo no encontrado\", el mensaje de la API te dice",
                      "el nombre nuevo del modelo - cámbialo en el campo \"Modelo\"."),
              tags$li("Si te sale un error de servidores saturados (503), espera un minuto y prueba",
                      "de nuevo - es algo temporal de Google, la app ya reintenta sola un par de veces."),
              tags$li("El tier gratis tiene un límite de usos por día - no es para uso masivo, pero",
                      "alcanza bien para uso personal.")
            ),
            p("Una vez generada, la sugerencia de la IA queda guardada en esa oferta - la",
              "vas a ver directamente en la columna \"Sugerencias\" de la tabla (columna \"IA\"",
              "con un ✅), y no hace falta volver a generarla si cambias de pestaña o eliges",
              "otra oferta y vuelves."),

            h4("5) Descarga tus resultados"),
            p("Con los botones \"⬇️ Descargar Excel\" y \"⬇️ Descargar PDF\" te llevas la tabla",
              "completa (títulos, empresas, match%, sugerencias y sugerencias de IA) para",
              "revisarla con calma o compartirla."),

            h4("Cosas para tener en cuenta"),
            tags$ul(
              tags$li("Los datos viven solo mientras esta sesión está abierta - si cierras la",
                      "pestaña del navegador sin descargar, se pierde lo que agregaste."),
              tags$li("Si seleccionas el mismo archivo PDF dos veces seguidas en \"Browse...\", algunos",
                      "navegadores no avisan que hay un archivo nuevo. La app ya tiene un arreglo para",
                      "esto, pero si algo se ve raro, prueba con un archivo distinto o refresca la página."),
              tags$li("Puedes eliminar ofertas mal cargadas: selecciona la fila en la tabla y",
                      "dale a \"🗑️ Eliminar seleccionadas\".")
            ),

            hr(),
            p(strong("¿Algo no funciona o tienes una duda/sugerencia?"), " Escríbeme directo a ",
              tags$a(href = paste0("mailto:", CONTACTO_EMAIL), CONTACTO_EMAIL),
              " - cuéntame qué estabas haciendo y, si puedes, manda captura de pantalla,",
              "así es más rápido ayudarte.")
          )
        )
      )
    )
  ),
  div(class = "contacto-footer",
      "Hecho por Josefina · Dudas o problemas: ",
      tags$a(href = paste0("mailto:", CONTACTO_EMAIL), CONTACTO_EMAIL))
)

# ------------------------------------------------------------
# 4. SERVER
# ------------------------------------------------------------
server <- function(input, output, session) {

  tokens_cv <- reactiveVal(NULL)
  cv_texto_completo <- reactiveVal("")
  ofertas <- reactiveVal(
    tibble(
      id = character(), titulo = character(), empresa = character(),
      region = character(), modalidad = character(), url = character(),
      descripcion = character(), sugerencia_ia = character()
    )
  )

  observeEvent(input$cv_file, {
    req(input$cv_file)
    resultado <- extraer_texto_pdf(input$cv_file$datapath)
    if (!resultado$ok) {
      showNotification(paste("Error leyendo el PDF del CV:", resultado$error),
                        type = "error", duration = 10)
      return()
    }
    if (nchar(trimws(resultado$texto)) == 0) {
      showNotification("El PDF del CV se abrió pero no se encontró texto (¿es una imagen escaneada?).",
                        type = "warning", duration = 8)
      return()
    }
    tokens_cv(tokenizar_significativas(resultado$texto))
    cv_texto_completo(resultado$texto)
    showNotification(paste0("CV cargado y analizado (", nchar(resultado$texto), " caracteres)."),
                      type = "message", duration = 4)
  })

  estado_pdf_oferta <- reactiveVal("")
  texto_pdf_extraido <- reactiveVal("")

  observeEvent(input$oferta_pdf, {
    req(input$oferta_pdf)
    resultado <- extraer_texto_pdf(input$oferta_pdf$datapath)
    if (!resultado$ok) {
      msg <- paste("❌ Error leyendo el PDF:", resultado$error)
      estado_pdf_oferta(msg)
      texto_pdf_extraido("")
      showNotification(msg, type = "error", duration = 10)
    } else if (nchar(trimws(resultado$texto)) == 0) {
      msg <- "⚠️ El PDF se abrió pero no se encontró texto (¿es una imagen escaneada?). Pega la descripción a mano."
      estado_pdf_oferta(msg)
      texto_pdf_extraido("")
      showNotification(msg, type = "warning", duration = 8)
    } else {
      texto_pdf_extraido(resultado$texto)
      # Intentamos igual llenar el cuadro visible por si el navegador sí
      # lo soporta - si no, la vista previa de abajo y el botón "Agregar
      # oferta" van a usar texto_pdf_extraido() de todas formas.
      updateTextAreaInput(session, "manual_descripcion", value = resultado$texto)
      msg <- paste0("✅ Texto del PDF cargado (", nchar(resultado$texto), " caracteres). Revísalo en la vista previa de abajo.")
      estado_pdf_oferta(msg)
      showNotification(msg, type = "message", duration = 6)
    }
    session$sendCustomMessage("resetFileInput", "oferta_pdf")
  })

  output$estado_pdf_oferta <- renderText({
    estado_pdf_oferta()
  })

  output$vista_previa_pdf <- renderText({
    texto <- texto_pdf_extraido()
    if (nchar(texto) == 0) {
      "(Sin PDF cargado todavía - esta vista previa se llena al subir un PDF de oferta.)"
    } else {
      texto
    }
  })

  output$contador_descripcion <- renderText({
    texto_actual <- input$manual_descripcion
    if (is.null(texto_actual)) texto_actual <- ""
    n <- nchar(trimws(texto_actual))
    n_pdf <- nchar(texto_pdf_extraido())
    if (n == 0 && n_pdf > 0) {
      paste0("El cuadro se ve vacío, pero hay ", n_pdf, " caracteres extraídos del PDF ",
             "guardados (revisa la vista previa arriba) - se usarán igual al agregar la oferta.")
    } else if (n == 0) {
      "0 caracteres. Si subiste un PDF, espera 1-2 segundos a que se llene solo antes de agregar la oferta."
    } else if (n < 150) {
      paste0(n, " caracteres - se ve corto para una descripción completa. Revisa que no le falte texto antes de agregar.")
    } else {
      paste0(n, " caracteres cargados.")
    }
  })

  observeEvent(input$agregar_manual, {
    req(input$manual_titulo, input$manual_empresa)

    # Si el cuadro de texto visible está vacío pero sí extrajimos texto
    # de un PDF (bug de sincronización del navegador), usamos ese texto
    # guardado en vez de bloquear al usuario.
    descripcion_final <- if (nchar(trimws(input$manual_descripcion)) > 0) {
      input$manual_descripcion
    } else {
      texto_pdf_extraido()
    }

    if (nchar(trimws(descripcion_final)) < 150) {
      showNotification(
        paste("La descripción parece muy corta (", nchar(trimws(descripcion_final)),
              "caracteres). Pega o sube una descripción más completa antes de agregar la oferta."),
        type = "warning", duration = 10
      )
      return()
    }

    nueva <- tibble(
      id = paste0("oferta_", as.integer(Sys.time()), "_",
                  sample(1000:9999, 1)),
      titulo = input$manual_titulo,
      empresa = input$manual_empresa,
      region = input$manual_region,
      modalidad = input$manual_modalidad,
      url = input$manual_url,
      descripcion = descripcion_final,
      sugerencia_ia = ""
    )

    ofertas(bind_rows(ofertas(), nueva))
    texto_pdf_extraido("")
    estado_pdf_oferta("")

    # limpia el formulario para la siguiente oferta
    updateTextInput(session, "manual_titulo", value = "")
    updateTextInput(session, "manual_empresa", value = "")
    updateTextInput(session, "manual_url", value = "")
    updateTextAreaInput(session, "manual_descripcion", value = "")

    showNotification("Oferta agregada.", type = "message", duration = 3)
  })

  observeEvent(input$eliminar_seleccionadas, {
    filas_sel <- input$tabla_ofertas_rows_selected
    df <- ofertas_con_analisis()
    if (is.null(filas_sel) || length(filas_sel) == 0 || nrow(df) == 0) return()
    ids_a_borrar <- df$id[filas_sel]
    ofertas(ofertas() %>% filter(!id %in% ids_a_borrar))
    showNotification(paste(length(ids_a_borrar), "oferta(s) eliminada(s)."),
                      type = "message", duration = 3)
  })

  # Tabla base + match% + sugerencias, recalculada cada vez que cambian
  # las ofertas o el CV
  ofertas_con_analisis <- reactive({
    df <- ofertas()
    if (nrow(df) == 0) return(df)

    if (!is.null(tokens_cv())) {
      # Antes esto usaba rowwise() + mutate() con un list-column y doble
      # indexado ([[1]]) - dentro de rowwise(), referenciar un list-column
      # recién creado ya te da el vector "desenvuelto", así que el [[1]]
      # extra terminaba agarrando solo la PRIMERA palabra de la oferta en
      # vez del texto completo. Por eso el match siempre daba ~0 y la
      # sugerencia siempre era una sola palabra, sin importar qué tan bien
      # se hubiera extraído el PDF. Usando purrr::map en vez de rowwise se
      # evita esa trampa por completo.
      cv_tokens <- tokens_cv()
      tokens_por_oferta <- purrr::map(df$descripcion, tokenizar_significativas)
      df$match_pct <- purrr::map_dbl(tokens_por_oferta, ~ similitud_coseno(cv_tokens, .x))
      df$sugerencias <- purrr::map2_chr(tokens_por_oferta, df$descripcion,
                                         ~ sugerir_mejoras_cv(cv_tokens, .x, .y))
      df <- df %>% arrange(desc(match_pct))
    } else {
      df <- df %>%
        mutate(match_pct = NA_real_, sugerencias = "Sube tu CV para ver sugerencias") %>%
        arrange(desc(titulo))
    }
    df
  })

  output$estado <- renderText({
    n <- nrow(ofertas())
    cv_msg <- if (is.null(tokens_cv())) {
      "Aún no subes tu CV."
    } else {
      paste0("CV cargado (", length(tokens_cv()), " palabras significativas detectadas).")
    }
    df <- ofertas()
    ofertas_msg <- if (n == 0) {
      ""
    } else {
      largos <- sapply(df$descripcion, function(d) nchar(trimws(d)))
      paste0(" Longitud de descripción por oferta: ", paste(largos, collapse = ", "), " caracteres.")
    }
    paste0(n, " oferta(s) agregada(s). ", cv_msg, ofertas_msg)
  })

  output$tabla_ofertas <- renderDT({
    df <- ofertas_con_analisis()
    if (nrow(df) == 0) {
      return(datatable(tibble(Mensaje = "Agrega tu primera oferta con el formulario de la izquierda."),
                        rownames = FALSE))
    }

    df_show <- df %>%
      mutate(
        # Si ya hay una sugerencia de la IA para esta oferta, se muestra
        # esa en vez de la heurística de palabras sueltas (mucho más
        # útil). La heurística queda solo como respaldo mientras no le
        # has pedido sugerencias a la IA a esa oferta todavía.
        tiene_ia = nchar(trimws(sugerencia_ia)) > 0,
        texto_mostrado = ifelse(tiene_ia, sugerencia_ia, sugerencias),
        # El texto de la IA viene con **negritas** estilo markdown - se
        # convierten a HTML para que se vean bien en vez de mostrar los
        # asteriscos literales.
        texto_mostrado = str_replace_all(texto_mostrado, "\\*\\*(.*?)\\*\\*", "<strong>\\1</strong>"),
        texto_mostrado = str_replace_all(texto_mostrado, "\n", "<br>"),
        IA = ifelse(tiene_ia, "✅", "-")
      ) %>%
      select(Match = match_pct, Título = titulo, Empresa = empresa,
             Región = region, Modalidad = modalidad,
             Sugerencias = texto_mostrado, IA, URL = url)

    dt <- datatable(df_show, rownames = FALSE, selection = "multiple",
                     escape = -which(names(df_show) == "Sugerencias"),
                     options = list(pageLength = 10)) %>%
      DT::formatStyle("Sugerencias", `white-space` = "normal", `line-height` = "1.4")
    if (!is.null(tokens_cv())) {
      dt <- dt %>% DT::formatStyle("Match", background = DT::styleColorBar(c(0, 100), "#c6e5c6"))
    }
    dt
  })

  # --- Selector dinámico de oferta para pedir sugerencias de IA ---
  output$selector_oferta_ia <- renderUI({
    df <- ofertas()
    if (nrow(df) == 0) {
      return(helpText("Agrega al menos una oferta para poder pedir sugerencias."))
    }
    opciones <- setNames(
      df$id,
      paste0(df$titulo, " - ", df$empresa,
             ifelse(nchar(trimws(df$sugerencia_ia)) > 0, " ✅ (ya tiene sugerencia)", ""))
    )
    # Mantiene la oferta que ya tenías elegida cuando el desplegable se
    # vuelve a dibujar (por ejemplo, justo después de generar una
    # sugerencia) - si no, perdías la selección y hasta parecía que la
    # respuesta de la IA "desaparecía".
    seleccionado <- if (!is.null(input$oferta_seleccionada_ia) &&
                         input$oferta_seleccionada_ia %in% opciones) {
      input$oferta_seleccionada_ia
    } else {
      opciones[1]
    }
    selectInput("oferta_seleccionada_ia", "Oferta a analizar",
                choices = opciones, selected = seleccionado)
  })

  # Error transitorio de la última consulta a la IA (se limpia solo en
  # la siguiente consulta exitosa). Separado de la sugerencia guardada
  # para que un redibujado del desplegable no lo borre por accidente.
  error_ia_reactivo <- reactiveVal(NULL)

  # La sugerencia mostrada se lee directamente de la oferta seleccionada
  # (fuente única de verdad = la tabla de ofertas), en vez de guardarse
  # aparte en un reactiveVal que un redibujado del selector podía pisar.
  sugerencia_actual_ia <- reactive({
    req(input$oferta_seleccionada_ia)
    df <- ofertas()
    fila <- df %>% filter(id == input$oferta_seleccionada_ia)
    if (nrow(fila) == 0) return("")
    fila$sugerencia_ia[1]
  })

  observeEvent(input$generar_ia, {
    req(input$oferta_seleccionada_ia)

    if (nchar(cv_texto_completo()) == 0) {
      showNotification("Primero sube tu CV (paso 1).", type = "warning", duration = 6)
      return()
    }

    # Toma la key y el modelo según el proveedor elegido con los radio
    # buttons.
    if (input$proveedor_ia == "groq") {
      api_key_actual <- trimws(input$api_key_groq)
      modelo_actual <- trimws(input$modelo_groq)
      nombre_proveedor <- "Groq"
      link_key <- "console.groq.com/keys"
    } else {
      api_key_actual <- trimws(input$api_key_gemini)
      modelo_actual <- trimws(input$modelo_gemini)
      nombre_proveedor <- "Google Gemini"
      link_key <- "aistudio.google.com/apikey"
    }

    if (nchar(api_key_actual) == 0) {
      showNotification(paste0("Falta tu API key de ", nombre_proveedor, ". Consíguela gratis en ", link_key, "."),
                        type = "warning", duration = 8)
      return()
    }

    df <- ofertas()
    fila <- df %>% filter(id == input$oferta_seleccionada_ia)
    if (nrow(fila) == 0) {
      showNotification("No se encontró esa oferta (¿la borraste?).", type = "error", duration = 6)
      return()
    }

    showNotification("Consultando a la IA... esto puede tardar unos segundos.",
                      type = "message", duration = 5)

    resp <- generar_sugerencias_ia(
      proveedor = input$proveedor_ia,
      api_key = api_key_actual,
      modelo = modelo_actual,
      cv_texto = cv_texto_completo(),
      oferta_titulo = fila$titulo[1],
      oferta_empresa = fila$empresa[1],
      oferta_texto = fila$descripcion[1]
    )

    if (!resp$ok) {
      error_ia_reactivo(resp$mensaje)
      return()
    }

    error_ia_reactivo(NULL)
    df_actualizado <- ofertas()
    df_actualizado$sugerencia_ia[df_actualizado$id == input$oferta_seleccionada_ia] <- resp$mensaje
    ofertas(df_actualizado)
  })

  output$resultado_ia <- renderUI({
    err <- error_ia_reactivo()
    if (!is.null(err)) {
      return(div(style = "color: #b00020;", strong("Error: "), err))
    }
    texto <- sugerencia_actual_ia()
    if (nchar(trimws(texto)) == 0) return(NULL)
    div(
      style = "background-color: #f5f5f5; padding: 15px; border-radius: 5px; white-space: pre-wrap;",
      texto
    )
  })

  output$descargar_excel <- downloadHandler(
    filename = function() {
      paste0("ofertas_match_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
    },
    content = function(file) {
      df <- ofertas_con_analisis()
      df_export <- df %>%
        select(Título = titulo, Empresa = empresa, Región = region,
               Modalidad = modalidad, `Match %` = match_pct,
               Sugerencias = sugerencias, `Sugerencia IA` = sugerencia_ia, URL = url)

      # Se arma el workbook a mano (en vez de write.xlsx con asTable=TRUE)
      # porque esa combinación generaba un archivo corrupto: dejaba una
      # referencia a un dibujo interno que nunca se creaba realmente,
      # y Excel/otros programas se negaban a abrirlo.
      wb <- createWorkbook()
      addWorksheet(wb, "Ofertas")
      writeData(wb, "Ofertas", df_export, headerStyle = createStyle(
        textDecoration = "bold", fgFill = "#D9E1F2"
      ))
      setColWidths(wb, "Ofertas", cols = 1:ncol(df_export), widths = "auto")
      freezePane(wb, "Ofertas", firstRow = TRUE)
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )

  output$descargar_pdf <- downloadHandler(
    filename = function() {
      paste0("ofertas_match_", format(Sys.Date(), "%Y%m%d"), ".pdf")
    },
    content = function(file) {
      df <- ofertas_con_analisis()

      # Envuelve texto largo en varias líneas para que quepa en la página
      # (si no, las celdas de Sugerencias/Título se cortan o se salen del
      # margen del PDF).
      envolver <- function(x, ancho = 40) {
        x <- ifelse(is.na(x), "", x)
        vapply(x, function(s) paste(strwrap(s, width = ancho), collapse = "\n"),
               character(1))
      }

      df_export <- df %>%
        transmute(
          Título = envolver(titulo, 25),
          Empresa = envolver(empresa, 20),
          Región = envolver(region, 15),
          Modalidad = modalidad,
          `Match %` = ifelse(is.na(match_pct), "-", paste0(match_pct, "%")),
          Sugerencias = envolver(sugerencias, 45),
          `Sugerencia IA` = envolver(sugerencia_ia, 45)
        )

      pdf(file, width = 22, height = max(4, 1 + 0.6 * nrow(df_export)), onefile = TRUE)
      grid::grid.newpage()
      titulo_grob <- grid::textGrob(
        paste0("Ofertas y match de CV - ", format(Sys.Date(), "%d-%m-%Y")),
        gp = grid::gpar(fontsize = 16, fontface = "bold")
      )
      if (nrow(df_export) == 0) {
        grid::grid.draw(titulo_grob)
      } else {
        tabla_grob <- gridExtra::tableGrob(
          df_export, rows = NULL,
          theme = gridExtra::ttheme_default(
            core = list(fg_params = list(hjust = 0, x = 0.02, cex = 0.8)),
            colhead = list(fg_params = list(fontface = "bold", cex = 0.85))
          )
        )
        gridExtra::grid.arrange(titulo_grob, tabla_grob, nrow = 2,
                                 heights = grid::unit(c(1, 1), c("lines", "null")))
      }
      dev.off()
    }
  )
}

shinyApp(ui, server)
