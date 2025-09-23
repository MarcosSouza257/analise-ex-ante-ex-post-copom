# Utilitários para análise econômica com dados do BCB/Focus

if (!requireNamespace("dplyr", quietly = TRUE)) stop("Pacote dplyr é necessário.")
if (!requireNamespace("rbcb", quietly = TRUE)) stop("Pacote rbcb é necessário.")
if (!requireNamespace("zoo", quietly = TRUE)) stop("Pacote zoo é necessário.")
if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Pacote ggplot2 é necessário.")
if (!requireNamespace("lubridate", quietly = TRUE)) stop("Pacote lubridate é necessário.")
if (!requireNamespace("rlang", quietly = TRUE)) stop("Pacote rlang é necessário.")

#' Baixa e prepara expectativas do Focus
#' @return tibble com colunas Data, Minimo, Maximo, Mediana
prepare_focus_expectations <- function(indic, type, data_ini, data_fim, shift_by_year = TRUE, filter_expr = "baseCalculo eq 0") {
  df <- rbcb::get_market_expectations(
    type  = type,
    indic = indic,
    start_date = as.character(data_ini),
    end_date   = as.character(data_fim),
    keep_names = TRUE,
    `$filter`  = filter_expr
  )

  df <- df %>%
    dplyr::arrange(DataReferencia) %>%
    dplyr::filter(DataReferencia == lubridate::year(Data) + 1,
                  Data >= data_ini & Data <= data_fim)

  if (shift_by_year) {
    df$Data <- df$Data + lubridate::years(1)
  }

  df %>% dplyr::select(Data, Minimo, Maximo, Mediana) %>% dplyr::arrange(Data)
}

#' Busca série do SGS; opcionalmente calcula acumulado 12m em %
fetch_real_series_df <- function(series_id, data_ini, data_fim, compute_12m_percent = FALSE, value_name = "Valor", out12_name = "Valor_12m") {
  df <- rbcb::get_series(series_id,
                         start_date = (data_ini %m-% months(12)),
                         end_date   = data_fim) %>%
    `colnames<-`(c("Data", value_name)) %>%
    dplyr::arrange(Data)

  if (compute_12m_percent) {
    df[[out12_name]] <- (zoo::rollapplyr(1 + df[[value_name]]/100, 12, prod, fill = NA) - 1) * 100
  }

  df %>% dplyr::filter(Data >= data_ini & Data <= data_fim)
}

#' Constrói gráfico padrão Copom vs indicador
build_copom_plot <- function(df_expect, df_real, real_col_name, data_ata, data_ini, data_fim,
                             title, subtitle, y_label, legend_label_real) {
  data_ata_menos12 <- data_ata %m-% months(12)
  data_ata_mais12  <- data_ata %m+% months(12)
  data_ata_mais24  <- data_ata %m+% months(24)

  ggplot2::ggplot(df_expect, ggplot2::aes(x = Data)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = Minimo, ymax = Maximo, fill = "Intervalo Min-Max"), alpha = 0.2) +
    ggplot2::geom_line(ggplot2::aes(y = Maximo, colour = "Máximo"), size = 0.6) +
    ggplot2::geom_line(ggplot2::aes(y = Minimo, colour = "Mínimo"), size = 0.6) +
    ggplot2::geom_line(ggplot2::aes(y = Mediana, colour = "Mediana"), size = 1.2) +
    ggplot2::geom_line(data = df_real, ggplot2::aes(x = Data, y = .data[[real_col_name]], colour = legend_label_real), size = 1.2) +
    ggplot2::geom_vline(xintercept = as.numeric(data_ata), linetype = "dashed", colour = "red", size = 1) +
    ggplot2::annotate("text", x = data_ata, y = Inf, label = "Ata do Copom", colour = "red", angle = 90, vjust = 1.2, size = 3.5) +
    ggplot2::geom_vline(xintercept = as.numeric(data_ata_menos12), linetype = "dotted", colour = "darkgreen", size = 1) +
    ggplot2::annotate("text", x = data_ata_menos12, y = Inf, label = "-12 meses", colour = "darkgreen", angle = 90, vjust = 1.2, size = 3.5) +
    ggplot2::geom_vline(xintercept = as.numeric(data_ata_mais12), linetype = "dotted", colour = "blue", size = 1) +
    ggplot2::annotate("text", x = data_ata_mais12, y = Inf, label = "+12 meses", colour = "blue", angle = 90, vjust = 1.2, size = 3.5) +
    ggplot2::geom_vline(xintercept = as.numeric(data_ata_mais24), linetype = "dotted", colour = "darkgrey", size = 1) +
    ggplot2::annotate("text", x = data_ata_mais24, y = Inf, label = "+24 meses", colour = "darkgrey", angle = 90, vjust = 1.2, size = 3.5) +
    ggplot2::scale_fill_manual(name = NULL, values = c("Intervalo Min-Max" = "lightblue")) +
    ggplot2::scale_colour_manual(name = NULL,
                        values = setNames(c("grey60", "grey60", "darkblue", "firebrick"),
                                          c("Máximo", "Mínimo", "Mediana", legend_label_real))) +
    ggplot2::guides(fill = ggplot2::guide_legend(order = 1), colour = ggplot2::guide_legend(order = 2)) +
    ggplot2::labs(title = title, subtitle = subtitle, caption = "Fonte: Pesquisa Focus (rbcb) e IBGE", x = "Data", y = y_label) +
    ggplot2::scale_x_date(limits = c(data_ini, data_fim), date_labels = "%b %Y", date_breaks = "3 months",
                 expand = ggplot2::expansion(mult = c(0.01, 0.02))) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.01, 0.05))) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 16),
          plot.subtitle = ggplot2::element_text(size = 12, color = "grey40"),
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
          panel.grid.minor = ggplot2::element_blank(),
          legend.position = "bottom",
          legend.title = ggplot2::element_blank())
}

#' Exporta dados do gráfico em CSV (sep=';')
export_plot_data <- function(df_expect, df_real, real_col_name, prefix, out_dir = "data") {
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  dados_expectativas <- df_expect %>% dplyr::select(Data, Minimo, Maximo, Mediana) %>% dplyr::arrange(Data)
  dados_real <- df_real %>% dplyr::transmute(Data, !!rlang::sym(real_col_name) := .data[[real_col_name]]) %>% dplyr::arrange(Data)
  utils::write.table(dados_expectativas, file = file.path(out_dir, paste0(prefix, "_expec.csv")),
                     sep = ";", dec = ",", row.names = FALSE, col.names = TRUE, qmethod = "double")
  utils::write.table(dados_real,        file = file.path(out_dir, paste0(prefix, "_real.csv")),
                     sep = ";", dec = ",", row.names = FALSE, col.names = TRUE, qmethod = "double")
}

#' Roda o pipeline completo para um indicador (Focus + SGS + CSV + gráfico)
run_indicator_pipeline <- function(indicador, data_ata, data_ini, data_fim, out_dir = "data") {
  # Expectativas Focus
  df_expect <- NULL
  if (!is.null(indicador$focus)) {
    df_expect <- tryCatch({
      prepare_focus_expectations(indic = indicador$focus$indic,
                                 type  = indicador$focus$type,
                                 data_ini = data_ini, data_fim = data_fim,
                                 shift_by_year = TRUE,
                                 filter_expr = indicador$focus$filter)
    }, error = function(e) {
      message(sprintf("[Aviso] Não foi possível obter expectativas Focus para '%s': %s", indicador$nome, e$message))
      NULL
    })
  }

  # Série realizada (SGS)
  df_real <- fetch_real_series_df(series_id = indicador$sgs$series_id,
                                  data_ini = data_ini, data_fim = data_fim,
                                  compute_12m_percent = indicador$sgs$compute_12m_percent,
                                  value_name = indicador$sgs$value_name %||% "Valor",
                                  out12_name = indicador$sgs$real_col)

  # Exportar dados
  if (!is.null(df_expect)) {
    export_plot_data(df_expect = df_expect, df_real = df_real,
                     real_col_name = indicador$sgs$real_col,
                     prefix = tolower(indicador$nome), out_dir = out_dir)
  } else {
    # Exportar somente série real
    dados_real <- df_real %>% dplyr::transmute(Data, !!rlang::sym(indicador$sgs$real_col) := .data[[indicador$sgs$real_col]]) %>% dplyr::arrange(Data)
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
    utils::write.table(dados_real, file = file.path(out_dir, paste0(tolower(indicador$nome), "_real.csv")),
                       sep = ";", dec = ",", row.names = FALSE, col.names = TRUE, qmethod = "double")
  }

  # Construir e salvar gráfico
  p <- build_copom_plot(df_expect = if (is.null(df_expect)) df_real %>% dplyr::transmute(Data = Data, Minimo = NA_real_, Maximo = NA_real_, Mediana = .data[[indicador$sgs$real_col]]) else df_expect,
                        df_real   = df_real,
                        real_col_name = indicador$sgs$real_col,
                        data_ata = data_ata, data_ini = data_ini, data_fim = data_fim,
                        title = indicador$title,
                        subtitle = indicador$subtitle,
                        y_label = indicador$sgs$y_label,
                        legend_label_real = indicador$sgs$legend_label)

  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  ggplot2::ggsave(filename = file.path(out_dir, paste0(tolower(indicador$nome), "_grafico_copom.png")),
                  plot = p, width = 10, height = 6, dpi = 300)

  invisible(list(expect = df_expect, real = df_real, plot = p))
}

# operador %||% (fallback)
`%||%` <- function(a, b) if (!is.null(a)) a else b


