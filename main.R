# Análise de Dados Econômicos - Brasil
# Autor: Marcos
# Data: 2025-09-10

# Verificar/instalar e carregar pacotes necessários
pkgs <- c("dplyr", "ggplot2", "rbcb", "knitr", "zoo", "lubridate", "rlang")
to_install <- pkgs[!pkgs %in% installed.packages()[, 1]]
if (length(to_install)) install.packages(to_install)
invisible(lapply(pkgs, require, character.only = TRUE))

if (file.exists(file.path("R", "utils.R"))) source(file.path("R", "utils.R"), encoding = "UTF-8")

## As funções utilitárias estão em R/utils.R; não redefinir aqui.

yaml_path <- file.path("config", "indicadores.yml")
if (!file.exists(yaml_path)) stop("Arquivo de configuração 'config/indicadores.yml' não encontrado.")
if (!requireNamespace("yaml", quietly = TRUE)) install.packages("yaml")
cfg <- yaml::yaml.load_file(yaml_path)

data_ata  <- as.Date(cfg$referencia$data_ata)
data_ini  <- data_ata %m-% months(cfg$referencia$janela_meses_pre + 12)
data_fim  <- data_ata %m+% months(cfg$referencia$janela_meses_pos + 12)

## ---------------- Indicadores a processar ----------------
indicadores <- cfg$indicadores

if (!dir.exists("data")) dir.create("data", recursive = TRUE)
results <- lapply(indicadores, function(ind) {
  run_indicator_pipeline(indicador = ind,
                         data_ata = data_ata,
                         data_ini = data_ini,
                         data_fim = data_fim,
                         out_dir = "data")
})