# Análise de Dados Econômicos - Brasil
# Autor: Marcos
# Data: 2025-09-10

# Verificar/instalar e carregar pacotes necessários
pkgs <- c("dplyr", "ggplot2", "rbcb", "knitr", "zoo", "lubridate")
to_install <- pkgs[!pkgs %in% installed.packages()[, 1]]
if (length(to_install)) install.packages(to_install)
invisible(lapply(pkgs, require, character.only = TRUE))

# 104ª Reunião - janeiro 2005
# Datas de referência
data_ata  <- as.Date("2005-01-27")
data_ini  <- data_ata %m-% months(24)
data_fim  <- data_ata %m+% months(24)
data_ata_menos12 <- data_ata %m-% months(12)
data_ata_mais12 <- data_ata %m+% months(12)
data_ata_mais24 <- data_ata %m+% months(24)

# Selic - Expectativa
df <- get_market_expectations(
  type  = "annual",
  indic = "Selic",
  start_date = as.character(data_ini),  # formato "YYYY-MM-DD"
  end_date   = as.character(data_fim),
  keep_names = TRUE,
  `$filter`  = "baseCalculo eq 0"   # mantém apenas baseCalculo = 0
)

if (interactive()) View(df)
# Filtrando Selic - Expectativa ---------------------
selic_expectativa <- df %>%
  arrange(DataReferencia) %>%
  filter(
    # regra: DataReferencia é o ano seguinte ao ano de Data
    DataReferencia == year(Data) + 1,
    # restringe DataReferencia aos anos de interesse
    DataReferencia %in% c(2004, 2005, 2006)
  )

# Igualando a data da expectativa para DataReferencia
selic_expectativa$Data <- selic_expectativa$Data + years(1)

if (interactive()) View(selic_expectativa)

# Obtendo os Dados reais de Selic ---------------------
selic_real <- get_series(1178,
                         start_date = (data_ini %m-% months(12)),
                         end_date   = data_fim) %>%
  `colnames<-`(c("Data", "Selic")) %>%
  arrange(Data)
  
if (interactive()) View(selic_real)

#--------Grafico----------------------------

# Salvar dados utilizados no gráfico em CSV
dados_expectativas <- selic_expectativa %>%
  dplyr::select(Data, Minimo, Maximo, Mediana) %>%
  arrange(Data)

dados_selic <- selic_real %>%
  dplyr::select(Data, Selic) %>%
  arrange(Data)

utils::write.table(dados_expectativas,
                   file = file.path("data", "selic_expec.csv"),
                   sep = ";", dec = ",", row.names = FALSE, col.names = TRUE, qmethod = "double")
utils::write.table(dados_selic,
                   file = file.path("data", "selic_real.csv"),
                   sep = ";", dec = ",", row.names = FALSE, col.names = TRUE, qmethod = "double")

# Gráfico
p <- 
  ggplot(selic_expectativa, aes(x = Data)) +
  # Ribbon das expectativas
  geom_ribbon(aes(ymin = Minimo, ymax = Maximo, fill = "Intervalo Min-Max (Expectativa)"), alpha = 0.2) +
  
  # Linhas das expectativas
  geom_line(aes(y = Maximo, colour = "Máximo (Expectativa)"), size = 0.6, show.legend = FALSE) +
  geom_line(aes(y = Minimo, colour = "Mínimo (Expectativa)"), size = 0.6, show.legend = FALSE) +
  geom_line(aes(y = Mediana, colour = "Mediana (Expectativa)"), size = 1.2) +
  
  # Linha da Selic (% a.a.)
  geom_line(data = selic_real,
            aes(x = Data, y = Selic, colour = "Selic (% a.a.)"), size = 1.2) +
  
  # Linha vertical da ata
  geom_vline(xintercept = as.numeric(data_ata), linetype = "dashed", colour = "red", size = 1) +
  annotate("text", x = data_ata, y = max(c(selic_expectativa$Maximo, selic_real$Selic), na.rm = TRUE) * 0.95,
           label = "Ata do Copom", colour = "red", angle = 90, vjust = -0.2, size = 3.5) +
  
  # Linha vertical -12 meses
  geom_vline(xintercept = as.numeric(data_ata_menos12), linetype = "dotted", colour = "darkgreen", size = 1) +
  annotate("text", x = data_ata_menos12, y = max(c(selic_expectativa$Maximo, selic_real$Selic), na.rm = TRUE) * 0.95,
           label = "-12 meses", colour = "darkgreen", angle = 90, vjust = -0.2, size = 3.5) +
  
  # Linha vertical +12 meses
  geom_vline(xintercept = as.numeric(data_ata_mais12), linetype = "dotted", colour = "blue", size = 1) +
  annotate("text", x = data_ata_mais12, y = max(c(selic_expectativa$Maximo, selic_real$Selic), na.rm = TRUE) * 0.95,
           label = "+12 meses", colour = "blue", angle = 90, vjust = -0.2, size = 3.5) +
  
  # Linha vertical +24 meses
  geom_vline(xintercept = as.numeric(data_ata_mais24), linetype = "dotted", colour = "darkgrey", size = 1) +
  annotate("text", x = data_ata_mais24, y = max(c(selic_expectativa$Maximo, selic_real$Selic), na.rm = TRUE) * 0.95,
           label = "+24 meses", colour = "darkgrey", angle = 90, vjust = -0.2, size = 3.5) +
  
  # Escalas de cores
  scale_fill_manual(name = NULL, values = c("Intervalo Min-Max (Expectativa)" = "lightblue")) +
  scale_colour_manual(name = NULL,
                      breaks = c("Selic (% a.a.)", "Mediana (Expectativa)"),
                      values = c("Selic (% a.a.)" = "firebrick",
                                 "Mediana (Expectativa)" = "darkblue")) +
  guides(fill = guide_legend(order = 1), colour = guide_legend(order = 2)) +
  
  # Rótulos
  labs(
    title = "Ata do Copom: expectativas de Selic vs Selic realizada",
    subtitle = "Expectativas do Focus (12 meses) vs Selic (% a.a.)",
    #caption = "Fonte: Pesquisa Focus (rbcb) e IBGE",
    x = "Data",
    y = "Selic (% a.a.)"
  ) +
  
  # Eixo X (fixa limites para incluir -12 meses e além)
  scale_x_date(limits = c(as.Date("2004-01-01"), data_fim),
               date_labels = "%b %Y", date_breaks = "3 months",
               expand = expansion(mult = c(0.01, 0.02))) +
  # Eixo Y com mais espaço no topo para os textos
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.15))) +
  
  # Tema
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "grey40"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank()
  )

# Salvar gráfico na pasta data
ggsave(filename = file.path("data", "5_selic.png"), plot = p,
       width = 10, height = 6, dpi = 300)