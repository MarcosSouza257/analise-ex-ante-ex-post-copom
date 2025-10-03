# Análise de Dados Econômicos - Brasil
# Autor: Marcos
# Data: 2025-09-10

# Verificar/instalar e carregar pacotes necessários
pkgs <- c("dplyr", "ggplot2", "rbcb", "knitr", "zoo", "lubridate")
to_install <- pkgs[!pkgs %in% installed.packages()[, 1]]
if (length(to_install)) install.packages(to_install)
invisible(lapply(pkgs, require, character.only = TRUE))

# 104 Reunião - janeiro 2005
# Datas de referência
data_ata  <- as.Date("2011-01-01")
data_ini  <- data_ata %m-% months(24)
data_fim  <- data_ata %m+% months(24)
data_ata_menos12 <- data_ata %m-% months(12)
data_ata_mais12 <- data_ata %m+% months(12)


# Carregar CSV de taxa de desemprego
desemp <- read.csv(file.path("data", "taxa_desemprego.csv"),
                   sep = ";", dec = ",", stringsAsFactors = FALSE)

# Normalizar nomes de colunas: Data e Taxa_Desemprego
if (!"Taxa_Desemprego" %in% names(desemp)) {
  if ("valor" %in% names(desemp)) {
    names(desemp)[names(desemp) == "valor"] <- "Taxa_Desemprego"
  } else if (ncol(desemp) >= 2) {
    names(desemp)[2] <- "Taxa_Desemprego"
  }
}

if (interactive()) View(desemp)

desemp <- desemp %>%
  mutate(Data = as.Date(Data)) %>%
  arrange(Data) %>%
  filter(Data >= (data_ata %m-% months(12)) & Data <= (data_ata %m+% months(12)))

if (interactive()) View(desemp)


# Gráfico
p <- 
  ggplot(desemp, aes(x = Data)) +
  
  # Linha da taxa de desemprego
  geom_line(aes(y = Taxa_Desemprego, colour = "Taxa de desemprego"), size = 1.2) +
  
  # Linha vertical da ata
  geom_vline(xintercept = as.numeric(data_ata), linetype = "dashed", colour = "red", size = 1) +
  annotate("text", x = data_ata, y = max(desemp$Taxa_Desemprego, na.rm = TRUE) * 0.95,
           label = "Ata do Copom", colour = "red", angle = 90, vjust = -0.2, size = 3.5) +
  
  # Linha vertical -12 meses
  geom_vline(xintercept = as.numeric(data_ata_menos12), linetype = "dotted", colour = "darkgreen", size = 1) +
  annotate("text", x = data_ata_menos12, y = max(desemp$Taxa_Desemprego, na.rm = TRUE) * 0.95,
           label = "-12 meses", colour = "darkgreen", angle = 90, vjust = -0.2, size = 3.5) +
  
  # Linha vertical +12 meses
  geom_vline(xintercept = as.numeric(data_ata_mais12), linetype = "dotted", colour = "blue", size = 1) +
  annotate("text", x = data_ata_mais12, y = max(desemp$Taxa_Desemprego, na.rm = TRUE) * 0.95,
           label = "+12 meses", colour = "blue", angle = 90, vjust = -0.2, size = 3.5) +
  
  # Escalas de cores
  scale_colour_manual(name = NULL,
                      values = c("Taxa de desemprego" = "firebrick")) +
  guides(colour = guide_legend(order = 1)) +
  
  # Rótulos
  labs(
    title = "Ata do Copom: taxa de desemprego vs expectativas",
    subtitle = "Janela: −12m a +24m da ata; Mediana do Focus vs desemprego observado",
    x = "Data",
    y = "Desemprego (%)"
  ) +
  
  # Eixo X (fixa limites para incluir -12 meses e além)
  scale_x_date(limits = c(data_ata %m-% months(12), data_ata %m+% months(12)),
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
ggsave(filename = file.path("data", "4_taxa_desemprego.png"), plot = p,
       width = 10, height = 6, dpi = 300)