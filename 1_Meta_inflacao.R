# An�lise de Dados Econ�micos - Brasil
# Autor: Marcos
# Data: 2025-09-10

# Verificar/instalar e carregar pacotes necess�rios
pkgs <- c("dplyr", "ggplot2", "rbcb", "knitr", "zoo", "lubridate")
to_install <- pkgs[!pkgs %in% installed.packages()[, 1]]
if (length(to_install)) install.packages(to_install)
invisible(lapply(pkgs, require, character.only = TRUE))

# 104� Reuni�o - janeiro 2005
# Datas de refer�ncia
data_ata  <- as.Date("2005-01-27")
data_ini  <- data_ata %m-% months(24)
data_fim  <- data_ata %m+% months(24)
data_ata_menos12 <- data_ata %m-% months(12)
data_ata_mais12 <- data_ata %m+% months(12)
data_ata_mais24 <- data_ata %m+% months(24)

# IPCA - Meta
ipca_meta <- get_series(13521,
                        start_date = (data_ini %m-% months(12)),
                        end_date   = data_fim) %>%
  `colnames<-`(c("Data", "Meta_IPCA")) %>%
  arrange(Data)

if (interactive()) View(ipca_meta)

# Obtendo os Dados reais de IPCA ---------------------
ipca_real <- get_series(433,
                        start_date = (data_ini %m-% months(12)),
                        end_date   = data_fim) %>%
  `colnames<-`(c("Data", "IPCA")) %>%
  arrange(Data) %>%
  mutate(
    IPCA_12m = (zoo::rollapplyr(1 + IPCA/100, 12, prod, fill = NA) - 1) * 100,
    #IPCA_ano = (cumprod(1 + IPCA/100) - 1) * 100
  )

if (interactive()) View(ipca_real)

#--------Grafico----------------------------

# Salvar dados utilizados no gráfico em CSV
dados_ipca <- ipca_real %>%
  dplyr::left_join(ipca_meta, by = "Data") %>%
  dplyr::select(Data, IPCA_12m, Meta_IPCA) %>%
  arrange(Data) %>%
  tidyr::fill(IPCA_12m, Meta_IPCA, .direction = "down")

if (!dir.exists("data")) dir.create("data", recursive = TRUE)
utils::write.table(dados_ipca,
                   file = file.path("data", "ipca_real_meta.csv"), 
                   sep = ";", dec = ",", row.names = FALSE, col.names = TRUE, qmethod = "double")

# Gráfico
p <- 
  ggplot(dados_ipca, aes(x = Data)) +
  
  # Linha do IPCA realizado acumulado em 12 meses
  geom_line(aes(y = IPCA_12m, colour = "IPCA Realizado 12m"), size = 1.2) +
  
  # Linha da Meta IPCA
  geom_line(aes(y = Meta_IPCA, colour = "Meta IPCA"), size = 1.2) +
  
  # Linha vertical da ata
  geom_vline(xintercept = as.numeric(data_ata), linetype = "dashed", colour = "red", size = 1) +
  annotate("text", x = data_ata, y = max(dados_ipca$IPCA_12m, dados_ipca$Meta_IPCA, na.rm = TRUE) * 0.95,
           label = "Ata do Copom", colour = "red", angle = 90, vjust = -0.2, size = 3.5) +
  
  # Linha vertical -12 meses
  geom_vline(xintercept = as.numeric(data_ata_menos12), linetype = "dotted", colour = "darkgreen", size = 1) +
  annotate("text", x = data_ata_menos12, y = max(dados_ipca$IPCA_12m, dados_ipca$Meta_IPCA, na.rm = TRUE) * 0.95,
           label = "-12 meses", colour = "darkgreen", angle = 90, vjust = -0.2, size = 3.5) +
  
  # Linha vertical +12 meses
  geom_vline(xintercept = as.numeric(data_ata_mais12), linetype = "dotted", colour = "blue", size = 1) +
  annotate("text", x = data_ata_mais12, y = max(dados_ipca$IPCA_12m, dados_ipca$Meta_IPCA, na.rm = TRUE) * 0.95,
           label = "+12 meses", colour = "blue", angle = 90, vjust = -0.2, size = 3.5) +
  
  # Escalas de cores
  scale_colour_manual(name = NULL,
                      values = c("IPCA Realizado 12m" = "firebrick",
                                 "Meta IPCA" = "darkblue")) +
  guides(colour = guide_legend(order = 1)) +
  
  # Rótulos
  labs(
    title = "Meta de Inflação vs IPCA",
    #subtitle = "IPCA acumulado em 12 meses vs Meta de Inflação",
    #caption = "Fonte: Pesquisa Focus (rbcb) e IBGE",
    x = "Data",
    y = "Inflação (%)"
  ) +
  
  # Eixo X (fixa limites para incluir -12 meses e além)
  scale_x_date(limits = c(as.Date("2004-01-01"), as.Date("2006-03-01")),
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
ggsave(filename = file.path("data", "1_Meta_inflacao.png"), plot = p,
       width = 10, height = 6, dpi = 300)