# ANÁLISE EX-ANTE E EX-POST DA ATA DO COPOM

Este repositório conduz uma análise sistemática de indicadores econômicos em janelas de 12 meses antes e 12–24 meses após uma ata de referência do Copom (neste projeto, 27/01/2005).

## Estrutura
- Scripts R numerados (`1_*.R` a `11_*.R`): cada script é autônomo, baixa dados (Focus/SGS ou CSV local), aplica transformações e salva saídas em `data/` (CSVs e PNGs).
- `data/`: saídas geradas (arquivos `*_expec.csv`, `*_real.csv` e gráficos `*.png`).

Observação: o projeto atual não depende mais de `main.R`, `R/utils.R` ou `config/indicadores.yml`.

## Como executar
No terminal dentro da pasta do projeto (`Etapa2`), execute qualquer script desejado:

```r
Rscript 1_Meta_inflacao.R
Rscript 2_IPCA.R
...
Rscript 11_divida_liquida.R
```

Os gráficos e tabelas serão salvos em `data/`. Os scripts criam a pasta se necessário.

## Dependências (R)
- dplyr, ggplot2, rbcb, knitr, zoo, lubridate
- ggnewscale (usado em `11_divida_liquida.R`)
- tidyr (usado pontualmente via `tidyr::fill` em `1_Meta_inflacao.R`)

## Referências das séries (SGS e fontes)
- 13521 — Meta para a inflação (% a.a.)
- 433 — Índice Nacional de Preços ao Consumidor Amplo (IPCA) — var. % mensal
- 7326 — Produto Interno Bruto — taxa de variação real no ano — var. % anual
- taxa_desemprego.csv — IBGE/PME via ipeadatapy (série PAN12_TD12)
- 1178 — Taxa de juros — Selic anualizada (base 252) — % a.a.
- 1 — Taxa de câmbio — Livre — Dólar americano (venda) — diário — u.m.c./US$
- 5727 — NFSP sem desvalorização cambial (% PIB) — fluxo acumulado em 12 meses — Resultado nominal — Total — Setor público consolidado — %
- 5793 — NFSP sem desvalorização cambial (% PIB) — fluxo acumulado em 12 meses — Resultado primário — Total — Setor público consolidado — %
- 189 — Índice Geral de Preços do Mercado (IGP-M) — var. % mensal
- 22707 — Balança comercial — Balanço de Pagamentos — mensal — saldo — US$ (milhões)
- 22885 — Investimentos diretos no país — IDP — mensal — líquido — US$ (milhões)
- 4513 — Dívida Líquida do Setor Público (% PIB) — Total — Setor público consolidado — %

## Séries e transformações por indicador
A menos que indicado, para expectativas (Focus) é usado `type = "annual"`, `keep_names = TRUE` e o filtro `$filter = "baseCalculo eq 0"`.

O filtro baseCalculo igual a 1 refere-se ao prazo de validade das expectativas informadas. Esta coluna pode ser 0 ou 1. baseCalculo igual a 0 considera as expectativas informadas nos últimos 30 dias no cálculo das estatísticas e baseCalculo igual a 1 considera os últimos 4 dias. Foi escolhida baseCalculo igual a 0 por ter uma amostra maior.

Em todos os casos, as expectativas são filtradas para `DataReferencia == year(Data) + 1` e `DataReferencia ∈ {2004, 2005, 2006}`, e a coluna `Data` é deslocada em +1 ano para alinhar à referência projetada.

### 1) Meta de inflação vs IPCA (arquivo `1_Meta_inflacao.R`)
- Séries SGS:
  - 13521 — Meta para a inflação (% a.a.) 
  - 433 — IPCA mensal (IPCA)
- Transformações:
  - IPCA_12m = (produtorio de 12 meses de (1 + IPCA/100) − 1) × 100
  - Junção por `Data` com Meta_IPCA e preenchimento para frente (`tidyr::fill`)
- Saídas: `data/ipca_real_meta.csv`, `data/1_Meta_inflacao.png`

### 2) IPCA com expectativas (arquivo `2_IPCA.R`)
- Expectativas Focus: `indic = "IPCA"`
- Série SGS real: 433 — IPCA mensal → cálculo de `IPCA_12m` como acima
- Transformações adicionais: filtra `IPCA_12m` para janela [ata−12m, ata+24m]
- Saídas: `data/ipca_expec.csv`, `data/ipca_real.csv`, `data/2_ipca_com_expectativa.png`

### 3) PIB (arquivo `3_PIB.R`)
- Expectativas Focus: `indic = "PIB Total"`
- Série SGS real: 7326 — PIB anual (%)
- Transformações: apenas ordenação por data (sem cálculo adicional)
- Saídas: `data/pib_expec.csv`, `data/pib_real.csv`, `data/3_pib_com_expectativa.png`

### 4) Taxa de desemprego (arquivo `4_taxa_desemprego.R`)
- Fonte: `data/taxa_desemprego.csv` — IBGE/PME via ipeadatapy (série PAN12_TD12). Colunas normalizadas para `Data` e `Taxa_Desemprego`.
- Transformações: conversão de data, ordenação e filtro para [ata−12m, ata+12m]
- Saídas: `data/4_taxa_desemprego.png`

### 5) Selic (arquivo `5_selic.R`)
- Expectativas Focus: `indic = "Selic"`
- Série SGS real: 1178 — Selic (% a.a.)
- Transformações: ordenação por data (sem cálculo adicional)
- Saídas: `data/selic_expec.csv`, `data/selic_real.csv`, `data/5_selic.png`

### 6) Câmbio (arquivo `6_cambio.R`)
- Expectativas Focus: `indic = "Câmbio"`
- Série SGS real: 1 — Câmbio R$/US$
- Transformações: ordenação por data (sem cálculo adicional)
- Saídas: `data/cambio_expec.csv`, `data/cambio_real.csv`, `data/6_cambio.png`

### 7) Resultado nominal (arquivo `7_resultado_nominal.R`)
- Expectativas Focus: `indic = "Resultado nominal"`
- Série SGS real: 5727 — NFSP sem desvalorização cambial (% PIB), fluxo 12m
- Transformações: `Resultado_Nominal = Resultado_Nominal × (−1)` (sinal invertido)
- Saídas: `data/resultado_nominal_expec.csv`, `data/resultado_nominal_real.csv`, `data/7_resultado_nominal.png`

### 7b) Resultado primário (arquivo `7_resultado_primario.R`)
- Expectativas Focus: `indic = "Resultado primário"`
- Série SGS real: 5793 — NFSP sem desvalorização cambial (% PIB), fluxo 12m
- Transformações: `Resultado_Primario = Resultado_Primario × (−1)` (sinal invertido)
- Saídas: `data/resultado_primario_expec.csv`, `data/resultado_primario_real.csv`, `data/7_resultado_primario.png`

### 8) IGP-M (arquivo `8_igp_m.R`)
- Expectativas Focus: `indic = "IGP-M"`
- Série SGS real: 189 — IGP-M mensal
- Transformações: `IGP_M_12m = (produtorio de 12 meses de (1 + IGP_M/100) − 1) × 100`
- Saídas: `data/igp_m_expec.csv`, `data/igp_m_real.csv`, `data/8_igp_m.png`

### 9) Balança comercial (arquivo `9_balanca_comercial.R`)
- Expectativas Focus: `indic = "Balança comercial"` com filtro adicional `IndicadorDetalhe == "Saldo"`
- Série SGS real: 22707 — Saldo (US$ milhões)
- Transformações:
  - `Balanca_Comercial = Balanca_Comercial / 1000` (milhões → bilhões de US$)
  - `Balanca_Comercial_12m = soma móvel de 12 meses`
- Saídas: `data/balanca_comercial_expec.csv`, `data/balanca_comercial_real.csv`, `data/9_balanca_comercial.png`

### 10) Investimento direto (arquivo `10_investimento_direto.R`)
- Expectativas Focus: `indic = "Investimento direto no país"`
- Série SGS real: 22885 — Investimento direto (US$ milhões)
- Transformações:
  - `Investimento_Direto = Investimento_Direto / 1000` (milhões → bilhões de US$)
  - `Investimento_Direto_12m = soma móvel de 12 meses`
- Saídas: `data/investimento_direto_expec.csv`, `data/investimento_direto_real.csv`, `data/10_investimento_direto.png`

### 11) Dívida líquida do setor público (arquivo `11_divida_liquida.R`)
- Expectativas Focus: `indic = "Dívida líquida do setor público"`
- Série SGS real: 4513 — Dívida líquida (% do PIB)
- Transformações: ordenação por data (sem cálculo adicional)
- Saídas: `data/divida_liquida_expec.csv`, `data/divida_liquida_real.csv`, `data/11_divida_liquida.png`

## Janela temporal comum
Todos os scripts usam como referência `data_ata = 2005-01-27`, definindo `data_ini = ata − 24 meses` e `data_fim = ata + 24 meses`. Os gráficos incluem linhas verticais em ata, ata−12m e ata+12/24m.

