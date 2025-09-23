# ANÁLISE EX-ANTE E EX-POST DA ATA DO COPOM

Este repositório conduz uma análise sistemática de indicadores econômicos em janelas de 12 meses antes e 12 meses após uma ata de referência do Copom.

## Estrutura
- `main.R`: orquestra o pipeline lendo a configuração e executando para cada indicador.
- `R/utils.R`: funções utilitárias (Focus, SGS, gráfico, exportação, pipeline).
- `config/indicadores.yml`: configuração de data da ata e lista de indicadores.
- `data/`: saídas (CSVs e PNGs) geradas pelo pipeline.

## Como executar
1. Edite `config/indicadores.yml` (data da ata e séries do SGS/Focus).
2. No terminal do projeto, execute:

```r
source('main.R')
```

Os gráficos e tabelas serão salvos em `data/`.

## Indicadores previstos
- Meta de inflação
- Inflação: mensal, acumulada 12m e anual
- PIB trimestral (crescimento)
- Desemprego
- Selic
- Câmbio
- Resultado primário

A lista pode ser expandida editando o YAML.
