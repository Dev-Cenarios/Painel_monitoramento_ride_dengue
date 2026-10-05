# Painel de Monitoramento de Dengue — RIDE-DF

Painel epidemiológico de dengue para a **RIDE-DF** (34 municípios em 3 UFs —
DF, GO e MG), em **R Shiny + echarts4r + bslib**, com dados servidos via
**DuckDB** sobre um único Parquet local. Arquitetura irmã do
[painel de monitoramento do DF](https://github.com/InVictorLopes/Dengue_monitoramento_df).

- **Fonte**: `data/dengue_ride_df.parquet`, extraído da base pública do
  InfoDengue (dados agregados por município e semana epidemiológica) por
  `etl_ride.R`. O app não consulta banco de dados em produção.
- **Duas abas**:
  - **Painel** — recorte geográfico hierárquico (RIDE-DF → UF → município),
    escolhido nos filtros ou clicando no mapa.
  - **Análises** — Epi Scanner, sempre com a RIDE-DF inteira (comparação
    entre municípios), independente do filtro da aba Painel.
- **Previsão** — camada opcional de forecasting (ensemble de modelos de série
  temporal), ver [`forecasting/README-FORECAST.md`](forecasting/README-FORECAST.md).

## Estrutura do projeto

```
dengue_ride/
├── global.R                  # Pacotes, conexão DuckDB, dicionário geográfico, malhas, helpers
├── ui.R                      # Interface (bslib::page_navbar)
├── server.R                  # Reactives compartilhadas + orquestração dos módulos
├── etl_ride.R                # PostgreSQL ("Info_Dengue".dengue) → data/dengue_ride_df.parquet
├── R/                        # Módulos Shiny (KPIs, mapas, série, Rt, heatmap, diagrama de controle, Epi Scanner)
├── data/
│   ├── dengue_ride_df.parquet
│   └── geo/                  # Malhas GeoJSON (UFs e municípios)
├── prep/                     # Scripts de preparação (rodados fora do app)
│   ├── 00_prepara_geojson.R
│   ├── 01_simplifica_malhas.R
│   └── 02_municipios_sem_historico_rt.R
├── forecasting/              # Treino e inferência do ensemble de previsão
└── deploy/shiny-server.conf  # Modelo de configuração do Shiny Server
```

## Pacotes necessários

```r
install.packages(c(
  "shiny", "bslib", "DBI", "duckdb", "dplyr", "dbplyr",
  "echarts4r", "DT"
))
```

Para o ETL, a preparação de malhas e o forecasting (rodados fora do app):
`RPostgres`, `arrow`, `sf`, `curl`, `lubridate`, `forecast`, `mgcv`, `ranger`.

## Configuração

Copie `.Renviron.example` para `.Renviron` (não versionado) e preencha as
credenciais do PostgreSQL usadas por `etl_ride.R`:

```
PG_HOST=...
PG_PORT=5432
PG_DB=...
PG_USER=...
PG_PASS=...
```

Reinicie a sessão do R para que o `.Renviron` seja lido.

## Como rodar

```r
# working directory = raiz do app (dengue_ride/)
shiny::runApp()
```

Na primeira subida, `global.R` recorta as malhas de `data/geo/` e grava um
cache (`data/geo/.malhas_base_cache.rds`, não versionado).

## Atualizando os dados

```bash
Rscript etl_ride.R                    # regrava data/dengue_ride_df.parquet
Rscript forecasting/train_models.R    # (opcional) retreina os modelos de previsão
```

Os artefatos de modelo (`forecasting/models/*.rds`) não são versionados — sem
eles o painel funciona normalmente, apenas com a camada de previsão
desativada.

## Deploy

`deploy/shiny-server.conf` traz um bloco `location` de exemplo com
`app_idle_timeout 0`, que mantém o processo R (e seus caches) vivo entre
visitas. Veja as instruções no próprio arquivo.

---
Desenvolvido com R + Shiny + echarts4r + bslib + DuckDB
