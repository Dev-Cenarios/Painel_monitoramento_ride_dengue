# Painel de Monitoramento de Dengue — RIDE-DF

Painel interativo para acompanhar a dengue nos **34 municípios da RIDE-DF**
(Região Integrada de Desenvolvimento do Distrito Federal e Entorno — DF, Goiás
e Minas Gerais), por semana epidemiológica. Ele mostra onde há mais casos, se
a transmissão está crescendo, se a temporada atual está acima do esperado e
traz uma **previsão de casos** até o fim da temporada.

Feito em **R + Shiny**, com gráficos em **echarts4r** e dados servidos via
**DuckDB** sobre um único arquivo **Parquet**. Painéis irmãos:
[Monitoramento DF](https://github.com/InVictorLopes/Dengue_monitoramento_df) ·
[Vigilância DF](https://github.com/InVictorLopes/Dengue_vigilancia_df).

> ℹ️ Os dados deste painel são **agregados por município e semana**, vindos
> da base pública do **InfoDengue** — não há dado individual de paciente.

---

## Sumário

1. [O que o painel mostra](#1-o-que-o-painel-mostra)
2. [Início rápido (passo a passo)](#2-início-rápido-passo-a-passo)
3. [De onde vêm os dados](#3-de-onde-vêm-os-dados)
4. [Ordem de execução dos scripts](#4-ordem-de-execução-dos-scripts)
5. [Metodologia](#5-metodologia)
6. [Estrutura do projeto](#6-estrutura-do-projeto)
7. [Publicação no Shiny Server](#7-publicação-no-shiny-server)
8. [Atualização automática (cron)](#8-atualização-automática-cron)
9. [Solução de problemas](#9-solução-de-problemas)
10. [Dados e privacidade](#10-dados-e-privacidade)
11. [Limitações conhecidas](#11-limitações-conhecidas)
12. [Glossário](#12-glossário)

---

## 1. O que o painel mostra

**Aba "Painel"** — responde ao **recorte geográfico** escolhido no topo
(RIDE-DF inteira → Estado → Município). Clicar num município no mapa também
aplica o recorte.

| Visualização | O que responde |
|---|---|
| **Indicadores (KPIs)** | Casos no ano mais recente, incidência da última semana, Rt da última semana com estimativa e abrangência do recorte. A cor escurece conforme a situação piora. |
| **Distribuição espacial** | Mapa de incidência por município no período escolhido. |
| **Série** | Casos notificados × casos estimados (nowcast), por semana ou mês, em casos ou incidência. |
| **Curva epidêmica** | Casos da temporada em análise contra o volume histórico de cada semana. |
| **Rt (InfoDengue)** | Número de reprodução efetivo estimado pelo InfoDengue; acima de 1 = expansão. |
| **Rt — Tendência por município (quadrantes)** | Compara **todos** os municípios: quais estão acima do normal para a época **e** crescendo. Ignora o recorte de propósito. |
| **Diagrama de controle & previsão** | A incidência da temporada contra o canal endêmico (quartis históricos), com a previsão de casos até o fim da temporada. |
| **Sazonalidade** | Mapa de calor ano × semana: em que semanas a transmissão sobe. |
| **Últimas semanas / SQL gerado** | Tabela das últimas semanas e a consulta usada (transparência). |

**Aba "Análises" (Epi Scanner)** — compara os municípios da RIDE-DF entre
si, sempre com a região inteira: semanas com Rt > 1, Rt máximo do ano mais
recente e casos acumulados desde o início da série.

---

## 2. Início rápido (passo a passo)

Este roteiro é para quem nunca rodou o painel. Siga na ordem.

### 2.1 Pré-requisitos

- **R 4.1 ou superior** (https://cran.r-project.org) e, de preferência, o
  **RStudio** (https://posit.co/download/rstudio-desktop/).
- Para só **abrir** o painel não é preciso banco: o Parquet com os dados já
  vem no repositório.
- Para **atualizar** os dados, uma das duas opções:
  - **qualquer pessoa**: só internet — `etl_ride_api.R` baixa direto da API
    pública do InfoDengue;
  - **equipe do projeto**: acesso ao banco PostgreSQL `cenarios_ai` —
    `etl_ride.R`.

### 2.2 Baixar o projeto

```bash
git clone https://github.com/InVictorLopes/Dengue_monitoramento_ride.git dengue_ride
```

Ou, no GitHub, use **Code → Download ZIP** e descompacte. Abra a pasta no
RStudio e defina-a como diretório de trabalho (**Session → Set Working
Directory → Choose Directory...**).

> Todos os comandos abaixo devem ser executados **com o diretório de
> trabalho na raiz do projeto** (a pasta que contém `global.R`). Confira com
> `getwd()`.

### 2.3 Instalar os pacotes (uma vez só)

```r
install.packages(c(
  # painel
  "shiny", "bslib", "DBI", "duckdb", "dplyr", "dbplyr", "echarts4r", "DT",
  "jsonlite",
  # atualização de dados e preparo das malhas
  "RPostgres", "arrow", "curl", "sf",
  # previsão (opcional)
  "forecast", "mgcv", "ranger", "lubridate"
))
```

> No Linux, o pacote `sf` precisa das bibliotecas de sistema GDAL, GEOS e
> PROJ (ex.: `sudo apt install libgdal-dev libgeos-dev libproj-dev`).

### 2.4 Abrir o painel (com os dados que já vêm no repositório)

```r
shiny::runApp()
```

Na primeira vez, o painel prepara as malhas dos mapas e grava um cache
(`data/geo/.malhas_base_cache.rds`) — as aberturas seguintes são mais
rápidas. A camada de **previsão** fica desativada até você treinar os
modelos (passo 2.6).

### 2.5 Atualizar os dados

**Opção A — pela API pública do InfoDengue (qualquer pessoa, só precisa de
internet):**

```r
source("etl_ride_api.R")
```

Leva cerca de 1 minuto e termina com
`[etl_ride_api] data/dengue_ride_df.parquet gravado: ... 34 municipio(s) ...`.
Para outro período, rode no terminal `Rscript etl_ride_api.R 2020` (a partir
de 2020) ou `Rscript etl_ride_api.R 2020 2024` (de 2020 a 2024). O padrão é de
2015 até o ano corrente.

**Opção B — pelo banco do projeto (equipe, precisa de credenciais):**

1. Copie `.Renviron.example` para `.Renviron` (na mesma pasta) e preencha:

   ```
   PG_HOST=endereço do servidor do banco
   PG_PORT=5432
   PG_DB=cenarios_ai
   PG_USER=seu usuário
   PG_PASS=sua senha
   ```

2. **Reinicie o R** (RStudio: **Session → Restart R**). Confira com
   `Sys.getenv("PG_HOST")`.
3. Rode o ETL:

   ```r
   source("etl_ride.R")
   ```

   Ao final aparece `Parquet da RIDE-DF gerado com sucesso com os 34 municípios!`.

> O `.Renviron` **nunca** vai para o GitHub (está no `.gitignore`). Não
> compartilhe esse arquivo nem cole seu conteúdo em chats ou e-mails.

### 2.6 Treinar os modelos de previsão (opcional)

No terminal (não no console do R), na pasta do projeto:

```bash
Rscript forecasting/train_models.R
```

Pode levar vários minutos. Gera `forecasting/models/*.rds` e
`forecasting/model_metrics.csv`. Reabra o painel e a previsão aparece no
Diagrama de controle.

---

## 3. De onde vêm os dados

| Dado | Origem | Onde fica no projeto | Versionado? |
|---|---|---|---|
| **Casos e estimativas semanais por município** | **InfoDengue** (https://info.dengue.mat.br — Fiocruz/FGV), pela API pública (`etl_ride_api.R`) ou pelo banco PostgreSQL `cenarios_ai`, tabela `"Info_Dengue".dengue` (`etl_ride.R`). | `data/dengue_ride_df.parquet` | ✅ Sim (dado público agregado) |
| **Lista dos 34 municípios** | Lei Complementar nº 163/2018 (composição da RIDE-DF), códigos IBGE de 7 dígitos. | dentro de `etl_ride.R` | ✅ Sim |
| **Malhas de UFs e municípios** | API de malhas do IBGE (`servicodados.ibge.gov.br/api/v3/malhas`), baixadas por `prep/00_prepara_geojson.R`. | `data/geo/*.geojson` | ✅ Sim |

**Principais colunas usadas do InfoDengue:**

| Coluna | Significado |
|---|---|
| `data_ini_se`, `se` | Data de início e código da semana epidemiológica (`AAAASS`) |
| `municipio_geocodigo`, `municipio_nome`, `uf`, `uf_ibge` | Identificação do município |
| `casos` | Casos **notificados** na semana |
| `casos_est` | Casos **estimados** (*nowcast*: corrige o atraso de notificação das semanas recentes) |
| `p_inc100k` | Incidência estimada por 100 mil habitantes |
| `rt` | Número de reprodução efetivo estimado pelo modelo do InfoDengue |
| `pop` | População do município |
| `tempmed`, `umidmed` | Temperatura e umidade médias da semana (usadas na previsão) |

### 3.1 Equipe do projeto

Os dados do InfoDengue são carregados no banco `cenarios_ai` (tabela
`"Info_Dengue".dengue`). Peça as credenciais à equipe, configure o
`.Renviron` e rode `etl_ride.R` (seção 2.5).

### 3.2 Sem acesso ao banco do projeto

- O `data/dengue_ride_df.parquet` versionado já permite rodar o painel
  inteiro, com os dados até a data do último commit.
- Para dados novos, use **`etl_ride_api.R`**: ele baixa cada município da
  API pública do InfoDengue (`https://info.dengue.mat.br/api/alertcity`,
  documentação em https://info.dengue.mat.br/services/api), renomeia as
  colunas para os nomes da tabela acima (`data_iniSE` → `data_ini_se`,
  `SE` → `se`, `Rt` → `rt` etc.), acrescenta a identificação do município e
  grava o mesmo Parquet que o `etl_ride.R` gera a partir do banco. Não
  precisa de credenciais.
- Os dois caminhos dão o mesmo resultado; pequenas diferenças nas semanas
  mais recentes são normais (o InfoDengue revisa os números conforme chegam
  notificações atrasadas).
- Para outra região, troque a lista de códigos IBGE (`RIDE_MUNICIPIOS` em
  `etl_ride_api.R`, ou `ride_codes_oficiais` em `etl_ride.R`): o painel
  monta os seletores e os mapas a partir do que existe no Parquet, sem lista
  de municípios no código do app. Se a região tiver outras UFs, rode também
  `prep/00_prepara_geojson.R`.

---

## 4. Ordem de execução dos scripts

| # | Script | Quando rodar | Gera |
|---|---|---|---|
| 0 | criar `.Renviron` | uma vez por máquina (só para atualizar dados) | credenciais |
| 1 | `etl_ride_api.R` (API pública) **ou** `etl_ride.R` (banco do projeto) | a cada atualização de dados | `data/dengue_ride_df.parquet` |
| 2 | `prep/00_prepara_geojson.R` | **só** se for preciso baixar as malhas de novo (já vêm no repositório) | `data/geo/*.geojson` |
| 3 | `prep/01_simplifica_malhas.R` | depois do passo 2 | as mesmas malhas, mais leves |
| 4 | `forecasting/train_models.R` | depois do passo 1 (opcional, recomendado a cada atualização) | `forecasting/models/*.rds`, `forecasting/model_metrics.csv` |
| 5 | `shiny::runApp()` | — | o painel |
| — | `prep/02_municipios_sem_historico_rt.R` | diagnóstico, quando quiser | `prep/out/*.csv` (lista de municípios "sem histórico" no mapa de tendência) |

Em produção, depois dos passos 1–4 é preciso **reiniciar o app** (ver
[seção 7](#7-publicação-no-shiny-server)).

---

## 5. Metodologia

### 5.1 Ano sanitário (temporada)

Os gráficos por temporada usam o **ano sanitário**: a temporada começa na
**SE 27** (fim de junho/início de julho, baixa transmissão) e termina na
SE 26 do ano seguinte. Assim o pico do verão nunca é cortado ao meio pela
virada do ano. Ex.: a SE 10 de 2025 pertence à temporada 2024/2025.

A **SE 53** (que só existe em alguns anos) é dividida meio a meio entre a
SE 52 e a SE 1 da mesma temporada, para que toda temporada tenha 52 posições
comparáveis.

### 5.2 Incidência

```
incidência = soma dos casos / população × 100.000
```

- Acima do nível municipal (UF ou RIDE-DF), é **recalculada** com a soma dos
  casos e a soma das populações — nunca é a média das incidências dos
  municípios (isso ignoraria o tamanho de cada um).
- Em períodos de várias semanas, a população não é somada semana a semana
  (usa-se a população do município, uma vez).
- Escala de cores do mapa e da sazonalidade, a partir dos cortes do
  Ministério da Saúde: < 100 · 100–299 · 300–499 · 500–999 · ≥ 1.000 casos
  por 100 mil.

### 5.3 Rt

- Vem **pronto do InfoDengue** (coluna `rt`), estimado por município.
- No nível de município, o valor é usado sem transformação.
- Para UF ou RIDE-DF, o painel mostra a **média do Rt dos municípios
  ponderada pelos casos** — é uma aproximação (o Rt de uma região exigiria
  reestimar o modelo com a série agregada) e o tooltip mostra quantos
  municípios estão com Rt > 1 como contraprova.
- Valores muito altos (municípios pequenos) são limitados a 5 no gráfico só
  para não achatar a visualização.

### 5.4 Diagrama de controle (canal endêmico)

- Incidência semanal da **temporada em análise** sobreposta ao **canal
  endêmico** das **5 temporadas anteriores**.
- O canal usa **exclusivamente quartis**: faixa inferior Q1–Q2, faixa
  superior Q2–Q3, linha central = mediana (Q2).
- Cada semana precisa de pelo menos **3 temporadas** de referência para ter
  canal.
- Opcional: linha de *nowcast* (incidência calculada com `casos_est`).

### 5.5 Curva epidêmica

Casos **absolutos** de cada semana da temporada em análise contra:
- a **soma** histórica de casos daquela semana nas temporadas de referência
  (barras); e
- a **mediana** histórica (linha).

Também mostra o *nowcast* da temporada (`casos_est`).

### 5.6 Rt — Tendência por município (quadrantes)

Apesar do nome, **não é o Rt epidemiológico**: é um indicador de anomalia
sazonal + tendência recente para comparar os municípios entre si.

- **Eixo X**: casos do município na janela atual (padrão: 4 semanas,
  ajustável de 1 a 8) ÷ **mediana** dos casos nessa mesma janela de semanas
  em anos anteriores. X > 1 = acima do normal para a época.
- **Eixo Y**: casos da janela atual ÷ casos da janela imediatamente anterior
  (mesmo ano). Y > 1 = subindo.
- **Cor = quadrante**: abaixo do normal · caindo / subindo; acima do normal
  · caindo / subindo (pior cenário).
- **Forma = incidência acumulada no ano**: ▲ < 10 · ◆ 10–50 · ● 50–300 ·
  ■ > 300 por 100 mil.
- **"Sem histórico suficiente"**: sem casos na janela anterior, mediana
  histórica zero/ausente, ou menos de **2 anos** de histórico na janela. O
  script `prep/02_municipios_sem_historico_rt.R` lista esses municípios e o
  motivo.

### 5.7 Epi Scanner (aba "Análises")

Por município, sempre com a RIDE-DF inteira:
- **Semanas com Rt > 1** desde o início da série;
- **Rt máximo** no ano mais recente;
- **Casos acumulados** desde o início da série.

Mapa + tabela "Top 10" para cada métrica.

### 5.8 Previsão de casos (forecasting)

Resumo — a documentação completa está em
[`forecasting/README-FORECAST.md`](forecasting/README-FORECAST.md).

- **Modelos**, agrupados por classe:
  - *Estatísticos*: ARIMA + Fourier, SARIMA, GAM binomial negativo (com
    clima e casos defasados em 4 semanas), STL + ETS;
  - *Machine learning*: Random Forest (`ranger`, opcional).
- **Avaliação** por *backtest* de origem rolante (24 origens × horizonte de
  1–4 semanas), com RMSE, MAE, MAPE, sMAPE e MASE.
- **Ensemble em dois níveis**, ponderado pelo inverso do RMSE: um por classe
  e um **global** que combina as classes. Faixa sombreada = intervalo de
  predição de 80% do ensemble global.
- Os modelos preveem **contagem de casos** (com a população como *offset*);
  a incidência é derivada depois.
- As **4 semanas mais recentes** são sempre reprojetadas (a notificação
  delas ainda está incompleta), e o horizonte vai até a **SE 26** do fim da
  temporada corrente.

---

## 6. Estrutura do projeto

```
dengue_ride/
├── global.R                  # Pacotes, DuckDB sobre o Parquet, dicionário geográfico, malhas, helpers
├── ui.R                      # Interface (bslib::page_navbar)
├── server.R                  # Reactives compartilhadas + orquestração dos módulos
├── etl_ride_api.R            # API pública do InfoDengue → data/dengue_ride_df.parquet (sem credenciais)
├── etl_ride.R                # PostgreSQL ("Info_Dengue".dengue) → data/dengue_ride_df.parquet
├── .Renviron.example         # Modelo das credenciais (copiar para .Renviron)
├── R/
│   ├── utils_ui.R            # Tema visual e componente de card
│   ├── mod_geo_filter.R      # Recorte: RIDE-DF → UF → município
│   ├── mod_kpi.R             # Quadros de indicadores
│   ├── mod_mapa.R            # Mapa de incidência
│   ├── mod_serie_temporal.R  # Notificado × nowcast
│   ├── mod_curva_epidemica.R # Curva epidêmica por temporada
│   ├── mod_rt.R              # Rt do InfoDengue
│   ├── mod_mapa_rt.R         # Tendência por município (quadrantes)
│   ├── mod_diagrama_controle.R # Canal endêmico + previsão
│   ├── mod_heatmap.R         # Sazonalidade ano × semana
│   └── mod_epi_scanner_*.R   # Aba "Análises"
├── data/
│   ├── dengue_ride_df.parquet
│   └── geo/                  # Malhas GeoJSON do IBGE (UFs e municípios)
├── prep/                     # Preparo de malhas e diagnóstico (fora do app)
├── forecasting/              # Treino e inferência da previsão (ver README-FORECAST.md)
└── deploy/shiny-server.conf  # Modelo de configuração do Shiny Server
```

**Como o painel lê os dados:** `global.R` abre um DuckDB em memória com uma
*view* sobre o Parquet; os filtros viram `WHERE` executados na leitura do
arquivo (*predicate pushdown*). O dicionário geográfico (UFs e municípios
disponíveis) e a série semanal por município são calculados uma vez na
inicialização — **não há lista de municípios escrita no app**: trocar o
Parquet troca os seletores.

**Variáveis de ambiente opcionais:** `DENGUE_PARQUET` (outro caminho para o
Parquet), `DENGUE_GEO_DIR` (outra pasta de malhas), `DENGUE_ROTULO_BASE`
(rótulo da região inteira, padrão "RIDE-DF").

---

## 7. Publicação no Shiny Server

1. Copie (ou clone) o projeto para a pasta de apps do Shiny Server — no
   servidor atual, `~/shiny-server/dengue_ride`.
2. Instale os pacotes **no R do servidor** (seção 2.3).
3. Se for atualizar dados no servidor, crie o `.Renviron` (seção 2.5).
4. Treine os modelos de previsão no servidor (seção 2.6).
5. Para o Shiny Server recarregar o app após atualizar dados ou código:

   ```bash
   touch ~/shiny-server/dengue_ride/restart.txt
   ```

**Desempenho:** o painel guarda caches no processo R (malhas, séries,
gráficos). Por padrão o Shiny Server encerra o processo segundos depois da
última visita, e cada visitante paga a inicialização inteira. O arquivo
`deploy/shiny-server.conf` traz um bloco `location` de exemplo com
`app_idle_timeout 0` (mantém o processo vivo) e explica como aplicar e
conferir — exige acesso de administrador ao servidor.

---

## 8. Atualização automática (cron)

Exemplo semanal (segunda-feira, 3h), editando com `crontab -e` no servidor e
ajustando o caminho:

```cron
0 3 * * 1 cd /home/USUARIO/shiny-server/dengue_ride && (Rscript etl_ride_api.R && Rscript forecasting/train_models.R && touch restart.txt) >> /tmp/dengue_ride_etl.log 2>&1
```

Troque `etl_ride_api.R` por `etl_ride.R` para usar o banco do projeto. O
`Rscript` lê o `.Renviron` da pasta em que é executado, por isso o `cd`
antes dos scripts é obrigatório.

---

## 9. Solução de problemas

| Sintoma | Causa provável | Solução |
|---|---|---|
| `Arquivo Parquet nao encontrado` | Diretório de trabalho errado ou Parquet ausente. | `getwd()` deve ser a pasta com `global.R`; rode `etl_ride_api.R` (ou `etl_ride.R`) se o arquivo não existir. |
| `could not connect to server` / `password authentication failed` (ETL) | `.Renviron` ausente/incompleto, R não reiniciado ou fora da rede do banco. | Confira `Sys.getenv("PG_HOST")`, reinicie o R, verifique rede/VPN. |
| Mensagem "Forecasting: artefatos ausentes" no log | Modelos não treinados. | Rode `Rscript forecasting/train_models.R` (opcional — o resto do painel funciona). |
| Mapa vazio ou "municípios sem polígono" | Malhas ausentes em `data/geo/`. | Rode `prep/00_prepara_geojson.R` e depois `prep/01_simplifica_malhas.R`. |
| Primeiro acesso muito lento | Processo R recém-iniciado (caches vazios). | Aplique o `deploy/shiny-server.conf` (`app_idle_timeout 0`). |
| Painel não mostra os dados novos no servidor | O app carrega parte dos dados só na inicialização. | `touch restart.txt` na pasta do app. |
| Município aparece como "Sem histórico suficiente" | Falta de histórico na janela de comparação. | Rode `Rscript prep/02_municipios_sem_historico_rt.R` para ver o motivo. |

---

## 10. Dados e privacidade

- O Parquet contém apenas **dados agregados por município e semana**,
  publicados pelo InfoDengue — por isso ele é versionado.
- O que **não** vai para o repositório: `.Renviron` (credenciais do banco),
  modelos treinados (`forecasting/models/*.rds`, grandes e regeneráveis) e
  caches (`data/geo/.malhas_base_cache.rds`, `app_cache/`).

---

## 11. Limitações conhecidas

- **Atraso de notificação**: as semanas mais recentes têm `casos`
  incompletos; use o *nowcast* (`casos_est`) para lê-las.
- **Rt regional** (UF/RIDE-DF) é uma média ponderada do Rt municipal, não uma
  reestimação.
- O **mapa de tendência** usa "Rt" no título, mas é uma razão de anomalia +
  tendência (seção 5.6).
- A **previsão** é calibrada na RIDE-DF agregada; em recortes pequenos
  (municípios com poucos casos) a incerteza é maior. Os mesmos dados de
  *backtest* definem os pesos e as métricas do ensemble (leve otimismo nas
  métricas do ensemble).
- `etl_ride.R` depende da atualização da tabela `"Info_Dengue".dengue` no
  banco do projeto; `etl_ride_api.R` depende da disponibilidade da API do
  InfoDengue.

---

## 12. Glossário

| Termo | Significado |
|---|---|
| **RIDE-DF** | Região Integrada de Desenvolvimento do DF e Entorno: DF + 29 municípios de GO + 4 de MG. |
| **InfoDengue** | Sistema de alerta de arboviroses (Fiocruz/FGV) que publica casos, estimativas e Rt por município e semana. |
| **SE** | Semana epidemiológica (domingo a sábado, 1 a 52/53 no ano). |
| **Ano sanitário / temporada** | Período da SE 27 de um ano à SE 26 do ano seguinte. |
| **Incidência** | Casos por 100 mil habitantes. |
| **Nowcast** | Estimativa dos casos que ainda vão ser notificados nas semanas recentes. |
| **Rt** | Quantos casos novos cada caso gera; > 1 indica expansão. |
| **Canal endêmico** | Faixa "esperada" de incidência por semana, calculada com o histórico (quartis). |
| **Ensemble** | Combinação das previsões de vários modelos, ponderada pelo desempenho de cada um. |
| **ETL** | Script que extrai do banco, trata e grava o arquivo lido pelo painel. |

---
Desenvolvido com R + Shiny + echarts4r + bslib + DuckDB
