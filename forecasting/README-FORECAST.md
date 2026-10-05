# Módulo de Forecasting — Dengue RIDE

Pipeline de previsão de casos de dengue para a RIDE-DF, inspirado na
metodologia do [Mosqlimate](https://mosqlimate.org/) (desafio *Sprint 2024/2025*
de previsão de dengue): vários modelos de série temporal treinados de forma
independente e combinados por um **ensemble** de média ponderada pelo desempenho
histórico de cada um.

A lógica de treino é **isolada do Shiny**. O app apenas carrega os artefatos
`.rds` e chama uma função. Nada de `auto.arima`/`gam` roda durante o boot do
painel.

---

## 1. Arquitetura de dados

Idêntica ao restante do projeto: **um único
Parquet local** (`data/dengue_ride_df.parquet`) lido **sob demanda pelo DuckDB**
com SQL puro (predicate/projection pushdown). Sem PostgreSQL.

`utils_forecast.R` **replica de propósito** duas coisas do `global.R`, para
poder rodar via `Rscript` sem subir o app:

| `global.R` | Equivalente aqui |
|---|---|
| `.dengue_conectar()` → `CREATE VIEW dengue AS SELECT * FROM read_parquet(...)` | `fc_connect()` |
| `consultar_serie_semanal(geo)` (agregação por recorte) | `fc_query_serie(con, where_sql)` |
| `aplicar_filtro_geo()` (`brasil` / `uf` / `municipio`) | `fc_resolve_geo(location_id)` |
| `ano_sanitario()`, `dobrar_semana_53()` | reimplementados sem alterar a regra |

### `serie_semanal` reproduzida em SQL

```sql
SELECT data_ini_se,
       max(se)                                  AS se,
       sum(casos)                               AS casos,
       sum(casos_est)                           AS casos_est,
       sum(pop)                                 AS pop,
       sum(tempmed * pop) / nullif(sum(pop),0)  AS tempmed,   -- média ponderada
       sum(umidmed * pop) / nullif(sum(pop),0)  AS umidmed
FROM dengue
WHERE <brasil = TRUE | uf_ibge = ? | municipio_geocodigo = ?>
GROUP BY data_ini_se
ORDER BY data_ini_se;
```

`casos` e `pop` **somam** sobre os municípios do recorte; temperatura e umidade
entram como **média ponderada pela população** (somar temperatura entre
municípios não tem sentido). No nível municipal a ponderação colapsa no próprio
valor.

---

## 2. Regras epidemiológicas respeitadas

- **Incidência** nunca é modelada diretamente: os modelos preveem **contagem de
  casos** com `offset(log(pop))`. Qualquer incidência é derivada depois como
  `sum(casos) / sum(pop) * 100.000` sobre a área — nunca média de razões.
- **Ano sanitário**: a saída de `get_dengue_forecast()` é rotulada por
  temporada (`Ano_Sanitario`, `Temporada` no formato `2024/2025`) e ordenável no
  eixo SE 27 → SE 26. A sazonalidade dos modelos é anual (período 52), então o
  ciclo de transmissão é aprendido inteiro, sem corte no réveillon.
- **SE 53**: dobrada nas semanas vizinhas (SE 52 / SE 1) da mesma temporada e do
  mesmo tipo (observado/previsto), como em `global.R::dobrar_semana_53()`.
- **Separação de abas**: os pesos do ensemble e as métricas são calculados sobre
  a **RIDE-DF agregada** (série mais longa e estável — mesma lógica da aba
  "Análises", que ignora o recorte). A função de inferência então reaproveita
  esses pesos e reajusta as *specs* ao recorte hierárquico pedido.

---

## 3. Base histórica e variáveis

Série semanal contínua (grade regularizada; semanas sem registro → `casos = 0`,
convenção de vigilância). Preditores exógenos (GAM e Random Forest):

| Variável | Origem | Uso |
|---|---|---|
| `semana` (SE 1–53) | `se % 100` | sazonalidade (spline cíclica no GAM) |
| `t` (índice contínuo) | posição na série | tendência plurianual lenta |
| `tempmed_l4`, `umidmed_l4` | clima do InfoDengue, **defasado 4 semanas** | efeito climático retardado |
| `casos_l4` | próprios casos, **defasados 4 semanas** | autocorrelação / nível recente |
| `inc_l4` | `casos_l4 / pop × 1e5` | só no RF: nível recente em taxa, para generalizar entre recortes |

ARIMA, SARIMA e STL+ETS usam só a própria série de `casos`.

---

## 4. Modelos, classes e o registro

Os modelos são organizados por **classe** (`FC_CLASSES` em `utils_forecast.R`).
Cada classe tem seu próprio ensemble; um **ensemble global** combina os
ensembles de classe. Para adicionar um modelo: escreva `fit_*` / `predict_*`
(contrato: `predict` devolve `list(mean, lower, upper)` de comprimento `h`, tudo
`>= 0`) e registre uma entrada em `FC_MODELOS` com a `classe`. Backtest,
ensembles, saída de `get_dengue_forecast()` e checkboxes do painel se ajustam
sozinhos.

| Classe | Modelo | Pacote | Detalhe |
|---|---|---|---|
| **Estatísticos** | ARIMA + Fourier | `forecast::auto.arima` | Não-sazonal + `K = 3` harmônicas de Fourier como `xreg` (a sazonalidade anual entra pelo `xreg`, não pela estrutura ARIMA). Box-Cox só sem zeros. IP analítico. |
| | SARIMA | `forecast::auto.arima` | ARIMA **sazonal** de fato: diferença sazonal (lag 52) + termos AR/MA sazonais. `auto.arima` busca a parte não-sazonal (`p,d,q`); a sazonal fica em `P,Q ≤ 1` e `D` é forçado a 1 com ≥ 2 estações (senão o `auto.arima` recairia num ARIMA simples). Sem Fourier — captura o ciclo anual por caminho diferente do M1. Box-Cox só sem zeros. IP analítico. |
| | GAM (Bin. Negativa) | `mgcv::gam` | `casos ~ s(SE, bs="cc") + s(t, bs="cr") + s(clima_l4) + s(casos_l4) + offset(log pop)`, `family = nb()`. IP por Monte Carlo (1500 sims). |
| | STL + ETS | `forecast::stlf` | STL (`s.window = "periodic"`, robusto) + ETS na série sazonalmente ajustada. Análogo viável de Holt-Winters para frequência 52. IP nativo. |
| **Machine Learning** | Random Forest | `ranger` *(opcional)* | Mesmas features do GAM + `inc_l` (casos_l4 em taxa /100 mil, para generalizar entre recortes). `quantreg = TRUE` → IP pelos quantis 10/90 das árvores, sem suposição paramétrica. |
| **Mecanicistas** | — | — | Classe registrada, **sem modelo ainda**. Plugue um SIR/SEIR aqui quando quiser. |

Defasagem dos preditores exógenos = **4 semanas** (`FC_LAG`). Nasceu do
horizonte do *backtest* (`h = 1..4`, ver §5): nesses 4 passos, prever não exige
extrapolar clima nem casos - os valores defasados já são todos observados.

**Horizonte operacional ≠ 4 semanas fixas.** `get_dengue_forecast()` projeta,
por padrão, **até a SE 26 do fim da temporada corrente**
(`fc_horizon_fim_temporada()`) - varia a cada chamada conforme a última semana
observada (normalmente 20-52 semanas, não um número fixo).

**O treino/previsão nunca ancora na última semana disponível.** As
`FC_SEMANAS_RECENTES` (4, por padrão) semanas mais recentes são SEMPRE
reprojetadas em vez de tomadas como definitivas - a contagem de `casos` dessas
semanas tende a estar incompleta (a notificação ainda não decantou).

O corte é contado a partir da **última linha que a base realmente tem**
(`d_obs$data_ini_se[nrow(d_obs)]`, respeitado o teto de `data_atual`) - **não**
a partir de `data_atual`/"hoje" em si. Essa distinção importa porque a base é
atualizada em lote, não ao vivo: se "hoje" já estiver semanas ou meses à frente
do último dado publicado (comum quando o Parquet não foi re-extraído
recentemente), recuar a partir de "hoje" não recuaria nada - o dado mais
recente já seria "velho" por conta própria, e a regra pareceria não fazer
efeito nenhum. Ancorando na última linha disponível, a reprojeção das 4
semanas vale **sempre**, incluindo quando a base está desatualizada.

O horizonte é contado a partir desse corte, mas a temporada-alvo (de onde sai
a SE 26) é sempre a da última linha disponível - garante que o horizonte nunca
fica curto demais pra cobrir nem as próprias 4 semanas recentes, mesmo quando
o corte cai perto do fim de uma temporada (ex.: temporada corrente muito jovem,
com menos de 4 semanas de histórico - nesse caso a reprojeção "vaza" pra
temporada anterior, e a nota do painel lista as duas temporadas com previsão
em vez de acusar erro). Essas semanas recentes saem com **dois registros** no
retorno: `Tipo = "observado"` (o que foi notificado até agora) e `Tipo =
"previsto"` (reestimativa do ensemble) - dá pra comparar os dois no gráfico.
Ajustável via `get_dengue_forecast(..., semanas_recentes = N)`.

Para horizontes acima de `FC_LAG` (4), os preditores defasados de GAM e Random Forest
**congelam no último valor observado** (`li <- pmax(pmin(n + seq_len(h) - lag,
n), 1L)` em `predict_fc_m2`/`predict_fc_rf`) - a partir da 5ª semana prevista,
a única fonte de variação semana a semana desses dois modelos é a sazonalidade
(`s(semana)` no GAM) e a tendência lenta (`t`); ARIMA+Fourier e STL+ETS não têm
essa limitação, pois dependem só da própria série de `casos`. Por isso o
ensemble tende a ficar mais próximo dos modelos estatísticos (que têm dinâmica
própria) à medida que o horizonte se afasta das primeiras 4 semanas.

> **`ranger` é dependência opcional.** Se não estiver instalado na sessão, a
> classe *Machine Learning* fica vazia (como a *Mecanicistas*): `FC_MODELOS`
> não registra o RF, `get_dengue_forecast()` não emite as colunas `*_ml`, o
> painel não mostra a seção ML, e o ensemble global passa a ser só o ensemble
> estatístico. Nada quebra. Para ligar o RF: `install.packages("ranger")` na
> biblioteca do R que roda o app, e re-rode `train_models.R`.
> (`DENGUE_FC_NO_RANGER=1` força esse caminho degradado mesmo com o pacote
> presente — útil para testar o deploy.)

---

## 5. Avaliação — backtest de origem rolante

Não é um holdout único. Para cada origem `o` nas últimas `N` semanas: treina em
`1:o` (janela **expansível**), prevê `o+1 … o+4`, coleta os erros por passo de
horizonte. Estilo *rolling-origin* do Mosqlimate.

- **N padrão:** 24 origens → 96 previsões por modelo (24 × 4 passos).
- **Treino mínimo:** 156 semanas (3 anos) antes da primeira origem.
- Ajuste: `Rscript forecasting/train_models.R --origens=24 --min_treino=156`.

### Métricas

| Métrica | Definição | Observação |
|---|---|---|
| **RMSE** | `sqrt(mean((obs − pred)²))` | penaliza erros grandes; base dos pesos do ensemble |
| **MAE** | `mean(\|obs − pred\|)` | erro típico em casos |
| **MAPE** | `mean(\|obs − pred\| / obs) · 100` | **só** em semanas com `obs ≥ 5` |
| sMAPE | `mean(2\|obs − pred\| / (\|obs\|+\|pred\|)) · 100` | simétrico, definido em `obs = 0` |
| MASE | `MAE / MAE_ingênuo_sazonal` (lag 52) | `< 1` ⇒ melhor que repetir a temporada anterior |

**Por que o MAPE não guarda tudo:** dengue tem semanas de entressafra com 0–2
casos; aí `|erro| / obs` explode ou fica indefinido. O MAPE é reportado (com o
piso de 5 casos e a contagem `n_mape` de semanas usadas), mas **RMSE, MAE e
MASE são os critérios primários** — e o RMSE é o que pondera o ensemble.

---

## 6. Ensembles em dois níveis

Toda média é ponderada pelo **inverso do RMSE** do backtest — estável mesmo com
as semanas de ~0 casos que desestabilizam o inverso do MAPE. Pesos sempre
renormalizados sobre os componentes disponíveis (algum modelo pode falhar num
recorte magro).

**Nível 1 — ensemble de cada classe.** Combina os modelos *daquela* classe:

```
w_i = (1 / RMSE_i) / Σ_{j ∈ classe} (1 / RMSE_j)
```

Classe com um só modelo → o ensemble é esse modelo. Classe sem modelo
(mecanicistas, hoje) → nenhum ensemble.

**Nível 2 — ensemble global.** Combina os *ensembles de classe*, cada um com
peso `(1 / RMSE_classe) / Σ (1 / RMSE_classe)` (RMSE do próprio ensemble de
classe no backtest).

Intervalo de predição (em cada nível): média ponderada dos limites 80% dos
componentes. Simples, sem supor independência.

`get_dengue_forecast()` devolve o ponto de **cada modelo** (`Prev_<chave>`), de
**cada ensemble de classe** (`Prev_ens_<classe>` + `Lim_inf_`/`Lim_sup_`) e do
**ensemble global** (`Casos_Previstos_Ensemble` + `Limite_Inferior`/`Superior`).
No painel do Diagrama de Controle, checkboxes por classe escolhem quais linhas
aparecem; a faixa sombreada é o IP 80% do global.

> **Nota de honestidade estatística:** os mesmos dados de backtest definem os
> pesos e reportam as métricas do ensemble — há um leve otimismo nas linhas
> `ensemble_*` da tabela. Para o uso operacional (peso relativo entre modelos)
> o efeito é pequeno; um esquema aninhado fica como melhoria futura.

---

## 7. Resultados

<!-- METRICS:START -->

_Backtest de origem rolante: 24 origens, horizonte 4 semanas, janela expansivel (>= 156 semanas de treino). Escopo: RIDE-DF. Atualizado em 2026-09-21 15:15._

Modelos agrupados por classe; um ensemble por classe (peso = inverso do RMSE dentro da classe) e um ensemble global que combina os ensembles de classe (peso = inverso do RMSE de cada classe).

| Classe | Modelo | RMSE | MAE | MAPE (%) | sMAPE (%) | MASE | Peso |
|---|---|---:|---:|---:|---:|---:|---:|
| estatistico | ARIMA + Fourier | 146.58 | 113.44 | 16.53 | 16.77 | 0.05 | 0.323 |
| estatistico | SARIMA | 174.78 | 143.90 | 21.04 | 21.06 | 0.07 | 0.271 |
| estatistico | GAM (Bin. Negativa) | 242.47 | 198.41 | 29.45 | 28.76 | 0.09 | 0.195 |
| estatistico | STL + ETS | 223.33 | 174.02 | 26.93 | 23.84 | 0.08 | 0.212 |
| ml | Random Forest | 276.76 | 196.79 | 32.68 | 23.21 | 0.09 | 1.000 |
| estatistico | *Ensemble Estatísticos* | 162.50 | 127.74 | 19.25 | 18.75 | 0.06 | 0.630 |
| ml | *Ensemble Machine Learning* | 276.76 | 196.79 | 32.68 | 23.21 | 0.09 | 0.370 |
| global | **Ensemble global** | 160.36 | 115.83 | 19.50 | 15.88 | 0.05 | -- |

> Peso de um modelo = seu peso DENTRO da classe. Peso de um ensemble de classe = seu peso no ensemble global. MAPE só em semanas com casos observados >= 5. MASE < 1 = melhor que o ingênuo sazonal (lag 52).

<!-- METRICS:END -->

Detalhamento por passo de horizonte (`h1`–`h4`) em
[`model_metrics.csv`](model_metrics.csv).

---

## 8. Artefatos gerados

```
forecasting/
├── README-FORECAST.md          este arquivo
├── utils_forecast.R            camada comum (DuckDB, modelos, métricas, ensemble, cache)
├── train_models.R              pipeline offline (Rscript)
├── get_forecast.R              get_dengue_forecast() para o Shiny
├── model_metrics.csv           métricas por classe × modelo × horizonte  (gerado)
└── models/
    ├── ensemble_config.rds     pesos (por modelo e por classe) + métricas + params + fingerprint  (gerado)
    ├── agg_ride_df.rds         modelos `cacheavel` pré-ajustados na RIDE-DF inteira  (gerado)
    └── cuts_cache.rds          modelos `cacheavel` pré-ajustados por UF/município da base  (gerado)
```

`ensemble_config.rds` guarda a *impressão digital* do Parquet (bytes + nº de
linhas — `mtime` é registrado mas NÃO entra na comparação de validade, ver
§8.1). `get_dengue_forecast()` usa os modelos pré-ajustados de `agg_ride_df.rds`
(recorte RIDE-DF) ou `cuts_cache.rds` (UF/município) sempre que: (1) o
conteúdo do Parquet bate com o do treino, e (2) o corte de treino ao vivo
(`fc_corte_treino()`) tem o mesmo nº de semanas que o usado no treino
(`n_treino`, gravado no artefato). Fora disso, reajusta as *specs* ao recorte
na hora — ARIMA + SARIMA + GAM + RF levam, juntos, ≈ 1–3 s numa série de
~600 semanas (a busca sazonal do SARIMA pode eventualmente levar bem mais,
dependendo da carga da máquina — por isso vale manter o cache sempre quente,
não confiar no caminho ao vivo para ser rápido).

### 8.1 Por que o cache "não pegava" (bug encontrado e corrigido em 2026-09-18)

Auditoria de performance depois de o painel ter ficado perceptivelmente mais
lento com o forecasting. Dois problemas, não um:

1. **Fingerprint por `mtime`.** `all.equal()` comparava o `mtime` (data de
   modificação) do Parquet gravado no treino contra o `mtime` atual do
   arquivo. Qualquer coisa que *toque* o arquivo sem mudar o conteúdo (sync
   de nuvem/OneDrive, `git checkout`, copiar a pasta) muda o `mtime` e
   invalidava o cache silenciosamente — mesmo com o Parquet **byte a byte
   idêntico**. Corrigido: `fc_fingerprint_ok()` compara só `bytes` + `nrow`
   (conteúdo); `mtime` continua registrado, só não decide mais nada.
2. **Descompasso entre o que foi treinado e o que a chamada ao vivo pede.**
   Mais grave: mesmo com o fingerprint corrigido, a condição antiga
   (`nrow(d_fit) == nrow(d_all)`) comparava o corte de treino ao vivo contra
   a série **inteira**, não contra o corte que os modelos `cacheavel` REALMENTE
   usaram no treino. Como a regra "sempre recua `FC_SEMANAS_RECENTES` (4)
   semanas" (§4) tira 4 linhas de `d_fit` em operação normal, essa igualdade
   é falsa o tempo todo — na prática o cache da RIDE-DF nunca era reaproveitado
   desde que a regra de recuo foi introduzida, mesmo antes do SARIMA existir.
   Corrigido treinando os modelos `cacheavel` no MESMO corte (`fc_corte_treino()`,
   função única usada nos dois lugares) e comparando contra o `n_treino`
   gravado no artefato, não contra `nrow(d_all)`.
3. **Só a RIDE-DF tinha cache.** Município e UF sempre reajustavam tudo ao
   vivo — a maioria das visitas ao Painel, que segue o recorte hierárquico.
   `train_models.R` agora pré-ajusta os modelos `cacheavel` para **todos os
   recortes que a base realmente tem** (UFs e municípios via `DISTINCT` no
   Parquet — escopo dinâmico, nada hardcoded) e salva em `cuts_cache.rds`.

**Medido** (mesma máquina, antes/depois, série de ~600 semanas):

| Chamada | Antes (sem cache válido) | Depois (cache válido) |
|---|---:|---:|
| RIDE-DF | ~2,5 s (podia passar de 45 s conforme a carga da máquina) | ~0,6 s |
| Um município (1ª chamada do processo) | ~1,6–2,5 s | ~2,6 s\* |
| Um município (chamadas seguintes) | ~1,6–2,5 s | ~0,3 s |

\* `cuts_cache.rds` reúne os fits de todos os recortes (~47 MB, ver §8.2) — a
PRIMEIRA leitura em cada processo R paga a desserialização inteira; as
próximas usam a cópia em memória (`fc_ler_rds_memo()`, chave = caminho +
tamanho + `mtime` do `.rds`) e caem para ~0,3 s.

### 8.2 Custo de RAM do pré-aquecimento

`cuts_cache.rds` guarda 4 modelos × 37 recortes (3 UF + 34 municípios) ≈ 47 MB
em disco; fica em memória inteiro depois da primeira leitura (mesma lógica de
`GEO_DICT`/malhas em `global.R`). Some a `agg_ride_df.rds` (~1,8 MB) dá **~50 MB
por processo R** que atende forecasting. Em um deploy com vários workers
Shiny simultâneos, multiplique por worker. Se isso for um problema de memória
no servidor, dá para cortar: (a) reduzir quais modelos entram como
`cacheavel` (hoje: arima, sarima, gam, rf), ou (b) pré-aquecer só os
municípios/UFs mais visitados em vez de todos os 37.

### 8.3 Mantendo o cache quente

O cache só continua válido enquanto ninguém adiciona uma nova semana ao
Parquet (o corte de treino desliza 1 linha a cada atualização). **Rode
`Rscript forecasting/train_models.R` toda vez que o Parquet for atualizado**
(ideal: logo depois do ETL que gera `data/dengue_ride_df.parquet`, ou num
cron/task scheduler diário) — sem isso, o painel continua funcionando
normalmente (cai pro reajuste ao vivo, só mais lento), nunca quebra.

---

## 9. Uso

### Treinar / reavaliar

```bash
cd dengue_ride
Rscript forecasting/train_models.R
# opcional: Rscript forecasting/train_models.R --origens=30 --min_treino=208
```

Recria os `.rds`, reescreve `model_metrics.csv` e atualiza a tabela da seção 7.

### Chamar a previsão

```r
source("forecasting/get_forecast.R", local = FALSE)

# RIDE-DF inteira
fc <- get_dengue_forecast(NULL, data_atual = Sys.Date())

# município (geocódigo IBGE de 7 dígitos)
fc_bsb <- get_dengue_forecast(5300108, data_atual = as.Date("2025-06-15"))

# UF (uf_ibge de 2 dígitos)
fc_go <- get_dengue_forecast(52)
```

Retorno (`data.frame`):

| coluna | descrição |
|---|---|
| `Data_SE` | data de início da semana epidemiológica |
| `SE` | semana epidemiológica `AAAASS` |
| `Semana_Epi`, `Ano_Sanitario`, `Temporada` | rótulos do ano sanitário |
| `Tipo` | `"observado"` ou `"previsto"` |
| `Pop` | população do recorte na semana |
| `Casos_Observados` | casos notificados (`NA` no futuro) |
| `Casos_Nowcast` | `casos_est` do InfoDengue |
| `Casos_Previstos_Ensemble`, `Limite_Inferior`, `Limite_Superior` | ponto + IP 80% do **ensemble global** |
| `Prev_arima`, `Prev_sarima`, `Prev_gam`, `Prev_stlets`, `Prev_rf` | ponto de cada modelo (`NA` se não rodou no recorte) |
| `Prev_ens_<classe>`, `Lim_inf_<classe>`, `Lim_sup_<classe>` | ponto + IP 80% do ensemble de cada classe com modelo |

Atributos: `classes`, `rotulos`, `pesos`, `pesos_efetivos` (global + por classe),
`data_atual`, `horizon`, `modelo_cache`.

---

## 10. Integração no painel Shiny

`get_forecast.R` não sobe modelo nenhum no boot — só define a função. No
`server.R`, dentro de um `reactive` com `bindCache` pelo recorte:

```r
source("forecasting/get_forecast.R", local = FALSE)

forecast_recorte <- shiny::reactive({
  g   <- geo()
  loc <- switch(g$nivel,
    municipio = g$geocodigo,
    uf        = g$uf_ibge,
    NULL)                          # brasil -> RIDE-DF
  get_dengue_forecast(loc, data_atual = Sys.Date(), con = con)
}) |>
  shiny::bindCache(geo(), Sys.Date())
```

Passar `con` (a conexão DuckDB de `global.R`) evita abrir uma segunda conexão
por chamada. O `bindCache` segue o mesmo padrão da `serie_semanal`
compartilhada: uma inferência por recorte, reaproveitada enquanto o recorte não
muda.

### Integração já feita no painel

O `global.R` faz `source("forecasting/get_forecast.R")` dentro de um `tryCatch`
e expõe `FORECAST_DISPONIVEL`, `FORECAST_METRICS`, `FORECAST_CLASSES` e
`FORECAST_ROTULO()`. Se os pacotes ou os `.rds` faltarem, o painel sobe sem a
camada de previsão.

A camada vive no **Diagrama de Controle** (`R/mod_diagrama_controle.R`), com
painel de opções à direita (`layout_columns(col_widths = c(9, 3))` em `ui.R`):

- checkbox **"Mostrar linha de nowcast"** — incidência por `casos_est` como
  série **separada** (pontilhada), ao lado da linha de casos notificados;
- **checkboxes por classe** com RMSE/MAPE do backtest (de `model_metrics.csv`):
  os três ensembles (global + Estatísticos + Machine Learning) e cada modelo
  individual sob sua classe. "Ensemble global" marcado por padrão;
- cada linha selecionada convertida para incidência (`casos ÷ pop × 1e5`),
  desenhada como continuação **tracejada** ancorada no último ponto notificado,
  até a SE 26 (§ acima). Só aparece na temporada corrente;
- **faixa do IP 80%** = ensemble global, mas **limitada às primeiras
  `FC_HORIZON_DEFAULT` (4) semanas previstas** - o horizonte validado no
  backtest. Depois disso os preditores de GAM/RF já congelaram (§4) e o IP
  deixa de ser calibrado, só cresceria (chegava a cobrir toda a escala do
  gráfico, escondendo o canal endêmico e reduzindo a linha de previsão a um
  traço imperceptível). As linhas de PONTO continuam até a SE 26 normalmente,
  só a faixa sombreada é curta;
- eixo X mostra a **SE em cima e o ano-calendário embaixo** (SE 27–52 → ano de
  início da temporada; SE 1–26 → ano seguinte).

O `reactive` da previsão usa `bindCache(geo(), Sys.Date())` — trocar de
temporada, marcar/desmarcar linha ou alternar o nowcast **não** re-treina nada.

---

## 11. Limitações e próximos passos

- **Classe Mecanicistas vazia** — estrutura pronta (`FC_CLASSES`), falta plugar
  um SIR/SEIR em `FC_MODELOS` com `classe = "mecanicista"`.
- Ensemble global com pesos fixos por horizonte. Pesos por passo `h` ajudariam
  (o STL+ETS e o RF degradam mais rápido no h4).
- O ensemble global herda o erro da classe ML enquanto ela tiver só o RF (mais
  fraco que o ARIMA sozinho). Tende a melhorar quando entrarem mais modelos de
  ML, ou com um esquema de pesos mais agressivo (ex.: inverso do RMSE²).
- Sem covariável de clima **futuro**: horizonte 4 = defasagem 4 contorna isso,
  mas limita a extensão do horizonte.
- `casos_est` é lido mas ainda não entra como preditor nem como alvo corrigido.
- Calibração dos intervalos (cobertura empírica do IP 80%) ainda não medida.
- Validação aninhada para remover o otimismo das linhas `ensemble_*`.
