# ==============================================================================
# R-BRAS 2026 — AVALIAÇÃO BIOECONÔMICA E G×A DE GENÓTIPOS ELITE DE CANA
# Script revisado (versão para o pôster)
# ==============================================================================
# Principais mudanças em relação ao teste_resumoRbras.R:
#  1. rm(list = ls()) foi para o início (antes ele apagava os dados recém-lidos).
#  2. Análise restrita a UMA série de ensaios (PLANTIO == 2019). O banco tem 4
#     séries (2018, 2019, 2020, 2021) com conjuntos de clones quase disjuntos;
#     juntar tudo em LOCAL misturava ensaios e blocos diferentes (REP 1 de 2018
#     e REP 1 de 2019 viravam o mesmo bloco).
#  3. A distância é a do ENSAIO (cada série/local tem uma área diferente), e não
#     a média do local entre anos.
#  4. Os 4 locais são escolhidos pela amplitude de distância (2 a 38 km),
#     não pelo número de clones.
#  5. Teste de razão de verossimilhança (LRT) para a interação G×A e
#     componentes de variância — sustentam a frase "interação G×A expressiva".
#  6. Uma única função de MCA, usada em todos os gráficos (antes a fórmula
#     estava repetida em três lugares).
#  7. Novo gráfico 4: MCA × distância simulada (0–150 km), que mostra o ponto
#     em que um clone deixa de ser viável conforme a usina fica mais longe.
#  8. Cores fixas por clone em todos os gráficos e figuras salvas em PNG.
# ==============================================================================

rm(list = ls())

library(tidyverse)
library(lme4)
library(tidytext)  # reorder_within() / scale_x_reordered()
library(metR)      # geom_text_contour()
library(ggrepel)   # geom_text_repel()

df_tah_estab_dist <- read_rds("dados_Rbras/df_tah_estab_dist.rds") %>% as.data.frame()

dir.create("saidas_poster", showWarnings = FALSE)

# ------------------------------------------------------------------------------
# 0. ESCOLHA DA SÉRIE E DOS LOCAIS
# ------------------------------------------------------------------------------
# Tabela de apoio: cada ensaio (LOCAL × PLANTIO) com distância, nº de clones e
# cortes disponíveis. Serve para justificar a escolha.
tab_ensaios <- df_tah_estab_dist %>%
  group_by(LOCAL, PLANTIO) %>%
  summarise(
    usina        = first(nome_usina),
    distancia_km = first(distancia_km),
    n_clones     = n_distinct(CLONE),
    cortes       = paste(c("C1", "C2", "C3")[c(any(!is.na(TCH_C1)),
                                               any(!is.na(TCH_C2)),
                                               any(!is.na(TCH_C3)))],
                         collapse = "+"),
    .groups = "drop"
  ) %>%
  arrange(PLANTIO, distancia_km)

print(tab_ensaios, n = Inf)

# Série 2019: 8 locais, 19 clones presentes em todos eles, distâncias de 2 a 38 km.
serie <- 2019

# 4 locais com distâncias bem espalhadas (uma por "faixa"):
#   USV  ~  2 km (mais perto)
#   UBE  ~  9 km
#   UVG  ~ 19 km
#   ULC  ~ 38 km (mais longe; única da série na 2ª faixa de frete junto com AGR)
# Alternativa para o mais distante: AGR (~32 km).
locais_poster <- c("USV", "UBE", "UVG", "ULC")

dados <- df_tah_estab_dist %>%
  filter(PLANTIO == serie, LOCAL %in% locais_poster) %>%
  mutate(across(c(CLONE, LOCAL, REP), as.factor)) %>%
  droplevels()

# ------------------------------------------------------------------------------
# 1. PARÂMETROS ECONÔMICOS
# ------------------------------------------------------------------------------
val_atr          <- 1.0816   # R$/kg de ATR
custo_corte      <- 28.53    # R$/t
custo_transbordo <- 10.83    # R$/t
perda_campo      <- 0.04     # 4% da cana perdida no campo durante a colheita

# Custo agrícola anualizado (formação amortizada em 5 anos + tratos)
# PENDENTE: confirmar a fonte dos valores. 3317.39 entra na amortização e nos
# tratos de C2/C3. Como esse custo é igual para todos os clones, ele só desloca
# a MCA (não muda o ranqueamento dentro do local).
amortizacao_anual <- (13003.76 + 3317.39) / 5
custo_agri_C1     <- amortizacao_anual + 2216.43
custo_agri_C2     <- amortizacao_anual + 2216.43 + 3317.39
custo_agri_C3     <- amortizacao_anual + 2216.43 + 3317.39
custo_agri_medio  <- mean(c(custo_agri_C1, custo_agri_C2, custo_agri_C3))  # 7692.25

# Frete (R$/t) pela tabela de faixas de distância
frete_ton <- function(d_km) {
  case_when(
    d_km <= 25  ~ 13.65,
    d_km <= 50  ~ 27.30,
    d_km <= 75  ~ 40.95,
    d_km <= 100 ~ 54.60,
    TRUE        ~ 54.60 + (d_km - 100) * 0.50
  )
}

# Margem de Contribuição Agronômica (R$/ha)
# Obs.: como o custo de CCT é linear em TCH, calcular por corte e tirar a média
# dá o mesmo resultado que usar o TCH médio. Por isso a nota antiga
# "calcular o transporte por corte e somar" não muda o resultado.
# Perda de campo (4%): só a cana efetivamente entregue na usina gera receita e
# paga CCT (corte, transbordo e frete são cobrados por tonelada entregue).
mca_fun <- function(TCH, ATR, d_km) {
  TCH_entregue <- TCH * (1 - perda_campo)
  receita   <- TCH_entregue * ATR * val_atr
  custo_cct <- TCH_entregue * (custo_corte + custo_transbordo + frete_ton(d_km))
  receita - custo_agri_medio - custo_cct
}

# ------------------------------------------------------------------------------
# 2. MODELOS MISTOS (REML/BLUP) E TESTE DA INTERAÇÃO G×A
# ------------------------------------------------------------------------------
# y = LOCAL (fixo) + CLONE + CLONE:LOCAL + bloco dentro de local + erro
ajusta_modelo <- function(var) {
  f_comp <- as.formula(paste(var, "~ LOCAL + (1|CLONE) + (1|CLONE:LOCAL) + (1|LOCAL:REP)"))
  f_red  <- as.formula(paste(var, "~ LOCAL + (1|CLONE) + (1|LOCAL:REP)"))
  m  <- lmer(f_comp, data = dados, REML = TRUE)
  m0 <- lmer(f_red,  data = dados, REML = TRUE)
  lrt <- anova(m0, m, refit = FALSE)   # LRT com REML (mesma parte fixa)

  vc  <- as.data.frame(VarCorr(m))
  s2g  <- vc$vcov[vc$grp == "CLONE"]
  s2ga <- vc$vcov[vc$grp == "CLONE:LOCAL"]
  s2e  <- vc$vcov[vc$grp == "Residual"]
  nl <- n_distinct(dados$LOCAL); nr <- n_distinct(dados$REP)

  list(
    modelo = m,
    resumo = tibble(
      variavel = var,
      s2_gen   = s2g,
      s2_GxA   = s2ga,
      s2_res   = s2e,
      h2_media = s2g / (s2g + s2ga / nl + s2e / (nl * nr)),  # herdabilidade na média de clones
      r2_GxA   = s2ga / (s2g + s2ga),                        # peso da G×A na variância genotípica
      LRT_GxA  = lrt$Chisq[2],
      p_GxA    = lrt$`Pr(>Chisq)`[2] / 2  # /2: teste na fronteira do espaço paramétrico
    )
  )
}

aj_tch <- ajusta_modelo("TCH_medio")
aj_atr <- ajusta_modelo("ATR_medio")
aj_tah <- ajusta_modelo("TAH_medio")

tab_variancia <- bind_rows(aj_tch$resumo, aj_atr$resumo, aj_tah$resumo)
print(tab_variancia)
write_csv(tab_variancia, "saidas_poster/tabela_componentes_variancia.csv")

# Valores preditos por clone em cada local (efeito fixo de LOCAL + BLUP de
# CLONE + BLUP de CLONE:LOCAL)
df_ajustado <- dados %>% distinct(CLONE, LOCAL)
re_gen <- ~ (1 | CLONE) + (1 | CLONE:LOCAL)
df_ajustado$TCH_ajust <- predict(aj_tch$modelo, newdata = df_ajustado, re.form = re_gen, allow.new.levels = TRUE)
df_ajustado$ATR_ajust <- predict(aj_atr$modelo, newdata = df_ajustado, re.form = re_gen, allow.new.levels = TRUE)
df_ajustado$TAH_ajust <- predict(aj_tah$modelo, newdata = df_ajustado, re.form = re_gen, allow.new.levels = TRUE)

# ------------------------------------------------------------------------------
# 3. DISTÂNCIAS E MCA
# ------------------------------------------------------------------------------
tabela_locais <- dados %>%
  group_by(LOCAL) %>%
  summarise(distancia_km = first(distancia_km),
            nome_usina   = first(nome_usina), .groups = "drop") %>%
  arrange(distancia_km) %>%
  mutate(Local_Label = paste0(LOCAL, " (", sub(".", ",", sprintf("%.1f", distancia_km), fixed = TRUE), " km)"),
         Local_Label = factor(Local_Label, levels = Local_Label))

df_ajustado <- df_ajustado %>%
  left_join(tabela_locais, by = "LOCAL") %>%
  mutate(
    frete_R_t = frete_ton(distancia_km),
    MCA       = mca_fun(TCH_ajust, ATR_ajust, distancia_km)
  )

# ------------------------------------------------------------------------------
# 4. SELEÇÃO DOS 10 GENÓTIPOS ELITE (presentes nos 4 locais, maior TAH médio)
# ------------------------------------------------------------------------------
clones_comuns <- dados %>%
  group_by(CLONE) %>%
  summarise(n_locais = n_distinct(LOCAL)) %>%
  filter(n_locais == length(locais_poster)) %>%
  pull(CLONE)

top_10 <- df_ajustado %>%
  filter(CLONE %in% clones_comuns) %>%
  group_by(CLONE) %>%
  summarise(TAH_media = mean(TAH_ajust)) %>%
  arrange(desc(TAH_media)) %>%
  slice_head(n = 10) %>%
  pull(CLONE) %>%
  as.character()

df_grafico <- df_ajustado %>%
  filter(CLONE %in% top_10) %>%
  mutate(CLONE = factor(CLONE, levels = top_10)) %>%
  group_by(LOCAL) %>%
  mutate(rank_TCH = rank(-TCH_ajust),
         rank_ATR = rank(-ATR_ajust),
         rank_TAH = rank(-TAH_ajust),
         rank_MCA = rank(-MCA)) %>%
  ungroup()

# Tabela de ranqueamentos (é ela que mostra: maior TCH/TAH ≠ maior MCA)
tab_ranks <- df_grafico %>%
  arrange(distancia_km, rank_MCA) %>%
  select(LOCAL, distancia_km, CLONE, TCH_ajust, ATR_ajust, TAH_ajust, MCA,
         rank_TCH, rank_TAH, rank_MCA)
print(tab_ranks, n = Inf)
write_csv(tab_ranks, "saidas_poster/tabela_ranks_TCH_TAH_MCA.csv")

# Paleta fixa por clone (mesma cor em todos os gráficos)
cores_clones <- setNames(viridisLite::turbo(length(top_10), begin = 0.05, end = 0.95), top_10)

tema_poster <- theme_bw(base_size = 14) +
  theme(
    strip.background = element_rect(fill = "#2E8B57"),
    strip.text       = element_text(color = "white", face = "bold", size = 12),
    legend.position  = "bottom"
  )

# ==============================================================================
# PLOT 1: TAH POR LOCAL (ordenado dentro de cada local)
# ==============================================================================
plot_tah <- df_grafico %>%
  mutate(CLONE_ord = reorder_within(CLONE, -TAH_ajust, Local_Label)) %>%
  ggplot(aes(x = CLONE_ord, y = TAH_ajust, fill = CLONE)) +
  geom_col(color = "black", linewidth = 0.3) +
  geom_text(aes(label = sub(".", ",", sprintf("%.1f", TAH_ajust), fixed = TRUE)), vjust = -0.5, size = 3.2, fontface = "bold") +
  facet_wrap(~ Local_Label, ncol = 2, scales = "free") +
  scale_x_reordered() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  scale_fill_manual(values = cores_clones) +
  tema_poster +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold")) +
  labs(x = "Genótipos", y = "TAH predito (t de açúcar/ha)", fill = "Genótipos")

print(plot_tah)

# ==============================================================================
# PLOT 2: ISOLINHAS DE MCA NO PLANO ATR × TCH
# ==============================================================================
grid_iso <- df_grafico %>%
  group_by(Local_Label, distancia_km) %>%
  reframe(expand.grid(
    ATR_ajust = seq(min(ATR_ajust) * 0.95, max(ATR_ajust) * 1.05, length.out = 100),
    TCH_ajust = seq(min(TCH_ajust) * 0.95, max(TCH_ajust) * 1.05, length.out = 100)
  )) %>%
  mutate(MCA_Grid = mca_fun(TCH_ajust, ATR_ajust, distancia_km))

plot_iso <- ggplot() +
  geom_contour(data = grid_iso,
               aes(x = ATR_ajust, y = TCH_ajust, z = MCA_Grid, color = after_stat(level)),
               linewidth = 0.7, linetype = "dashed") +
  geom_text_contour(data = grid_iso, aes(x = ATR_ajust, y = TCH_ajust, z = MCA_Grid),
                    stroke = 0.2, skip = 1, size = 3) +
  # MCA = 0 (ponto de equilíbrio), se cair dentro da janela do painel
  geom_contour(data = grid_iso, aes(x = ATR_ajust, y = TCH_ajust, z = MCA_Grid),
               breaks = 0, color = "red3", linewidth = 1.1) +
  scale_color_viridis_c(option = "magma", end = 0.85, guide = "none") +  # valores já aparecem nas linhas
  geom_point(data = df_grafico, aes(x = ATR_ajust, y = TCH_ajust, fill = CLONE),
             shape = 21, size = 3.5, color = "black") +
  geom_text_repel(data = df_grafico, aes(x = ATR_ajust, y = TCH_ajust, label = CLONE),
                  size = 3, fontface = "bold", box.padding = 0.3, point.padding = 0.2,
                  max.overlaps = Inf, segment.color = "grey50", segment.size = 0.4) +
  scale_fill_manual(values = cores_clones, guide = "none") +
  facet_wrap(~ Local_Label, scales = "free") +
  tema_poster +
  labs(x = "ATR predito (kg/t)", y = "TCH predito (t/ha)")

print(plot_iso)

# ==============================================================================
# PLOT 3: MCA POR LOCAL (ranqueado dentro de cada local)
# ==============================================================================
rotulo_real <- scales::label_dollar(prefix = "R$ ", big.mark = ".", decimal.mark = ",", accuracy = 1)

plot_mca <- df_grafico %>%
  mutate(CLONE_ord = reorder_within(CLONE, -MCA, Local_Label)) %>%
  ggplot(aes(x = CLONE_ord, y = MCA, fill = CLONE)) +
  geom_col(color = "black", linewidth = 0.3) +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  # Rótulo em mil R$ e na vertical, para não sobrepor quando os valores são próximos
  geom_text(aes(label = sub(".", ",", sprintf("%.1f", MCA / 1000), fixed = TRUE),
                hjust = ifelse(MCA >= 0, -0.15, 1.15)),
            angle = 90, size = 3.2, fontface = "bold") +
  facet_wrap(~ Local_Label, scales = "free", ncol = 2) +
  scale_x_reordered() +
  scale_y_continuous(expand = expansion(mult = c(0.08, 0.22)), labels = rotulo_real) +
  scale_fill_manual(values = cores_clones) +
  tema_poster +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
        legend.position = "none") +
  labs(x = "Genótipos (ordenados pela MCA no local)",
       y = "MCA (R$/ha)",
       caption = "Valores sobre as barras em mil R$/ha")

print(plot_mca)

# ==============================================================================
# PLOT 4: MCA × DISTÂNCIA SIMULADA (análise de sensibilidade)
# ==============================================================================
# Mantém os valores preditos de cada clone em cada local e varia apenas a
# distância até a usina. A linha vertical marca a distância real do ensaio.
dist_sim <- seq(0, 150, by = 1)

df_sim <- df_grafico %>%
  select(LOCAL, Local_Label, distancia_km, CLONE, TCH_ajust, ATR_ajust) %>%
  crossing(dist_sim = dist_sim) %>%
  mutate(MCA_sim = mca_fun(TCH_ajust, ATR_ajust, dist_sim))

# Distância a partir da qual cada clone tem MCA negativa em cada local
dist_inviavel <- df_sim %>%
  filter(MCA_sim < 0) %>%
  group_by(LOCAL, CLONE) %>%
  summarise(dist_MCA_negativa = min(dist_sim), .groups = "drop") %>%
  arrange(LOCAL, dist_MCA_negativa)
print(dist_inviavel, n = Inf)
write_csv(dist_inviavel, "saidas_poster/tabela_distancia_inviabilidade.csv")

plot_dist <- ggplot(df_sim, aes(x = dist_sim, y = MCA_sim, color = CLONE)) +
  geom_hline(yintercept = 0, linewidth = 0.5, color = "red3") +
  geom_vline(data = tabela_locais, aes(xintercept = distancia_km),
             linetype = "dashed", color = "grey30") +
  geom_line(linewidth = 0.9) +
  facet_wrap(~ Local_Label, scales = "free_y", ncol = 2) +
  scale_color_manual(values = cores_clones) +
  scale_y_continuous(labels = rotulo_real) +
  tema_poster +
  guides(color = guide_legend(nrow = 2)) +
  labs(x = "Distância até a usina (km) — linha tracejada = distância real",
       y = "MCA (R$/ha)", color = "Genótipos")

print(plot_dist)

# ------------------------------------------------------------------------------
# 5. SALVAR FIGURAS (300 dpi)
# ------------------------------------------------------------------------------
ggsave("saidas_poster/fig1_TAH.png",        plot_tah,  width = 10, height = 8, dpi = 300)
ggsave("saidas_poster/fig2_isolinhas.png",  plot_iso,  width = 11, height = 9, dpi = 300)
ggsave("saidas_poster/fig3_MCA.png",        plot_mca,  width = 10, height = 8, dpi = 300)
ggsave("saidas_poster/fig4_MCA_distancia.png", plot_dist, width = 10, height = 8, dpi = 300)
