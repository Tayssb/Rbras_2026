# ==============================================================================
# SCRIPT PARA O PÔSTER: MODELAGEM COM SOMMER (GxA) E GRÁFICOS BIOECONÔMICOS
# ==============================================================================
# Carregar pacotes
library(tidyverse)
library(sommer) 
library(metR)  

## TEM QUE CORRIGIR: tem que ser calculado o custo de transporte para cada corte
## e depois somar os custos 

df_tah_estab_dist <- read_rds("dados_3/df_tah_estab_dist.rds") %>% as.data.frame()
rm(list = ls())

# ------------------------------------------------------------------------------
# 1. PREPARAÇÃO DOS DADOS E PARÂMETROS ECONÔMICOS
# ------------------------------------------------------------------------------
# Garantir que as variáveis do delineamento sejam fatores para o sommer
df_tah_estab_dist <- df_tah_estab_dist %>%
  mutate(across(c(CLONE, LOCAL, REP), as.factor))


# Parâmetros Econômicos 
val_atr <- 1.0816
custo_corte <- 28.53
custo_transbordo <- 10.83
fator_perda <- 1.04 # 4% de perda

# Custo Agrícola Fixo Anualizado
amortizacao_anual <- (13003.76 + 3317.39) / 5
custo_agri_C1 <- amortizacao_anual + 2216.43
custo_agri_C2 <- amortizacao_anual + 2216.43 + 3317.39
custo_agri_C3 <- amortizacao_anual + 2216.43 + 3317.39
custo_agri_medio <- mean(c(custo_agri_C1, custo_agri_C2, custo_agri_C3)) 

# ==============================================================================
# EXTRAÇÃO DE BLUPs NATIVA DO LME4 (À PROVA DE BALAS)
# ==============================================================================
library(lme4)
library(dplyr)

# 1. Ajustar os modelos mistos no lme4 (Caso ainda não tenha rodado)
mod_tah <- lmer(TAH_medio ~ LOCAL + (1|CLONE) + (1|CLONE:LOCAL) + (1|LOCAL:REP), data = df_tah_estab_dist)
mod_tch <- lmer(TCH_medio ~ LOCAL + (1|CLONE) + (1|CLONE:LOCAL) + (1|LOCAL:REP), data = df_tah_estab_dist)
mod_atr <- lmer(ATR_medio ~ LOCAL + (1|CLONE) + (1|CLONE:LOCAL) + (1|LOCAL:REP), data = df_tah_estab_dist)

# 2. Criar uma tabela (grid) apenas com as combinações únicas de CLONE e LOCAL
df_ajustado <- df_tah_estab_dist %>%
  dplyr::distinct(CLONE, LOCAL)

# Calcular os BLUPs forçando o lme4 a aceitar as parcelas que tinham 'NA' no ajuste
df_ajustado$TAH_ajust <- predict(mod_tah, newdata = df_ajustado, re.form = ~(1|CLONE) + (1|CLONE:LOCAL), allow.new.levels = TRUE)

df_ajustado$TCH_ajust <- predict(mod_tch, newdata = df_ajustado, re.form = ~(1|CLONE) + (1|CLONE:LOCAL), allow.new.levels = TRUE)

df_ajustado$ATR_ajust <- predict(mod_atr, newdata = df_ajustado, re.form = ~(1|CLONE) + (1|CLONE:LOCAL), allow.new.levels = TRUE)

str(df_tah_estab_dist)

# ==============================================================================
# 4. ADICIONAR DISTÂNCIAS E CALCULAR A MARGEM ECONÔMICA (MCA)
# ==============================================================================
# Extrair distâncias e nomes de usina do banco original
tabela_locais <- df_tah_estab_dist %>%
  group_by(LOCAL) %>%
  summarise(distancia_km = mean(distancia_km, na.rm = TRUE),
            nome_usina = first(nome_usina), .groups = "drop")

# Juntar ao df_ajustado
df_ajustado <- df_ajustado %>% left_join(tabela_locais, by = "LOCAL")

# Parâmetros econômicos
val_atr <- 1.0816
custo_corte <- 28.53
custo_transbordo <- 10.83
fator_perda <- 1.04 # 4% de perda
custo_agri_medio <- 7692.25 # (Já calculado nos passos iniciais)

# Calcular logística e ACM (MCA)
df_ajustado <- df_ajustado %>%
  mutate(
    custo_transporte = case_when(
      distancia_km <= 25  ~ 13.65,
      distancia_km <= 50  ~ 27.30,
      distancia_km <= 75  ~ 40.95,
      distancia_km <= 100 ~ 54.60,
      TRUE                ~ 54.60 + ((distancia_km - 100) * 0.50)
    ),
    Custo_Ton_CCT = custo_corte + custo_transbordo + custo_transporte,
    Receita = TCH_ajust * ATR_ajust * val_atr,
    Custo_CCT_Total = (TCH_ajust * Custo_Ton_CCT) * fator_perda,
    MCA = Receita - custo_agri_medio - Custo_CCT_Total
  )

# ==============================================================================
# FILTRAGEM: OS MESMOS 10 CLONES EM COMUM NOS 4 LOCAIS (SEM DISTÂNCIA)
# ==============================================================================
library(dplyr)
library(ggplot2)
library(tidytext) # Necessário para ordenar o eixo X por painel

df_tah_estab_dist |>
  filter(CLONE != "RB127825") |>
  droplevels() -> df_tah_estab_dist

# 1. Identificar os 4 locais com mais dados
top_4_locais <- df_tah_estab_dist %>%
  group_by(LOCAL) %>%
  summarise(n = n_distinct(CLONE)) %>%
  arrange(desc(n)) %>%
  slice_head(n = 4) %>%
  pull(LOCAL)

# 2. Encontrar os clones que existem em TODOS esses 4 locais (Intersection)
clones_comuns <- df_tah_estab_dist %>%
  filter(LOCAL %in% top_4_locais) %>%
  group_by(CLONE) %>%
  summarise(n_locais = n_distinct(LOCAL)) %>%
  filter(n_locais == 4) %>% 
  pull(CLONE)

# 3. Selecionar os 10 melhores dentro desse grupo comum (baseado no TAH geral)
top_10_comuns <- df_ajustado %>%
  filter(CLONE %in% clones_comuns, LOCAL %in% top_4_locais) %>%
  group_by(CLONE) %>%
  summarise(TAH_media = mean(TAH_ajust, na.rm = TRUE)) %>%
  arrange(desc(TAH_media)) %>%
  slice_head(n = 10) %>%
  pull(CLONE)

# 4. Preparar o banco para o plot (Removendo a distância e usando apenas o LOCAL)
df_grafico_final <- df_ajustado %>%
  filter(LOCAL %in% top_4_locais, CLONE %in% top_10_comuns) %>%
  mutate(
    Local_Label = LOCAL, # Apenas o nome da usina, sem distância
    # Ordena os clones do maior para o menor TAH DENTRO de cada local
    CLONE_ordenado = reorder_within(CLONE, -TAH_ajust, LOCAL)
  )

# ==============================================================================
# PLOT 1: BARRAS ORDENADAS POR LOCAL E SEM DISTÂNCIA
# ==============================================================================
plot_final_barras <- ggplot(df_grafico_final, aes(x = CLONE_ordenado, y = TAH_ajust, fill = CLONE)) +
  geom_col(color = "black", linewidth = 0.3) +
  geom_text(aes(label = sprintf("%.1f", TAH_ajust)), vjust = -0.5, size = 3.5, fontface = "bold") +
  coord_cartesian(ylim = c(0, max(df_grafico_final$TAH_ajust) * 1.15)) +
  facet_wrap(~ Local_Label, ncol = 2, scales = "free_x") + 
  scale_fill_viridis_d(option = "turbo") +
  scale_x_reordered() + 
  theme_bw(base_size = 14) +
  theme(
    legend.position = "bottom", # <--- AQUI: Mudamos de "none" para "bottom"
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    strip.background = element_rect(fill = "#2E8B57"),
    strip.text = element_text(color = "white", face = "bold", size = 12)
  ) +
  labs(
    x = "Genótipos", 
    y = "Tonelada de Açúcar por hectare (t/ha)",
    fill = "Genótipos" 
  )

print(plot_final_barras)

# ==============================================================================
# PLOT 2: ISOLINHAS FINANCEIRAS (MCA) COM AS DISTÂNCIAS NOS RÓTULOS
# ==============================================================================
library(metR) # Necessário para os rótulos das isolinhas

# 1. Preparar os pontos garantindo que a distância esteja no rótulo do painel
df_pontos_iso <- df_grafico_final %>%
  mutate(
    # Resgata a distância do banco original para garantir consistência
    distancia_km = sapply(LOCAL, function(loc) {
      mean(df_tah_estab_dist$distancia_km[df_tah_estab_dist$LOCAL == loc], na.rm = TRUE)
    }),
    Local_Label = paste0(LOCAL, " (", round(distancia_km, 1), " km)")
  )

# 2. Criar as malhas de relevo financeiro por usina (usando a distância correta)
grids_list <- lapply(unique(df_pontos_iso$Local_Label), function(loc_label) {
  
  # Extrai a distância numérica a partir do texto do rótulo ou do banco
  d_km <- mean(df_pontos_iso$distancia_km[df_pontos_iso$Local_Label == loc_label], na.rm = TRUE)
  
  c_transp <- case_when(
    d_km <= 25  ~ 13.65,
    d_km <= 50  ~ 27.30,
    d_km <= 75  ~ 40.95,
    d_km <= 100 ~ 54.60,
    TRUE        ~ 54.60 + ((d_km - 100) * 0.50)
  )
  c_cct <- custo_corte + custo_transbordo + c_transp
  
  # Grid de ATR e TCH baseado nos limites dos BLUPs daquele painel
  expand.grid(
    ATR_ajust = seq(min(df_pontos_iso$ATR_ajust[df_pontos_iso$Local_Label == loc_label])*0.95, 
                    max(df_pontos_iso$ATR_ajust[df_pontos_iso$Local_Label == loc_label])*1.05, length.out = 100),
    TCH_ajust = seq(min(df_pontos_iso$TCH_ajust[df_pontos_iso$Local_Label == loc_label])*0.95, 
                    max(df_pontos_iso$TCH_ajust[df_pontos_iso$Local_Label == loc_label])*1.05, length.out = 100),
    Local_Label = loc_label
  ) %>%
    mutate(MCA_Grid = (TCH_ajust * ATR_ajust * val_atr) - custo_agri_medio - (TCH_ajust * c_cct * fator_perda))
})

df_grid_isolinhas <- bind_rows(grids_list)

# ==============================================================================
# PLOT 2: ISOLINHAS FINANCEIRAS (MCA) COM RÓTULOS INTELIGENTES (SEM EMPELOTAMENTO)
# ==============================================================================
library(metR)
library(ggrepel) # Pacote essencial para afastar os textos automaticamente

plot_isoquanta <- ggplot() +
  # Camada de relevo (isolinhas da MCA)
  geom_contour(data = df_grid_isolinhas, aes(x = ATR_ajust, y = TCH_ajust, z = MCA_Grid, color = after_stat(level)),
               linewidth = 0.8, linetype = "dashed") +
  scale_color_viridis_c(option = "magma", name = "Margem (R$/ha)") + 
  
  # Rótulos financeiros nas linhas
  geom_text_contour(data = df_grid_isolinhas, aes(x = ATR_ajust, y = TCH_ajust, z = MCA_Grid), 
                    stroke = 0.2, skip = 1, size = 3) +
  
  # Pontos reais preditos (BLUPs de ATR e TCH)
  geom_point(data = df_pontos_iso, aes(x = ATR_ajust, y = TCH_ajust, fill = CLONE), 
             shape = 21, size = 3.5, color = "black") +
  scale_fill_viridis_d(option = "turbo") +
  
  # SUBSTUIÇÃO DO GEOM_TEXT COMUM PELO GEOM_TEXT_REPEL:
  geom_text_repel(data = df_pontos_iso, 
                  aes(x = ATR_ajust, y = TCH_ajust, label = CLONE),
                  size = 3.2, 
                  fontface = "bold",
                  box.padding = 0.3,      
                  point.padding = 0.2,    
                  max.overlaps = Inf,     
                  segment.color = "grey50", 
                  segment.linewidth = 0.4) +
  
  facet_wrap(~ Local_Label, scales = "free") +
  theme_bw(base_size = 14) +
  theme(
    strip.background = element_rect(fill = "#2E8B57"),
    strip.text = element_text(color = "white", face = "bold", size = 12),
    legend.position = "bottom" # <--- Ativado para exibir as legendas no rodapé
  ) +
  labs(
    x = "Açúcar Total Recuperável (kg/t)",
    y = "Tonelada de Cana por Hectare (t/ha)",
    fill = "Genótipos Elite" # <--- Título para a legenda das bolinhas
  )

print(plot_isoquanta)


# ==============================================================================
# PLOT 3: GRÁFICO DE BARRAS DE LUCRO (MCA) - RANQUEADO POR MARGEM EM CADA LOCAL
# ==============================================================================
library(dplyr)
library(ggplot2)
library(tidytext) # Essencial para ordenar o eixo X de forma independente por painel

# 1. Preparar os dados da elite comum, ordenando do maior para o menor pela MCA
df_grafico_mca <- df_grafico_final %>%
  mutate(
    Local_Label = paste0(LOCAL, " (", round(distancia_km, 1), " km)"),
    # A MÁGICA DO RANQUEAMENTO: Ordena os clones do maior para o menor MCA DENTRO de cada local
    CLONE_ordenado_mca = reorder_within(CLONE, -MCA, Local_Label)
  )

# 2. Gerar o Gráfico de Barras de Lucro
plot_mca_barras <- ggplot(df_grafico_mca, aes(x = CLONE_ordenado_mca, y = MCA, fill = CLONE)) +
  geom_col(color = "black", linewidth = 0.3) +
  
  # Adiciona o valor exato da MCA em formato de moeda em cima da barra
  geom_text(aes(label = scales::label_dollar(prefix = "R$ ", big.mark = ".", decimal.mark = ",", accuracy = 1)(MCA)), 
            vjust = -0.5, size = 3.2, fontface = "bold", color = "black") +
  
  # Dá um "Zoom" no eixo Y para realçar as diferenças de lucro entre os clones
  coord_cartesian(ylim = c(min(df_grafico_mca$MCA) * 0.85, 
                           max(df_grafico_mca$MCA) * 1.15)) +
  
  # scales = "free_x" é obrigatório para o reorder_within funcionar por painel
  facet_wrap(~ Local_Label, scales = "free_x", ncol = 2) +
  
  # Mantém exatamente a mesma paleta "turbo" para preservar a cor de cada clone
  scale_fill_viridis_d(option = "turbo") + 
  scale_x_reordered() + # Limpa os sufixos técnicos do reorder_within no eixo X
  
  theme_bw(base_size = 14) +
  theme(
    legend.position = "none", 
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    strip.background = element_rect(fill = "#2E8B57"), # Verde-escuro padrão do pôster
    strip.text = element_text(color = "white", face = "bold", size = 12),
    plot.title = element_text(margin = margin(b = 10)) 
  ) +
  labs(
    title = "Ranqueamento Econômico dos Genótipos (MCA)",
    subtitle = "Margem de Contribuição Agronômica (R$/ha) considerando a distância logística",
    x = "Genótipos (Ordenados por Lucratividade Local)",
    y = "Margem de Contribuição Agronômica (R$/ha)"
  )

print(plot_mca_barras)
