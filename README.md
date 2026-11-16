# Custo logístico muda a seleção de clones de cana-de-açúcar

Material do pôster apresentado na **70ª RBras (2026)**: script, dados, resultados e pôster.

**Autores:** Tays Silva e Luiz Alexandre Peternelli, Universidade Federal de Viçosa (UFV)
**Contato:** tays.batista@ufv.br

## Resumo

A seleção de clones de cana-de-açúcar costuma priorizar a produtividade (TCH e TAH) e deixa de lado o custo de levar a cana até a usina. Neste trabalho, 10 genótipos elite foram avaliados em 4 usinas, a distâncias de 2 a 38 km, com valores preditos por modelos mistos (REML/BLUP). A partir deles calculou-se a Margem de Contribuição Agrícola (MCA, R$/ha), que desconta os custos agrícolas e de corte, carregamento e transporte (CCT).

**Principal resultado:** o clone de maior TAH nem sempre é o mais lucrativo. Na Usina Uberaba (UBE), o líder em TAH caiu para 7º lugar em MCA.

## Estrutura

```
R_bras_2026/
├── poster_Rbras_2026.pptx            pôster final
├── resumo_poster/                    resumo submetido (.docx e .pdf) e QR code do pôster
├── versoes_anteriores/               versões anteriores do resumo
└── script_Rbras_2026/
    ├── teste_resumoRbras_revisado.R  script da análise (versão usada no pôster)
    ├── teste_resumoRbras.R           versão anterior do script
    ├── script_Rbras_2026.Rproj       projeto do RStudio
    ├── dados_Rbras/                  dados dos ensaios
    └── saidas_poster/                tabelas e figuras geradas pelo script
```

## Como reproduzir

1. Abra `script_Rbras_2026/script_Rbras_2026.Rproj` no RStudio.
2. Instale os pacotes: `install.packages(c("tidyverse", "lme4", "tidytext", "metR", "ggrepel"))`.
3. Rode `teste_resumoRbras_revisado.R`. As tabelas e figuras são gravadas em `saidas_poster/`.

## Análise

- **Dados:** série de ensaios plantada em 2019; usinas USV (2,1 km), UBE (9,2 km), UVG (18,7 km) e ULC (37,8 km); 4 repetições.
- **Modelo:** y = Xβ + Zu + ε, com local como efeito fixo e clone, clone × local e bloco dentro de local como efeitos aleatórios (`lme4`). A interação G×A foi testada pela razão de verossimilhança.
- **Margem:** MCA = TCH(1 − p)(ATR·P − CCT) − C, em que p = 4% de perda no campo; P = R$ 1,0816/kg de ATR; CCT = R$ 39,36/t mais frete por faixa de 25 km; C = R$ 7.692/ha (custo agrícola médio anual).
- **Distância:** a real de cada ensaio (Tabela 1 do pôster) e uma simulada de 0 a 150 km (Figura 2 do pôster).

## Referências

- Bates, D., Mächler, M., Bolker, B., & Walker, S. (2015). Fitting linear mixed-effects models using lme4. *Journal of Statistical Software*, 67(1), 1–48.
- Amorim, F. R. D., Patino, M. T. O., & Santos, D. F. L. (2021). Soil tillage and sugarcane planting: an assessment of cost and economic viability. *Scientia Agricola*, 79(1), e20190317.

## Agradecimentos

CNPq, CAPES, FAPEMIG e FINEP.
