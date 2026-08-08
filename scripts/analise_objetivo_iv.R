library(dplyr)
library(tidyr)
library(here)

# ---------------------------------------------------------------------
# Este script consolida resultados de 3 scripts anteriores para responder
# diretamente ao objetivo (iv): dentro do CNAE 26, Desenvolvimento de
# projetos se aproxima de Administração ou de Produção -- em ADOÇÃO,
# em GRAU DE UTILIZAÇÃO, e em termos de posição estatística (heterogeneidade)?
# ---------------------------------------------------------------------

if (!exists("assimetria_larga")) {
  source(here("scripts", "analise_assimetria.R"))
}
if (!exists("grau_utilizacao_larga")) {
  source(here("scripts", "analise_grau_utilizacao.R"))
}
if (!exists("cnae26_posicao")) {
  source(here("scripts", "analise_heterogeneidade.R"))
}

# ---------------------------------------------------------------------
# 1) ADOÇÃO: % de firmas de CNAE 26 que adotaram a tecnologia, por função
#    (já calculado em analise_assimetria.R)
# ---------------------------------------------------------------------

adocao_cnae26 <- assimetria_larga %>%
  filter(recorte == "cnae_26") %>%
  select(tecnologia, administracao, projeto, producao) %>%
  rename(adocao_administracao = administracao,
         adocao_projeto = projeto,
         adocao_producao = producao)

# ---------------------------------------------------------------------
# 2) GRAU DE UTILIZAÇÃO: % de uso abrangente ENTRE ADOTANTES de CNAE 26,
#    por função (já calculado em analise_grau_utilizacao.R)
# ---------------------------------------------------------------------

grau_cnae26 <- grau_utilizacao_larga %>%
  filter(recorte == "cnae_26") %>%
  select(tecnologia, administracao, projeto, producao) %>%
  rename(abrangente_administracao = administracao,
         abrangente_projeto = projeto,
         abrangente_producao = producao)

# ---------------------------------------------------------------------
# 3) POSIÇÃO ESTATÍSTICA: onde CNAE 26 cai na distribuição das 24
#    divisões, para os pares Admin-Projeto e Projeto-Produção
#    (já calculado em analise_heterogeneidade.R)
# ---------------------------------------------------------------------

posicao_cnae26 <- cnae26_posicao %>%
  filter(par %in% c("admin_projeto", "projeto_producao")) %>%
  select(tecnologia, par, asimetria_pp, z_score, percentil, aviso_cv) %>%
  pivot_wider(
    names_from = par,
    values_from = c(asimetria_pp, z_score, percentil, aviso_cv),
    names_glue = "{par}_{.value}"
  )

# ---------------------------------------------------------------------
# Consolidado final: uma linha por tecnologia, com adoção + grau de
# utilização + posição estatística lado a lado
# ---------------------------------------------------------------------

objetivo_iv_consolidado <- adocao_cnae26 %>%
  left_join(grau_cnae26, by = "tecnologia") %>%
  left_join(posicao_cnae26, by = "tecnologia") %>%
  mutate(
    # diagnóstico direto: em cada dimensão, Projeto está mais perto de
    # Administração ou de Produção?
    adocao_projeto_mais_perto_de = if_else(
      abs(adocao_projeto - adocao_administracao) < abs(adocao_projeto - adocao_producao),
      "Administração", "Produção"
    ),
    abrangente_projeto_mais_perto_de = if_else(
      abs(abrangente_projeto - abrangente_administracao) < abs(abrangente_projeto - abrangente_producao),
      "Administração", "Produção"
    )
  )

print(
  objetivo_iv_consolidado %>%
    select(tecnologia, adocao_administracao, adocao_projeto, adocao_producao,
           adocao_projeto_mais_perto_de),
  n = Inf
)

print(
  objetivo_iv_consolidado %>%
    select(tecnologia, abrangente_administracao, abrangente_projeto, abrangente_producao,
           abrangente_projeto_mais_perto_de),
  n = Inf
)

print(
  objetivo_iv_consolidado %>%
    select(tecnologia, projeto_producao_z_score, projeto_producao_percentil, projeto_producao_aviso_cv,
           admin_projeto_z_score, admin_projeto_percentil, admin_projeto_aviso_cv),
  n = Inf
)

write.csv(
  objetivo_iv_consolidado,
  here("data", "processed", "objetivo_iv_consolidado.csv"),
  row.names = FALSE, fileEncoding = "UTF-8"
)

# ejemplo de uso: quantas tecnologias mostram Projeto mais perto de Administração
# (tanto em adoção quanto em grau de utilização), com posição estatística confiável
# objetivo_iv_consolidado %>%
#   filter(adocao_projeto_mais_perto_de == "Administração",
#          abrangente_projeto_mais_perto_de == "Administração",
#          projeto_producao_aviso_cv == "OK")