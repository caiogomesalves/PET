library(stringr)

soma_digi <- function(x) {
    x <- as.character(x)
    sep <- unlist(strsplit(x, ""))
    sep <- as.numeric(sep)
    soma <- sum(sep)
    if (str_length(soma) == 1) {
        return(soma)
    }
    else {
        return(soma_digi(soma))
    }
}

somas <- numeric(9999)
for (i in 1:9999) {
    soma_i <- soma_digi(i)
    if (str_length(soma_i) == 1) {
        somas[i] <- soma_i
    }
    else {
        soma_i <- soma_digi(soma_i)
    }
}
#-----------------------------------------------------------------------
#                                                       Caio Gomes Alves
#                                              github.com/caiogomesalves
#                                                caio.gomesalves@ufpr.br
#                        Programa de Educação Tutorial (PET-Estatística)
#                Department of Statistics · Federal University of Paraná
#                                       2023-out-24 · Curitiba/PR/Brazil
#-----------------------------------------------------------------------
library(readxl)
library(highcharter)

dados_denny <- read_xlsx("Base.xlsx")


# Gráfico simples:
table(dados_denny$Hora_abordagem) %>%
    as.data.frame() %>%
    hchart(
        type = "spline",
        hcaes(y = Freq),
        dataLabels = list(enabled = T)
    )

# Gráfico mais completo:
tabela <- table(dados_denny$Hora_abordagem) %>%
    as.data.frame()

highchart() %>%
    hc_xAxis(categories = tabela$Var1) %>%
    hc_add_series(
        data = tabela$Freq,
        type = "areaspline",
        name = "Frequência",
        dataLabels = list(enabled = T)
    ) %>%
    hc_add_theme(hc_theme_alone())
