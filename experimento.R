# Instalar e carregar pacotes necessários
if (!require("ExpDE")) install.packages("ExpDE", dependencies = TRUE)
if (!require("smoof")) install.packages("smoof", dependencies = TRUE)

library(ExpDE)
library(smoof)

# Gerar 28 valores uniformemente espaçados entre 2 e 150
dimensoes <- seq(2, 150, length.out = 28)
# Arredondar para os valores inteiros mais próximos
dimensoes <- round(dimensoes)
print(dimensoes)

# Número de repetições por dimensao
repeticoes_por_dimensao <- read.csv("repeticoes.csv")
repeticoes_por_dimensao <- as.data.frame(repeticoes_por_dimensao)

# Função para gerar funções de Rosenbrock para diferentes dimensões
fn <- function(X){
  if(!is.matrix(X)) X <- matrix(X, nrow = 1) # <- if a single vector is passed as X
  Y <- apply(X, MARGIN = 1,
             FUN = smoof::makeRosenbrockFunction(dimensions = dim))
  return(Y)
}

## Config 1 - C
recpars1 <- list(name = "recombination_blxAlphaBeta", alpha = 0, beta = 0)
mutpars1 <- list(name = "mutation_rand", f = 4)

## Config 2 - D
recpars2 <- list(name = "recombination_eigen", othername = "recombination_bin", cr = 0.9)
mutpars2 <- list(name = "mutation_best", f = 2.8)


# Tabela para armazenar resultados
resultados <- data.frame(
  Dimensao = integer(),
  Repeticao = integer(),
  Configuracao = character(),
  Fbest = numeric()
)

iniExecucao<-Sys.time()
message("Hora de inicio da execucao: ",iniExecucao)

# Executar o experimento
for (dim in dimensoes) {
  
  # Numero de repeticoes para a dimensao atual
  repeticoes <- repeticoes_por_dimensao$Repeticoes[repeticoes_por_dimensao$Dimensao == dim]
    
  for (rep in 1:repeticoes) {
    iniRep<-Sys.time()
    
    message("Dimensao atual: ",dim, " Repeticao: ",rep)
    
    # Gerar problema para a dimensão
    selpars <- list(name = "selection_standard")
    stopcrit <- list(names = "stop_maxeval", maxevals = 5000 * dim, maxiter = 100 * dim)
    probpars <- list(name = "fn", xmin = rep(-5, dim), xmax = rep(10, dim))
    popsize = 5 * dim
    
    # Configuração 1
    out1 <- ExpDE(
      mutpars = mutpars1,
      recpars = recpars1,
      popsize = popsize,
      selpars = list(name = "selection_standard"),
      stopcrit = stopcrit,
      probpars = probpars,
      showpars = list(show.iters = "none")
    )
    
    # Configuração 2
    out2 <- ExpDE(
      mutpars = mutpars2,
      recpars = recpars2,
      popsize = popsize,
      selpars = list(name = "selection_standard"),
      stopcrit = stopcrit,
      probpars = probpars,
      showpars = list(show.iters = "none")
    )
    
    # Armazenar resultados
    resultados <- rbind(
      resultados,
      data.frame(Dimensao = dim, Repeticao = rep, Configuracao = "Config1", Fbest = out1$Fbest),
      data.frame(Dimensao = dim, Repeticao = rep, Configuracao = "Config2", Fbest = out2$Fbest)
    )
    
    fimRep<-Sys.time()
    message("Dim: ", dim, " rep: ", rep, " Duracao: ", difftime(fimRep,iniRep, units = c("mins")), " min")
    
  }
}
fimExecucao<-Sys.time()
message("Hora de fim da execucao: ",fimExecucao,
        " \nDuracao: ", difftime(fimExecucao,iniExecucao, units = c("mins")), " min")



# Salvar resultados em um arquivo CSV
write.csv(resultados, "experimento.csv", row.names = FALSE)
