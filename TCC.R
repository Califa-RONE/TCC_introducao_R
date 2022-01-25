#Trabalho de conclusão do módulo introdutório, Marcelo Pariz Jr

#o trabalho consiste em analisar uma base de dados de uma empresa que contem dados sobre as vendas
#efetuadas no mês de Junho, printar e salvar um Data Frame com os dados.

#a análiuse produz resultados diversos, como: contagem de itens orçados de cada vendedor, itens vendidos,
#dentre os itens que não foram vendidos, calcular a porcentagem que possuia saldo positivo em estoque etc.

#instalando os pacotes utilizados

install.packages(c("readr","dplyr"))

#habilitando os pacotes

library(dplyr)
library(readr)
library(readxl)

#importando base de dados pelo "import Dataset"

base_de_dados_orcamentos <- read_excel("orcamentos.xlsx")

#iniciando variáveis

vendedores <- unique(base_de_dados_orcamentos$profissional_interno)
itens_orcados_vendedor <- NULL
itens_orcados_vendedor[1:length(vendedores)] <- 0
itens_orcados_vendidos_vendedor <- NULL
itens_orcados_vendidos_vendedor[1:length(vendedores)] <- 0
itens_saldo_positivo <- NULL
itens_saldo_positivo[1:length(vendedores)] <- 0
vetor_contagem_itens_orcados_vendedor <- NULL
vetor_contagem_itens_saldo_positivo <- NULL
vetor_contagem_itens_vendidos <- NULL
porcentagens_saldo_positivo <-  NULL
porcentagens_saldo_positivo[1:length(vendedores)] <- 0
valor_total_vendido_vendedor <- NULL
valor_total_vendido_vendedor[1:length(vendedores)] <- 0
vetor_calculo_total_vendido_vendedor <- NULL
  
clientes <- unique(base_de_dados_orcamentos$cliente)
itens_orcados_cliente <- NULL
itens_orcados_cliente[1:length(clientes)] <- 0
vetor_contagem_itens_orcados_cliente <-  NULL

fornecedores <- unique(base_de_dados_orcamentos$fornecedor)
itens_orcados_fornecedor <- NULL
itens_orcados_fornecedor[1:length(fornecedores)] <- 0 
vetor_contagem_itens_orcados_fornecedor <- NULL

#criando função de contagem de itens orçados

contagem_itens_orcados_vendedor <- function(profissional){
  for(y in 1:length(vendedores)){
   if(profissional == vendedores[y]){
     for(x in 1:length(base_de_dados_orcamentos$n_orcamento)){
       if(base_de_dados_orcamentos[x,"profissional_interno"] == vendedores[y]){
         itens_orcados_vendedor[y] <- itens_orcados_vendedor[y] + 1
       }
     }
     return(itens_orcados_vendedor[y])
   }
  }
}

#Criando função de contagem de itens vendidos

contagem_itens_vendidos <- function(vendedor){
for(b in 1:length(vendedores)){
  if(vendedores[b] == vendedor){
    for(c in 1:length(base_de_dados_orcamentos$n_orcamento))
    {
      if(base_de_dados_orcamentos[c,"profissional_interno"] == vendedores[b]){
        if(base_de_dados_orcamentos[c,"qnt_orcado"] > base_de_dados_orcamentos[c,"qnt_saldo"]){
           itens_orcados_vendidos_vendedor[b] <- itens_orcados_vendidos_vendedor[b] + 1   
        }
        }
    }
    return(itens_orcados_vendidos_vendedor[b])
    }
  }
}

#calculando valor total em $ vendido por cada vendedor

calculo_valor_vendido <- function(autor){
  for (v9 in 1:length(vendedores)) {
    if(autor == vendedores[v9]){
      for (v10 in 1:length(base_de_dados_orcamentos$n_orcamento)) {
        if(base_de_dados_orcamentos[v10,"profissional_interno"] == vendedores[v9]){
          valor_total_vendido_vendedor[v9] <- base_de_dados_orcamentos[v10,"preco_unit"] + valor_total_vendido_vendedor[v9]
        }
      }
      return(valor_total_vendido_vendedor[v9]) 
    }
  }
}

#criando função de contagem de itens que não foram vendidos, mas que tinham saldo em estoque

contagem_itens_saldo_positivo <- function(colaborador){
  for(x1 in 1:length(vendedores)){
    if(vendedores[x1] == colaborador){
      for(y1 in 1:length(base_de_dados_orcamentos$n_orcamento)){
        if(base_de_dados_orcamentos[y1,"profissional_interno"] == vendedores[x1]){
          if(base_de_dados_orcamentos[y1,"qnt_orcado"] == base_de_dados_orcamentos[y1,"qnt_saldo"]){
           if(base_de_dados_orcamentos[y1,"saldo1"] > 0 | base_de_dados_orcamentos[y1,"saldo2"] > 0){
             itens_saldo_positivo[x1] <- itens_saldo_positivo[x1] + 1  
           }   
          }
        }
      }
      return(itens_saldo_positivo[x1])
    }
    
  }
}

#criando a função para contagem de itens orçados por cliente

contagem_itens_orcados_cliente <- function(cliente_teste){
  for (c1 in 1:length(clientes)) {
      if(cliente_teste == clientes[c1]) 
      {
        for(c2 in 1:length(base_de_dados_orcamentos$cliente)){
          if(base_de_dados_orcamentos[c2,"cliente"] == cliente_teste){
            itens_orcados_cliente[c1] <- itens_orcados_cliente[c1] + 1
          }
        }
        return(itens_orcados_cliente[c1])
      }
  }
}

#criando função para contagem de itens orçados por fornecedor

contagem_itens_orcados_fornecedor <- function(fornecedor){
  for(f1 in 1:length(fornecedores)){
    if(fornecedor == fornecedores[f1]){
      for(f2 in 1:length(base_de_dados_orcamentos$n_orcamento)){
        if(base_de_dados_orcamentos[f2,"fornecedor"] == fornecedor){
          itens_orcados_fornecedor[f1] <- itens_orcados_fornecedor[f1] + 1 
        }
      }
      return(itens_orcados_fornecedor[f1])
    }
  }
}
  
#Contando a quantidade de itens orçados por vendedor, quantos foram vendidos e dos que não foram vendidos quantos tinham saldo positivo em estoque

for(z in 1:length(vendedores)){
  vetor_contagem_itens_orcados_vendedor[z] <- contagem_itens_orcados_vendedor(vendedores[z])
  vetor_contagem_itens_vendidos[z] <- contagem_itens_vendidos(vendedores[z])
  vetor_contagem_itens_saldo_positivo[z] <- contagem_itens_saldo_positivo(vendedores[z])
  vetor_calculo_total_vendido_vendedor[z] <- calculo_valor_vendido(vendedores[z])
}

for(x2 in 1:length(vendedores)){
  porcentagens_saldo_positivo[x2] <- ((vetor_contagem_itens_saldo_positivo[x2]*100) / (vetor_contagem_itens_orcados_vendedor[x2] - vetor_contagem_itens_vendidos[x2]))
}

#vendedor que mais orçou no mês de Junho

vendedor_mais_orcou <- vendedores[match(max(vetor_contagem_itens_orcados_vendedor), vetor_contagem_itens_orcados_vendedor)]
media_itens_orcados_dia <- vetor_contagem_itens_orcados_vendedor / 22
vendedor_mais_vendeu <- vendedores[match(max(vetor_contagem_itens_vendidos), vetor_contagem_itens_vendidos)]
vendedor_mais_vendeu_valor <- max(vetor_contagem_itens_vendidos)

#contando a quantidade de itens orcados por cliente

for(z1 in 1:length(clientes)){
  vetor_contagem_itens_orcados_cliente[z1] <- contagem_itens_orcados_cliente(clientes[z1])
}

#cliente que mais orçou no mês de Junho

cliente_mais_orcou <- clientes[match(max(vetor_contagem_itens_orcados_cliente) , vetor_contagem_itens_orcados_cliente)]
cliente_mais_orcou_valor <- max(vetor_contagem_itens_orcados_cliente)

#contando a quantidade de itens cotados por fornecedor

for(f3 in 1:length(fornecedores)){
  vetor_contagem_itens_orcados_fornecedor[f3] <- contagem_itens_orcados_fornecedor(fornecedores[f3])
}

#fornecedor mais cotado

fornecedor_mais_cotado <- fornecedores[match(max(vetor_contagem_itens_orcados_fornecedor), vetor_contagem_itens_orcados_fornecedor)]
fornecedor_mais_cotado_valor <- max(vetor_contagem_itens_orcados_fornecedor)

#Exibindo informações na tela

for (ex in 1:length(vendedores)) {
  print(paste("O vendedor ",vendedores[ex]," orçou um total de ",vetor_contagem_itens_orcados_vendedor[ex]," itens no mês de Junho. Desses itens, ",vetor_contagem_itens_vendidos[ex]," foram vendidos. Dos itens que não foram vendidos, ",porcentagens_saldo_positivo[ex],"% possuiam saldo positivo em estoque."))
}

print(paste("O vendedor que mais cotou itens no mês de Junho foi: ",vendedor_mais_orcou," e conseguiu vender um total de: ",vendedor_mais_vendeu_valor," itens."))

print(paste("O cliente que mais cotou em Junho foi: ",cliente_mais_orcou,". Cotando um total de: ",cliente_mais_orcou_valor," itens."))

print(paste("O fornecedor mais cotado em JUnho foi: ",fornecedor_mais_cotado_valor,". Sendo cotado: ",fornecedor_mais_cotado_valor," vezes."))

#criando Data Frame com os dados obtidos e salvando

resultados_obtidos_TCC <- data.frame(vendedores, vetor_contagem_itens_orcados_vendedor, vetor_contagem_itens_vendidos, vetor_contagem_itens_saldo_positivo, porcentagens_saldo_positivo)

write_excel_csv2(resultados_obtidos_TCC)