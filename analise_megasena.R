#declaração colunas de bolas sorteadas
sorteio_01 = resultados_megasena$V3
sorteio_02 = resultados_megasena$V4
sorteio_03 = resultados_megasena$V5
sorteio_04 = resultados_megasena$V6
sorteio_05 = resultados_megasena$V7
sorteio_06 = resultados_megasena$V8

#aquisição de quantidade de sorteios por dezena individual em tabelas
quantidade_numeros_sorteados_01 = table(sorteio_01)
quantidade_numeros_sorteados_02 = table(sorteio_02)
quantidade_numeros_sorteados_03 = table(sorteio_03)
quantidade_numeros_sorteados_04 = table(sorteio_04)
quantidade_numeros_sorteados_05 = table(sorteio_05)
quantidade_numeros_sorteados_06 = table(sorteio_06)

#impressão resultados individuais
plot(quantidade_numeros_sorteados_01)
plot(quantidade_numeros_sorteados_02)
plot(quantidade_numeros_sorteados_03)
plot(quantidade_numeros_sorteados_04)
plot(quantidade_numeros_sorteados_05)
plot(quantidade_numeros_sorteados_06)

#união de todas as colunas com numeros de sorteios
todos = cbind(resultados_megasena$V3,resultados_megasena$V4,resultados_megasena$V5,resultados_megasena$V6,resultados_megasena$V7,resultados_megasena$V8)

#separação de todas as colunas por valor individual
tabela = table(todos)

#impressão tabela com valores mais sorteados
plot(tabela, main = "Gráfico Número Sorteados",ylab = "Quantidade",xlab = "Números Sorteados", ylim = c(0,240), col = "blue")

#Primeiro mais Sorteado
sort(c(tabela), decreasing = TRUE)[1]

#Segundo mais Sorteado
sort(c(tabela),decreasing = TRUE)[2]

#Terceiro mais Sorteado
sort(c(tabela),decreasing = TRUE)[3]

#Quarto mais Sorteado
sort(c(tabela),decreasing = TRUE)[4]

#Quinto mais Sorteado
sort(c(tabela),decreasing = TRUE)[5]

#Sexto mais Sorteado
sort(c(tabela),decreasing = TRUE)[6]

#script para verificação de presença de 3 numeros distintos em todas as linhas
X <- 2 #Numero 1 
Y <- 33 #Numero 2
Z <- 55 #Numero 3
c <- 0 #zera contador auxiliar
for(val in resultados_megasena$V1){
  if((sorteio_01[val]==X || sorteio_02[val]==X || sorteio_03[val]==X || sorteio_04[val]==X || sorteio_05[val]==X || sorteio_06[val]==X)
     && (sorteio_01[val]==Y || sorteio_02[val]==Y || sorteio_03[val]==Y || sorteio_04[val]==Y || sorteio_05[val]==Y || sorteio_06[val]==Y)
    && (sorteio_01[val]==Z || sorteio_02[val]==Z || sorteio_03[val]==Z || sorteio_04[val]==Z || sorteio_05[val]==Z || sorteio_06[val]==Z)){
    c = c+1
  }
}
#quantidade de vezes que sairam os 3 números
print(c)


#Sequencia numeros para alimentação script de verificação
x<-60
y<-59
z<-58
count<-0
for(x in 60:1){
  for(y in 59:1){
    for(z in 58:1){
      
      X <- x #valor 1 
      Y <- y #valor 2
      Z <- z #valor 3
      c <- 0 #zera contador auxiliar
      
      for(val in resultados_megasena$V1){
        if(X != Y && X != Z && Z != Y){
          if((sorteio_01[val]==X || sorteio_02[val]==X || sorteio_03[val]==X || sorteio_04[val]==X || sorteio_05[val]==X || sorteio_06[val]==X)
             && (sorteio_01[val]==Y || sorteio_02[val]==Y || sorteio_03[val]==Y || sorteio_04[val]==Y || sorteio_05[val]==Y || sorteio_06[val]==Y)
            && (sorteio_01[val]==Z || sorteio_02[val]==Z || sorteio_03[val]==Z || sorteio_04[val]==Z || sorteio_05[val]==Z || sorteio_06[val]==Z)){
            c = c+1
          }
        }
      }
      count = count + 1
      if(z < y && y < x){
        print(rbind(x,y,z))
        #quantidade vezes
        resultado = cbind(x,y,z,c)
        write.table(resultado , file="resultados.txt" , append = TRUE, row.names = FALSE,col.names = FALSE,sep = ",")
      }
      }
  }
}

library(plyr)
#comando para ordenar a lista conforme V4 (numero de vezes da combinação de numeros)
resultados_lista_ordenada <- list(arrange(resultados,V4))

#escreve em arquivo os resultados ordenados
write.table(resultados_lista_ordenada , file="resultados_ordenados.txt" , append = TRUE, row.names = FALSE,col.names = FALSE,sep = ",")

#Sequencia numeros para alimentação script de verificação
x<-60
y<-59
z<-58
r<-57
s<-56
t<-55
count<-0

for(x in 60:1){
  for(y in 59:1){
    for(z in 58:1){
      for(r in 57:1){
        for(s in 56:1){
          for(t in 55:1){
            
            X <- x #valor 1 
            Y <- y #valor 2
            Z <- z #valor 3
            R <- r #valor 3
            S <- s #valor 3
            T <- t #valor 3
            c <- 0 #zera contador auxiliar
            
            for(val in resultados_megasena$V1){
              if(X != Y && X != Z && Z != Y){
                if((    sorteio_01[val]==X || sorteio_02[val]==X || sorteio_03[val]==X || sorteio_04[val]==X || sorteio_05[val]==X || sorteio_06[val]==X)
                   && (sorteio_01[val]==Y || sorteio_02[val]==Y || sorteio_03[val]==Y || sorteio_04[val]==Y || sorteio_05[val]==Y || sorteio_06[val]==Y)
                   && (sorteio_01[val]==Z || sorteio_02[val]==Z || sorteio_03[val]==Z || sorteio_04[val]==Z || sorteio_05[val]==Z || sorteio_06[val]==Z)
                   && (sorteio_01[val]==R || sorteio_02[val]==R || sorteio_03[val]==R || sorteio_04[val]==R || sorteio_05[val]==R || sorteio_06[val]==R)
                   && (sorteio_01[val]==S || sorteio_02[val]==S || sorteio_03[val]==S || sorteio_04[val]==S || sorteio_05[val]==S || sorteio_06[val]==S)
                   && (sorteio_01[val]==T || sorteio_02[val]==T || sorteio_03[val]==T || sorteio_04[val]==T || sorteio_05[val]==T || sorteio_06[val]==T)
                ){
                  c = c+1
                }
              }
            }
            count = count + 1
            if(t<s && s<r && r<z && z < y && y < x){
              print(rbind(x,y,z,r,s,t))
              #quantidade vezes
              resultado = cbind(x,y,z,r,s,t,c)
              write.table(resultado , file="resultados_6_dezenas.txt" , append = TRUE, row.names = FALSE,col.names = FALSE,sep = ",")
            }
          }
        }
      }
    }
  }
}
