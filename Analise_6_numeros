#Sequencia numeros para alimentação 3 numeros script de verificaÃ§Ã£o
x<-60
y<-59
z<-58
a<-57
b<-56
c<-55
count<-0
for(x in 60:1){
  for(y in 59:1){
    for(z in 58:1){
      for(x in 57:1){
        for(y in 56:1){
          for(z in 55:1){
            
            X <- x #valor 1 
            Y <- y #valor 2
            Z <- z #valor 3
            A <- a #valor 1 
            B <- b #valor 2
            C <- c #valor 3
            c <- 0 #zera contador auxiliar
            
            for(val in resultados_megasena$V1){
              if(X != Y && X != Z && Z != Y){
                if((	 sorteio_01[val]==X || sorteio_02[val]==X || sorteio_03[val]==X || sorteio_04[val]==X || sorteio_05[val]==X || sorteio_06[val]==X)
                   && (sorteio_01[val]==Y || sorteio_02[val]==Y || sorteio_03[val]==Y || sorteio_04[val]==Y || sorteio_05[val]==Y || sorteio_06[val]==Y)
                   && (sorteio_01[val]==Z || sorteio_02[val]==Z || sorteio_03[val]==Z || sorteio_04[val]==Z || sorteio_05[val]==Z || sorteio_06[val]==Z)
                   && (sorteio_01[val]==A || sorteio_02[val]==A || sorteio_03[val]==A || sorteio_04[val]==A || sorteio_05[val]==A || sorteio_06[val]==A)
                   && (sorteio_01[val]==B || sorteio_02[val]==B || sorteio_03[val]==B || sorteio_04[val]==B || sorteio_05[val]==B || sorteio_06[val]==B)
                   && (sorteio_01[val]==C || sorteio_02[val]==C || sorteio_03[val]==C || sorteio_04[val]==C || sorteio_05[val]==C || sorteio_06[val]==C)
                ){
                  c = c+1
                }
              }
            }
            count = count + 1
            if(z < y && y < x){
              print(rbind(x,y,z))
              #quantidade vezes
              resultado = cbind(x,y,z,c)
              write.table(resultado , file="resultados_6_numeros.txt" , append = TRUE, row.names = FALSE,col.names = FALSE,sep = ",")
            }
          }
        }
      }
    }
  }
}
