library(rvest)
library(dplyr)
library(data.table)
library(readxl)

urllinkmain <- "https://www.vivareal.com.br/aluguel/minas-gerais/uberlandia/bairros/santa-monica/#onde=Brasil,Minas%20Gerais,Uberl%C3%A2ndia,Bairros,Santa%20M%C3%B4nica,,,,BR%3EMinas%20Gerais%3ENULL%3EUberlandia%3EBarrios%3ESanta%20Monica,,,"

openCon <- urllinkmain %>% read_html

total_busca <- openCon %>%
  html_node("[class='results-summary__count js-total-records']") %>%
  html_text()

total_busca <- as.numeric(gsub("\\.", "", total_busca))

pag <- trunc(total_busca / 36)

DFLink <- vector()

for (i in 2:pag){
  urllinkpre=paste(strsplit(urllinkmain,"#onde")[[1]][1],"?",sep="")
  urllinkpost=strsplit(urllinkmain,"#onde")[[1]][2]
  pagina = paste("pagina=",i,"#onde")
  pagina = gsub("\\s","",pagina)
  novolink = paste(urllinkpre,pagina,urllinkpost, sep="")
  DFLink[i] <- novolink
}

DFLink[1] <- urllinkmain

DFtextos <- as.data.frame(matrix(ncol = 1,nrow=1))
colnames(DFtextos) <- c("name.f")
DFtextos$name.f <- as.character(DFtextos$name.f)
f <- 1

i = 1
for (i in 1:length(DFLink)){
  url <- DFLink[i]
  openCon <- url %>% read_html 
  name <- openCon %>%
    html_nodes("[class='property-card__content']") %>%
    html_text()
  for (f in 1:length(name)){
    newRow <- data.frame(name[f])
    names(newRow) <- names(DFtextos)
    DFtextos <- rbind(DFtextos,newRow)
  }
}

i = 1
metros2 <- vector()
quartos <- vector()
aluguel <- vector()
for (i in 2:nrow(DFtextos)){
  anunc_terms <- strsplit(DFtextos[i,], " ")
  metros2[i] <- anunc_terms[[1]][11]
  qt <- anunc_terms[[1]][7]
  quartos[i] <- ifelse(is.na(qt)|qt=="",1,qt)
  indice_mes <- which(anunc_terms[[1]] %in% "/Mês")
  if (length(anunc_terms[[1]]) >= indice_mes) {
    aluguel[i] <- anunc_terms[[1]][indice_mes - 1]
  } else {
    aluguel[i] <- NA  # ou algum outro valor padrão
  }
}

DFunico <- data.frame(metros2,quartos,aluguel)

write.csv(DFunico, "DFunico.csv")
