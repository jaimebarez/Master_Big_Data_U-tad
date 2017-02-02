install.packages("rvest")
install.packages("httr")

library("rvest")
library("httr")


# 
# Dado el nombre de un actor, obtiene su url de imdb
#
get_url_actor <- function(actor_name){
  imdb_url<-"http://www.imdb.com"
  peticion_http<-httr::GET(
    paste(imdb_url,"/find", sep = ""), 
    query = list(
      q = actor_name, 
      s="nm",
      exact="true",
      ref_="fn_al_nm_ex"
    )
  )
  url_actor<-(
    ( read_html(httr::content(peticion_http,"text")) 
      %>%
        html_nodes(".findSection .result_text a")
    )[1] 
    %>% html_attr("href")
  )
  return 
  paste(imdb_url,url_actor,sep = "")        
}

#
# Dado el nombre de un actor, obtiene su sus películas
#
get_films_actor <- function(actor_name){
  kb<- read_html(get_url_actor(actor_name))
  
  return (kb %>% 
            html_nodes(".filmo-category-section .filmo-row b a")) %>%
    html_text() 
}



#Esta llamada nos daría las películas en común entre estos dos actores
pelis_comun<-(intersect(get_films_actor("Verne Troyer"), get_films_actor("Mike Myers")))
print(pelis_comun)

#Url con los mejores actores, según imdb
best_actors_url<-"http://www.imdb.com/list/ls000004615/"

#Cargamos una lista con los nombres de los mejores actores
best_actors_ever<-read_html(best_actors_url) %>%html_nodes(".article .list .list_item .info b a") %>% html_text()

dos_mejores_actores = best_actors_ever[1:2]

#   PRUEBAS:
#
#aaa<-sapply(best_actors_ever[1:2], get_films_actor)
#typeof(aaa)



#a <- data.frame(matrix(, ncol = 2, nrow = 1))
#names(a)<-c("actor","pelis")
#c<-a

#for (i in best_actors_ever[1:2]){
#  b<-merge(i,aaa[i])
#  names(b)<-c("actor","pelis")
##  c<-rbind(a,b)
#}

#names(merge("Jack Nicholson",aaa["Jack Nicholson"]))