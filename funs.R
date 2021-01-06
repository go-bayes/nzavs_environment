# functions

data_read <- function(){
  df <-as.data.frame(readRDS("/Users/jbul716/The\ Virtues\ Project\ Dropbox/Joseph\ Bulbulia/00Bulbulia\ Pubs/2020/ldf.5"))
  return(df)
}

data_take_longitudinal_cases <- function(df){
  #take longitudinal cases
  out <- df%>%
    dplyr::filter(YearMeasured==1)%>%
    dplyr::group_by(Id) %>% filter(n() > 1)%>%
    dplyr::filter(n() !=0)%>%
    dplyr::ungroup(Id)
  return(out)
}


# show_unique_id <- function(df,y){
#   id =  paste0(y) # name of Id variable
#   numb <- length(unique(df$id)) # count # of ids
#   print(numb)
# }
