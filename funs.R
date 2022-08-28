# functions

# data_take_longitudinal_cases <- function(df){
#   #take longitudinal cases
#   out <- df%>%
#     dplyr::filter(YearMeasured==1)%>%
#     dplyr::group_by(Id) %>% filter(n() > 1)%>%
#     dplyr::filter(n() !=0)%>%
#     dplyr::ungroup(Id)%>%
#     dplyr::mutate(PoliticalConservativism = Pol.Orient)
#   return(out)
# }


# show_unique_id <- function(df,y){
#   id =  paste0(y) # name of Id variable
#   numb <- length(unique(df$id)) # count # of ids
#   print(numb)
# }
