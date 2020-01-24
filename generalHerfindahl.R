#' Calculate the Herfindahl index and competition for non-MEDSL data
#' 
#' This function takes the long datasets for other election information and computes the Herfindahl index measure of competition so that 
#' one can compare it to the raw precinct results 
#' @param data The long dataframe with the election results of interest
#' @param office_field The name of the office/race field that the results will be summarized by.
#' @param district_field The name of the district field that when combined with the office will provide the ID by which to summarise the 
#' results by
#' @param vote_field The name of the field for vote totals.  
#' @param cand_field The name of the field for candidate, which when combined with the office and other fields, should provide the atomic unit
#' by which to square the proportions of.
#' @param other_fields The name of the other identifying fields (i.e. district, state, county, etc.) by which the user can score levels of 
#' competition to. Can list up to 4. Will be included in the group_by command as well
#' @return The dataframe with the competition scores by race for the provided dataset, and the following fields: 
#'     \itemize{
#'     
#' }
#'  \itemize{
#' }
#' @export
#' 

generalHerfindahl <- function(data,office_field, district_field, vote_field, cand_field, other_fields){
  if(missing(office_field)==TRUE){
    stop("Office Field not provided. Cannot continue")
  }else if(missing(district_field)==T){
    stop("District field not provided. Cannot continue")
  }else if(missing(vote_field)==T){
    stop("Vote field not provided. Cannot continue.")
  }else if(missing(other_fields)==T){
    print("Other fields not provided. Continuing")
  }
  cand_position <- match(cand_field, names(data))
  office_position <- match(office_field, names(data))
  vote_position <- match(vote_field, names(data))
  dist_position <- match(district_field, names(data))
###possible to run with just the position information
  if(missing(other_fields)==FALSE){
    other_id_posiitons <- match(other_fields, names(data))
    storage_other <- list()
    for(j in 1:length(other_id_posiitons)){
      storage_other[[j]] <- as.symbol(colnames(data)[other_id_posiitons[j]])
    }
    if(length(other_id_posiitons)==1){
      var1 <- storage_other[[1]]
      data <- data %>% group_by(office_sym, dist_sym, var1) %>% mutate(total_vote=sum(vote_sym, na.rm=T))
      data <- data %>% group_by(office_sym, dist_sym, var1) %>% arrange(-vote_sym)
      data$vote_prop2 <- (data[,vote_position]/data$total_vote)^2 
      data_herfindahl <- data %>% group_by(office_sym, dist_sym, var1) %>% summarise(herf_score=sum(vote_prop2, na.rm=T),
                                                                                     max_vote=first(vote_sym),
                                                                                     max_cand=first(cand_sym))
    }else if(length(other_id_posiitons)==2){
      var1 <- storage_other[[1]]
      var2 <- storage_other[[2]]
      data <- data %>% group_by(office_sym, dist_sym, var1, var2) %>% mutate(total_vote=sum(vote_sym, na.rm=T))
      data <- data %>% group_by(office_sym, dist_sym, var1) %>% arrange(-vote_sym)
      data$vote_prop2 <- (data[,vote_position]/data$total_vote)^2 
      data_herfindahl <- data %>% group_by(office_sym, dist_sym, var1,var2) %>% summarise(herf_score=sum(vote_prop2, na.rm=T),
                                                                                     max_vote=first(vote_sym),
                                                                                     max_cand=first(cand_sym))
    }else if(length(other_id_posiitons)==3){
      var1 <- storage_other[[1]]
      var2 <- storage_other[[2]]
      var3 <- storage_other[[3]]
      data <- data %>% group_by(office_sym, dist_sym, var1, var2, var3) %>% mutate(total_vote=sum(vote_sym, na.rm=T))
      data <- data %>% group_by(office_sym, dist_sym, var1,var2,var3) %>% arrange(-vote_sym)
      data$vote_prop2 <- (data[,vote_position]/data$total_vote)^2 
      data_herfindahl <- data %>% group_by(office_sym, dist_sym, var1,var2,var3) %>% summarise(herf_score=sum(vote_prop2, na.rm=T),
                                                                                   max_vote=first(vote_sym),
                                                                                   max_cand=first(cand_sym))
    }else if(length(other_id_posiitons)==4){
      var1 <- storage_other[[1]]
      var2 <- storage_other[[2]]
      var3 <- storage_other[[3]]
      var4 <- storage_other[[4]]
      data <- data %>% group_by(office_sym, dist_sym, var1, var2, var3, var4) %>% mutate(total_vote=sum(vote_sym, na.rm=T))
      data <- data %>% group_by(office_sym, dist_sym, var1,var2, var3, var4) %>% arrange(-vote_sym)
      data$vote_prop2 <- (data[,vote_position]/data$total_vote)^2 
      data_herfindahl <- data %>% group_by(office_sym, dist_sym, var1,var2, var3, var4) %>% summarise(herf_score=sum(vote_prop2, na.rm=T),
                                                                                   max_vote=first(vote_sym),
                                                                                   max_cand=first(cand_sym))
    }else if(length(other_id_posiitons)>4){
      stop("Too many other field IDs provided.")
  }
  }else{
    data <- data %>% group_by(office_sym, dist_sym) %>% mutate(total_vote=sum(vote_sym, na.rm=T))
    data <- data %>% group_by(office_sym, dist_sym) %>% arrange(-vote_sym)
    data$vote_prop2 <- (data[,vote_position]/data$total_vote)^2 
    data_herfindahl <- data %>% group_by(office_sym, dist_sym) %>% summarise(herf_score=sum(vote_prop2, na.rm=T),
                                                                                                    max_vote=first(vote_sym),
                                                                                                    max_cand=first(cand_sym))
    }###note: with the match command, I can pull out the columns of interest with the raw output without the need to change
  data_herfindahl$competition <- 1 - data_herfindahl$herf_score

  return(data_herfindahl)
}


df1 <- data %>%
  group_by_at(vars(office_position,dist_position)) %>%
  summarize(Value = mean(totalvotes))
