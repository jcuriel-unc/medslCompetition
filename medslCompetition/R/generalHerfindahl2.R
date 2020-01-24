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
#' competition to. Will be included in the group_by command.
#' @return The dataframe with the competition scores by race for the provided dataset, and the following fields: 
#'     \itemize{
#'     \item herf_score = The herfindahl index score of votes, where scores of one approximate elections where the vote share went entirely to
#'     one candidate, and scores approaching zero fragmented between numerous candidates
#'     \item max_vote = The vote total for the candidate with the most votes for a given election, i.e. the winning total.
#'     \item max_cand = The candidate with the highest vote total for a given election, i.e. the winner. 
#'     \item competition = The reverse herfindahl index such that higher scores equate to greater competition, and scores closer to zero
#'     less competition. 
#'     
#' }
#' @export
#' @examples 
#' house_all <- read.csv("1976-2018-house.csv")
#' test_house<-generalHerfindahl(house_all, office_field = "office", district_field = "district", vote_field = 'candidatevotes', 
#' cand_field = "candidate", other_fields = c("state_fips", "year", "stage") )
#' 

generalHerfindahl <- function(data,office_field, district_field, vote_field, cand_field, other_fields){
  data <- as.data.frame(data)
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
  cols_group <- c(office_position,dist_position)
  data$votes <- as.numeric(data[,vote_position])
  data$candidate_name <- data[,cand_position]
###possible to run with just the position information
  if(missing(other_fields)==FALSE){
    other_id_posiitons <- match(other_fields, names(data))
    print(other_id_posiitons)
    cols_ext <- c(cols_group,other_id_posiitons)
    print(cols_ext)
    data <- data %>% group_by_at(vars(cols_ext)) %>% mutate(total_vote=sum(votes,na.rm=T))
    data <- data %>% group_by_at(vars(cols_ext)) %>% arrange(-votes)
    data$vote_prop2 <- (data$votes/data$total_vote)^2 
    data_herfindahl <- data %>% group_by_at(vars(cols_ext)) %>% summarise(herf_score=sum(vote_prop2, na.rm=T),
                                                                                     max_vote=first(votes),
                                                                                     max_cand=first(candidate_name))

  }else{
    data <- data %>% group_by_at(vars(cols_group)) %>% mutate(total_vote=sum(votes,na.rm=T))
    data <- data %>% group_by_at(vars(cols_group)) %>% arrange(-votes)
    data$vote_prop2 <- (data$votes/data$total_vote)^2 
    data_herfindahl <- data %>% group_by_at(vars(cols_group)) %>% summarise(herf_score=sum(vote_prop2, na.rm=T),
                                                                            max_vote=first(votes),
                                                                            max_cand=first(candidate_name))
    }###note: with the match command, I can pull out the columns of interest with the raw output without the need to change
  data_herfindahl$competition <- 1 - data_herfindahl$herf_score

  return(data_herfindahl)
}
