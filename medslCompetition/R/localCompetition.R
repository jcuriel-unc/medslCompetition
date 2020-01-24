#' Calculates the competition for MEDSL local elections data 
#' @param data The data frame precinct object with the local election data. Should be formatted with proper MEDSL standards. 
#' @param mag  The optional command that if true, will trigger a revised herfindahl index in order to deal with multimemeber districts  
#' @return The the competition dataframe object. Will have the following fields:
#'    \itemize{
#'    \item office = The name of the office for the given race.
#'    \item district = the district for the given race. For counties and jurisdictions within MEDSL data with empty districts, coded as the 
#'    name of the county and/or jurisdiction. Additionally, in the event that there is a numeric value, the name of the appropriate county
#'    or jurisdiction is pasted on
#'    \item stage = The stage for a given race, i.e. gen, primary, runoff, etc. 
#'    \item special = Whether an election was a sepcial election. 
#'    \item herf_score = The sum of squared proportions for all of the candidates within the given precinct data. Scores closer to one reflect
#'    complete lack of diversity in the vote share, centered around a single candidate, and scores closer to zero reflect complete 
#'    fragmentation and diversity within the voteshare. Note that scores closer to zero might be due to higher district magnitudes, or in the
#'    event of improperly cleaned data, errors in the fields of office, county, jurisdiction, candidate, and/or party.  
#'    \item state_fips = The FIPs code for a state. 
#'    \item partisan = The race had at least one of the two major parties openly contest the race. 1 if partisan, and 0 otherwise. 
#'    \item first_county = The name of the county where the competition is held, or the name of the first county that appears for the race 
#'    in the event of multiple counties in a given race.
#'    \item first_jurisdiction = the name of the jurisdiction where a competition is held, or the name of the first jurisdiction that appears 
#'    for the race in the event of multiple jurisdictions for a given race.    
#'    \item county_count = The number of counties for a given race. 
#'    \item jurisdiction_count - The number of jurisdictions within a given race.
#'    \item party_count = The number of unique parties found to have contested elections. Note that this also includes blanks. Note2: 
#'    Given the writing of this package on January 23, 2020, the MEDSL data has yet to have the party_simplified category coded in. 
#'    \item total_vote = The total votes cast for a given race. 
#'    \item max_vote = The maximum vote share for the candidate within the race, and usually the sole winner. 
#'    \item max_cand = The name of the candidate/ballot item with the maximum vote share. 
#'    \item cand_count = The number of unique candidates found within the data. Note that in the event of detailed write-ins for states 
#'    such as Vermont and New Hampshire, these might number in the hundreds. 
#'    \item competition = The reverse of the herfindahl score such that scores closer to one equate to greater competition, and scores of 
#'    zero to non-competition. 
#' }
#' @export
#' @examples
#' mn_results <- read.csv("2018-il-local_precinct.csv")
#' mn_comp <- localCompetition(mn_results)
#' head(mn_results$magnitude)
#' 1 1 3 1 3 
#' mn_comp2 <- localCompetition(mn_results,mag=TRUE)
#' 
#' 
localCompetition <- function(data, mag = c(TRUE,FALSE)){
  data <- subset(data, dataverse=="local")
  ###adding in an optional argument in order to determine if a district should be labeled competitive or not. Will merge on data related 
  #to how many seats are elected for a given seat, and then if equals, will rate as not competitive 
  data$office <- str_to_upper(data$office)
  data$jurisdiction <- str_to_upper(data$jurisdiction)
  data$county <- str_to_upper(data$county)
  data$candidate <- str_to_upper(data$candidate)
  data <- subset(data, candidate != "UNDERVOTES")
  data <- subset(data, candidate != "OVERVOTES")
  data <- subset(data, candidate != "UNDER VOTES")
  data <- subset(data, candidate != "OVER VOTES")
  data$district <- stri_trim(data$district, "both")
  data <- data %>% group_by(state_fips,office,district,stage,special,candidate) %>% mutate(jurisdiction_count=n_distinct(jurisdiction))
  data <- data %>% group_by(state_fips,office,district,stage,special,candidate) %>% mutate(county_count=n_distinct(county))
  data$district[data$jurisdiction_count > 1 & data$county_count==1 & data$district==""] <-
    data$county[data$jurisdiction_count > 1 & data$county_count==1 & data$district==""]
  data$district[data$jurisdiction_count < 2 & data$county_count==1 & data$district==""] <-
    data$county[data$jurisdiction_count < 2 & data$county_count==1 & data$district==""]
  ###adding code here so that I can get the district's number correct
  data$dist_num <- 0
  data$dist_num[str_detect(data$dist_num, "[:digit:]")] <- 1
  data$dist_len <- nchar(data$district)
  data$district[data$dist_num==1 & data$dist_len==1 & data$county_count==1] <- 
    paste(data$county, sep=" ", data$district)[data$dist_num==1 & data$dist_len==1 & data$county_count==1]
  data$district[data$dist_num==1 & data$dist_len==1 & data$county_count>1 & data$jurisdiction==1] <- 
    paste(data$jurisdiction, sep=" ", data$district)[data$dist_num==1 & data$dist_len==1 & data$county_count==1]
  ####running another command here just to paste the word count in if it appears 
  data$district[data$dist_num==1 & data$dist_len==1 & str_detect(data$office, "COUNTY")] <- 
    paste(data$county, sep=" ", data$district)[data$dist_num==1 & data$dist_len==1 & str_detect(data$office, "COUNTY")]
  ######
  ####here is where I take care of the writein cands 
  data2 <- subset(data, district=="") 
  data <- subset(data, district != "")
  data2 <- subset(data2, select=-c(jurisdiction_count,county_count))
  data2 <- data2 %>% group_by(state_fips,office,district,stage,special,candidate) %>% mutate(jurisdiction_count=n_distinct(jurisdiction))
  data2 <- data2 %>% group_by(state_fips,office,district,stage,special,candidate) %>% mutate(county_count=n_distinct(county))
  ###command here to find the word "county"; seems the best solution
  data2$district[str_detect(data2$office, "COUNTY")] <- paste(data2$county,sep="",data2$district)[str_detect(data2$office, "COUNTY")]
  data2$district <- str_trim(data2$district)
  data2$district[data2$jurisdiction_count > 1 & data2$county_count==1 & data2$district==""] <-
    data2$county[data2$jurisdiction_count > 1 & data2$county_count==1 & data2$district==""]
  data2$district[data2$jurisdiction_count < 2 & data2$county_count==1 & data2$district==""] <-
    data2$county[data2$jurisdiction_count < 2 & data2$county_count==1 & data2$district==""]
  ####commands to deal with other write-in data where the coding is off 
  data2$district[data2$dist_num==1 & data2$dist_len==1 & str_detect(data2$office,"COUNTY")] <- 
    paste(data2$county, sep=" ", data2$district)[data2$dist_num==1 & data2$dist_len==1 & str_detect(data2$office,"COUNTY")]
  data2$district[data2$dist_num==1 & data2$dist_len==1 & str_detect(data2$office,"TOWN")] <- 
    paste(data2$jurisdiction, sep=" ", data2$district)[data2$dist_num==1 & data2$dist_len==1 & str_detect(data2$office,"TOWN")]
  
  data <- rbind(data, data2)###everything should be complete here 
  if(missing(mag)==F & mag==TRUE){
    data_sum <- data %>% group_by(state_fips,office,district,magnitude,stage,special,candidate) %>% summarise(votes = sum(votes,na.rm=T),
                                                                                                    jurisdiction=first(jurisdiction),
                                                                                                    county=first(county),
                                                                                                    jurisdiction_count=first(jurisdiction_count),
                                                                                                    county_count=first(county_count))
    
  }else{
    data_sum <- data %>% group_by(state_fips,office,district,stage,special,candidate) %>% summarise(votes=sum(votes,na.rm=T),
                                                                                                              jurisdiction=first(jurisdiction),
                                                                                                              county=first(county),
                                                                                                              jurisdiction_count=first(jurisdiction_count),
                                                                                                              county_count=first(county_count),
                                                                                                    party=first(party))
  }

  data_sum <- data_sum %>% group_by(state_fips,office,district,stage,special) %>% mutate(total_vote = sum(votes, na.rm=T)) 
  data_sum$prop_vote2 <- (data_sum$votes/data_sum$total_vote)^2
  data_sum$partisan <- 0
  data_sum$partisan[str_detect(data_sum$party, "democrat") | str_detect(data_sum$party, "republican")] <- 1
  data_sum <- data_sum %>% group_by(state_fips,office,district,stage,special) %>% mutate(party_count=n_distinct(party))
  ####sorting here by max vote so that I can pull out the first cand that appears, which will allow me to find the winner 
  data_sum <- data_sum[with(data_sum, order(-votes)), ]
  data_sum <- data_sum %>% group_by(state_fips,office,district,stage,special) %>% mutate(cand_count = n_distinct(candidate))
  if( missing(mag)==F & mag==TRUE){
    data_sum <- data_sum %>% group_by(state_fips,office,district,stage,special) %>% arrange(-votes)
    data_sum$mag2 <- data_sum$magnitude 
    data_sum2 <- data_sum %>% group_by(state_fips,office,district,magnitude,stage,special) %>% slice(mag2:n())
    data_sum2<- data_sum2 %>% group_by(state_fips,office,district,stage,special) %>% 
      mutate(total_vote2 = sum(votes, na.rm=T)) 
    data_sum2$prop_vote2b <- (data_sum2$votes/data_sum2$total_vote2)^2
    data_herfindahl <- data_sum2 %>% group_by(state_fips,office,district,stage,special) %>% summarise(herf_score = sum(prop_vote2b,na.rm=T),
                                                                                                      partisan=max(partisan,na.rm=T),
                                                                                                      jurisdiction_count=first(jurisdiction_count),
                                                                                                      county_count=first(county_count),
                                                                                                      party_count=first(party_count),
                                                                                                      total_vote=first(total_vote),max_vote=max(votes,na.rm=T),
                                                                                                      total_vote2=first(total_vote2),
                                                                                                      max_cand=first(candidate),
                                                                                                      cand_count=first(cand_count),
                                                                                                      magnitude=first(magnitude),
                                                                                                      first_county=first(county))
    
    data_herfindahl$herf_score[data_herfindahl$herf_score==0] <- 1
  }else{
    data_sum <- data_sum %>% group_by(state_fips,office,district,stage,special) %>% arrange(-votes)
    data_herfindahl <- data_sum %>% group_by(state_fips,office,district,stage,special) %>% summarise(herf_score = sum(prop_vote2,na.rm=T),
                                                                                                     partisan=max(partisan,na.rm=T),
                                                                                                     jurisdiction_count=first(jurisdiction_count),
                                                                                                     county_count=first(county_count),
                                                                                                     party_count=first(party_count),
                                                                                                     total_vote=first(total_vote),max_vote=max(votes,na.rm=T),
                                                                                                     max_cand=first(candidate),cand_count=first(cand_count),
                                                                                                     first_county=first(county),
                                                                                                     first_jurisdiction=first(jurisdiction))
    data_herfindahl$herf_score[data_herfindahl$herf_score==0] <- 1
    
  }
  data_herfindahl$competition <- 1 - data_herfindahl$herf_score
  return(data_herfindahl)
}
