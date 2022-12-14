library(baseballr)
library(tidyverse)
library(DBI)
library(RPostgreSQL)
library(tidyverse)
library(readr)
library(dplyr)
library(data.table)
library(xgboost)
library(caret)
library(ggplot2)
library(cowplot)
library(qdapTools)
library(mosaic)
library(SHAPforxgboost)
library(caTools)
set.seed(3)
delete_and_upload <- function(df, 
                              year, 
                              db_driver = "PostgreSQL", 
                              dbname, 
                              user, 
                              password, 
                              host = 'local_host', 
                              port = 5432) {
  
  pg <- dbDriver(db_driver)
  
  statcast_db <- dbConnect(pg, 
                           dbname = dbname, 
                           user = user, 
                           password = password,
                           host = host, 
                           port = posrt)
  
  query <- paste0('DELETE from statcast where game_year = ', year)
  
  dbGetQuery(statcast_db, query)
  
  dbWriteTable(statcast_db, "statcast", df, append = TRUE)
  
  dbDisconnect(statcast_db)
  rm(statcast_db)
}

annual_statcast_query <- function(season) {
  
  data_base_column_types <- read_csv("https://app.box.com/shared/static/q326nuker938n2nduy81au67s2pf9a3j.csv")
  
  dates <- seq.Date(as.Date(paste0(season, '-03-01')),
                    as.Date(paste0(season, '-12-01')), by = '4 days')
  
  date_grid <- tibble::tibble(start_date = dates, 
                              end_date = dates + 3)
  
  safe_savant <- purrr::safely(scrape_statcast_savant)
  
  payload <- purrr::map(.x = seq_along(date_grid$start_date), 
                        ~{message(paste0('\nScraping week of ', date_grid$start_date[.x], '...\n'))
                          
                          payload <- safe_savant(start_date = date_grid$start_date[.x], 
                                                 end_date = date_grid$end_date[.x], type = 'pitcher')
                          
                          return(payload)
                        })
  
  payload_df <- purrr::map(payload, 'result')
  
  number_rows <- purrr::map_df(.x = seq_along(payload_df), 
                               ~{number_rows <- tibble::tibble(week = .x, 
                                                               number_rows = length(payload_df[[.x]]$game_date))}) %>%
    dplyr::filter(number_rows > 0) %>%
    dplyr::pull(week)
  
  payload_df_reduced <- payload_df[number_rows]
  
  payload_df_reduced_formatted <- purrr::map(.x = seq_along(payload_df_reduced), 
                                             ~{cols_to_transform <- c("fielder_2", "pitcher_1", "fielder_2_1", "fielder_3",
                                                                      "fielder_4", "fielder_5", "fielder_6", "fielder_7",
                                                                      "fielder_8", "fielder_9")
                                             
                                             df <- purrr::pluck(payload_df_reduced, .x) %>%
                                               dplyr::mutate_at(.vars = cols_to_transform, as.numeric) %>%
                                               dplyr::mutate_at(.vars = cols_to_transform, function(x) {
                                                 ifelse(is.na(x), 999999999, x)
                                               })
                                             
                                             character_columns <- data_base_column_types %>%
                                               dplyr::filter(class == "character") %>%
                                               dplyr::pull(variable)
                                             
                                             numeric_columns <- data_base_column_types %>%
                                               dplyr::filter(class == "numeric") %>%
                                               dplyr::pull(variable)
                                             
                                             integer_columns <- data_base_column_types %>%
                                               dplyr::filter(class == "integer") %>%
                                               dplyr::pull(variable)
                                             
                                             df <- df %>%
                                               dplyr::mutate_if(names(df) %in% character_columns, as.character) %>%
                                               dplyr::mutate_if(names(df) %in% numeric_columns, as.numeric) %>%
                                               dplyr::mutate_if(names(df) %in% integer_columns, as.integer)
                                             
                                             return(df)
                                             })
  
  combined <- payload_df_reduced_formatted %>%
    dplyr::bind_rows()
  
  combined
}


format_append_statcast <- function(df) {
  
  # function for appending new variables to the data set
  
  additional_info <- function(df) {
    
    # apply additional coding for custom variables
    
    df$hit_type <- with(df, ifelse(type == "X" & events == "single", 1,
                                   ifelse(type == "X" & events == "double", 2,
                                          ifelse(type == "X" & events == "triple", 3, 
                                                 ifelse(type == "X" & events == "home_run", 4, NA)))))
    
    df$hit <- with(df, ifelse(type == "X" & events == "single", 1,
                              ifelse(type == "X" & events == "double", 1,
                                     ifelse(type == "X" & events == "triple", 1, 
                                            ifelse(type == "X" & events == "home_run", 1, NA)))))
    
    df$fielding_team <- with(df, ifelse(inning_topbot == "Bot", away_team, home_team))
    
    df$batting_team <- with(df, ifelse(inning_topbot == "Bot", home_team, away_team))
    
    df <- df %>%
      dplyr::mutate(barrel = ifelse(launch_angle <= 50 & launch_speed >= 98 & launch_speed * 1.5 - launch_angle >= 117 & launch_speed + launch_angle >= 124, 1, 0))
    
    df <- df %>%
      dplyr::mutate(spray_angle = round(
        (atan(
          (hc_x-125.42)/(198.27-hc_y)
        )*180/pi*.75)
        ,1)
      )
    
    df <- df %>%
      dplyr::filter(!is.na(game_year))
    
    return(df)
  }
  
  df <- df %>%
    additional_info()
  
  df$game_date <- as.character(df$game_date)
  
  df <- df %>%
    dplyr::arrange(game_date)
  
  df <- df %>%
    dplyr::filter(!is.na(game_date))
  
  df <- df %>%
    dplyr::ungroup()
  
  df <- df %>%
    dplyr::select(setdiff(names(.), c("error")))
  
  return(df)
}
SSW <- function(combined) {
  combined<-mutate(combined,Inferred_Axis = atan(pfx_z/pfx_x)*(180/pi))
  combined<-mutate(combined,Inf2 = Inferred_Axis+90)
  combined<-mutate(combined,Inf3 = ifelse(pfx_x < 0, Inf2 +180, Inf2))
  combined2<-mutate(combined,diff = ifelse(abs(spin_axis-Inf3) <= 180,abs(spin_axis-Inf3), 360-abs(spin_axis-Inf3)))
  return(combined2)
  
}
pitch_type_numerical<-function(combined17){
  combined17 <- mutate(combined17, pitch_type_num = derivedFactor(
    '0' = (pitch_type==''),
    '1' = (pitch_type=='FC'),
    '2' = (pitch_type=='FF'),
    '3' = (pitch_type=='SI'),
    '4' = (pitch_type=='SL'),
    '5' = (pitch_type=='CH'),
    '6' = (pitch_type=='CU'),
    '7' = (pitch_type=='KN'),
    '8' = (pitch_type=='EP'),
    '9' = (pitch_type=='FS'),
    '10' = (pitch_type=='PO'),
    '11' = (pitch_type=='CS'),
    '12' = (pitch_type=='FA'),
    '13' = (pitch_type=='KC'),
    '14' = (pitch_type=='SC'),
    '15' = (pitch_type=='FO'),
    .method = "first",
    .default = 0
  ))
  return(combined17)
}
pitch_type_numerical.1<-function(combined17){
  combined17 <- mutate(combined17, pitch_type_num.1 = derivedFactor(
    '0' = (pitch_type.1==''),
    '1' = (pitch_type.1=='FC'),
    '2' = (pitch_type.1=='FF'),
    '3' = (pitch_type.1=='SI'),
    '4' = (pitch_type.1=='SL'),
    '5' = (pitch_type.1=='CH'),
    '6' = (pitch_type.1=='CU'),
    '7' = (pitch_type.1=='KN'),
    '8' = (pitch_type.1=='EP'),
    '9' = (pitch_type.1=='FS'),
    '10' = (pitch_type.1=='PO'),
    '11' = (pitch_type.1=='CS'),
    '12' = (pitch_type.1=='FA'),
    '13' = (pitch_type.1=='KC'),
    '14' = (pitch_type.1=='SC'),
    '15' = (pitch_type.1=='FO'),
    .method = "first",
    .default = 0
  ))
  return(combined17)
}
get_RV <- function(data) {
  thing1<-rep(0,0)
  thing<-rep(0,0)
  df <- mutate(data, BIP = derivedFactor(
    '1' = (description!='hit_into_play'),
    .method = "first",
    .default = 0
  ))
  df<-filter(df,BIP==1)
  for (i in 1:nrow(df)) {
    if (df[['balls']][i]==0) {
      if (df[['strikes']][i]==0){
        if (df[['description']][i]=='foul_bunt'|df[['description']][i]=='missed_bunt'|df[['description']][i]=='called_strike'|df[['description']][i]=='foul_tip'|df[['description']][i]=='bunt_foul_tip'|df[['description']][i]=='foul_pitchout'|df[['description']][i]=='foul'|df[['description']][i]=='swinging_strike'|df[['description']][i]=='swinging_strike_blocked'|df[['description']][i]=='swinging_pitchout'){
          thing<-c(thing,-0.0015082)
        }
        if (df[['description']][i]=='ball'|df[['description']][i]=='pitchout'|df[['description']][i]=='blocked_ball'|df[['description']][i]=='intent_ball'){
          thing<-c(thing,0.0049702)
        }}
      if (df[['strikes']][i]==1){
        if (df[['description']][i]=='foul_bunt'|df[['description']][i]=='missed_bunt'|df[['description']][i]=='called_strike'|df[['description']][i]=='foul_tip'|df[['description']][i]=='bunt_foul_tip'|df[['description']][i]=='foul_pitchout'|df[['description']][i]=='foul'|df[['description']][i]=='swinging_strike'|df[['description']][i]=='swinging_strike_blocked'|df[['description']][i]=='swinging_pitchout'){
          thing<-c(thing,-0.0027908)
        }
        if (df[['description']][i]=='ball'|df[['description']][i]=='pitchout'|df[['description']][i]=='blocked_ball'|df[['description']][i]=='intent_ball'){
          thing<-c(thing,0.0032315)
        }}
      if (df[['strikes']][i]==2){
        if (df[['description']][i]=='foul_bunt'|df[['description']][i]=='missed_bunt'|df[['description']][i]=='called_strike'|df[['description']][i]=='foul_tip'|df[['description']][i]=='bunt_foul_tip'|df[['description']][i]=='swinging_strike'|df[['description']][i]=='swinging_strike_blocked'|df[['description']][i]=='swinging_pitchout'){
          thing<-c(thing,-0.3389518)
        }
        if (df[['description']][i]=='ball'|df[['description']][i]=='pitchout'|df[['description']][i]=='blocked_ball'|df[['description']][i]=='intent_ball'){
          thing<-c(thing,-0.00035)
        }
        if (df[['description']][i]=='foul'|df[['description']][i]=='foul_pitchout'){
          thing<-c(thing,0)
        }}}
    if(df[['balls']][i]==1) {
      if (df[['strikes']][i]==0){
        if (df[['description']][i]=='foul_bunt'|df[['description']][i]=='missed_bunt'|df[['description']][i]=='called_strike'|df[['description']][i]=='foul_tip'|df[['description']][i]=='bunt_foul_tip'|df[['description']][i]=='foul_pitchout'|df[['description']][i]=='foul'|df[['description']][i]=='swinging_strike'|df[['description']][i]=='swinging_strike_blocked'|df[['description']][i]=='swinging_pitchout'){
          thing<-c(thing,-0.0032469)
        }
        if (df[['description']][i]=='ball'|df[['description']][i]=='pitchout'|df[['description']][i]=='blocked_ball'|df[['description']][i]=='intent_ball'){
          thing<-c(thing,0.0062869)
        }}
      if (df[['strikes']][i]==1){
        if (df[['description']][i]=='foul_bunt'|df[['description']][i]=='missed_bunt'|df[['description']][i]=='called_strike'|df[['description']][i]=='foul_tip'|df[['description']][i]=='bunt_foul_tip'|df[['description']][i]=='foul_pitchout'|df[['description']][i]=='foul'|df[['description']][i]=='swinging_strike'|df[['description']][i]=='swinging_strike_blocked'|df[['description']][i]=='swinging_pitchout'){
          thing<-c(thing,-0.0063723)
        }
        if (df[['description']][i]=='ball'|df[['description']][i]=='pitchout'|df[['description']][i]=='blocked_ball'|df[['description']][i]=='intent_ball'){
          thing<-c(thing,0.0055201)
        }}
      if (df[['strikes']][i]==2){
        if (df[['description']][i]=='foul_bunt'|df[['description']][i]=='missed_bunt'|df[['description']][i]=='called_strike'|df[['description']][i]=='foul_tip'|df[['description']][i]=='bunt_foul_tip'|df[['description']][i]=='swinging_strike'|df[['description']][i]=='swinging_strike_blocked'|df[['description']][i]=='swinging_pitchout'){
          thing<-c(thing,-0.3386018)
        }
        if (df[['description']][i]=='ball'|df[['description']][i]=='pitchout'|df[['description']][i]=='blocked_ball'|df[['description']][i]=='intent_ball'){
          thing<-c(thing,0.0043994)
        }
        if (df[['description']][i]=='foul'|df[['description']][i]=='foul_pitchout'){
          thing<-c(thing,0)
        }}}
    if(df[['balls']][i]==2) {
      if (df[['strikes']][i]==0){
        if (df[['description']][i]=='foul_bunt'|df[['description']][i]=='missed_bunt'|df[['description']][i]=='called_strike'|df[['description']][i]=='foul_tip'|df[['description']][i]=='bunt_foul_tip'|df[['description']][i]=='foul_pitchout'|df[['description']][i]=='foul'|df[['description']][i]=='swinging_strike'|df[['description']][i]=='swinging_strike_blocked'|df[['description']][i]=='swinging_pitchout'){
          thing<-c(thing,-0.0040137)
        }
        if (df[['description']][i]=='ball'|df[['description']][i]=='pitchout'|df[['description']][i]=='blocked_ball'|df[['description']][i]=='intent_ball'){
          thing<-c(thing,0.0135254)
        }}
      if (df[['strikes']][i]==1){
        if (df[['description']][i]=='foul_bunt'|df[['description']][i]=='missed_bunt'|df[['description']][i]=='called_strike'|df[['description']][i]=='foul_tip'|df[['description']][i]=='bunt_foul_tip'|df[['description']][i]=='foul_pitchout'|df[['description']][i]=='foul'|df[['description']][i]=='swinging_strike'|df[['description']][i]=='swinging_strike_blocked'|df[['description']][i]=='swinging_pitchout'){
          thing<-c(thing,-0.007493)
        }
        if (df[['description']][i]=='ball'|df[['description']][i]=='pitchout'|df[['description']][i]=='blocked_ball'|df[['description']][i]=='intent_ball'){
          thing<-c(thing,0.0071924)
        }}
      if (df[['strikes']][i]==2){
        if (df[['description']][i]=='foul_bunt'|df[['description']][i]=='missed_bunt'|df[['description']][i]=='called_strike'|df[['description']][i]=='foul_tip'|df[['description']][i]=='bunt_foul_tip'|df[['description']][i]=='swinging_strike'|df[['description']][i]=='swinging_strike_blocked'|df[['description']][i]=='swinging_pitchout'){
          thing<-c(thing,-0.3430012)
        }
        if (df[['description']][i]=='ball'|df[['description']][i]=='pitchout'|df[['description']][i]=='blocked_ball'|df[['description']][i]=='intent_ball'){
          thing<-c(thing,0.007325)
        }
        if (df[['description']][i]=='foul'|df[['description']][i]=='foul_pitchout'){
          thing<-c(thing,0)
        }}}
    if(df[['balls']][i]==3) {
      if (df[['strikes']][i]==0){
        if (df[['description']][i]=='foul_bunt'|df[['description']][i]=='missed_bunt'|df[['description']][i]=='called_strike'|df[['description']][i]=='foul_tip'|df[['description']][i]=='bunt_foul_tip'|df[['description']][i]=='foul_pitchout'|df[['description']][i]=='foul'|df[['description']][i]=='swinging_strike'|df[['description']][i]=='swinging_strike_blocked'|df[['description']][i]=='swinging_pitchout'){
          thing<-c(thing,-0.0103467)
        }
        if (df[['description']][i]=='ball'|df[['description']][i]=='pitchout'|df[['description']][i]=='blocked_ball'|df[['description']][i]=='intent_ball'){
          thing<-c(thing,0.3219667)
        }}
      if (df[['strikes']][i]==1){
        if (df[['description']][i]=='foul_bunt'|df[['description']][i]=='missed_bunt'|df[['description']][i]=='called_strike'|df[['description']][i]=='foul_tip'|df[['description']][i]=='bunt_foul_tip'|df[['description']][i]=='foul_pitchout'|df[['description']][i]=='foul'|df[['description']][i]=='swinging_strike'|df[['description']][i]=='swinging_strike_blocked'|df[['description']][i]=='swinging_pitchout'){
          thing<-c(thing,-0.0073604)
        }
        if (df[['description']][i]=='ball'|df[['description']][i]=='pitchout'|df[['description']][i]=='blocked_ball'|df[['description']][i]=='intent_ball'){
          thing<-c(thing,0.3323134)
        }}
      if (df[['strikes']][i]==2){
        if (df[['description']][i]=='foul_bunt'|df[['description']][i]=='missed_bunt'|df[['description']][i]=='called_strike'|df[['description']][i]=='foul_tip'|df[['description']][i]=='bunt_foul_tip'|df[['description']][i]=='swinging_strike'|df[['description']][i]=='swinging_strike_blocked'|df[['description']][i]=='swinging_pitchout'){
          thing<-c(thing,-0.3503262)
        }
        if (df[['description']][i]=='ball'|df[['description']][i]=='pitchout'|df[['description']][i]=='blocked_ball'|df[['description']][i]=='intent_ball'){
          thing<-c(thing,0.3396738)
        }
        if (df[['description']][i]=='foul'|df[['description']][i]=='foul_pitchout'){
          thing<-c(thing,0)
        }}}
    if(df[['description']][i]=='hit_by_pitch'){
      thing<-c(thing,.72)
      #split up the c() functions for efficiency
      thing1<-c(thing1,thing)
      thing<-rep(0,0)
    }}
  thing1<-c(thing1,thing)
  df<-filter(df,balls!=4)
  combined5<-cbind(df,thing1)
  
  #filter for only bip
  
  bip<-na.omit(data,cols = c('launch_speed','launch_angle'))
  bip<-filter(bip,type=='X')
  bip <- mutate(bip, BIP = derivedFactor(
    '1' = (description!='hit_into_play'),
    .method = "first",
    .default = 0
  ))
  
  #RV of bip=xwobacon
  
  bip<-mutate(bip,thing1=estimated_woba_using_speedangle)
  combined6<-rbind(combined5,bip)
  combined6<-mutate(combined6,pitches=1)
  combined6<-filter(combined6,balls<4)
  
  #subtract the mean RV by count, making or new RV count independent
  
  combined7<- mutate(combined6, epoints = derivedFactor(
    '0.0428864914062796' = (balls==0&strikes==0),
    '0.0614201488121436' = (balls==0&strikes==1),
    '-0.00523955861616877' = (balls==0&strikes==2),
    '0.0622226761743764' = (balls==1&strikes==0),
    '0.0736926018089744' = (balls==1&strikes==1),
    '0.00481201574267403' = (balls==1&strikes==2),
    '0.0748142028993431' = (balls==2&strikes==0),
    '0.0866305798396565' = (balls==2&strikes==1),
    '0.0205508602787794' = (balls==2&strikes==2),
    '0.123123929852487' = (balls==3&strikes==0),
    '0.188901454685714' = (balls==3&strikes==1),
    '0.12199878471559' = (balls==3&strikes==2),
    .method = "first",
    .default = 0
  ))
  combined7<-mutate(combined7,RV2=thing1-as.numeric(as.character(epoints))-.00003089)
  return(combined7)
}
pitch_pairing<-function(combined11){
  group <- combined11 %>%
    group_by(pitcher, game_year, pitch_type) %>%
    summarize(Pitches = sum(pitches),
              HB=mean(pfx_x),
              IVB=mean(pfx_z),
              SWW=mean(diff),
              X=mean(release_pos_x),
              Y=mean(release_extension),
              Z=mean(release_pos_z),
              velo=mean(release_speed),
              stuff=(mean(predict2)),
              xRV=mean(predict),
              RV=mean(RV2),
              command=mean(predict2)-mean(predict),
              xRV=mean(predict))
  #group<-filter(group,Pitches>20)
  
  #create group just grouped by pitcher, year to find pitch usage
  
  grouptotal <- combined11 %>%
    group_by(pitcher, game_year) %>%
    summarize(Pitches = sum(pitches))
  group<-merge(group,grouptotal,by=c('pitcher','game_year'))
  group<-mutate(group,usage=Pitches.x/Pitches.y)
  group<-mutate(group,concat=paste(pitcher,game_year))
  
  #create 2 pitch combinations to help understand the relationships between pitches
  
  uniquething<-unique(group$concat)
  dfdfdf2 <- data.frame(matrix(ncol = 28, nrow = 0))
  for (i in 1:length(uniquething)){
    thingg<-filter(group,concat==uniquething[i])
    for (j in 1:nrow(thingg)){
      thinggg<-thingg[-c(j), ]
      if (nrow(thinggg)!='0'){
        for (k in 1:nrow(thinggg)){
          thingggg<-c(thingg[j,],thinggg[k,])
          dfdfdf2<-rbind(dfdfdf2,thingggg)
        }}
    }
  }
  return(dfdfdf2)  
}

differences<-function(combined,dfdfdf2,game_year){
  combined17<-mutate(combined,fps=release_speed*5280/3600)
  combined17<-mutate(combined17,time=(((-fps)+sqrt(fps^2+2*ay*(60.5-release_extension)))/ay)-.167)
  combined17<-mutate(combined17,time50=(((-fps)+sqrt(fps^2+2*ay*(10.5-release_extension)))/ay))
  combined17<-mutate(combined17,fps50=-vy0)
  combined17<-mutate(combined17,cx=vx0-ax*time50)
  combined17<-mutate(combined17,cx1=release_pos_x)
  combined17<-mutate(combined17,cz=vz0-az*time50)
  combined17<-mutate(combined17,cz1=release_pos_z)
  dfdfdf3<-filter(dfdfdf2,game_year==game_year)
  dfdfdf4<-pitch_type_numerical(pitch_type_numerical.1(dfdfdf3))
  combined17<-pitch_type_numerical(combined17)
  combined17<-filter(combined17,pitch_type_num!=0)
  combined17<-filter(combined17,game_year==game_year)
  combined17<-filter(combined17,time>0)
  tunnel<-rep(0,0)
  tunnel2<-data.frame(matrix(ncol = 0, nrow = 0))
  for (k in 1:nrow(dfdfdf4)){
    degrom<-filter(combined17,pitcher==dfdfdf4[k,]$pitcher&pitch_type_num==dfdfdf4[k,]$pitch_type_num)
    degrom2<-filter(combined17,pitcher==dfdfdf4[k,]$pitcher&pitch_type_num==dfdfdf4[k,]$pitch_type_num.1)
    listy<-data.frame(matrix(ncol = 0, nrow = 0))
    if (nrow(degrom)>0&nrow(degrom2)>0){
      for (i in 1:nrow(degrom)){
        pitch<-degrom[i,]
        t = seq(0,pitch$time,.01)
        d<-mean(sqrt(((((pitch$az*t^2)/2)+pitch$cz*t+pitch$cz1)-(((degrom2$az*t^2)/2)+degrom2$cz*t+degrom2$cz1))^2+(((((pitch$ax)*t^2)/2)+pitch$cx*t+pitch$cx1)-((((degrom2$ax)*t^2)/2)+degrom2$cx*t+degrom2$cx1))^2),,na.rm=TRUE)
        dmovez<-mean(pitch$pfx_z-degrom2$pfx_z,na.rm=TRUE)
        dmovex<-mean(pitch$pfx_x-degrom2$pfx_x,na.rm=TRUE)
        dvelo<-mean(pitch$release_speed-degrom2$release_speed,na.rm=TRUE)
        ro<-c(d,dmovez,dmovex,dvelo)
        listy<-rbind(listy,ro)
      }
      colnames(listy)[1] <- "d"
      colnames(listy)[2] <- "dmovez"
      colnames(listy)[3] <- "dmovex"
      colnames(listy)[4] <- "dvelo"
      tunnel<-c(quantile(listy$d,probs=.05),mean(listy$dmovez),mean(listy$dmovex),mean(listy$dvelo))
      tunnel2<-rbind(tunnel2,tunnel)
    }else{
      tunnel2<-rbind(tunnel2,c(10000,10000,10000,10000))
    }
    print(nrow(tunnel2))
  }
  colnames(tunnel2)[1] <- "d"
  colnames(tunnel2)[2] <- "dmovez"
  colnames(tunnel2)[3] <- "dmovex"
  colnames(tunnel2)[4] <- "dvelo"
  return(tunnel2)
  
}
primary_finder<-function(combined7,dfdfdf4,tunnel2){
  usage <- combined7 %>%
    group_by(pitcher,game_year, pitch_type) %>%
    summarize(Rv=mean(RV2),
              pitches=sum(pitches),
              velo<-mean(release_speed))
  usage2 <- combined7 %>%
    group_by(pitcher,game_year) %>%
    summarize(Rv=mean(RV2),
              pitches=sum(pitches))
  usage3<-merge(usage,usage2,by=c('pitcher','game_year'))
  usage3<-mutate(usage3,usage=pitches.x/pitches.y)
  colnames(usage3)[6] <- "velo"
  pitchers<-unique(usage3$pitcher)
  df2<-rep(0,0)
  for (i in 1:length(pitchers)){
    df<-filter(usage3,pitcher==pitchers[i])
    df3<-filter(df,pitch_type=='SI'|pitch_type=='FF'|pitch_type=='FC')
    if(nrow(df)>0){
      df<-df3
    } else {
      df<-df
    }
    df<-df[order(df$pitches.x,decreasing=TRUE),]
    primary<-df[1,]$pitch_type
    df2<-c(df2,primary)
  }
  pitchers<-cbind(pitchers,df2)
  group <- combined7 %>%
    group_by(pitcher, pitch_type) %>%
    summarize(Rv=mean(RV2),
              HB=mean(pfx_x),
              IVB=mean(pfx_z),
              SSW=mean(diff,na.rm=TRUE),
              X=mean(release_pos_x,na.rm=TRUE),
              Y=mean(release_extension,na.rm=TRUE),
              Z=mean(release_pos_z,na.rm=TRUE),
              velo=mean(release_speed,na.rm=TRUE))
  group<-pitch_type_numerical(group)
  colnames(pitchers)[1] <- "pitcher"
  group1<-merge(group,pitchers,by='pitcher',all.x=TRUE)
  group1<-drop_na(group1)
  colnames(group1)[ncol(group1)] <- "pitch_type.1"
  group1<-pitch_type_numerical.1(group1)
  dfdfdf4<-pitch_type_numerical(pitch_type_numerical.1(dfdfdf4))
  df<-cbind(dfdfdf4$pitcher,dfdfdf4$pitch_type_num,dfdfdf4$pitch_type_num.1,tunnel2)
  df<-filter(df,dmovex!=10000)
  colnames(df)[1] <- "pitcher"
  colnames(df)[2] <- "pitch_type_num"
  colnames(df)[3] <- "pitch_type_num.1"
  primaries<-filter(group1,pitch_type_num==pitch_type_num.1)
  group1<-filter(group1,pitch_type_num!=pitch_type_num.1)
  group2<-merge(group1,df,by=c('pitch_type_num','pitch_type_num.1','pitcher'))
  return(list(primaries,group2))
}
make_group_all<-function(combined7){
  combined7<-mutate(combined7,pitches=1)
  groupall <- combined7 %>%
    group_by(pitcher,pitch_type) %>%
    summarize(pitches=sum(pitches))
  return(groupall)
}
merge_with_groupall<-function(group2,groupall){
  group2<-merge(group2,groupall,by=c('pitcher','pitch_type'))
  return(group2)
}

train_model1<-function(group2,params){
  weready<-subset(group2,select=c(Rv,velo,X,Y,Z,HB,IVB,SSW,d,dmovez,dmovex,dvelo,pitches))
  weready<-drop_na(weready)
  spl = sample.split(weready$Rv, SplitRatio = 0.1)
  valid <- subset(weready, spl == TRUE)
  weready<- subset(weready, spl == FALSE)
  data_set_size<-floor(nrow(weready)*.8)
  index<-sample(1:nrow(weready),size=data_set_size,replace=T)
  training<-weready[index,]
  testing<-weready[-index,]
  x<-select(training,-Rv)
  all<-select(weready,-Rv)
  testx<-select(testing,-Rv)
  train_label<-training[,'Rv']
  train_mtx<-xgb.DMatrix(data=as.matrix(x), label=as.matrix(train_label))
  test_label<-testing[,"Rv"]
  test_mtx<-xgb.DMatrix(data=as.matrix(testx),label=as.matrix(test_label))
  params<-params
  watchlist<-list(train=train_mtx,test=test_mtx)
  model<-xgb.train(data=train_mtx,params=params,nrounds=1000,watchlist=watchlist,early_stopping_rounds = 7)
  valid2<-select(valid,-Rv)
  predict<-predict(model,xgb.DMatrix(as.matrix(valid2)))
  combined8<-cbind(valid,predict)
  ml = lm(Rv~predict, data = combined8) 
  print(summary(ml)$r.squared)
  importance_matrix <- xgb.importance(colnames(train_mtx), model = model)
  xgb.plot.importance(importance_matrix, rel_to_first = TRUE, xlab = "Relative importance")
  return(model)
}
train_model2<-function(primaries,params){
  weready<-subset(primaries,select=c(Rv,velo,X,Y,Z,HB,IVB,SSW))
  weready<-drop_na(weready)
  spl = sample.split(weready$Rv, SplitRatio = 0.1)
  valid <- subset(weready, spl == TRUE)
  weready<- subset(weready, spl == FALSE)
  data_set_size<-floor(nrow(weready)*.8)
  index<-sample(1:nrow(weready),size=data_set_size,replace=T)
  training<-weready[index,]
  testing<-weready[-index,]
  x<-select(training,-Rv)
  all<-select(weready,-Rv)
  testx<-select(testing,-Rv)
  train_label<-training[,'Rv']
  train_mtx<-xgb.DMatrix(data=as.matrix(x), label=as.matrix(train_label))
  test_label<-testing[,"Rv"]
  test_mtx<-xgb.DMatrix(data=as.matrix(testx),label=as.matrix(test_label))
  params<-params
  watchlist<-list(train=train_mtx,test=test_mtx)
  model<-xgb.train(data=train_mtx,params=params,nrounds=1000,watchlist=watchlist,early_stopping_rounds = 7)
  valid2<-select(valid,-Rv)
  predict<-predict(model,xgb.DMatrix(as.matrix(valid2)))
  combined8<-cbind(valid,predict)
  ml = lm(Rv~predict, data = combined8) 
  print(summary(ml)$r.squared)
  importance_matrix <- xgb.importance(colnames(train_mtx), model = model)
  xgb.plot.importance(importance_matrix, rel_to_first = TRUE, xlab = "Relative importance")
  return(model)
}
main<-function(){
  primaries_total <- data.frame(matrix(ncol = 0, nrow = 0))
  secondaries_total<-data.frame(matrix(ncol = 0, nrow = 0))
  primaries_total2 <- data.frame(matrix(ncol = 0, nrow = 0))
  secondaries_total2<-data.frame(matrix(ncol = 0, nrow = 0))
  for(game_year in 2017:2021){
    if(game_year!=2020){
      combined<-annual_statcast_query(game_year)
      combined<-filter(combined,game_type=='R')
      combined1<-filter(combined,stand=='R')
      combined4<-SSW(combined1)
      combined5<-get_RV(combined4)
      pairings<-pitch_pairing(combined5)
      df<-differences(combined5,pairings)
      prim<-primary_finder(combined5,pairings,df)
      primaries<-prim[1]
      secondaries<-prim[2]
      primaries<-merge_with_groupall(primaries,make_group_all(combined1))
      primaries<-mutate(primaries,game_year=game_year)
      secondaries<-merge_with_groupall(secondaries,make_group_all(combined1))
      secondaries<-mutate(secondaries,game_year=game_year)
      primaries_total<-rbind(primaries_total,primaries)
      secondaries_total<-rbind(secondaries_total,secondaries)
      combined1<-filter(combined,stand=='L')
      combined4<-SSW(combined1)
      combined5<-get_RV(combined4)
      pairings<-pitch_pairing(combined5)
      df<-differences(combined5,pairings)
      prim<-primary_finder(combined5,pairings,df)
      primaries<-prim[1]
      secondaries<-prim[2]
      primaries<-merge_with_groupall(primaries,make_group_all(combined1))
      primaries<-mutate(primaries,game_year=game_year)
      secondaries<-merge_with_groupall(secondaries,make_group_all(combined1))
      secondaries<-mutate(secondaries,game_year=game_year)
      primaries_total2<-rbind(primaries_total2,primaries)
      secondaries_total2<-rbind(secondaries_total2,secondaries)
    }}
  secondaries_total<-filter(secondaries_total,pitches>100)
  secondaries_total2<-filter(secondaries_total2,pitches>100)
  primaries_total<-filter(primaries_total,pitches>100)
  primaries_total2<-filter(primaries_total2,pitches>100)
  params<-list(booster='gbtree',eta=0.05,min_child_weight=4,max_depth=4,subsample=0.990817212196998,colsample_bytree=0.8584547852166,"obj"="reg:squarederror","eval_metric"="mae")
  params2<-list(booster='gbtree',eta=0.241268337203655,min_child_weight=3,max_depth=3,subsample=0.990817212196998,colsample_bytree=0.8584547852166,"obj"="reg:squarederror","eval_metric"="mae")
  righty_secondary_model<-train_model1(secondaries_total,params)
  lefty_secondary_model<-train_model1(secondaries_total2,params)
  righty_primary_model<-train_model2(primaries_total,params2)
  lefty_primary_model<-train_model2(primaries_total2,params2)
  eight<-annual_statcast_query(2022)
  combined<-filter(eight,game_type=='R')
  combined<-filter(combined,stand=='R')
  combined4<-SSW(combined)
  combined5<-get_RV(combined4)
  pairings<-pitch_pairing(combined5)
  df<-differences(combined5,pairings)
  prim<-primary_finder(combined5,pairings,df)
  primaries<-prim[1]
  secondaries<-prim[2]
  primaries_right<-merge_with_groupall(primaries,make_group_all(combined))
  primaries_right<-mutate(primaries_right,game_year=2022)
  secondaries_right<-merge_with_groupall(secondaries,make_group_all(combined))
  secondaries_right<-mutate(secondaries_right,game_year=2022)
  combined<-filter(eight,game_type=='R')
  combined<-filter(combined,stand=='L')
  combined4<-SSW(combined)
  combined5<-get_RV(combined4)
  pairings<-pitch_pairing(combined5)
  df<-differences(combined5,pairings)
  prim<-primary_finder(combined5,pairings,df)
  primaries<-prim[1]
  secondaries<-prim[2]
  primaries_left<-merge_with_groupall(primaries,make_group_all(combined))
  primaries_left<-mutate(primaries_left,game_year=2022)
  secondaries_left<-merge_with_groupall(secondaries,make_group_all(combined))
  secondaries_left<-mutate(secondaries_left,game_year=2022)
  predict_right_primary<-predict(righty_primary_model,xgb.DMatrix(as.matrix(subset(primaries_right,select=c(velo,X,Y,Z,HB,IVB,SSW)))))
  predict_left_primary<-predict(lefty_primary_model,xgb.DMatrix(as.matrix(subset(primaries_left,select=c(velo,X,Y,Z,HB,IVB,SSW)))))
  predict_left_secondaries<-predict(lefty_secondary_model,xgb.DMatrix(as.matrix(subset(secondaries_left,select=c(velo,X,Y,Z,HB,IVB,SSW,d,dmovez,dmovex,dvelo,pitches)))))
  predict_right_secondaries<-predict(righty_secondary_model,xgb.DMatrix(as.matrix(subset(secondaries_right,select=c(velo,X,Y,Z,HB,IVB,SSW,d,dmovez,dmovex,dvelo,pitches)))))
  right_primary_final<-cbind(primaries_right,predict_right_primary)
  left_primary_final<-cbind(primaries_left,predict_left_primary)
  right_secondaries_final<-cbind(secondaries_right,predict_right_secondaries)
  left_secondaries_final<-cbind(secondaries_left,predict_left_secondaries)
  return(list(right_primary_final,left_primary_final,right_secondaries_final,left_secondaries_final,righty_secondary_model,lefty_secondary_model,righty_primary_model,lefty_primary_model))
}
main<-main()
right_primary_final<-main[[1]]
left_primary_final<-main[2]
right_secondaries_final<-main[3]
left_secondaries_final<-main[4]
righty_secondary_model<-main[5]
lefty_secondary_model<-main[6]
righty_primary_model<-main[7]
lefty_primary_model<-main[8]