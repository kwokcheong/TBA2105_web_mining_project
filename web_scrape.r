library(ggplot2)
library(httr)
library(tibble)
library(jsonlite)
library(stringr)
library(tidyr)
library(readxl)
library("writexl")
library(rvest)
library(recommenderlab)
library(dplyr)
#setwd("~/Desktop/projects/TBA2105_web_mining_project")
# Initialize an empty data frame
df_hotel_info <- data.frame(matrix(ncol=10, nrow=0))
cols <- c('star', 'hotelName','hotelId','location','freeCancelTag', 'favorityCountMsg','score','scoreDescription', 'totalReviews', 'topQuality')
colnames(df_hotel_info) <- cols

#NOTE: Refer to this documentation https://rdrr.io/cran/httr/man/add_headers.html for instructions on adding headers
# Trip.com scraping the HotelSeachNearby api function
apiCallNearbyHotel <- function(masterHotelId){
  res = POST(
    "https://sg.trip.com/restapi/soa2/16708/json/hotelSearchNearby?x-traceID=1658849562341.1pmnm5-1666016479066-1666203502",
    add_headers('authority = sg.trip.com'),
    add_headers('accept = application/json'),
    add_headers('accept-language = en-US,en;q=0.9'),
    add_headers('content-type = application/json;charset=UTF-8'),
    add_headers('cookie = ibu_home_language_match_union_flag=0; _abtest_userid=fb70a796-d789-4b0b-80d8-afdd54b85933; _gcl_au=1.1.562621262.1658849563; _RGUID=751c843f-5682-48a5-a9ff-1e84de692fb8; _RDG=281a5c31eaa24722803983fe33c78ee19f; _RSG=bYaWMz3XSK7u.IQUJe7_dA; _bfaStatusPVSend=1; ibu_online_home_language_match={"isFromTWNotZh" =false,"isFromIPRedirect" =false,"isFromLastVisited" =true,"isRedirect" =false,"isShowSuggestion" =false,"lastVisited" ="https =//sg.trip.com?locale=en-sg"}; cookiePricesDisplayed=SGD; _tp_search_latest_channel_name=hotels; _gid=GA1.2.1760276045.1663081390; _RF1=137.132.215.24; g_state={"i_p" =1663088594680,"i_l" =1}; ibu_h5_site=SG; ibu_h5_group=trip; ibu_h5_local=en-sg; ibu_h5_lang=ensg; ibu_h5_curr=SGD; IBU_TRANCE_LOG_URL=%2Fhotels%2Fsingapore-hotel-detail-996041%2Fparkroyal-on-beach-road-singapore%2F; librauuid=; _clck=5quac4|1|f4u|0; IBU_TRANCE_LOG_P=37682130973; hotel_h5_view_ids=%5B%22996041%22%5D; IBU_showtotalamt=1; ibu-h5-pop-hotel=0; ibu_online_permission_cls_ct=2; ibu_online_permission_cls_gap=1663082802191; hotelhst=1164390341; ibulanguage=SG; ibulocale=en_sg; intl_ht1=h4%3D73_687472%2C73_909897%2C73_994990%2C73_996041; _bfa=1.1658849562341.1pmnm5.1.1658849562341.1663083559409.2.32.10320668588; _bfs=1.18; _ubtstatus=%7B%22vid%22%3A%221658849562341.1pmnm5%22%2C%22sid%22%3A2%2C%22pvid%22%3A32%2C%22pid%22%3A10320668147%7D; hotel=687472; _ga=GA1.2.523270998.1658849562; _uetsid=505649c0337611ed8b2fb3e69a5735ed; _uetvid=3d45d8900cf811edadcee3300718def6; _clsk=kgr7wv|1663083673350|9|0|l.clarity.ms/collect; _bfi=p1%3D10320668147%26p2%3D10320668147%26v1%3D32%26v2%3D31; _bfaStatus=success; _ga_X437DZ73MR=GS1.1.1663081390.3.1.1663083757.0.0.0'),
    add_headers('dnt = 1'),
    add_headers('origin = https =//sg.trip.com'),
    add_headers('p = 37682130973'),
    add_headers('referer = https =//sg.trip.com/hotels/w/detail?hotelid=687472&curr=SGD&locale=en-SG&checkin=2022-09-28&checkout=2022-09-29&hoteluniquekey=H4sIAAAAAAAAAOPqY-HilWA0-A8DjEJMHIxSP5g5fr_dt5rF4regIwMIbJnl4Alm5Mx3COApBDN_tDg0Mgpn-zx-dK7RYRJjLyNn_6GvGjGvGxwEwYzDNQ5KzhzdL5byC4hLbHj3SFWBUROm0RDGsChz1HWWeQ0ywhPGCGLleJApwRLFxrF3jpsEh5IOx_01CwQFtCTOP76npcAKMcQhBGoIkGFxkg-o5yYPUA-DExvHv2XMEiwzGD_80tzICFbyoMhhByPTCcabTAuYLkybwLiLCaroEBMrxy19CZZTQPoJUPslJoZbTAyPmCAOeMXE8IkJ6ohfMC1NzBClXcwMk5ghumcxQ9QvYmY4xQIRusQymTG6Wik7tVLJykRHqSSzJCdVyUopzDFESUcpMTe_NK8EyDUy0TM0BQqUJFZ4poAVJifmJJfmJJakhlQWADWY1epgGhKcWlSWmZyqkJaaimyYsZmepSHCMHMshsXeYmF4xMIUHfuJheEXC8TRTawMXawMk1jZOObtZZJgEWIFR7yUgrGJZVJqmoG5hVFKskmieZKlZYpJUqKJUVKyqYmJoamZAp_G4sNNM9mMWLsYmYLdrZilGN08GIPYTC0MzV1comS4mIPdXQRXn78a9ua8sIMUiKcI4yWxpubpBrtnXOMqYOxi5BBg9GCMYKxgXMXIsIGR8QQj4yVGDmdH3wBHT3e_R4y8Hv4hrj7x7o4-Pq5Bka8YQSYBAAQ5MOS3AgAA'),
    add_headers('sec-ch-ua = "Google Chrome";v="105", "Not)A;Brand";v="8", "Chromium";v="105"'),
    add_headers('sec-ch-ua-mobile = ?1'),
    add_headers('sec-ch-ua-platform = "Android"'),
    add_headers('sec-fetch-dest = empty'),
    add_headers('sec-fetch-mode = cors'),
    add_headers('sec-fetch-site = same-origin'),
    add_headers('user-agent = Mozilla/5.0 (Linux; Android 6.0; Nexus 5 Build/MRA58N) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/105.0.0.0 Mobile Safari/537.36'),
    body = sprintf('{"searchCondition":{"masterHotelId":%d,"adult":1,"child":0,"age":"","priceType":1,"priceTypes":"1","cityId":73,"checkIn":"2022-10-26","checkOut":"2022-10-27","roomNum":1,"isBusiness":0},"filterCondition":{},"mapType":null,"isNewMapType":"1","Head":{"Locale":"en-SG","Currency":"SGD","AID":"30074","SID":"1622045","ClientID":"1658849562341.1pmnm5","OUID":"ctag.hash.463e4d8ff02f","CAID":"30074","CSID":"1622045","COUID":"ctag.hash.463e4d8ff02f","TimeZone":"8","PageID":"10320668577","HotelExtension":{"WebpSupport":true,"Qid":201198490140,"hasAidInUrl":false,"group":"TRIP","PID":"1e97f1cd-b771-44f3-9290-d98333b53452"},"Frontend":{"vid":"1658849562341.1pmnm5","sessionID":"9","pvid":"74"},"P":"91033365022","Device":"WAP","Version":"0"}}', as.integer(masterHotelId)),
    encode = "json",
    verbose()
    )
  data <- fromJSON(rawToChar(res$content))
  data <- data$Response$hotelList
  data <- removeData(data)
  data <- unnest(as.data.frame(data), cols = c(base, position, encourage, comment))
  return(data)
}

# with checkin checkout params in "yyyy-mm-dd" format
apiCallNearbyHotel_checkin <- function(masterHotelId){
  res = POST(
    "https://sg.trip.com/restapi/soa2/16708/json/hotelSearchNearby?x-traceID=1658849562341.1pmnm5-1666016479066-1666203502",
    add_headers('authority = sg.trip.com'),
    add_headers('accept = application/json'),
    add_headers('accept-language = en-US,en;q=0.9'),
    add_headers('content-type = application/json;charset=UTF-8'),
    add_headers('cookie = ibu_home_language_match_union_flag=0; _abtest_userid=fb70a796-d789-4b0b-80d8-afdd54b85933; _gcl_au=1.1.562621262.1658849563; _RGUID=751c843f-5682-48a5-a9ff-1e84de692fb8; _RDG=281a5c31eaa24722803983fe33c78ee19f; _RSG=bYaWMz3XSK7u.IQUJe7_dA; _bfaStatusPVSend=1; ibu_online_home_language_match={"isFromTWNotZh" =false,"isFromIPRedirect" =false,"isFromLastVisited" =true,"isRedirect" =false,"isShowSuggestion" =false,"lastVisited" ="https =//sg.trip.com?locale=en-sg"}; cookiePricesDisplayed=SGD; _tp_search_latest_channel_name=hotels; _gid=GA1.2.1760276045.1663081390; _RF1=137.132.215.24; g_state={"i_p" =1663088594680,"i_l" =1}; ibu_h5_site=SG; ibu_h5_group=trip; ibu_h5_local=en-sg; ibu_h5_lang=ensg; ibu_h5_curr=SGD; IBU_TRANCE_LOG_URL=%2Fhotels%2Fsingapore-hotel-detail-996041%2Fparkroyal-on-beach-road-singapore%2F; librauuid=; _clck=5quac4|1|f4u|0; IBU_TRANCE_LOG_P=37682130973; hotel_h5_view_ids=%5B%22996041%22%5D; IBU_showtotalamt=1; ibu-h5-pop-hotel=0; ibu_online_permission_cls_ct=2; ibu_online_permission_cls_gap=1663082802191; hotelhst=1164390341; ibulanguage=SG; ibulocale=en_sg; intl_ht1=h4%3D73_687472%2C73_909897%2C73_994990%2C73_996041; _bfa=1.1658849562341.1pmnm5.1.1658849562341.1663083559409.2.32.10320668588; _bfs=1.18; _ubtstatus=%7B%22vid%22%3A%221658849562341.1pmnm5%22%2C%22sid%22%3A2%2C%22pvid%22%3A32%2C%22pid%22%3A10320668147%7D; hotel=687472; _ga=GA1.2.523270998.1658849562; _uetsid=505649c0337611ed8b2fb3e69a5735ed; _uetvid=3d45d8900cf811edadcee3300718def6; _clsk=kgr7wv|1663083673350|9|0|l.clarity.ms/collect; _bfi=p1%3D10320668147%26p2%3D10320668147%26v1%3D32%26v2%3D31; _bfaStatus=success; _ga_X437DZ73MR=GS1.1.1663081390.3.1.1663083757.0.0.0'),
    add_headers('dnt = 1'),
    add_headers('origin = https =//sg.trip.com'),
    add_headers('p = 37682130973'),
    add_headers('referer = https =//sg.trip.com/hotels/w/detail?hotelid=687472&curr=SGD&locale=en-SG&checkin=2022-09-28&checkout=2022-09-29&hoteluniquekey=H4sIAAAAAAAAAOPqY-HilWA0-A8DjEJMHIxSP5g5fr_dt5rF4regIwMIbJnl4Alm5Mx3COApBDN_tDg0Mgpn-zx-dK7RYRJjLyNn_6GvGjGvGxwEwYzDNQ5KzhzdL5byC4hLbHj3SFWBUROm0RDGsChz1HWWeQ0ywhPGCGLleJApwRLFxrF3jpsEh5IOx_01CwQFtCTOP76npcAKMcQhBGoIkGFxkg-o5yYPUA-DExvHv2XMEiwzGD_80tzICFbyoMhhByPTCcabTAuYLkybwLiLCaroEBMrxy19CZZTQPoJUPslJoZbTAyPmCAOeMXE8IkJ6ohfMC1NzBClXcwMk5ghumcxQ9QvYmY4xQIRusQymTG6Wik7tVLJykRHqSSzJCdVyUopzDFESUcpMTe_NK8EyDUy0TM0BQqUJFZ4poAVJifmJJfmJJakhlQWADWY1epgGhKcWlSWmZyqkJaaimyYsZmepSHCMHMshsXeYmF4xMIUHfuJheEXC8TRTawMXawMk1jZOObtZZJgEWIFR7yUgrGJZVJqmoG5hVFKskmieZKlZYpJUqKJUVKyqYmJoamZAp_G4sNNM9mMWLsYmYLdrZilGN08GIPYTC0MzV1comS4mIPdXQRXn78a9ua8sIMUiKcI4yWxpubpBrtnXOMqYOxi5BBg9GCMYKxgXMXIsIGR8QQj4yVGDmdH3wBHT3e_R4y8Hv4hrj7x7o4-Pq5Bka8YQSYBAAQ5MOS3AgAA'),
    add_headers('sec-ch-ua = "Google Chrome";v="105", "Not)A;Brand";v="8", "Chromium";v="105"'),
    add_headers('sec-ch-ua-mobile = ?1'),
    add_headers('sec-ch-ua-platform = "Android"'),
    add_headers('sec-fetch-dest = empty'),
    add_headers('sec-fetch-mode = cors'),
    add_headers('sec-fetch-site = same-origin'),
    add_headers('user-agent = Mozilla/5.0 (Linux; Android 6.0; Nexus 5 Build/MRA58N) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/105.0.0.0 Mobile Safari/537.36'),
    body = sprintf('{"searchCondition":{"masterHotelId":%d,"adult":1,"child":0,"age":"","priceType":1,"priceTypes":"1","cityId":73,"checkIn":"2022-10-23","checkOut":"2022-10-24","roomNum":1,"isBusiness":0},"filterCondition":{},"mapType":null,"isNewMapType":"1","Head":{"Locale":"en-SG","Currency":"SGD","AID":"30074","SID":"1622045","ClientID":"1658849562341.1pmnm5","OUID":"ctag.hash.463e4d8ff02f","CAID":"30074","CSID":"1622045","COUID":"ctag.hash.463e4d8ff02f","TimeZone":"8","PageID":"10320668577","HotelExtension":{"WebpSupport":true,"Qid":201198490140,"hasAidInUrl":false,"group":"TRIP","PID":"1e97f1cd-b771-44f3-9290-d98333b53452"},"Frontend":{"vid":"1658849562341.1pmnm5","sessionID":"9","pvid":"74"},"P":"91033365022","Device":"WAP","Version":"0"}}', as.integer(masterHotelId)),
    encode = "json",
    verbose()
  )
  data <- fromJSON(rawToChar(res$content))
  data <- data$Response$hotelList
  data <- removeData(data)
  data <- unnest(as.data.frame(data), cols = c(base, position, encourage, comment))
  return(data)
}

# This function helps to remove the unused data, for easy reference on data avail, can check here
removeData <- function(data){
  data$base$picture = NULL
  data$base$starType = NULL
  data$base$badge = NULL
  data$base$hotelUniqueKey = NULL
  data$base$isFavoriteHotel = NULL
  data$money = NULL
  data$position$cityId = NULL
  data$position$cityName = NULL
  data$position$distance = NULL
  data$position$lat = NULL
  data$position$lng = NULL
  data$position$regionType = NULL
  data$position$countryId = NULL
  data$encourage$roomHoldMessage = NULL
  data$encourage$freeBreakfastLabel = NULL
  data$encourage$childrenFreeTag = NULL
  data$encourage$loginGuide = NULL
  data$comment$scoreMax = NULL
  data$seoInfo = NULL
  return(data)
}

# adds column if does not exist
add_cols <- function(df, cols) {
  add <- cols[!cols %in% names(df)]
  if(length(add) != 0) df[add] <- NA
  return(df)
}

# Create a vector hotelIds
hotel_info_df <- read_excel("trip_com_HOTEL_ID.xlsx")
hotelIds <- as.vector(hotel_info_df$HOTEL_ID)
head(hotelIds)
# New hotel ids -> MANAULLY factor in more locations
hotelIds2 <- c(712600, 56704737,7982267,996309, 687490)

# Grabbing hotel information using Rvest - note that price cannot be obtained as it is probably rendered using javascript
h = read_html("https://sg.trip.com/hotels/detail/?cityId=73&hotelId=10231080&checkIn=2022-10-21&checkOut=2022-10-22&adult=1&children=0&subStamp=1456&crn=1&ages=&travelpurpose=0&curr=SGD&link=button&hoteluniquekey=H4sIAAAAAAAAAOPaxcTFJMEoxMTBKLWeiaNl2pFPjBa_BR37D33ViAnudPAEMy72OgTwFDKAgMZEh0bGjAR7eYWQLodJjE2MnGAV7pUOgoW2XNcX15Q6KLlwnDz2g0dARKL50XF5BUZNsEaGfgdDGMMi1jExJEgdZIQnjBHExnFhBqMESxQbx801qRIcSgoKLBCdDVFQnUCGxTPOIFaO84wSTFEMTmwc_3YzSbDMYGz5W7uREazkR6LDDkamA4z_YYDxBOMexgVMU_YX7GKCqj_ExMoxUUyC5RQTwyUmhltMDI-AMs9WAO1-xcTwiQlq_y-Y8iZmhi5mhknMEE2zmKFqFzEzSPEkplkYppkkJRqaJqYoCGmcuHtyFpuR9CRGpmD3U4zChmZmZsYmJkaGpmYWFnoZ5YaFBVbMUoxuHoxBbBbmZi4G5lEyXMzB7i6CHLazQ-d7izhIgXiKMF4Sa2qebrB7xluuAsYGRsYuRg4BRg_GCMYKxleMIIUA8OoWe70BAAA&subChannel=")
hotel_name = h %>% html_nodes(".detail-headline_name  ") %>% html_text()
hotel_amenities = h %>% html_nodes(xpath = '/html/body/div[3]/section/article/div/div[3]/section/div[1]/div[2]/div[1]/p/span')

sample <- apiCallNearbyHotel(hotelIds[1])

# make a post request to get hotel place info
# make a post request to find hotel details around

for(i in hotelIds){
  data <- apiCallNearbyHotel(i)
  data <- add_cols(data, cols)
  df_hotel_info <- rbind(df_hotel_info, as.data.frame(data))
  df_hotel_info <- unique(df_hotel_info)
}

# manual list - some hotels were left out likely due to check in checkout date unavailability! Solution is to manually search
for(i in hotelIds2){
  data <- apiCallNearbyHotel_checkin(i)
  data <- add_cols(data, cols)
  df_hotel_info <- rbind(df_hotel_info, as.data.frame(data))
  df_hotel_info <- unique(df_hotel_info)
}

# Checkpoint - write.csv
df_hotel_info_csv <- subset(df_hotel_info, select = -c(topQuality))
write.csv(df_hotel_info_csv,"hotel_info_list.csv", row.names = FALSE)

# Add point of interest to hotels using API




# Read csv 
hotel_master_data = read.csv("hotel_info_list.csv")
hotel_ids <- hotel_master_data$hotelId

# initialize a new user review data frame
user_review_data <- data.frame(matrix(ncol=13, nrow=0))
cols <- c('hotelId', 'userId','nickname', 'checkinDate','content','travelType', 'travelTypeText', 'ratingAll','ratingLocation','ratingFacility', 'ratingService', 'ratingRoom', 'commentLevel')
colnames(user_review_data) <- cols

# Page should be set to 40 - we can aim to cycle through 3 pages each hotel -> that should obtain us 120 users per hotel and since we have 268 hotels that is 32160 user data
# We hope to obtain repeat users but lets just hope for now
apiCallUserReview <- function(masterHotelId){
  
  user_data <- data.frame(matrix(ncol=13, nrow=0))
  cols <- c('hotelId', 'userId','nickname', 'checkinDate','content','travelType', 'travelTypeText', 'ratingAll','ratingLocation','ratingFacility', 'ratingService', 'ratingRoom', 'commentLevel')
  colnames(user_data) <- cols

  for(i in masterHotelId ){
    for(k in 1:3){
      res = POST(
        "https://sg.trip.com/restapi/soa2/24077/clientHotelCommentList",
        add_headers('User-Agent: Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:106.0) Gecko/20100101 Firefox/106.0'),
        add_headers('Accept: application/json'),
        add_headers('Accept-Language: en-US,en;q=0.5'),
        add_headers('Accept-Encoding: gzip, deflate, br'),
        add_headers('P: 47887882168'),
        add_headers('PID: ce3f61ab-ca1c-40f5-a07e-90268165235b'),
        add_headers('Content-Type: application/json'),
        add_headers('Origin: https://sg.trip.com'),
        add_headers('Connection: keep-alive'),
        add_headers('Cookie: ibulanguage=SG; ibulocale=en_sg; cookiePricesDisplayed=SGD; _abtest_userid=e4a61ab1-1321-4cb1-8067-e3e566e27d8b; IBU_showtotalamt=1; _bfa=1.1666344215688.hw1qp.1.1666442735463.1666448956339.5.37.1; _ubtstatus=%7B%22vid%22%3A%221666344215688.hw1qp%22%2C%22sid%22%3A5%2C%22pvid%22%3A37%2C%22pid%22%3A10320668147%7D; _gcl_au=1.1.813124332.1666344216; _ga_X437DZ73MR=GS1.1.1666414659.2.1.1666417832.0.0.0; _ga=GA1.2.156763681.1666344217; _gid=GA1.2.1653792527.1666344217; _fbp=fb.1.1666344216997.1265240203; _RF1=222.164.149.41; _RSG=9F4m_99a.zCzTlr5WcgIN8; _RDG=28a03e25478b0529ec15ae4adf1c31395a; _RGUID=cf7e9092-7fc0-43c9-85d2-aa6805d6f77d; _bfaStatusPVSend=1; _bfaStatus=fail; hotel=10231080; _uetsid=0a93ffc0512211ed8aa7f31417cc35c1; _uetvid=0a940be0512211eda5b9db1a18f8ee1a; ibu_home_language_match_union_flag=0; ibu_online_home_language_match={"isFromTWNotZh":false,"isFromIPRedirect":false,"isFromLastVisited":false,"isRedirect":false,"isShowSuggestion":false,"lastVisited":"https://sg.trip.com?locale=en-sg"}; _tp_search_latest_channel_name=hotels; IBU_TRANCE_LOG_P=47887882168; IBU_TRANCE_LOG_URL=%2Fhotels%2Fdetail%2F%3FcityId%3D73%26hotelId%3D687592%26checkIn%3D2022-10-21%26checkOut%3D2022-10-22%26adult%3D1%26children%3D0%26subStamp%3D1456%26crn%3D1%26ages%3D%26travelpurpose%3D0%26curr%3DSGD%26link%3Dbutton%26hoteluniquekey%3DH4sIAAAAAAAAAOPaxcTFJMEoxMTBKLWeiaNl2pFPjBa_BR37D33ViAnudPAEMy72OgTwFDKAgMZEh0bGjAR7eYWQLodJjE2MnGAV7pUOgoW2XNcX15Q6KLlwnDz2g0dARKL50XF5BUZNsEaGfgdDGMMi1jExJEgdZIQnjBHExnFhBqMESxQbx801qRIcSgoKLBCdDVFQnUCGxTPOIFaO84wSTFEMTmwc_3YzSbDMYGz5W7uREazkR6LDDkamA4z_YYDxBOMexgVMU_YX7GKCqj_ExMoxUUyC5RQTwyUmhltMDI-AMs9WAO1-xcTwiQlq_y-Y8iZmhi5mhknMEE2zmKFqFzEzSPEkplkYppkkJRqaJqYoCGmcuHtyFpuR9CRGpmD3U4zChmZmZsYmJkaGpmYWFnoZ5YaFBVbMUoxuHoxBbBbmZi4G5lEyXMzB7i6CHLazQ-d7izhIgXiKMF4Sa2qebrB7xluuAsYGRsYuRg4BRg_GCMYKxleMIIUA8OoWe70BAAA%26subChannel%3D; intl_ht1=h4%3D73_10231080%2C73_962739%2C73_687592; librauuid=; hotelhst=1164390341'),
        add_headers('Sec-Fetch-Dest: empty'),
        add_headers('Sec-Fetch-Mode: cors'),
        add_headers('Sec-Fetch-Site: same-origin'),
        add_headers('TE: trailers'),
        body = sprintf('{"hotelId":%d,"pageIndex":%d,"pageSize":40,"orderBy":0,"commentTagList":[],"commentTagV2List":[],"travelTypeList":[],"roomList":[],"packageList":[],"commonStatisticList":[],"UnusefulReviewPageIndex":1,"repeatComment":1,"functionOptions":["IntegratedTARating","hidePicAndVideoAgg"],"webpSupport":true,"platform":"online","pageID":"10320668147","head":{"Version":"","userRegion":"SG","Locale":"en-SG","LocaleController":"en-SG","TimeZone":"8","Currency":"SGD","PageId":"10320668147","webpSupport":true,"userIP":"","P":"47887882168","ticket":"","clientID":"1666344215688.hw1qp","Frontend":{"vid":"1666344215688.hw1qp","sessionID":5,"pvid":37},"group":"TRIP","bu":"IBU","platform":"PC","Union":{"AllianceID":"","SID":"","Ouid":""},"HotelExtension":{"group":"TRIP","hasAidInUrl":false,"Qid":778584087182,"WebpSupport":true,"PID":"ce3f61ab-ca1c-40f5-a07e-90268165235b"}}}', as.integer(i), as.integer(k)),
        verbose()
      )
      data <- content(res)
      if(length(data$groupList) != 0){
        for(j in 1:length(data$groupList[[1]]$commentList)){
          df = data.frame(
            hotelId = i,
            userId = data$groupList[[1]]$commentList[[j]]$userInfo$userId,
            nickname = data$groupList[[1]]$commentList[[j]]$userInfo$nickName,
            checkinDate = data$groupList[[1]]$commentList[[j]]$checkinDate,
            content = data$groupList[[1]]$commentList[[j]]$content,
            travelType = data$groupList[[1]]$commentList[[j]]$travelType,
            travelTypeText = data$groupList[[1]]$commentList[[j]]$travelTypeText,
            ratingAll = data$groupList[[1]]$commentList[[j]]$ratingInfo$ratingAll,
            ratingLocation = data$groupList[[1]]$commentList[[j]]$ratingInfo$ratingLocation,
            ratingFacility = data$groupList[[1]]$commentList[[j]]$ratingInfo$ratingFacility,
            ratingService = data$groupList[[1]]$commentList[[j]]$ratingInfo$ratingService,
            ratingRoom = data$groupList[[1]]$commentList[[j]]$ratingInfo$ratingRoom,
            commentLevel = data$groupList[[1]]$commentList[[j]]$ratingInfo$commentLevel
          )
          user_data <- rbind(user_data, df)
        }
      }
    }
  }
  return(user_data)
}
user_review_data <- apiCallUserReview(hotel_ids)
# shorter testing 
hotelIds3 <- c(97209990, 905760)

user_review_data <- apiCallUserReview(hotelIds3)

# write as csv SUCCESS~!
write.csv(user_review_data,"user_review_data.csv", row.names = FALSE)


# testing code
res = POST(
  "https://sg.trip.com/restapi/soa2/24077/clientHotelCommentList",
  add_headers('User-Agent: Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:106.0) Gecko/20100101 Firefox/106.0'),
  add_headers('Accept: application/json'),
  add_headers('Accept-Language: en-US,en;q=0.5'),
  add_headers('Accept-Encoding: gzip, deflate, br'),
  add_headers('P: 47887882168'),
  add_headers('PID: ce3f61ab-ca1c-40f5-a07e-90268165235b'),
  add_headers('Content-Type: application/json'),
  add_headers('Origin: https://sg.trip.com'),
  add_headers('Connection: keep-alive'),
  add_headers('Referer: https://sg.trip.com/hotels/detail/?cityId=73&hotelId=10231080&checkin=2022-11-21&checkout=2022-11-22&adult=1&children=0&subStamp=1456&crn=1&travelpurpose=0&curr=SGD&link=button&hoteluniquekey=H4sIAAAAAAAAAOPaxcTFJMEoxMTBKLWeiaNl2pFPjBa_BR37D33ViAnudPAEMy72OgTwFDKAgMZEh0bGjAR7eYWQLodJjE2MnGAV7pUOgoW2XNcX15Q6KLlwnDz2g0dARKL50XF5BUZNsEaGfgdDGMMi1jExJEgdZIQnjBHExnFhBqMESxQbx801qRIcSgoKLBCdDVFQnUCGxTPOIFaO84wSTFEMTmwc_3YzSbDMYGz5W7uREazkR6LDDkamA4z_YYDxBOMexgVMU_YX7GKCqj_ExMoxUUyC5RQTwyUmhltMDI-AMs9WAO1-xcTwiQlq_y-Y8iZmhi5mhknMEE2zmKFqFzEzSPEkplkYppkkJRqaJqYoCGmcuHtyFpuR9CRGpmD3U4zChmZmZsYmJkaGpmYWFnoZ5YaFBVbMUoxuHoxBbBbmZi4G5lEyXMzB7i6CHLazQ-d7izhIgXiKMF4Sa2qebrB7xluuAsYGRsYuRg4BRg_GCMYKxleMIIUA8OoWe70BAAA&subChannel='),
  add_headers('Cookie: ibulanguage=SG; ibulocale=en_sg; cookiePricesDisplayed=SGD; _abtest_userid=e4a61ab1-1321-4cb1-8067-e3e566e27d8b; IBU_showtotalamt=1; _bfa=1.1666344215688.hw1qp.1.1666442735463.1666448956339.5.37.1; _ubtstatus=%7B%22vid%22%3A%221666344215688.hw1qp%22%2C%22sid%22%3A5%2C%22pvid%22%3A37%2C%22pid%22%3A10320668147%7D; _gcl_au=1.1.813124332.1666344216; _ga_X437DZ73MR=GS1.1.1666414659.2.1.1666417832.0.0.0; _ga=GA1.2.156763681.1666344217; _gid=GA1.2.1653792527.1666344217; _fbp=fb.1.1666344216997.1265240203; _RF1=222.164.149.41; _RSG=9F4m_99a.zCzTlr5WcgIN8; _RDG=28a03e25478b0529ec15ae4adf1c31395a; _RGUID=cf7e9092-7fc0-43c9-85d2-aa6805d6f77d; _bfaStatusPVSend=1; _bfaStatus=fail; hotel=10231080; _uetsid=0a93ffc0512211ed8aa7f31417cc35c1; _uetvid=0a940be0512211eda5b9db1a18f8ee1a; ibu_home_language_match_union_flag=0; ibu_online_home_language_match={"isFromTWNotZh":false,"isFromIPRedirect":false,"isFromLastVisited":false,"isRedirect":false,"isShowSuggestion":false,"lastVisited":"https://sg.trip.com?locale=en-sg"}; _tp_search_latest_channel_name=hotels; IBU_TRANCE_LOG_P=47887882168; IBU_TRANCE_LOG_URL=%2Fhotels%2Fdetail%2F%3FcityId%3D73%26hotelId%3D687592%26checkIn%3D2022-10-21%26checkOut%3D2022-10-22%26adult%3D1%26children%3D0%26subStamp%3D1456%26crn%3D1%26ages%3D%26travelpurpose%3D0%26curr%3DSGD%26link%3Dbutton%26hoteluniquekey%3DH4sIAAAAAAAAAOPaxcTFJMEoxMTBKLWeiaNl2pFPjBa_BR37D33ViAnudPAEMy72OgTwFDKAgMZEh0bGjAR7eYWQLodJjE2MnGAV7pUOgoW2XNcX15Q6KLlwnDz2g0dARKL50XF5BUZNsEaGfgdDGMMi1jExJEgdZIQnjBHExnFhBqMESxQbx801qRIcSgoKLBCdDVFQnUCGxTPOIFaO84wSTFEMTmwc_3YzSbDMYGz5W7uREazkR6LDDkamA4z_YYDxBOMexgVMU_YX7GKCqj_ExMoxUUyC5RQTwyUmhltMDI-AMs9WAO1-xcTwiQlq_y-Y8iZmhi5mhknMEE2zmKFqFzEzSPEkplkYppkkJRqaJqYoCGmcuHtyFpuR9CRGpmD3U4zChmZmZsYmJkaGpmYWFnoZ5YaFBVbMUoxuHoxBbBbmZi4G5lEyXMzB7i6CHLazQ-d7izhIgXiKMF4Sa2qebrB7xluuAsYGRsYuRg4BRg_GCMYKxleMIIUA8OoWe70BAAA%26subChannel%3D; intl_ht1=h4%3D73_10231080%2C73_962739%2C73_687592; librauuid=; hotelhst=1164390341'),
  add_headers('Sec-Fetch-Dest: empty'),
  add_headers('Sec-Fetch-Mode: cors'),
  add_headers('Sec-Fetch-Site: same-origin'),
  add_headers('TE: trailers'),
  body = sprintf('{"hotelId":%d,"pageIndex":%d,"pageSize":40,"orderBy":0,"commentTagList":[],"commentTagV2List":[],"travelTypeList":[],"roomList":[],"packageList":[],"commonStatisticList":[],"UnusefulReviewPageIndex":1,"repeatComment":1,"functionOptions":["IntegratedTARating","hidePicAndVideoAgg"],"webpSupport":true,"platform":"online","pageID":"10320668147","head":{"Version":"","userRegion":"SG","Locale":"en-SG","LocaleController":"en-SG","TimeZone":"8","Currency":"SGD","PageId":"10320668147","webpSupport":true,"userIP":"","P":"47887882168","ticket":"","clientID":"1666344215688.hw1qp","Frontend":{"vid":"1666344215688.hw1qp","sessionID":5,"pvid":37},"group":"TRIP","bu":"IBU","platform":"PC","Union":{"AllianceID":"","SID":"","Ouid":""},"HotelExtension":{"group":"TRIP","hasAidInUrl":false,"Qid":778584087182,"WebpSupport":true,"PID":"ce3f61ab-ca1c-40f5-a07e-90268165235b"}}}', as.integer(97209990), 1),
  verbose()
)
data = content(res)

for(j in 1:40){
    df = data.frame(
    hotelId = 10231080,
    userId = data$groupList[[1]]$commentList[[j]]$userInfo$userId,
    nickname = data$groupList[[1]]$commentList[[j]]$userInfo$nickName,
    checkinDate = data$groupList[[1]]$commentList[[j]]$checkinDate,
    content = data$groupList[[1]]$commentList[[j]]$content,
    travelType = data$groupList[[1]]$commentList[[j]]$travelType,
    travelTypeText = data$groupList[[1]]$commentList[[j]]$travelTypeText,
    ratingAll = data$groupList[[1]]$commentList[[j]]$ratingInfo$ratingAll,
    ratingLocation = data$groupList[[1]]$commentList[[j]]$ratingInfo$ratingLocation,
    ratingFacility = data$groupList[[1]]$commentList[[j]]$ratingInfo$ratingFacility,
    ratingService = data$groupList[[1]]$commentList[[j]]$ratingInfo$ratingService,
    ratingRoom = data$groupList[[1]]$commentList[[j]]$ratingInfo$ratingRoom,
    commentLevel = data$groupList[[1]]$commentList[[j]]$ratingInfo$commentLevel
  )
  user_review_data <- rbind(user_review_data, df)
}


# Read csv 
user_data = read.csv("workfile_user_review_data.csv")
hotel_master_data = read.csv("hotel_info_list.csv")

user_id_list = unique(user_data$userId_cleaned)
hotel_id_list = unique(hotel_master_data$hotelId)
hotel_name_list = unique(hotel_master_data$hotelName)
                  
# Create matrix user to hotel id with rating all < This will be the first approach 
mat <- matrix(0,
              ncol = length(hotel_id_list), 
              nrow = length(user_id_list)
              )
colnames(mat) <- hotel_id_list
rownames(mat) <- user_id_list

for(i in 1:nrow(user_data)){ 
print(i)
 hotel_index =  which(hotel_id_list == user_data[i,]$hotelId )
 user_index = which(user_id_list == user_data[i,]$userId_cleaned)
 mat[user_index,hotel_index] <- user_data[i,]$ratingAll
}

mat[mat==0] <- NA
colnames(mat) <- hotel_name_list
test <- mat[1:5,]
train <- mat[5:nrow(mat),]
target <- mat[1:10,]

test <- test %>% as("matrix")  %>% as("realRatingMatrix")
train <- train %>% as("matrix")  %>% as("realRatingMatrix")
target <- target %>% as("matrix")  %>% as("realRatingMatrix")

model <- Recommender(train, method = "UBCF")

recommendations <- predict(model, target, type="ratingMatrix")
recommendations %>% as("matrix") %>% View()

recommendations <- predict(model, target)
recommendations %>% as("list") %>% View()


model2 <- Recommender(train, method = "IBCF")

recommendations <- predict(model2, target, type="ratingMatrix")
recommendations %>% as("matrix") %>% View()

recommendations <- predict(model2, target)
recommendations %>% as("list") %>% View()


# Next up : Evaluation 



