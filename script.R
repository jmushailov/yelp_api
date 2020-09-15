# Clear workspace
rm(list=ls())
# Package for making our tables clean
require(tidyverse)
# Wrapper for CURL
require(httr)
# Reads text files
require(readr)


# Defining the Function to search and parse
YelpScr <- function(offset=offsetP){
  
  # Define our storage dataframe where we will store results
  storage = data.frame()
  
  for(i in 1:length(offsetP)){  
    
    url <- modify_url(yelp, path = c("v3", "businesses", "search"),
                      query = list(term = term, location = location, 
                                   limit = limit,
                                   radius = radius,
                                   # For next pages -> offset 51 for pg 2, 101 for pg 3.....
                                   offset = offsetP[i]
                      ))
    
    # Our GET request, querying the server
    res <- GET(url, add_headers('Authorization' = paste("bearer", client_key)))
    
    # Getting our results
    results <- content(res)
    
    # Parsin our results
    yelp_httr_parse <- function(x) {
      
      # Defining the variables we want
      parse_list <- list(id = x$id, 
                         name = x$name, 
                         rating = x$rating, 
                         review_count = x$review_count, 
                         latitude = x$coordinates$latitude, 
                         longitude = x$coordinates$longitude, 
                         address1 = x$location$address1, 
                         city = x$location$city, 
                         state = x$location$state, 
                         zip_code = x$location$zip_code,
                         distance = x$distance,
                         phone = x$phone,
                         url = x$url
      )
      
      # Quick lapply to clean the data up
      parse_list <- lapply(parse_list, FUN = function(x) ifelse(is.null(x), "", x))
      
      # Making our data into a tibble for easy access
      df <- tibble(id=parse_list$id,
                   name=parse_list$name, 
                   rating = parse_list$rating, 
                   review_count = parse_list$review_count, 
                   latitude=parse_list$latitude, 
                   longitude = parse_list$longitude, 
                   address1 = parse_list$address1, 
                   city = parse_list$city, 
                   state = parse_list$state, 
                   zip_code = x$location$zip_code,
                   distance = x$distance,
                   phone = x$phone,
                   url = x$url)
      df
    }
    
    # Results list
    results_list <- lapply(results$businesses, FUN = yelp_httr_parse)
    
    payload <- do.call("rbind", results_list)
    
    
    # Show analysis on every pageg
    ratingsCalc <- function(payload){
      x <- payload$rating * payload$review_count 
      x2 <- round( (sum(x) / sum(payload$review_count)), 2)
      print(paste0("Average Yelp rating for stores within a radius of about ", radius, " feet of ", location, 
                   ", using the search term ***", term,  "*** ", "is: ", x2, " stars. This is calculated using a total of ", sum(payload$review_count), " reviews"))
      
    }
    
    # Show the messages as it queries Yelp
    msg <- ratingsCalc(payload)
    strsplit(msg, "\n")
    
# Store the results in our dataframe
storage = rbind(storage, payload)    

    
    if(sum(payload$review_count)==0){ break}
  }
  # Return our final storage DF
  return(storage)
  }







# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                 Running Our Code
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# We want to avoid explicitly having our client id/key in our code. Instead we can do 3 things:
  # 1) Open a .txt with our id/key in rows 1/2, respectively
  # 2) Use the R Keyring library to save your keys 
  # 3) (Wouldn't recommend) Passing your keys as global objects manually

# Opening text file, reading the keys, closing
f=open('C:/path/of/your/credentials.txt',"r")
lines=f.readlines()
client_id=lines[0]
client_key=lines[1]
f.close()


# POST command to access API
res <- POST("https://api.yelp.com/oauth2/token",
            body = list(grant_type = "client_credentials",
                        client_id = client_id))

# Generate our token
token <- content(res)$access_token

# Search parameters
yelp <- "https://api.yelp.com" # Base URL
term <- "Avacado Toast" # Search term
location <- "10013" # Manhattan Zip Code
categories <- NULL # Optional filter, looking at specific category of place... ex: bar, park, etc.
limit <- 50 # Biggest limit of results Yelp returns per page
radius <- 16000 # Radius, in meters from location. Max is 40,000 (25 miles)

# Number of pages you want to query. 50 = 1 page of results. So 600 is 600/50 = 12 pages of results
  # The function automatically stops if it reaches an empty page
offsetP <- seq(1,601, 50)

# This will query our database with our function!
storage <- YelpScr(offsetP)


# Export it
save_path = "C:/users/path/to/where/you/want/it"
nameOfExport <- paste0(term,"_", radius, "_meter_radius_of_",location)
write.table(storage, paste0(save_path, nameOfExport, ".csv"), row.names = FALSE)





