
library(rvest)
library(plyr)
library(dplyr)
library(tidyr)
library(stringr)


# Harvesting data from the USDA portal
address <- "https://www.justice.gov/atr/ag-workshops-alphabetical-list-individuals-or-entities-who-submitted-comments"

# Getting alist of links which we need to harves and Combing
#   all links into a dataframe
links_df <- 
  
  # Getting an html page
  read_html(address) %>% 
  
  # Extracting node with data which we need
  html_nodes(., "p:nth-child(6)") %>% 
  
  # Converting node data into a one big charactar
  as.character() %>% 
  
  # Spliting one big character node into a list of atomic vectors
  strsplit(., "<br>") %>% 
  unlist() %>% 
  
  # Parsing atomic vectors to ceparate it into future columns
  str_replace(., ".+?(?=\\/atr)", "") %>% 
  str_replace(., "\">", ";") %>% 
  str_replace(., "<\\/a>.+?(?=[a-z,A-Z])", ";") %>% 
  as.data.frame() %>% 
  
  # Renaming colunms named in a default way
  rename_(.dots = setNames(names(.), "Vars")) %>%
  
  # Separating one column into three different
  separate(Vars, c("link", "code", "name"), sep = ";") %>% 
  
  # Expanding path to the information using the absolute path
  mutate(link = str_c("https://www.justice.gov", link)) %>% 
  
  # Making good data frame
  tbl_df  



# Preparing code for harvesting one data object with data from the list of links

# sample_data <- 
#   links_df %>% 
#   sample_n(10)

# Hatvesting one node function --------------------------------------------

# Harvesging strategy
# 1. Use Link and Node code to get this node.
# 2. Check if this node has any resul and produce an error code if it doesnot
# 3. Add node to the overall data

# Getting data of one node in the character format
harv_nodes <-
  function(link, node_code = ".node__content") {
    
  node_data <- try(read_html(link), silent = T) 
  node_data <- try(html_nodes(node_data, node_code), silent = T)

  if(class(node_data) == "try-error") {
    problem <- TRUE
    node_data <- NA
  } else {
    node_data <- as.character(node_data)
    problem <- FALSE
  }
  
  return(list(node_data, problem))
  
  }

# Harvesting all website --------------------------------------------------


# Initializing firtst line of harvested data
harvested_data <- 
  links_df %>% 
  slice(1) %>%
  mutate(
    node = as.character(harv_nodes(link)[[1]]),
    problem = harv_nodes(link)[[2]]
  )

# Harvesting all pages
d_ply(links_df[2:nrow(links_df),],
      .(link, code),
      function(y) {
        node_data <- harv_nodes(y$link)
        harvested_data <<-
          bind_rows(
            harvested_data,
            mutate(
              y,
              node = as.character(node_data[[1]]),
              problem = node_data[[2]]
              ))
      },
      .progress = "text") %>%
  tbl_df()

# Saving harvested results
save(harvested_data, file = "data/main_pages.Rdata")

write.csv(harvested_data, "data/main_pages.csv", row.names = FALSE)
