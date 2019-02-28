
if(!require(pacman)) install.packages("pacman")

pacman::p_load(
  tidyverse, data.table, stringr, lubridate, xml2, tm, varhandle,
  foreach, magrittr, RCurl, ssh, devtools, rvest, robotstxt)

products <- data.frame(NULL, stringsAsFactors = F)
prod <- data.frame(NULL, stringsAsFactors = F)
#-----------------

wbpg_main <- "https://www.fritz-berger.de/"
output_file_csv <-  paste0("./data/", str_extract(wbpg_main, "(?<=www\\.)[\\w-_]+(?=\\.)" ),
                           "_", str_replace_all(today(),"-",""),"_.csv")
if (file.exists(output_file_csv)) file.remove(output_file_csv)

rtxt <- robotstxt::get_robotstxt(domain = wbpg_main)
rtxt
# paths_allowed(paths = "/", domain = wbpg_main, bot = "*",
#               user_agent = utils::sessionInfo()$R.version$version.string,
#               warn = TRUE, force = FALSE,
#               ssl_verifypeer = c(1, 0), use_futures = TRUE, robotstxt_list = NULL)

uk_container <- read_html(wbpg_main) %>%
  html_nodes(xpath='//ul[@class="uk-navbar-nav"]//a[@href]')

wbpg_list <- data.frame(links = uk_container %>% map_chr(html_attr, 'href'),
                        subitems = uk_container %>% map_chr(html_attr, 'class'),
                        stringsAsFactors = F) %>%
  filter(!is.na(subitems)) %>%
  pull(links)
#          !str_detect(links, "https://www.fritz-berger.de/[^\\/]+/$")) %>%


foreach::foreach(i = seq_along(wbpg_list)) %do% {
  wbpg_ <- wbpg_list[[i]]
  Sys.sleep(sample(20, 1))
  pagination <- NULL
  uk_pagination <- read_html(wbpg_) %>%
    html_nodes(xpath='//ul[@class="uk-pagination uk-pagination-right"]//a') %>%
    html_attr("href") %>%
    #map_chr(., `[[`, "href") %>%
    unique() %>% sort()
  # pg.max <- 0
  if (length(uk_pagination) > 0) {
    # pg.max <- max(as.integer(str_extract(uk_pagination, "\\d+$")))
    pagination <- c(wbpg_, paste0("https://www.fritz-berger.de", uk_pagination))
  } else {
    pagination <- wbpg_
  }

  foreach(j = seq_along(pagination)) %do% {
    wbpg_ <- pagination[[j]]
    Sys.sleep(sample(10, 1))
    uk_description <- read_html(wbpg_) %>%
      html_nodes(xpath='//div[@class="description"]/a')

    #links
    prod.link <- uk_description %>% map_chr(xml_attr,"href")

    prod.name <- uk_description %>%
      html_nodes(xpath='//a/div[@class="product-description"]/span') %>%
      html_text() %>%
      str_replace_all('\n|\\s{2,}', '')

    prod.price <- uk_description %>%
      html_nodes(xpath='//a//div[@class="prod_price"]') %>%
      map(html_text) %>%
      str_replace_all('[\n€\\.]|(ab)|\\s{2,}', '') %>%
      str_replace_all('\\,', '.') %>%
      str_extract('\\d+\\.\\d+(?=\\s$)') %>%
      as.numeric()

    prod <-  data.frame(prod.name, prod.link, prod.price,
                        wbpg_ = str_extract(wbpg_, "(?<=de/)[\\w\\d\\-\\_\\/]+(?=(\\/$|\\/?))"), time_ = today(),
                        stringsAsFactors = F)

    products <- products %>% bind_rows(prod)
    products <- distinct_all(products)
  }

}

fwrite(products, output_file_csv, append = F, sep="\t", col.names = T, showProgress = T)



