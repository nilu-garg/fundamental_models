
#library(fpc,cluster,factoextra,gtools)

domain <- "pega.com"
companyname <- "Pegasystems (PEGA)"
metrics <- as.data.frame(gs_read(ss=be, ws = companyname))
metrics$Start <- as.Date(metrics$Start, format = "%m/%d/%Y")
metrics$End <- as.Date(metrics$End, format = "%m/%d/%Y")
sel_metric <- "Total Revenue ($ millions)"
metrics <- metrics %>% select("Start","End",sel_metric)
platform = 'total'

con <- dbConnect(RMySQL::MySQL(), host = keys$host,dbname = keys$dbname, user=keys$id,password=keys$pw)
domain_id <- get_domain_id(con,domain)
engagement <- get_visits_table(con,domain_id,platform)
engagement <- engagement[order(engagement$date),] 
eng <- engagement
tom <- metrics[,c(1,2)]
colnames(tom) <- c("date","q")
mr <- left_join(x = engagement,y = tom,by = "date")
firstdate <- mr %>% dplyr::filter(!is.na(q)) %>% slice(1)
qmonth <- firstdate$date
engagement <- m_to_q(mr,qmonth)

pp_data <- get_pp_table(con,domain_id,platform)


setDT(pp_data)[, "firstfolder" := tstrsplit(pg_url, "/")[1]][,"keywords" := gsub("/",", ",gsub("[&%.?=+-]","/",gsub(domain,"",tolower(pg_url))))]
pp_data = pp_data %>% mutate("pg_url" = tolower(pg_url)) %>% mutate("len" = nchar(as.character(pg_url)))  %>% mutate("sbdc" = str_count(firstfolder,"\\.")) %>%
          mutate("count_#" = str_count(pg_url,'#')) %>% mutate("count_/" = str_count(pg_url,'/')) %>% mutate('count_='= str_count(pg_url,'\\=')) %>% 
          mutate('count_?'= str_count(pg_url,'\\?')) %>% mutate ('count_&'= str_count(pg_url,'\\&')) %>% mutate('count_%'= str_count(pg_url,'\\%')) %>% 
          mutate('count_='= str_count(pg_url,'\\=')) %>% mutate('count_+'= str_count(pg_url,'\\+'))

kf <- data.frame(words = unlist(strsplit(tolower(pp_data$keywords), ", "))) %>% group_by(words) %>% 
  dplyr::summarise(n = n()) %>% arrange(desc(n))
kf = kf[c(2:31),]

for (i in 1:nrow(kf)){
  w <- toString(kf$words[i])
  cn = paste0("if_",w)
  pp_data[,cn] <- as.logical(lapply(pp_data$pg_url, chkFUN, w))
  }

pp_c <- pp_data %>% select(-"share",-"keywords",-"date",-"firstfolder")
#pp_c <- pp_c[,-c(1)]
pp_c <- distinct(pp_c)
#pp_c1 <- pp_c[pp_c$len <= 256,]
pp_c <- na.omit(pp_c)
#pp_c$firstfolder <- as.factor(pp_c$firstfolder)
pp_c1 <- pp_c$pg_url
pp_c <- pp_c %>% select(-"pg_url")
pp_c[,c(1:9)] <- scale(pp_c[,c(1:9)])

eps = 0.2
minpts = ncol(pp_c) + 1

clust_func <- function(pp_c,eps,minpts){
  db <- fpc::dbscan(pp_c, eps = eps, MinPts = minpts,method = "hybrid",showplot = 1)
  return(db$cluster)
}

db <- clust_func(pp_c,eps,minpts)

pp_c <- cbind(pp_c1,db$cluster,pp_c)

setnames(pp_c,"pp_c1","pg_url")
setnames(pp_c,"db$cluster","cluster")
pp_clust_list <- pp_c %>% select("pg_url","cluster")

cp = max(pp_c$cluster)
cmb = combinations(cp+1,3,c(0:cp),repeats.allowed = F)

results <- data.frame()
for(form in c("visits","pageviews")){
  con <- dbConnect(RMySQL::MySQL(), host = keys$host,dbname = keys$dbname, user=keys$id,password=keys$pw)
  pp_data <- get_pp_table(con,domain_id,platform)
  pp_data <- merge(pp_data,engagement,by = 'date')
  pp_data$visits <- pp_data$visits * pp_data$share
  pp_data$pageviews <- pp_data$pageviews * pp_data$share
  pp_data <- merge(pp_data,pp_clust_list,by= "pg_url")
  setnames(pp_data,form,"page_activity")
  for(i in 1:nrow(cmb)){
    tryCatch({
      print(i)
      res <- do_reg_clust(pp_data,metrics[,c("Start", sel_metric)],form,"yoyper",platform,sel_metric,cmb[i,])
      results <- rbind(results, res)
      res <- do_reg_clust(pp_data,metrics[,c("Start", sel_metric)],form,"yoydiff",platform,sel_metric,cmb[i,])
      results <- rbind(results, res)
    },error = function(err){
      print(paste0("Error with domainlist ",cmb[i,]) )  }
    )
  }
}

clust_results <- pp_c%>% dplyr::select(-pg_url) %>%
  group_by(cluster) %>% do(the_summary = summary(.))



cl_select = 24
pp_c1 <- pp_c %>% filter(cluster==cl_select)
clust_rows <- nrow(pp_c1)
setDT(pp_c1)[, "firstfolder" := tstrsplit(pg_url, "/")[1]][,"keywords" := gsub("/",", ",gsub("[&%.?=+-]","/",gsub(domain,"",tolower(pg_url))))]
kf <- data.frame(words = unlist(strsplit(tolower(pp_c1$keywords), ", "))) %>% group_by(words) %>% 
  dplyr::summarise(n = n()) %>% arrange(desc(n))

#pp_data = pp_data[order(pp_data$date,-pp_data$share),]
#pp_data1 <- pp_data %>% group_by(date) %>% filter(share > mean(share,na.rm = T))
#pp_data2 <- pp_data[pp_data$len <= 256,]
#pp_data[,'marketing'] <- as.logical(lapply(pp_data$pg_url, chkFUN, "utm"))


#Plot DBSCAN results
#plot(db, pp_c, main = "DBSCAN", frame = FALSE)
#fviz_cluster(db, pp_c, stand = FALSE, frame = FALSE, geom = "point")

#g.dist1 = daisy(pp_c,metric = "gower")
#gower_mat <- as.matrix(g.dist1)
#pc = pamk(g.dist1,krange = 1:50)
#dp =  data.frame(pc$pamobject$clustering)
#dbscan::kNNdistplot(pp_c, k =  50)
#abline(h = 20, lty = 3)