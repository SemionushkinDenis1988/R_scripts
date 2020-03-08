library(data.table)

products <- fread("products.csv")

products[1:10, ]

products[products$price >10000, ]

with(iris, iris[species == "virginica", ])

products[products$price >10000, ]

products[price >10000]
 
products[(price > 10000) & 
           (brand %in% c("Epson", "Apple"))]

products[available == TRUE, ]

products[3, ]
iris[3, ]
products[3]
iris[3]

products[!(brand %in% c("Apple", "Epson"))]

products[!c(1:10)]

products[, list(name = name, 
                price.1k = price / 1000)]

products[, list(name, 
                price.1k = price / 1000)]

order(products$price, decreasing = T) #возвращает индексы 

products[order(products$price, decreasing = T), list(name, 
                                                     price.1k = price / 1000)]

products[order(products$price, decreasing = T), list(name, 
                                                     price.1k = paste0(price / 1000, " тыс.руб"))]

head(products[order(products$price, decreasing = T), list(name, 
                                                          price.1k = paste0(price / 1000, " тыс.руб"))], 5)

products[order(products$price, decreasing = T), list(name, 
                                                     price.1k = paste0(price / 1000, " тыс.руб"))]$price.1k

products[, .(name, price)] == products[, .(name, price)]

products[, .(name, price), with = F] #with = F означает работу с датафреймом

products[order(-price), .(name = head(name), 
                          price = head(price))]
products[, .(price = sum(price))]

a <- products[, list(name.with.brand = paste0(brand, " - ", name))]
a[order(name.with.brand)]

products[, list(name.with.brand = paste0(brand, " - ", name))][order(name.with.brand)]

products[, .(price = {
  a <- mean(price)
  b <- median(price)
  c(min(price), max(price), a/b)
})]

products[, .(mean.price = mean(price)), , by = brand]

products[order(-price), .(name = head(name, 3), 
                          price = head(price, 3)), by = brand]
#задача 1

filter.expensive.available <- function(products, brands){
  products[(price >= 500000) & (brand %in% brands) & (available == T)]
}

filter.expensive.available(products, c("Lenovo", "Gefest"))

#задача 2
purchases <- fread("purchases.csv")

ordered.short.purchase.data <- function(purchases){
  purchases[order(-price)][!quantity<0][,.(ordernumber, product_id)]
}

ordered.short.purchase.data<- function(purchases) {    
  purchases[order(-price)][quantity >= 0][, .(ordernumber, product_id)]
  }


ordered.short.purchase.data(sample.purchases)

purchases <- sample.purchases
ordered.short.purchase.data(purchases)

sample.purchases <- data.table(price = c(100000, 6000, 7000, 5000000),
                               ordernumber = 1:4,
                               quantity = c(1,2,1,-1),
                               product_id = 1:4)

sample.purchases[!sample.purchases$quantity<0]

#задача 3
purchases.median.order.price <- function(purchases){
  median(purchases[quantity >= 0,.(price = sum(price*quantity)), by = .(ordernumber)]$price, na.rm = T)
}

purchases.median.order.price <- function(purchases) {    
  median(purchases[quantity >= 0][, list(w = sum(price * quantity)), by=list(ordernumber)]$w)}


purchases.median.order.price(sample.purchases)

sample.purchases <- data.table(price = c(100000, 6000, 7000, 5000000),
                               ordernumber = c(1,2,2,3),
                               quantity = c(1,2,1,-1),
                               product_id = 1:4)
