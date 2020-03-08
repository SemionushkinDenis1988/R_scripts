# step 1 fread function

library(data.table)

system.time(fread("products.csv"))
system.time(read.table("products.csv", header = T, sep = ";"))

products <- fread("products.csv")


# step 2 data.table vs dataframe
products[1:10, ]

products[products$price > 10000, ]

with(iris, iris[Species == "virginica", ])
#iris$Species
products[price > 10000]

products[(price > 1000) & 
           (brand %in% c("Epson", "Apple"))]


# step 3 data filtering

products[available, ]
products[available == TRUE, ]

products[3, ]
iris[3, ]
products[3]
iris[3]

products[!(brand %in% c("Apple", "Epson"))]

products[!(1:10)]


# step 4 data transformation

products[, list(name,
                price.1k = price / 1000)]

order(products$price, decreasing = T)
products[order(price, decreasing = T)]
products[order(price, decreasing = T), list(name, price.1k = price / 1000)]

products[order(price, decreasing = T),
         list(name, price.1k = paste0(price / 1000, " тыс.руб"))]

head(products[order(price, decreasing = T), 
              list(name, price.1k = paste0(price / 1000, " тыс.руб"))], 5)


# step 5 data transformation advanced


products[order(price, decreasing = T),
         list(price.1k = paste0(price / 1000, " тыс.руб"))]$price.1k

products[, list(name, price)]
products[, .(name, price)]

products[, c("name", "price"), with = F]

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

library(data.table)


products <- fread("products.csv", colClasses = c(price = "double"))


products[price<1000,
         name.with.price := paste0(name, " ( ", price, " руб.)")]
products[order(-price)]


products[, price := price / max(price), by=brand]

products

library(data.table)

purchases <- fread("purchases.csv")
products <- fread("products.csv")


setkey(purchases, product_id, externalsessionid)
setkey(products, product_id, brand)



key(purchases)
key(products)


merge(purchases, products, by = "product_id")
# merge(purchases, products, by = c("col1", "col2"))
merge(purchases, products, by.x = "product_id", by.y = "product_id")
merge(purchases, products, all.x = T, all.y = F)
merge(purchases, products)


purchases[products, on = "product_id"]

purchases[products]




setkey(products, product_id, price)
setkey(purchases, product_id, ordernumber)
purchases[products]





setkey(purchases, product_id, externalsessionid)
setkey(products, product_id, brand)

products[purchases]



# J, SJ, CJ
products[J(c(158, 208, 10001, 826355, 958238))]
products[data.table(
  c(158, 208, 10001, 826355, 958238)
)]
products[.(c(158, 208, 10001, 826355, 958238))]
products[list(c(158, 208, 10001, 826355, 958238))]

print(SJ(c(158, 208, 10001, 826355, 958238)))
key(SJ(c(158, 208, 10001, 826355, 958238)))

print(CJ(c(158, 826355, 958238),
         c("Supra", "Func")))
key(CJ(c(158, 826355, 958238),
       c("Supra", "Func")))

library(data.table)

purchases <- fread("purchases.csv")
products <- fread("products.csv")

purchases.with.brands <- merge(
  purchases,
  products[, list(product_id, brand)],
  by="product_id"
)

pop.20.brands <- head(
  purchases.with.brands[, 
                        list(
                          total.brand.users = length(unique(externalsessionid))
                        ),
                        by=brand][order(-total.brand.users)], 20)

users <- purchases.with.brands[, list(unique.brands = length(unique(brand)),
                                      items = .N,
                                      brand = brand[1]),
                               by=externalsessionid]

brand.loyal.users <- users[items > 1][unique.brands == 1][, list(total.loyal.users = .N), by=brand]

brand.stats <- merge(
  pop.20.brands,
  brand.loyal.users,
  by="brand"
)

brand.stats[, loyal := total.loyal.users / total.brand.users]

brand.stats[order(-loyal)]

