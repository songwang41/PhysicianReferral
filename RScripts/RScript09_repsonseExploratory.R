load("./Data/Payment_a_2013.RData")
service <- Payment[,c("hcpcs_code","hcpcs_description")]
service <- cbind(service,1)
code <- service$hcpcs_code
desp <- service$hcpcs_description

 aggdata <-aggregate(x = service, by= list(code,desp), 
                    FUN=sum, na.rm=TRUE)

attach(mtcars)
aggdata <-aggregate(mtcars, by=list(cyl,vs), 
                    FUN=sum, na.rm=TRUE)
print(aggdata)
detach(mtcars)
