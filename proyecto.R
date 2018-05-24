# install.packages("random")
# install.packages("randomNames")
#setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(random)
library(randomNames)

Album<-read.csv("Album.csv", encoding = "UTF-8", check.names = F)
Artist<-read.csv("Artist.csv", encoding = "UTF-8", check.names = F)
Customer<-read.csv("Customer.csv", encoding = "UTF-8", check.names = F)
Employee<-read.csv("Employee.csv", encoding = "UTF-8", check.names = F)
Invoice<-read.csv("Invoice.csv", encoding = "UTF-8", check.names = F)
InvoiceLine<-read.csv("InvoiceLine.csv", encoding = "UTF-8", check.names = F)
MediaType<-read.csv("MediaType.csv", encoding = "UTF-8", check.names = F)
Playlist<-read.csv("Playlist.csv", encoding = "UTF-8", check.names = F)
PlaylistTrack<-read.csv("PlaylistTrack.csv", encoding = "UTF-8", check.names = F)
Track<-read.csv("Track.csv", encoding = "UTF-8", check.names = F)
randoms<-read.csv("randoms.csv", encoding = "UTF-8", check.names = F)



# NEW CUSTOMER ------------------------------------------------------------

CustomerNew<-data.frame(matrix(ncol=length(Customer), nrow=0))

cId<-max(Customer$CustomerId)


cat ("INGRESE NUMERO DE FILAS: ")
n <- scan("stdin", n=1)

#n<-readline(prompt="INGRESE NUMERO DE FILAS: ")
#comentar cat y scan y usar el codigo de arriba para correr en rstudio

i<-1
n<-as.numeric(n)
while(i<n){
  cId<-cId+1
  
  firstName<- randomNames(which.names = "first")
  lastName<- randomNames(which.names = "last")
  
  company<- ""
  address<- toString(randoms$address[sample(1:100, 1)])
  
  country<-toString(randoms$country[sample(1:100, 1)])
  
  if(country=="USA"){
    state<- sample(state.abb, 1)
  }else{
    state<- ""
  }
  city<- toString(randoms$city[sample(1:100, 1)])
  postalCode<- toString(sample(state.area, 1))
  phone<- toString(randoms$phone[sample(1:100, 1)])
  fax<-""
  email<-toString(randoms$email[sample(1:100, 1)])
  
  repId<-unique(Customer$SupportRepId)
  support<-repId[sample(1:length(repId), 1)]
  
  x<-data.frame(cId, firstName, lastName, company, address, city, state, country, postalCode, phone, fax, email, support)
  names(x)<-colnames(Customer)
  
  CustomerNew <- rbind(x, CustomerNew)
  i<-i+1
}
# /NEW CUSTOMER ------------------------------------------------------------


# NEW INVOICELINE ------------------------------------------------------------
InvoiceLineNew<-data.frame(matrix(ncol=length(InvoiceLine), nrow=0))

invoiceLineId<-max(InvoiceLine$InvoiceLineId)
invoiceId<-max(Invoice$InvoiceId)

for(i in min(CustomerNew$CustomerId):max(CustomerNew$CustomerId)){
  invoiceId<-invoiceId+1
  
  n<-sample(1:20, 1)
  j<-1
  while(n>j){
    invoiceLineId<-invoiceLineId+1
    
    track<-Track[sample(1:max(Track$TrackId), 1), ]
    
    
    x<-data.frame(invoiceLineId, invoiceId, track$TrackId, track$UnitPrice, 1)
    names(x)<-colnames(InvoiceLine)
    InvoiceLineNew <- rbind(x, InvoiceLineNew)
    j<-j+1
  }
}
# /NEW INVOICELINE ------------------------------------------------------------


# NEW INVOICE ------------------------------------------------------------
InvoiceNew<-data.frame(matrix(ncol=length(Invoice), nrow=0))

invoiceId<-max(Invoice$InvoiceId)

for(i in min(CustomerNew$CustomerId):max(CustomerNew$CustomerId)){
  invoiceId<-invoiceId+1
  
  customer<-CustomerNew[CustomerNew$CustomerId==i,]

  invoiceDate<- format(Sys.Date(), "%d %m %Y")
  
  total<- as.vector(InvoiceLineNew[InvoiceLineNew$InvoiceId==invoiceId, "UnitPrice"])

  x<-data.frame(invoiceId, i, invoiceDate, customer$Address, customer$City, customer$State, customer$Country, customer$PostalCode, sum(as.numeric(total)))
  names(x)<-colnames(Invoice)
  InvoiceNew <- rbind(x, InvoiceNew)
  
}
# /NEW INVOICE ------------------------------------------------------------


write.csv(CustomerNew, file = "CustomerNew.csv", row.names = FALSE)
print("CREADO CustomerNew.csv")
write.csv(InvoiceNew, file = "InvoiceNew.csv", row.names = FALSE)
print("CREADO InvoiceNew.csv")
write.csv(InvoiceLineNew, file = "InvoiceLineNew.csv", row.names = FALSE)
print("CREADO InvoiceLineNew.csv")
