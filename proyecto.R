# install.packages("RPostgreSQL")
# install.packages("random")
# install.packages("randomNames")

require("random")
require("randomNames")
require("RPostgreSQL")

#setwd(dirname(rstudioapi::getSourceEditorContext()$path))


# CONNCECTION AND TABLES --------------------------------------------------
{
  cat("INGRESE NOMBRE DE DATABASE: ");
  db <- readLines("stdin",n=1);
  cat("INGRESE NOMBRE DE USUARIO: ");
  us <- readLines("stdin",n=1);
  cat("INGRESE PASSWORD: ");
  ps <- readLines("stdin",n=1);
}


drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = db, host = "localhost", port = 5432, user = us, password = ps)

randoms<-read.csv("MOCK_DATA.csv", encoding = "UTF-8", check.names = F)


{
  cat("INGRESE DIAS PARA SIMULAR: ");
  days <- readLines("stdin",n=1);
  cat("SE SIMULARAN", days, "DIAS\n")
}

days<-as.numeric(days)
d<-1
for(d in 1:days){
  cat("DIA",d,"\n")
  
  # NEW EMPLOYEE ------------------------------------------------------------
  Employee<-dbGetQuery(con, 'SELECT * FROM "Employee"')
  EmployeeNew<-data.frame(matrix(ncol=length(Employee), nrow=0))
  
  eId<-max(Employee$EmployeeId)
  
  i<-1
  n<-sample(1:5,1)
  while(i<=n){
    eId<-eId+1
    
    firstName<- randomNames(which.names = "first")
    lastName<- randomNames(which.names = "last")
    
    title<- toString(randoms$title[sample(1:1000, 1)])
    rep<-sample(1:6, 1)
    
    birth<- as.Date(paste(sample(1947:1995,1), sample(1:12,1), sample(1:30,1), sep = "-"))
    hire<- as.Date(paste(sample(2002:2010,1), sample(1:12,1), sample(1:30,1), sep = "-")) 
    
    address<- toString(randoms$address[sample(1:1000, 1)])
    
    country<-"United States"#toString(randoms$country[sample(1:1000, 1)])
    
    if(country=="United States"){
      state<- sample(state.abb, 1)
    }else{
      state<- ""
    }
    city<- toString(randoms$city[sample(1:1000, 1)])
    postal<- toString(sample(state.area, 1))
    phone<- toString(randoms$phone[sample(1:1000, 1)])
    fax<- toString(randoms$phone[sample(1:1000, 1)])
    email<-toString(randoms$email[sample(1:1000, 1)])
    
    x<-data.frame(eId, firstName, lastName, title, rep, birth, hire, address, city, state, country, postal, phone, fax, email)
    names(x)<-colnames(Employee)
    
    EmployeeNew <- rbind(x, EmployeeNew)
    i<-i+1
  }
  dbWriteTable(con, "Employee", value = EmployeeNew, append = TRUE, row.names = FALSE)
  cat("SE AGREGARON",nrow(EmployeeNew),"FILAS A LA TABLA Employee\n")
  # /NEW EMPLOYEE ------------------------------------------------------------
  
  
  # NEW Artist ------------------------------------------------------------
  Artist<- dbGetQuery(con, 'SELECT * FROM "Artist"')
  ArtistNew<-data.frame(matrix(ncol=length(Artist), nrow=0))
  
  aId<-max(Artist$ArtistId)
  
  
  i<-1
  n<-sample(1:10,1)
  while(i<=n){
    aId<-aId+1
    
    x<-sample(1:4, 1)
    
    if(x==1){
      firstName<- randomNames(which.names = "first")
      lastName<- randomNames(which.names = "last")
      name<-paste(firstName, lastName)
    }
    if(x==2){
      firstName<- toString(randoms$phrase1[sample(1:1000, 1)])
      lastName<- toString(randoms$phrase1[sample(1:1000, 1)])
      name<-paste(firstName, lastName)
    }
    if(x==3){
      name<- toString(randoms$appname[sample(1:1000, 1)])
    }
    if(x==4){
      firstName<- toString(randoms$words1[sample(1:1000, 1)])
      lastName<- toString(randoms$words2[sample(1:1000, 1)])
      name<-paste(firstName, lastName)
    }
    
    
    x<-data.frame(aId, name)
    names(x)<-colnames(Artist)
    
    ArtistNew <- rbind(x, ArtistNew)
    i<-i+1
  }
  dbWriteTable(con, "Artist", value = ArtistNew, append = TRUE, row.names = FALSE)
  cat("SE AGREGARON",nrow(ArtistNew),"FILAS A LA TABLA Artist\n")
  # /NEW Atrtist ------------------------------------------------------------
  
  
  # NEW Album ------------------------------------------------------------
  Artist<- dbGetQuery(con, 'SELECT * FROM "Artist"')
  Album<- dbGetQuery(con, 'SELECT * FROM "Album"')
  AlbumNew<-data.frame(matrix(ncol=length(Album), nrow=0))
  
  bId<-max(Album$AlbumId)
  
  
  i<-1
  n<-sample(1:10, 1)
  while(i<=n){
    bId<-bId+1
    
    x<-sample(1:4, 1)
    
    if(x==1){
      title<- toString(randoms$phrase1[sample(1:1000, 1)])
    }
    if(x==2){
      firstName<- toString(randoms$phrase1[sample(1:1000, 1)])
      lastName<- toString(randoms$phrase2[sample(1:1000, 1)])
      title<-paste(firstName, lastName)
    }
    if(x==3){
      title<- toString(randoms$words1[sample(1:1000, 1)])
    }
    if(x==4){
      firstName<- toString(randoms$words1[sample(1:1000, 1)])
      lastName<- toString(randoms$words2[sample(1:1000, 1)])
      title<-paste(firstName, lastName)
    }
    
    aId<- sample(min(Artist$ArtistId):max(Artist$ArtistId), 1)
    
    x<-data.frame(bId, title, aId)
    names(x)<-colnames(Album)
    
    AlbumNew <- rbind(x, AlbumNew)
    i<-i+1
  }
  dbWriteTable(con, "Album", value = AlbumNew, append = TRUE, row.names = FALSE)
  cat("SE AGREGARON",nrow(AlbumNew),"FILAS A LA TABLA Album\n")
  # /NEW Album ------------------------------------------------------------
  
  
  # NEW TRACK ------------------------------------------------------------
  Album<- dbGetQuery(con, 'SELECT * FROM "Album"')
  Track<- dbGetQuery(con, 'SELECT * FROM "Track"')
  
  TrackNew<-data.frame(matrix(ncol=length(Track), nrow=0))
  
  tId<-max(Track$TrackId)
  
  for(i in min(AlbumNew$AlbumId):max(AlbumNew$AlbumId)){
    
    n<-sample(1:10, 1)
    j<-1
    while(n>=j){
      
      tId<-tId+1
      
      x<-sample(1:4, 1)
      
      if(x==1){
        name<- toString(randoms$phrase1[sample(1:1000, 1)])
      }
      if(x==2){
        firstName<- toString(randoms$phrase1[sample(1:1000, 1)])
        lastName<- toString(randoms$phrase2[sample(1:1000, 1)])
        name<-paste(firstName, lastName)
      }
      if(x==3){
        name<- toString(randoms$words1[sample(1:1000, 1)])
      }
      if(x==4){
        firstName<- toString(randoms$words1[sample(1:1000, 1)])
        lastName<- toString(randoms$words2[sample(1:1000, 1)])
        name<-paste(firstName, lastName)
      }
      
      aId<- i
      
      type<-sample(min(Track$MediaTypeId):max(Track$MediaTypeId), 1)
      
      genre<-sample(min(Track$GenreId):max(Track$GenreId), 1)
      
      firstName<- randomNames(which.names = "first")
      lastName<- randomNames(which.names = "last")
      composer<-paste(firstName, lastName)
      
      ms <- sample(min(Track$Milliseconds):max(Track$Milliseconds), 1)
      
      bytes <- sample(min(Track$Bytes):max(Track$Bytes), 1)
      
      price<- 0.99
      
      x<-data.frame(tId, name, aId, type, genre, composer, ms, bytes, price)
      names(x)<-colnames(Track)
      TrackNew <- rbind(x, TrackNew)
      j<-j+1
    }
  }
  dbWriteTable(con, "Track", value = TrackNew, append = TRUE, row.names = FALSE)
  cat("SE AGREGARON",nrow(TrackNew),"FILAS A LA TABLA Track\n")
  # /NEW TRACK ------------------------------------------------------------
  
  # NEW CUSTOMER ------------------------------------------------------------
  Customer<-dbGetQuery(con, 'SELECT * FROM "Customer"')
  CustomerNew<-data.frame(matrix(ncol=length(Customer), nrow=0))
  
  cId<-max(Customer$CustomerId)
  
  i<-1
  n<-sample(1:50, 1)
  while(i<=n){
    cId<-cId+1
    
    firstName<- randomNames(which.names = "first")
    lastName<- randomNames(which.names = "last")
    
    company<- ""
    address<- toString(randoms$address[sample(1:1000, 1)])
    
    country<-toString(randoms$country[sample(1:1000, 1)])
    
    if(country=="United States"){
      state<- sample(state.abb, 1)
    }else{
      state<- ""
    }
    city<- toString(randoms$city[sample(1:1000, 1)])
    postalCode<- toString(sample(state.area, 1))
    phone<- toString(randoms$phone[sample(1:1000, 1)])
    fax<-""
    email<-toString(randoms$email[sample(1:1000, 1)])
    
    repId<-unique(Customer$SupportRepId)
    support<-repId[sample(1:length(repId), 1)]
    
    x<-data.frame(cId, firstName, lastName, company, address, city, state, country, postalCode, phone, fax, email, support)
    names(x)<-colnames(Customer)
    
    CustomerNew <- rbind(x, CustomerNew)
    i<-i+1
  }
  dbWriteTable(con, "Customer", value = CustomerNew, append = TRUE, row.names = FALSE)
  cat("SE AGREGARON",nrow(CustomerNew),"FILAS A LA TABLA Customeer\n")
  # /NEW CUSTOMER ------------------------------------------------------------
  
  
  # NEW INVOICELINE ------------------------------------------------------------
  Invoice<-dbGetQuery(con, 'SELECT * FROM "Invoice"')
  InvoiceLine<-dbGetQuery(con, 'SELECT * FROM "InvoiceLine"')
  Track<-dbGetQuery(con, 'SELECT * FROM "Track"')
  
  InvoiceLineNew<-data.frame(matrix(ncol=length(InvoiceLine), nrow=0))
  
  invoiceLineId<-max(InvoiceLine$InvoiceLineId)
  invoiceId<-max(Invoice$InvoiceId)
  
  for(i in min(CustomerNew$CustomerId):max(CustomerNew$CustomerId)){
    invoiceId<-invoiceId+1
    
    n<-sample(1:10, 1)
    j<-1
    while(n>=j){
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
  
    invoiceDate<- as.Date(format(Sys.Date(), "%Y-%m-%d 00:00:00"))+d
    
    total<- as.vector(InvoiceLineNew[InvoiceLineNew$InvoiceId==invoiceId, "UnitPrice"])
  
    x<-data.frame(invoiceId, i, invoiceDate, customer$Address, customer$City, customer$State, customer$Country, customer$PostalCode, sum(as.numeric(total)))
    names(x)<-colnames(Invoice)
    InvoiceNew <- rbind(x, InvoiceNew)
    
  }
  dbWriteTable(con, "Invoice", value = InvoiceNew, append = TRUE, row.names = FALSE)
  dbWriteTable(con, "InvoiceLine", value = InvoiceLineNew, append = TRUE, row.names = FALSE)
  cat("SE AGREGARON",nrow(InvoiceNew),"FILAS A LA TABLA Invoice\n")
  cat("SE AGREGARON",nrow(InvoiceLineNew),"FILAS A LA TABLA InvoiceLine\n")

  # /NEW INVOICE ------------------------------------------------------------
  cat("DIA",d,"FINALIZADO\n")
}

# close the connection
dbDisconnect(con)
dbUnloadDriver(drv)
cat("CONEXION TERMINADA")
