### TA ####

###### IMPORTANTE SI ESTA EN EQUIPOS CON IDIOMA DIFERENTE AL INGLES ########
# Se creara una variable global llamada weekend_days que tendrá por defecto los días 
# Sábado y Domingo en español, pero si usted se encuentra en una sistema que use otro idioma porfavor
# cambie esos 2 días a su lenguaje nativo


weekend_days = c("sábado","domingo")


#Cargamos los datos en la variable Hotel para su posterior análisis
hotel_df <- read.csv("../data/hotel_bookings_miss.csv",header=TRUE,stringsAsFactors = FALSE,sep =";")

# Analizamos la estructura, las columnas y los tipos de datos de nuestro DataFrame utilizando las siguientes funciones: "
summary(hotel_df)

#Para temas de comodidad al realizar la limpieza de los datos, cambiamos el nombre la columna "i..hotel" por "hotel"
colnames(hotel_df)[1] <- "hotel"


#PRE-PROCESAMIENTO DE DATOS

## De acuerdo a la fuente del csv las siguientes columnas deben ser categóricas:
###Reasignar los tipos de datos correspondientes:

hotel_df$hotel <- as.factor(hotel_df$hotel)
hotel_df$is_canceled <-as.factor(hotel_df$is_canceled)
hotel_df$arrival_date_month <-as.factor(hotel_df$arrival_date_month)
hotel_df$country <- as.factor(hotel_df$country)
hotel_df$meal <- as.factor(hotel_df$meal)
hotel_df$agent <- as.factor(hotel_df$agent)
hotel_df$assigned_room_type <- as.factor(hotel_df$assigned_room_type)
hotel_df$company <- as.factor(hotel_df$company)
hotel_df$customer_type <- as.factor(hotel_df$customer_type)
hotel_df$deposit_type <- as.factor(hotel_df$deposit_type)
hotel_df$distribution_channel <- as.factor(hotel_df$distribution_channel)
hotel_df$is_repeated_guest <- as.factor(hotel_df$is_repeated_guest)
hotel_df$market_segment <- as.factor(hotel_df$market_segment)
hotel_df$reservation_status <- as.factor(hotel_df$reservation_status)
hotel_df$reserved_room_type <- as.factor(hotel_df$reserved_room_type)


#Podemos observar con la siguiten función que las columnas que presentan NA's son:
colnames(hotel_df)[colSums(is.na(hotel_df)) > 0]

###Comenzamos con la limpieza de datos faltantes###
# arrival_date_year

## Estamos considerando la fecha del último status_check 
## para rellenar los NA's de las filas
hotel_df$arrival_date_year[4765] <- 2016
hotel_df$arrival_date_year[8314] <- 2016
hotel_df$arrival_date_year[9896] <- 2017
hotel_df$arrival_date_year[10887] <- 2017
hotel_df$arrival_date_year[10936] <- 2017
hotel_df$arrival_date_year[11019] <- 2017

# Revisamos las columnas con datos vacios restantes
colnames(hotel_df)[colSums(is.na(hotel_df)) > 0]

# ArrivalDateWeekNumber:


# Creamos una función que nos retorne los números de la 
# semana pasando como dato de entrada 
# el subset con los Na'S


week_number <- function(x){
  aux <- c()
  for(i in 1:nrow(x))
  {
    year  <- x$arrival_date_year[i]
    month <- match(x$arrival_date_month[i], month.name)
    day   <- x$arrival_date_day_of_month[i]
    text <- sprintf("%s-%s-%s",year,month,day)
    week <- format(as.Date(text), "%W")
    
    aux <- append(aux,week)
  }
  aux
}

# Creamos un dataframe con las filas que presentan datos vacios en la columna
na_arrival_week <- hotel_df[is.na(hotel_df$arrival_date_week_number),]

# Llenamos los datos vacios en las columnas 
# utilizando nuestra función week_number
# pasandole como parametro el dataframe creado anteriormente

hotel_df$arrival_date_week_number[is.na(hotel_df$arrival_date_week_number)] <- week_number(na_arrival_week)

# Revisamos las columnas con datos vacios restantes
colnames(hotel_df)[colSums(is.na(hotel_df)) > 0]


# arrival_date_day_of_month

# Como estamos tratando de encontrar el número del día del mes, 
# vamos a realizar una comparación del 
# Dataframe original con el subset 
# de datos vacios para hallar la tendencia de reserva 
# de ese día del mes para las vacaciones.

# Para ello vamos ubicar a las filas 
# que presenten el mismo day_of_week_number y año, 
# para luego colocar la moda en el espacio vacío.


#Función para calcular la moda

mode <- function(x) {
  return(as.numeric(names(which.max(table(x)))))
}

# Creamos esta función para asignar la moda a los valores 
# que se encuentran vacios dentro del DataFrame original

assign_mode_to_arrival_day_month <- function(df){
  #Iteramos sobre los índices del subset con los datos vacios
  for (x in as.numeric(rownames(df))){
    a<-as.integer(mode(hotel_df$arrival_date_day_of_month[hotel_df$arrival_date_week_number == as.integer(hotel_df$arrival_date_week_number[x])
                                                          & hotel_df$arrival_date_year == as.integer(hotel_df$arrival_date_year[x]) 
                                                          & !is.na(hotel_df$arrival_date_day_of_month)]))
    # Asignación al dataframe original
    hotel_df$arrival_date_day_of_month[x] <<- a
    
  }
}
# Creamos el dataframe con los datos vaciós de la columna evaluada
na_arrival_day_month <- hotel_df[is.na(hotel_df$arrival_date_day_of_month),]

# Ejecutamos la función
assign_mode_to_arrival_day_month(na_arrival_day_month)

# Revisamos las columnas con datos vacios restantes
colnames(hotel_df)[colSums(is.na(hotel_df)) > 0]




# Para las columnas week_nights y weekend_nights crearemos 2 funciones
# que se encarguen de llenar sus datos faltantes
# tomando los dias entre el arrival_date y reservation_status_date
# para ver cuales son días de semana(Lunes a Viernes) 
# o fines de semana(Sabado/Domingo), esto solo se puede hacer
# cuando el último reservation_status tiene como valor "Check-Out"
# sino, se asignara 0 debido a que signifa que no hay forma de determinar
# el valor real (tampoco podemos poner NA)



# Función que retorna una valor Date teniendo como parametros
# day, month, year
date_from_separated <- function(day, month_name, year) {
  month <- match(month_name, month.name)
  text <- paste(year, month, day, sep="-")
  # print("foo text")
  # print(text)
  return (as.Date(text, "%Y-%m-%d"))
}

# Función para llenar stays_in_week_nights
fill_na_week_nights <- function(df) {
  for (i in rownames(df)){
    if (df[i,]$reservation_status == "Check-Out") {
      arrival_date <- date_from_separated(df[i,]$arrival_date_day_of_month, df[i,]$arrival_date_month, df[i,]$arrival_date_year)
      checkout_date <- as.Date(df[i,]$reservation_status_date, "%d/%m/%Y")
      
      if (as.numeric(difftime(checkout_date, arrival_date, units = c("days"))) > 1) {
        df[i,]$stays_in_week_nights <- sum(!weekdays(seq(
          from=arrival_date,
          to=checkout_date,
          by="days"
        ))%in% weekend_days)
      } else {
        df[i,]$stays_in_week_nights <- ifelse(sum(!weekdays(c(arrival_date)) %in% weekend_days) == 1, 1, 0)
      }
    } else {
      df[i,]$stays_in_week_nights <- 0
    }
  }
  df
}

#Función para llenar stays_in_weekend_nights
fill_na_weekend_nights <- function(df) {
  for (i in rownames(df)){
    if (df[i,]$reservation_status == "Check-Out") {
      arrival_date <- date_from_separated(df[i,]$arrival_date_day_of_month, df[i,]$arrival_date_month, df[i,]$arrival_date_year)
      checkout_date <- as.Date(df[i,]$reservation_status_date, "%d/%m/%Y")
      
      if (as.numeric(difftime(checkout_date, arrival_date, units = c("days"))) > 1) {
        df[i,]$stays_in_weekend_nights <- sum(weekdays(seq(
          from=arrival_date,
          to=checkout_date,
          by="days"
        ))%in% weekend_days)
      } else {
        df[i,]$stays_in_weekend_nights <- ifelse(sum(weekdays(c(arrival_date)) %in% weekend_days) == 1, 1, 0)
      }
    } else {
      df[i,]$stays_in_weekend_nights <- 0
    }
  }
  df
}


# Llenar stays_in_week_nights
na_stays_week_nights <- hotel_df[is.na(hotel_df$stays_in_week_nights),]
hotel_df[is.na(hotel_df$stays_in_week_nights),] <- fill_na_week_nights(na_stays_week_nights)

# Revisamos las columnas con datos vacios restantes
colnames(hotel_df)[colSums(is.na(hotel_df)) > 0]

# Llenar stays_in_weekend_nights
na_stays_weekend_nights <- hotel_df[is.na(hotel_df$stays_in_weekend_nights),]
hotel_df[is.na(hotel_df$stays_in_weekend_nights),] <- fill_na_weekend_nights(na_stays_weekend_nights)

# Revisamos las columnas con datos vacios restantes
colnames(hotel_df)[colSums(is.na(hotel_df)) > 0]


# Para niños y bebés, no hay forma de saber 
# la cantidad con respecto a otras variables,
# por lo que llenamos con 0

hotel_df$children[is.na(hotel_df$children)] <- 0
hotel_df$babies[is.na(hotel_df$babies)] <- 0

# Para adultos, suponemos 
# que al menos un adulto debe estar presente 
# para hacer tanto Check-In como Check-Out

hotel_df$adults[is.na(hotel_df$adults)] <- 1

# Revisamos las columnas con datos vacios restantes
colnames(hotel_df)[colSums(is.na(hotel_df)) > 0]

# Debido a que los datos en lead_time y days_in_waiting_list
# no pueden ser encontrados utilizando otras variables,
# decidimos utilizar la media como estrategía de llenado
# para estos campos vacios; sin embargo, no podemos
# hacer esto hasta que eliminos los datos atípicos de sus columnas
# ya que podrían afectar los resultados.

######################### MEDIA CON OUTLIERS ###############################
summary(hotel_df)

hotel_df2 <- hotel_df

hotel_df <- hotel_df2
# Verificamos las columnas que presentan outliers
# utilizando la siguiente función


# Revisar Outliers
identificar_outliers <- function(df){
  aux <- c()
  for (x in colnames(df)){
    if(is.numeric(df[[x]])){
      if(length(boxplot(df[[x]], plot= FALSE)$out) > 0){
        aux <- append(aux,x)
      }
    }
  }
  aux
}


identificar_outliers(hotel_df2)

# Cambio de los outliers con la mediana/media

# Si el valor atípico está por debajo del quinto percentil 
# lo reemplazamos con la media.
# Si el valor atípico está por encima del percentil 95, 
# lo reemplazamos con la mediana.

fix_outliers <- function(x, removeNA = TRUE, flag=FALSE){ 
  #Calculamos los quantiles 1) por arriba del 5% y por debajo del 95% 
  quantiles <- quantile(x, c(0.05, 0.95), na.rm = removeNA) 
  if (flag){
    x[x<quantiles[1]] <- mean(x, na.rm = removeNA) 
  }
  x[x>quantiles[2]] <- median(x, na.rm = removeNA) 
  x 
}


# Limpieza de las columnas que presentan datos atípicos

boxplot(hotel_df$adults)
summary(hotel_df$adults)
length(boxplot(hotel_df$adults,plot = FALSE)$out)
table(boxplot(hotel_df$adults,plot = FALSE)$out)
table(hotel_df$adults)
hotel_df$adults <- fix_outliers(hotel_df$adults, TRUE)

boxplot(hotel_df$children)
summary(hotel_df$children)
length(boxplot(hotel_df$children,plot = FALSE)$out)
table(boxplot(hotel_df$children,plot = FALSE)$out)
#hotel_df$children <- fix_outliers(hotel_df$children)

boxplot(hotel_df$booking_changes)
summary(hotel_df$booking_changes)
length(boxplot(hotel_df$booking_changes,plot = FALSE)$out)
table(boxplot(hotel_df$booking_changes,plot = FALSE)$out)

table(hotel_df$booking_changes)
hotel_df$booking_changes[hotel_df$booking_changes >8] <- 1
# hotel_df$booking_changes <- fix_outliers(hotel_df$booking_changes)

boxplot(hotel_df$previous_cancellations)
summary(hotel_df$previous_cancellations)
length(boxplot(hotel_df$previous_cancellations,plot = FALSE)$out)
table(boxplot(hotel_df$previous_cancellations,plot = FALSE)$out)
hotel_df$previous_cancellations <- fix_outliers(hotel_df$previous_cancellations)
table(hotel_df$previous_cancellations)

boxplot(hotel_df$required_car_parking_space)
summary(hotel_df$required_car_parking_spaces)
length(boxplot(hotel_df$required_car_parking_spaces,plot = FALSE)$out)
table(boxplot(hotel_df$required_car_parking_spaces,plot = FALSE)$out)
hotel_df$required_car_parking_spaces[hotel_df$required_car_parking_spaces == 8] <- 0

# hotel_df$required_car_parking_spaces <- fix_outliers(hotel_df$required_car_parking_spaces)

boxplot(hotel_df$babies)
summary(hotel_df$babies)
length(boxplot(hotel_df$babies,plot = FALSE)$out)
table(boxplot(hotel_df$babies,plot = FALSE)$out)
#hotel_df$babies <- fix_outliers(hotel_df$babies)


boxplot(hotel_df$stays_in_weekend_nights)
summary(hotel_df$stays_in_weekend_nights)
length(boxplot(hotel_df$stays_in_weekend_nights,plot = FALSE)$out)
table(boxplot(hotel_df$stays_in_weekend_nights,plot = FALSE)$out)
hotel_df$stays_in_weekend_nights <- fix_outliers(hotel_df$stays_in_weekend_nights)
table(hotel_df$stays_in_weekend_nights)

boxplot(hotel_df$stays_in_week_nights)
summary(hotel_df$stays_in_week_nights)
length(boxplot(hotel_df$stays_in_week_nights,plot = FALSE)$out)
table(boxplot(hotel_df$stays_in_week_nights,plot = FALSE)$out)
hotel_df$stays_in_week_nights <- fix_outliers(hotel_df$stays_in_week_nights)
table(hotel_df$stays_in_week_nights)

# No podemos cambiar adr porque depende de otras variables que 
# probablemente no esten en el dataframe
boxplot(hotel_df$adr)
summary(hotel_df$adr)
length(boxplot(hotel_df$adr,plot = FALSE)$out)
table(boxplot(hotel_df$adr,plot = FALSE)$out)
hotel_df$adr <- fix_outliers(hotel_df$adr,TRUE)


boxplot(hotel_df$lead_time)
summary(hotel_df$lead_time)
length(boxplot(hotel_df$lead_time,plot = FALSE)$out)
table(boxplot(hotel_df$lead_time,plot = FALSE)$out)
hotel_df$lead_time <- fix_outliers(hotel_df$lead_time,TRUE)



boxplot(hotel_df$previous_bookings_not_canceled)
summary(hotel_df$previous_bookings_not_canceled)
length(boxplot(hotel_df$previous_bookings_not_canceled,plot = FALSE)$out)
table(boxplot(hotel_df$previous_bookings_not_canceled,plot = FALSE)$out)
hotel_df$previous_bookings_not_canceled <- fix_outliers(hotel_df$previous_bookings_not_canceled,TRUE)
table(hotel_df$previous_bookings_not_canceled)

boxplot(hotel_df$days_in_waiting_list)
summary(hotel_df$days_in_waiting_list)
length(boxplot(hotel_df$days_in_waiting_list,plot = FALSE)$out)
table(boxplot(hotel_df$days_in_waiting_list,plot = FALSE)$out)
hotel_df$days_in_waiting_list <- fix_outliers(hotel_df$days_in_waiting_list,TRUE)

# No se quitaron los valores outliers porque 
# consideramos que los pedidos no escapan tanto 
# de la racionalidad
boxplot(hotel_df$total_of_special_requests)
summary(hotel_df$total_of_special_requests)
length(boxplot(hotel_df$total_of_special_requests,plot = FALSE)$out)
table(boxplot(hotel_df$total_of_special_requests,plot = FALSE)$out)
# hotel_df$total_of_special_requests <- fix_outliers(hotel_df$total_of_special_requests)
table(hotel_df$total_of_special_requests)


# Una vez tratado el tema de los outliers
# podemos terminar de llenar los campos vacios en
# las columnas days_in_waiting_list y
# lead_time

# days_in_waiting_list
hotel_df$days_in_waiting_list[is.na(hotel_df$days_in_waiting_list)] <- as.integer(mean(hotel_df$days_in_waiting_list[!is.na(hotel_df$days_in_waiting_list)]))

# lead_time: 
hotel_df$lead_time[is.na(hotel_df$lead_time)] <- as.integer(mean(hotel_df$lead_time[!is.na(hotel_df$lead_time)]))


## CREAMOS EL ARCHIVO CSV CON EL DATAFRAME LIMPIO##
write.csv(hotel_df,'../data/dataset_clean.csv', na="NA",row.names=FALSE)





