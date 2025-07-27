
# Libreria utilizada.

library(dplyr)

# 1 - Escribir las tablas en R. 

# Creando vectores de la tabla 1.

dni <- c(43345678,42223344,39654123,42886655,43123412)

nombre <- c("Agustina",
             "Pablo",
             "Damián",
             "Paola",
             "Jimena")

apellido <- c("Rodriguez",
              "Perez",
              "González",
              "Méndez",
              "Jiménez")

edad <- c(21,22,26,22,21)


localidad <- c("Rosario",
               "San Nicolás",
               "Rosario",
               "Rosario",
               "Pergamino")

provincia <- c("Santa Fe",
               "Buenos Aires",
               "Santa Fe",
               "Santa Fe",
               "Buenos Aires")

tel <- c(3415534567,3365421555,3415454321,3415544667,2477548788)

# Creando tabla 1.

tabla_uno <- data.frame(dni, nombre, apellido, edad, localidad, provincia, tel)

# Visualizando tabla 1.

View(tabla_uno)


# Creando vectores de la tabla 2

dni <- c(42145221,39803344)


nombre <- c("Carolina",
            "Nicolás")

apellido <- c("Páez",
              "López")

edad <- c(23,26)


localidad <- c("Rosario",
               "Roldán")

provincia <- c("Santa Fe",
               "Santa Fe")

tel <- c(NA,3415422811.0)

# Creando tabla 2.

tabla_dos <- data.frame(dni, nombre, apellido, edad, localidad, provincia, tel)

# Visualizando tabla 2.

View(tabla_dos)

# Modificando una celda especifica.

tabla_uno[2,3] <- "Pérez"

################################################################################

# 2 - Combinando las dos celdas.

Union_tablas <- tabla_uno %>% full_join(tabla_dos,by=c("dni" = "dni",
                                                       "nombre" = "nombre",
                                                       "apellido" = "apellido",
                                                       "edad" = "edad",
                                                       "localidad" = "localidad",
                                                       "provincia" = "provincia",
                                                       "tel"="tel")) 

#Visualizando Union_tablas.
View(Union_tablas)

################################################################################

# 3 Agregar una nueva fila para incluir los datos de una persona adicional 
# (GastónRodríguez. DNI: 42.495.245. Tiene 20 años y es de Rosario, Santa Fe. Suteléfono es 9832746349).

nueva_fila = data.frame(dni = 42495245,
                        nombre = "Gastón",
                        apellido = "Rodríguez",
                        edad = 20 ,
                        localidad = "Rosario",
                        provincia = "Santa Fe",
                        tel = 9832746349)

Union_tablas <- rbind(Union_tablas, nueva_fila)

View(Union_tablas)

################################################################################

# 4 En un nuevo objeto, Modificar el valor del teléfono en la primera fila de la segunda
# tabla para completar el dato faltante con el siguiente número: 2039857310.

tabla_dos_mod <- data.frame(dni, nombre, apellido, edad, localidad, provincia, tel)

tabla_dos_mod[1,7] <- 2039857310

View(tabla_dos_mod)

#################################################################################

# 5 Eliminar la columna DNI del objeto creado en el punto 4.

tabla_dos_mod <- select(tabla_dos_mod, -dni)

View(tabla_dos_mod)

#################################################################################

# 6 Exportar en formato .csv la tabla obtenida en el punto 3.

# Ubicación local

ruta_archivo <- "C:/Users/brian/Desktop/Dataframe_UGR_ICD_TP2/datos_exp.csv"

# Exportando

write.csv(Union_tablas, file= ruta_archivo , sep = ",", row.names = FALSE)

#################################################################################



