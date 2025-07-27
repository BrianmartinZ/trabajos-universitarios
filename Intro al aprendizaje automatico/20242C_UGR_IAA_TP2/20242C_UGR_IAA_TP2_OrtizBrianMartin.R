# Cargo mi archivo csv.
getwd() # pregunto sobre la ubicación de mi proyecto

fish_data <- read.csv("Fish.csv") #cargando mi dataset

# Verifico la carga.

head(fish_data)

# Descripción de las Columnas:

# Species -> Especies de pez

# Weight -> Peso en gramos

# Length1 -> Longitud vertical en cm

# Length2 -> Longitud diagonal en cm

# Length3 -> Longitud transversal en cm

# Height -> Altura en cm

# Width -> Ancho en cm

# resumen estadístico

summary(fish_data)

# Convertir la columna "Species" en formato factor. (Es la variable objetivo)
fish_data$Species <- as.factor(fish_data$Species)

# Cuento el numero de filas de mi dataset: 
len_muestra <- nrow(fish_data) # 159
len_muestra

#--------------------       PARTICIONANDO EN TRAIN Y TEST       ---------------------------

set.seed(123) # establezco la semilla para generar números aleatorios.

fish_muestra <- sample(len_muestra,len_muestra)
print(paste("Muestra Aleatoria:",len_muestra))

fish_muestra

#--------------------------------------------------------------------------------------------

# Aquí se define la proporción train
len_train <- len_muestra*0.8  
print(paste("Longitud muestra train:",len_train))

# muestra train
fish_train <- sample(fish_muestra,len_train) 
fish_train # va devolver 127 aleatorios de esos 159

# Aqui se define la proporción test
fish_test <- fish_muestra[!fish_muestra %in% fish_train]
len_test <- length(fish_test) # largo del dataset test
print(paste("Longitud muestra test:", len_test))

fish_test

#---------------------------------------------------------------------------------------------

fish_data_train <- fish_data[fish_train, ]
fish_data_test <- fish_data[fish_test, ]

#train
fish_datamod_train <- fish_data_train[,-1] # 80% de los datos.
fish_output_train <- fish_data_train[,1] 

fish_output_train # especies del 80%

#test
fish_datamod_test <- fish_data_test[,-1] # 30% para test.
fish_output_real_test <- fish_data_test[,1]

fish_output_real_test # el 30% real

#------------------KNN-------------------------------------------------------------------------------

library(class)
library(ggplot2)

fish_output_test_knn <- knn(
  
  train = fish_datamod_train,# especifica el conjunto de datos de entrenamiento. (el 80%)
  cl = fish_output_train, # recibe las etiquetas o clases reales correspondientes a ese conjunto de datos de entrenamiento
  test = fish_datamod_test, # le entregamos los datos cuantitativos del 30% para que intente predecir las clases.
  k = 14 # Le indica el número de vecinos más cercanos a considerar para la clasificación.
  
)
fish_output_test_knn

# Elaboramos la matriz de confusión

mc_knn <- table(fish_output_test_knn, fish_output_real_test)
print(paste("Matriz de confusión:"))
mc_knn

ac_knn <- mean(fish_output_test_knn == fish_output_real_test)
print(paste("accuracy:", ac_knn))


#---------------------------------------------------------------------------------------------------

k <- 1:50
knn_values <- data.frame(k, accuracy = 0)

# Creamos un bucle for para probar diferentes valores de k
for (n in k){
  fish_output_test_knn <- knn(
    train = fish_datamod_train,
    cl = fish_output_train,
    test = fish_datamod_test,
    k = n
  )
  knn_values$accuracy[n] <- mean(fish_output_test_knn == fish_output_real_test)
}


#grafico

ggplot(knn_values) +
  aes(k, accuracy) + # Define los ejes del gráfico;(x,y)
  geom_line(colour="blue",lwd = 1, alpha = 0.5) + # Dibuja una línea azul con los valores de k y accuracy
  geom_point(colour="black") + # Añade puntos negros en cada valor de k.
  geom_smooth(colour="red", lwd = 0.5) + # Dibuja una línea de suavizado en rojo que indica la tendencia general de la precisión a medida que varía k
  
  theme( #  Modifica el tamaño de las etiquetas de los ejes x y y
    axis.text.x = element_text(size = 12), 
    axis.text.y = element_text(size = 12)
  )

knn_values

#------------------ Regresión Lineal -----------------------------

# Es importante mencionar que este tipo de modelo no es la técnica más adecuada para un variable
# dependiente categórica, sin embargo, vamos a utilizarla de forma experimental, convirtiendo
# las especies en variables numérica y asignando a cada una de ellas un número correspondiente.

# Convertir la variable de especies a número
fish_output_train_num <- as.numeric(fish_output_train)

# Ajustar un modelo de regresión lineal
fish_rl <- lm(fish_output_train_num ~ ., data = fish_datamod_train)

# Ver el resumen del modelo
summary(fish_rl)

# Hacer predicciones sobre el conjunto prueba
fish_output_test_rl <- predict(fish_rl, newdata = fish_datamod_test)

# Mapeo de especies numéricas a sus nombres
species_names <- levels(factor(fish_output_train))

# Convertir las predicciones a especies
fish_predict_species_rl <- species_names[round(fish_output_test_rl)]

# Mostrar
print(fish_predict_species_rl)

# Matriz de confusión
mc_rl <- table(Predicción = fish_predict_species_rl, Actual = fish_output_real_test)

print(mc_rl)

# Accuracy
ac_rl <- mean (fish_predict_species_rl == fish_output_real_test)
print(paste("Accuracy:", ac_rl))

# Crear un dataframe con los valores reales y las predicciones
df_rl <- data.frame(
  Predicción = fish_predict_species_rl,
  Real = fish_output_real_test,
  Predicción_Numerica = fish_output_test_rl
)

# Graficar
ggplot(df_rl, aes(x = Predicción_Numerica, y = as.numeric(Real))) +
  geom_point(aes(color = Real), size = 3) +  # Puntos de datos reales
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Línea de regresión
  labs(title = "Regresión Lineal para Predicción de Especies de Peces",
       x = "Predicción (Valores Numéricos)",
       y = "Especies (Valores Numéricos)") +
  theme_minimal()


#---------------  Árbol de decision  ------------------------------

# Cargo las librerías necesarias.
library(rpart)
library(rpart.plot)

head(fish_data)

# Crear un árbol de decisión con el conjunto de entrenamiento
fish_ad <- rpart(Species ~ ., data = fish_data_train, method = "class",control = rpart.control(minsplit = 1, minbucket = 1))

# Visualizar el árbol de decisión con rpart.plot
rpart.plot(fish_ad, type = 2, extra = 1)

# Realizar predicciones con el árbol de decisión en el conjunto de prueba
fish_output_test_ad <- predict(fish_ad, fish_data_test, type = "class")

# el vector de clases reales
fish_output_real_test

# Matriz de confusión
mc_ad <- table(fish_output_real_test, fish_output_test_ad)
print("Matriz de confusión:")
print(mc_ad)


# Calcular la exactitud en el conjunto de prueba
ac_ad <- mean(fish_output_test_ad == fish_output_real_test)
print(paste("Accuracy:", ac_ad))

# Nos devuelve un Accuracy del 71% a diferencia del algoritmo KNN que fue del 53%
# Por lo cual, se obtuvo una mejor precisión en la predicción de los datos.

##--------------  Regresión Logística multinomial ---------------------------------------------

# En este caso vamos a optar por usar la regresión logística multinomial para la variable categórica "Species"
# ya que nos permite analizar mas de 2 especies.

# Cargamos la librería
library(ggplot2)
library(nnet)


# Ajustar un modelo de regresión logística multinomial
fish_rlm <- multinom(Species ~ Weight + Length1 + Length2 + Length3 + Height + Width, 
                     data = fish_data_train)

# Resumen del modelo
summary(fish_rlm)

# Hacer predicciones
fish_output_test_rlm <- predict(fish_rlm, newdata = fish_data_test) # Clases predichas

# Matriz de confusión
mc_rlm <- table(Predicted = fish_output_test_rlm, Actual = fish_output_real_test)

# Muestro en consola
print(mc_rlm)

# Calculamos la presicion
ac_rlm <- mean(fish_output_test_rlm == fish_output_real_test)
print(paste("Accuracy:", round(ac_rlm * 100, 2),"%"))

# El resultado es del modelo de regresión logística multinomial es del 96%.

#------------------------   Random Forest   -------------------------------

# Instalamos los paquetes necesarios.
install.packages("randomForest")

# Cargamos la librerias
library(randomForest)

# Ajustamos el modelo de Random Forest

fish_rf <- randomForest(fish_output_train ~ Weight + Length1 + Length2 + Length3 + Height + Width,
                        data = fish_data_train,
                        ntree = 1000) #Numero de árboles en el bosque

# visualización

plot(fish_rf)


fish_output_test_rf <- predict(fish_rf, fish_data_test)

fish_output_test_rf

# Recordar que ya estaba creado el vector de clases reales
fish_output_real_test

# Matriz de confusión
mc_rf <- table(fish_output_real_test, fish_output_test_rf)
print("Matriz de Confusion:")
print(mc_rf)

# Calcular la precision:
ac_rf <- mean(fish_output_test_rf == fish_output_real_test)
print(paste("Accuracy:", ac_rf))

# Nos devuelve un resultado del 71%, igual al arbol de decision.

#---------------- Naive Bayes -------------------------------

# Cargo la libreria e1071
install.packages("e1071")
library(e1071)
library(ggplot2)

# Creamos un modelo de naive bayes
fish_nb <- naiveBayes(Species ~ ., data = fish_data_train)

# Realizamos las predicciones
fish_output_test_nb <- predict(fish_nb, fish_data_test)

# Matriz de confusión
mc_nb <- table(Real = fish_output_real_test, Predicción = fish_output_test_nb)
print(mc_nb)

# Calcular la precisión
ac_nb <- mean(fish_output_test_nb == fish_output_real_test)
print(paste("Accuracy:", ac_nb))

# Nos devuelve una precisión del 50%

# Creamos un nuevo dataframe con las predicciones y las características originales
fish_output_test_nb_results <- data.frame(fish_data_test, Prediccion = fish_output_test_nb)

# Graficamos
plot_nb <- ggplot(fish_output_test_nb_results, aes(x = Weight,y = Length2, color = Species, shape = Prediccion)) +
  geom_point(size = 3) + # Aumenta el tamaño de puntos
  ggtitle("Clasificación con Naive Bayes en Conjunto de test") + 
  labs(
    color = "Real",
    shape = "Predicción",
    x = "Peso del pez", # Ajusta el nombre del eje x
    y = "Longitud diagonal del pez"  # Ajusta el nombre del eje y
  ) +
  theme(
    axis.text = element_text(size = 12), # Ajusta el tamaño del texto de los ejes
    axis.title = element_text(size = 14) # Ajusta el tamaño del título de los ejes.
  )

plot_nb

#---------------- Máquinas de Vector Soporte (SVM) -------------------------------

# Carga de librerías para SVM
library(e1071)  
library(ggplot2)

# Entrenar el modelo SVM con todas las variables independientes
fish_svm <- svm(Species ~ ., data = fish_data_train, kernel = "radial", cost = 1, scale = TRUE)

# Realiza predicciones en el conjunto de prueba
fish_output_test_svm <- predict(fish_svm, fish_data_test)
fish_output_test_svm

#Matriz de confusión
mc_svm <- table(Real = fish_output_real_test, Predicción = fish_output_test_svm)
print(mc_svm)

# Calcular la precisión
ac_svm <- mean(fish_output_test_svm == fish_output_real_test)
print(paste("Accuracy:", ac_svm))


# Crear un gráfico de dispersión con colores para las clases

# Crear un nuevo dataframe con las predicciones y las características originales
fish_output_test_svm_results <- data.frame(fish_data_test, Prediccion = fish_output_test_svm)

#Gráfico:
plot_svm<-ggplot(fish_output_test_svm_results, aes(x = Weight,y = Length2, color = Species, shape = Prediccion)) +
  geom_point(size = 3) +  # Ajusta el tamaño de los puntos
  ggtitle("Clasificación con SVM en Conjunto de Test") +
  labs(
    color = "Real",
    shape = "Predicción",
    x = "Peso del pez",  # Ajusta el nombre del eje x
    y = "Longitud diagonal del pez"     # Ajusta el nombre del eje y
  ) +
  theme(
    axis.text = element_text(size = 12),  # Ajusta el tamaño del texto de los ejes
    axis.title = element_text(size = 14)  # Ajusta el tamaño del título de los ejes
  )

plot_svm

#-----------------------------------------------------------------------------------------------------------------

# Crear un dataframe con los resultados de todos los algoritmos
resultados_df <- data.frame(
  KNN = fish_output_test_knn,
  Regresion_Lineal = fish_predict_species_rl,
  Arboles_de_Decision = fish_output_test_ad,
  Random_Forest = fish_output_test_rf,
  NaiveBayes = fish_output_test_nb,
  Regresion_Logistica_multi = fish_output_test_rlm,
  SVM = fish_output_test_svm
)


# Crear un dataframe para almacenar la información de errores
errores_df <- data.frame(
  Algoritmo = character(0),
  Accuracy = numeric(0),
  Errores = numeric(0)
)

# Función para calcular la cantidad de errores
calcular_errores <- function(resultados) {
  errores <- sum(resultados != fish_output_real_test)
  accuracy <- 1 - (errores / length(fish_output_real_test))
  return(c(errores, accuracy))
}

# Calcular errores y precisión para cada algoritmo
for (col_name in colnames(resultados_df)) {
  errores_accuracy <- calcular_errores(resultados_df[, col_name])
  errores_df <- rbind(errores_df, c(col_name, errores_accuracy[2], errores_accuracy[1]))
}

# Renombrar las columnas del dataframe
colnames(errores_df) <- c("Algoritmo", "Accuracy", "Errores")

# Mostrar la tabla de errores y precisión
print(errores_df)

#--------------------------------------------------------------------------------------------




