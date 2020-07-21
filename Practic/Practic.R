library(EEM)

#regions are Africa and North
region <- c("Africa", "North")

countFiles = 15

#ways to data #пути до файлов с данными
strAfrica = "D:\\matstat\\kurs\\Hexane_extr_Kivu_Lake\\"
strNorth = "D:\\matstat\\kurs\\VD_DOM_Permafrost\\"

#names of files of Africa #имена файлов Африки
arrStrAfrica <- c("1.1_70.",
                  "1.2_21",
                  "1.3_68",
                  "1.4_114",
                  "1.5_11",
                  "1.6_37",
                  "2.3_5 (400)",
                  "2.3_5 (600)",
                  "2.3_5",
                  "2.4_7",
                  "3.1_14",
                  "3.2_69",
                  "3.3_15 (600)",
                  "3.4_20(800)",
                  "3.4_20",
                  "3.5_43")

#names of files of North #имена файлов Севера
arrStrNorth <- c("1701",
                 "1702",
                 "1704",
                 "1706",
                 "1708_1to10",
                 "1708_1to20",
                 "1711",
                 "1712",
                 "1727",
                 "1728",
                 "1729",
                 "1730",
                 "1732",
                 "1733",
                 "1734")


#type of files #окончание пути до файла с данными, тип файла
strEnd = ".txt"

#amino acids
vectAA <- c()
vectAA[1:5] = 0

#names of Amino Acids
namesAA <- c("C","A","M","B","T")

#matrix of Amino Acids (AA) from different regions
aminoAcids <- matrix(data = vectAA, nrow = 2, ncol = 5)
colnames(aminoAcids) = namesAA
rownames(aminoAcids) = region

#diferent deltas from Africa's file and North's file
delta <- c(5, 2)

#start number of all files
startNum = 250

#coordinate of rectangles
crdRect <- matrix(data = c(320, 250, 310, 270, 270, 
                           350, 260, 320, 280, 280,
                           420, 380, 380, 300, 320,
                           480, 480, 420, 320, 350),
                  nrow = 5, ncol = 4)
colnames(crdRect) <- c("Left", "Right", "Down", "Up")
rownames(crdRect) <- namesAA
#View(coordsRect)

corr_names <- c("K x m1", "K x m2")

myK <- matrix(data = 0, nrow = countFiles, ncol = 2)
colnames(myK) <- region

my_m1 <- matrix(data = 0, nrow = countFiles, ncol = 2)
colnames(my_m1) <- region

my_m2 <- matrix(data = 0, nrow = countFiles, ncol = 2)
colnames(my_m2) <- region

spearman_matr <- matrix(data = 0, nrow = 2, ncol = 2)
colnames(spearman_matr) <- region
rownames(spearman_matr) <- corr_names

pearson_matr <- matrix(data = 0, nrow = 2, ncol = 2)
colnames(pearson_matr) <- region
rownames(pearson_matr) <- corr_names

quadr_matr <- matrix(data = 0, nrow = 2, ncol = 2)
colnames(quadr_matr) <- region
rownames(quadr_matr) <- corr_names


#function draw rectangles - the area of peaks of intensity
#функция рисует прямоугольники - области пиков интенсивности

drawRect <- function()
{
  rect(crdRect["C","Left"], crdRect["C","Down"], crdRect["C","Right"], crdRect["C","Up"], col = NA, border = "red") 
  rect(crdRect["A","Left"], crdRect["A","Down"], crdRect["A","Right"], crdRect["A","Up"], col = NA, border = "green") 
  rect(crdRect["M","Left"], crdRect["M","Down"], crdRect["M","Right"], crdRect["M","Up"], col = NA, border = "brown") 
  rect(crdRect["B","Left"], crdRect["B","Down"], crdRect["B","Right"], crdRect["B","Up"], col = NA, border = "yellow") 
  rect(crdRect["T","Left"], crdRect["T","Down"], crdRect["T","Right"], crdRect["T","Up"], col = NA, border = "black") 
}


AfrAAA <- matrix(data = 0, nrow = countFiles, ncol = 5)
colnames(AfrAA) <- namesAA

NorAAA <- matrix(data = 0, nrow = countFiles, ncol = 5)
colnames(NorAA) <- namesAA

#find AA from file and add to table AfrAA or NorAA
findAA <- function(dataWithFluo, name = region[1], itr = 1, AfrAA, NorAA)
{
  a <- matrix(0)
  dataMatr <- dataWithFluo[[1]]
  if (name == region[1]) 
  {
    for (i in 1:5)
    {
      a <- dataMatr[(c(crdRect[namesAA[i],"Down"] - startNum + 1)):(c(crdRect[namesAA[i],"Up"] - startNum + 1)),
                    (c(crdRect[namesAA[i],"Left"] - startNum)/delta[1] + 1):(c(crdRect[namesAA[i],"Right"] - startNum)/delta[1] + 1)]
      vectAA[i] = sum(a)
    }
    AfrAA[itr, ] = vectAA
    return(AfrAA)
  } else if (name == region[2]) {
    for (i in 1:5)
    {
      a <- dataMatr[(c(crdRect[namesAA[i],"Down"] - startNum + 1)):(c(crdRect[namesAA[i],"Up"] - startNum + 1)),
                    (c(crdRect[namesAA[i],"Left"] - startNum)/delta[2] + 1):(c(crdRect[namesAA[i],"Right"] - startNum)/delta[2] + 1)]
      vectAA[i] = sum(a)
    }
    NorAA[itr, ] = vectAA
    return(NorAA)
  } else {
    return(-1)
  }
}

#draw some information about AA
drawAA <- function(vec = "", reg = region[1], idx = 1)
{
  if (length(vec) != length(vectAA))
  {
    return(-1)
  } else {
    max1 = max(vec)
    #print(max1)
    if (reg == region[1])
    {
      png(width = 534, height = 404, filename = paste(region[1], arrStrAfrica[idx], ".png"))
      barplot(vec, names.arg = namesAA, horiz = T, xlab = "intensity", ylab = "agent",
              las = 1, main = "Africa", xlim = c(0, max1*1.4), col = "green")
    } else if (reg == region[2]) {
      png(width = 534, height = 404, filename = paste(region[2], arrStrNorth[idx], ".png"))
      barplot(vec, names.arg = namesAA, horiz = T, xlab = "intensity", ylab = "agent",
              las = 1, main = "North", xlim = c(0, max1*1.4), col = "steelblue")
    } else {
      return(-2)
    }
    dev.off()
  }
}

#matrix with wavelengths #матрица длин волн
#Africa :: cutEX = 400:600, cutEM = 600:700
#North :: cutEX = 400:800, cutEM = 600:700
mWavelengts <- matrix(data = c(400,600,600,700,400,800,600,700), nrow = 2, ncol = 4, byrow = TRUE)
colnames(mWavelengts) = c("cutExLeft",
                          "cutExRight",
                          "cutEmLeft",
                          "cutEmRight" );
rownames(mWavelengts) = c("Africa", "North")
#View(mWavelengts)

findK <- function(vec)
{
  if (length(vec) != length(vectAA))
  {
    return(-1)
  }
  return((vec[1] + vec[2])/(vec[4] + vec[5]))
}

find_m1 <- function(vec)
{
  return(vec[3]/(vec[1]+vec[2]))
}

find_m2 <- function(vec)
{
  return(vec[3]/(vec[4]+vec[5]))
}

Pearson <- function(vec1, vec2)
{
  return(cor(vec1, vec2, method = "pearson"))
}

Spearman <- function(vec1, vec2)
{
  return(cor(vec1, vec2, method = "spearman"))
}

quadrant_cc <- function(matrix1, matrix2, num1, num2, size)
{
  n14 = matrix1[matrix1[,num1] > median(matrix1[,num1]),] #правая часть от y
  n1 = n14[n14[,2] > median(matrix2[,num2]),] # первый квадрант
  n23 = matrix1[matrix1[,num1] < median(matrix1[,num1]),] #левая часть от y
  n3 = n23[n23[,2] < median(matrix2[,num2]),] #третий квадрант
  count = length(n1[,1]) + length(n3[,1])
  return((2*count - size) / size)
}


#выборочно строим информацию по файлу
example_foo <- function(AfrAA, NorAA, k)
{
  allF <- vector()
  for (k in 1:15)#countFiles)
  {
    #Africa
    strName = arrStrAfrica[k]
    wholeWay = paste0(strAfrica, strName, strEnd)
    data <- readEEM(wholeWay)
    
    #обрезаем график
    dataCut <- cutEEM(data, 
                      cutEX = mWavelengts["Africa","cutExLeft"]:mWavelengts["Africa","cutExRight"],
                      cutEM = mWavelengts["Africa","cutEmLeft"]:mWavelengts["Africa","cutEmRight"])
    
    #удаляем лучи рэлеевского рассеяния
    dataWithFluoAfr <- delScattering(dataCut, rep = 0) 
    ####    drawEEM(dataWithFluo, n = 1)
    ####    drawRect()
    
    AfrAA <- findAA(dataWithFluoAfr, name = region[1], itr = k, AfrAA, NorAA)
    
    #North
    strName2 = arrStrNorth[k]
    wholeWay2 = paste0(strNorth, strName2, strEnd)
    data <- readEEM(wholeWay2)
    
    #обрезаем график
    dataCut <- cutEEM(data, 
                      cutEX = mWavelengts["North","cutExLeft"]:mWavelengts["North","cutExRight"],
                      cutEM = mWavelengts["North","cutEmLeft"]:mWavelengts["North","cutEmRight"])
    
    #удаляем лучи рэлеевского рассеяния
    dataWithFluoNor <- delScattering(dataCut, rep = 0) 
    ####    drawEEM(dataWithFluo, n = 1)
    ####    drawRect()
    
    NorAA <- findAA(dataWithFluoNor, name = region[2], itr = k, AfrAA, NorAA)
    
    #drawAA(AfrAA[k,], region[1], k)
    #drawAA(NorAA[k,], region[2], k)
    
    #draw hist
    aminoAcids[1,] = AfrAA[k,] + aminoAcids[1,]
    aminoAcids[2,] = NorAA[k,] + aminoAcids[2,]
    
    #ищем К, m1 и m2
    
    myK[k,1] = findK(AfrAA[k,])
    myK[k,2] = findK(NorAA[k,])
    
    my_m1[k,1] = find_m1(AfrAA[k,])
    my_m1[k,2] = find_m1(NorAA[k,])
    
    my_m2[k,1] = find_m2(AfrAA[k,])
    my_m2[k,2] = find_m2(NorAA[k,])
    names(dataWithFluoAfr) <- "Africa"
    names(dataWithFluoNor) <-  "North"
   # allF <- append(allF, dataWithFluoAfr)
    #allF <- append(allF, dataWithFluoNor)
    allF <- rbind(allF,data.frame(NameFile = strName, Region = "Africa", C = AfrAA[k, 1], A  = AfrAA[k, 2],M = AfrAA[k, 3],B = AfrAA[k, 4],T = AfrAA[k, 5]))
    allF <- rbind(allF,data.frame(NameFile = strName2, Region = "North", C = NorAA[k, 1], A  = NorAA[k, 2],M = NorAA[k, 3],B = NorAA[k, 4],T = NorAA[k, 5]))
    
  }
  print(allF)
  View(allF)
  allF.pca <- prcomp(allF[,c(3:7)], scale. = TRUE, center = TRUE)

  View(allF.pca$rotation)
  View(allF.pca)
  View(allF.pca$x)
  print(summary(allF.pca))
  
  
  plot(allF.pca)
  
  
  allF.p <- predict(allF.pca)
  biplot(allF.pca, xlabs = abbreviate(allF[,2], 1, method = "both.sides"))
  grid()
  plot(allF.p[,1:2], type = "n", xlab = "PC1", ylab = "PC2")
  #text(allF.p[,1:2], labels = abbreviate(allF[,1], 1, method = "both.sides"))
  text(allF.p[,1:2])
  grid()
  
  #plot(allF.p[,3:4], type = "n", xlab = "PC3", ylab = "PC4")
  #text(allF.p[,3:4], labels = abbreviate(allF[,1], 1, method = "both.sides"))
  #grid()
  
  #View(AfrAA)
  #View(NorAA)
  #drawAA(aminoAcids[region[1],], region[1], 20)
  #drawAA(aminoAcids[region[2],], region[2], 20)
  #View(aminoAcids)
  #View(myK)
  #View(my_m1)
  #View(my_m2)
  spearman_matr[1, 1] <- Spearman(myK[, 1], my_m1[, 1])
  spearman_matr[1, 2] <- Spearman(myK[, 2], my_m1[, 2])
  spearman_matr[2, 1] <- Spearman(myK[, 1], my_m2[, 1])
  spearman_matr[2, 2] <- Spearman(myK[, 2], my_m2[, 2])
  pearson_matr[1, 1] <- Pearson(myK[, 1], my_m1[, 1])
  pearson_matr[1, 2] <- Pearson(myK[, 2], my_m1[, 2])
  pearson_matr[2, 1] <- Pearson(myK[, 1], my_m2[, 1])
  pearson_matr[2, 2] <- Pearson(myK[, 2], my_m2[, 2])
  
  #Выборочный квадрантный коэффициент корреляции
  
  #size_matr = 15
  
  #quadr_matr[1, 1] <- quadrant_cc(myK, my_m1, 1, 1, size_matr)
  #quadr_matr[1, 2] <- quadrant_cc(myK, my_m1, 2, 2, size_matr)
  #quadr_matr[2, 1] <- quadrant_cc(myK, my_m2, 1, 1, size_matr)
  #quadr_matr[2, 2] <- quadrant_cc(myK, my_m2, 2, 2, size_matr)
  
  #View(spearman_matr)
  #View(pearson_matr)
  #View(quadr_matr)
 
  #pppp <- allF[,c(2:6)]
  #result <- prcomp(pppp) # mean-centering is enabled by default
  #print(result)
  #plotScore(result, xPC = 1, yPC = 2)
}

example_foo(AfrAAA, NorAAA, 1)

