Sum_mas1_x <- c()
Sum_mas1_x[1:5] = 0

#считываем данные Африки
#data <- readEEM("D:\\matstat\\kurs\\Hexane_extr_Kivu_Lake\\1.2_21.txt")
#data <- readEEM("D:\\matstat\\kurs\\Hexane_extr_Kivu_Lake\\1.3_68.txt")
#data <- readEEM("D:\\matstat\\kurs\\Hexane_extr_Kivu_Lake\\1.4_114.txt")
#data <- readEEM("D:\\matstat\\kurs\\Hexane_extr_Kivu_Lake\\1.5_11.txt")
#data <- readEEM("D:\\matstat\\kurs\\Hexane_extr_Kivu_Lake\\1.6_37.txt")
data <- readEEM("D:\\matstat\\kurs\\Hexane_extr_Kivu_Lake\\2.3_5.txt")
#data <- readEEM("D:\\matstat\\kurs\\Hexane_extr_Kivu_Lake\\2.4_7.txt")
#data <- readEEM("D:\\matstat\\kurs\\Hexane_extr_Kivu_Lake\\3.1_14.txt")
#data <- readEEM("D:\\matstat\\kurs\\Hexane_extr_Kivu_Lake\\3.2_69.txt")
#data <- readEEM("D:\\matstat\\kurs\\Hexane_extr_Kivu_Lake\\4.4_87.txt") # read in a file 
drawEEM(data, n = 1) 
#обрезаем
data_del_cut <- cutEEM(data, cutEX = 410:500)
drawEEM(data_del_cut, 1)
#удаляем лучи рэлеевского рассеяния
data_del_Scattering <- delScattering(data_del_cut, rep = NA) 
drawEEM(data_del_Scattering, 1)
#выделяем пики
#par(new=TRUE)
#plot(c(0, 400), c(0, 600), type= "n", xlab = "", ylab = "")
rect(xleft = 320,ybottom =  420,xright =  350,ytop =  480, col = NA, border = "red") 
rect(250, 380, 260, 480, col = NA, border = "green") 
rect(310, 380, 320, 420, col = NA, border = "blue") 
rect(270, 300, 280, 320, col = NA, border = "yellow") 
rect(270, 320, 280, 350, col = NA, border = "white") 
legend(x = 360, y = 600, c("C", "A", "M","B", "T"), col = c("red","green","blue","yellow", "white"), lty = c(1, 1, 1, 1, 1), bg = "cyan")
data_matrix <- data_del_Scattering[[1]]
mas1_x[1:5] = 0
#выделяем матрицу пиков С
matrix_1 <- data_matrix[c(171:231),c(15:21)]
# вставляем вместо NA нули
matrix_1[is.na(matrix_1)] <- 0
#суммируем интенсивности в области пика
mas1_x[1] = sum(matrix_1)
#выделяем матрицу пиков A
matrix_2 <- data_matrix[c(131:231),c(1:3)]
matrix_2[is.na(matrix_2)] <- 0
mas1_x[2] = sum(matrix_2)
#выделяем матрицу пиков M
matrix_3 <- data_matrix[c(131:171),c(13:15)]
matrix_3[is.na(matrix_3)] <- 0
mas1_x[3] = sum(matrix_3)
#выделяем матрицу пиков B
matrix_4 <- data_matrix[c(51:71),c(5:7)]
matrix_4[is.na(matrix_4)] <- 0
mas1_x[4] = sum(matrix_4)
#выделяем матрицу пиков T
matrix_5 <- data_matrix[c(71:101),c(5:7)]
matrix_5[is.na(matrix_5)] <- 0
mas1_x[5] = sum(matrix_5)
mas1_x
Sum_mas1_x = Sum_mas1_x + mas1_x
Sum_mas1_x 

Sum_mas2_x <- c()
Sum_mas2_x[1:5] = 0
#считываем данные Севера
#data2 <- readEEM("D:\\matstat\\kurs\\VD_DOM_Permafrost\\1704.txt") # read in a file
#data2 <- readEEM("D:\\matstat\\kurs\\VD_DOM_Permafrost\\1706.txt") # read in a file
#data2 <- readEEM("D:\\matstat\\kurs\\VD_DOM_Permafrost\\1711.txt") # read in a file
#data2 <- readEEM("D:\\matstat\\kurs\\VD_DOM_Permafrost\\1712.txt") # read in a file
#data2 <- readEEM("D:\\matstat\\kurs\\VD_DOM_Permafrost\\1727.txt") # read in a file
#data2 <- readEEM("D:\\matstat\\kurs\\VD_DOM_Permafrost\\1728.txt") # read in a file
#data2 <- readEEM("D:\\matstat\\kurs\\VD_DOM_Permafrost\\1729.txt") # read in a file
#data2 <- readEEM("D:\\matstat\\kurs\\VD_DOM_Permafrost\\1730.txt") # read in a file
#data2 <- readEEM("D:\\matstat\\kurs\\VD_DOM_Permafrost\\1732.txt") # read in a file
data2 <- readEEM("D:\\matstat\\kurs\\VD_DOM_Permafrost\\1734.txt") # read in a file
drawEEM(data2, n = 1) 
data2_del_Scattering <- delScattering(data2, rep = NA) 
drawEEM(data2_del_Scattering, 1)
rect(xleft = 320,ybottom =  420,xright =  350,ytop =  480, col = NA, border = "red") 
rect(250, 380, 260, 480, col = NA, border = "green") 
rect(310, 380, 320, 420, col = NA, border = "blue") 
rect(270, 300, 280, 320, col = NA, border = "yellow") 
rect(270, 320, 280, 350, col = NA, border = "white")  
#считаем интенсивности Севера
data2_matrix <- data2[[1]]
mas2_x <- c(5)
matrix2_1 <- data2_matrix[c(171:231),c(36:51)]
matrix2_1[is.na(matrix2_1)] <- 0
mas2_x[1] = sum(matrix2_1)
matrix2_2 <- data2_matrix[c(131:231),c(1:6)]
matrix2_2[is.na(matrix2_2)] <- 0
mas2_x[2] = sum(matrix2_2)
matrix2_3 <- data2_matrix[c(131:171),c(31:36)]
matrix2_3[is.na(matrix2_3)] <- 0
mas2_x[3] = sum(matrix2_3)
matrix2_4 <- data2_matrix[c(51:71),c(11:16)]
matrix2_4[is.na(matrix2_4)] <- 0
mas2_x[4] = sum(matrix2_4)
matrix2_5 <- data2_matrix[c(71:101),c(11:16)]
matrix2_5[is.na(matrix2_5)] <- 0
mas2_x[5] = sum(matrix2_5)
mas2_x

Sum_mas2_x = Sum_mas2_x + mas2_x
Sum_mas2_x 


#заголовки пиков
mas_y = c("C","A","M","B", "T")
#гистограмма Африки
barplot(Sum_mas1_x, names.arg = mas_y, horiz=TRUE, las = 1, main = "Африка", xlim = c(0,800000), col = "blue")
#гистограмма Севера
barplot(Sum_mas2_x, names.arg = mas_y, horiz=TRUE, las = 1, main = "Север", xlim = c(0,800000), col = "blue")





