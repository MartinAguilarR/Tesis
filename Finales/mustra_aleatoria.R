
install.packages("tidyverse")

library("tidyverse")
library("dplyr")
library("readr")

# carga el data frame con los comentarios
data <- read_csv("C:\\Users\\Admin\\Desktop\\tesis\\Tesis\\Finales\\DF_Final.csv")

# se genera una muestra aleatoria de 367 obs
muestreo <- sample_n(data, size= 367)
nrow(muestreo)

# ver las observaciones
head(muestreo)
view(muestreo)

####-----------------------Analisis de sentimientos -------------------####
install.packages("SnowballC")
install.packages("syuzhet")
install.packages("tidytext")

library(SnowballC)
library(syuzhet)
library("tidytext")

###----se separan los comentarios en palabras----####
data_tokens <- unnest_tokens(tbl= muestreo,
                             output = "Palabra",
                             input = "Comentario",
                             token = "words")

###--se eliminan dos vaiables para dejar la var. palabra como un vector --##
borrar <- c("Categoria","Usuario")
data_tokens2 <- data_tokens[ , !(names(data_tokens) %in% borrar)]

View(data_tokens2)

#Transformamos data_tokens2 en un vector para 
#poder utilizar la función get_nrc_sentiment

palabra_df = c()
for(i in data_tokens2) { palabra_df <- i }
class(palabra_df)

#Aplicamos la función indicando el vector y el idioma y creamos
#un nuevo data frame llamado emocion_df
emocion_df <- get_nrc_sentiment(char_v = palabra_df, language = "spanish")

#Unimos emocion.df con el vector data_tokens2 para ver como
#trabajó la función get_nrc_sentiment cada una de las palabras
emocion_df2 <- cbind(data_tokens2, emocion_df)
head(emocion_df2)

#====================================================================================#
# ANALISIS MANUAL DE LA MUESTRA #
#====================================================================================#

# se crean las variables que son las correctas 
emocion_df2$positivo <- 0
emocion_df2$negativo <- 0

View(emocion_df2)
emocion_df2 <- select(emocion_df2, -anger, -anticipation, -disgust, -fear, -joy, -sadness, -surprise, -trust)

# trabajo manual
emocion_df2[1,4] <- 1
emocion_df2[5,4] <- 1
emocion_df2[9,4] <- 1
emocion_df2[14,4] <- 1
emocion_df2[17,4] <- 1
emocion_df2[18,4] <- 1
emocion_df2[55,5] <- 1
emocion_df2[68,4] <- 1
emocion_df2[77,4] <- 1
emocion_df2[81,4] <- 1
emocion_df2[85,4] <- 1
emocion_df2[87,4] <- 1
emocion_df2[95,4] <- 1
emocion_df2[125,4] <- 1
emocion_df2[129,5] <- 1
emocion_df2[135,4] <- 1
emocion_df2[141,5] <- 1
emocion_df2[142,4] <- 1
emocion_df2[146,5] <- 1
emocion_df2[154,4] <- 1
emocion_df2[159,4] <- 1
emocion_df2[172,4] <- 1
emocion_df2[175,4] <- 1
emocion_df2[181,4] <- 1
emocion_df2[183,4] <- 1
emocion_df2[185,4] <- 1
emocion_df2[189,4] <- 1
emocion_df2[194,4] <- 1
emocion_df2[196,4] <- 1
emocion_df2[200,4] <- 1
emocion_df2[203,5] <- 1
emocion_df2[213,4] <- 1
emocion_df2[209,4] <- 1
emocion_df2[222,4] <- 1
emocion_df2[226,5] <- 1
emocion_df2[239,4] <- 1
emocion_df2[249,4] <- 1
emocion_df2[255,4] <- 1
emocion_df2[261,4] <- 1
emocion_df2[269,4] <- 1
emocion_df2[279,4] <- 1
emocion_df2[330,4] <- 1
emocion_df2[334,4] <- 1
emocion_df2[340,4] <- 1
emocion_df2[344,4] <- 1
emocion_df2[391,4] <- 1
emocion_df2[395,4] <- 1
emocion_df2[340,4] <- 1
emocion_df2[437,4] <- 1
emocion_df2[441,4] <- 1
emocion_df2[445,4] <- 1
emocion_df2[466,4] <- 1
emocion_df2[473,4] <- 1
emocion_df2[479,4] <- 1
emocion_df2[492,4] <- 1
emocion_df2[441,4] <- 1
emocion_df2[496,4] <- 1
emocion_df2[500,4] <- 1
emocion_df2[515,4] <- 1
emocion_df2[519,4] <- 1
emocion_df2[536,4] <- 1
emocion_df2[538,4] <- 1
emocion_df2[542,4] <- 1
emocion_df2[558,4] <- 1
emocion_df2[594,4] <- 1
emocion_df2[598,4] <- 1
emocion_df2[601,4] <- 1
emocion_df2[608,4] <- 1
emocion_df2[620,4] <- 1
emocion_df2[642,4] <- 1
emocion_df2[644,4] <- 1
emocion_df2[649,4] <- 1
emocion_df2[653,4] <- 1
emocion_df2[656,4] <- 1
emocion_df2[662,4] <- 1
emocion_df2[682,4] <- 1
emocion_df2[687,4] <- 1
emocion_df2[691,4] <- 1
emocion_df2[601,4] <- 1
emocion_df2[720,5] <- 1
emocion_df2[731,5] <- 1
emocion_df2[732,4] <- 1
emocion_df2[742,4] <- 1
emocion_df2[753,4] <- 1
emocion_df2[759,5] <- 1
emocion_df2[800,4] <- 1
emocion_df2[802,4] <- 1
emocion_df2[809,4] <- 1
emocion_df2[811,4] <- 1
emocion_df2[824,4] <- 1
emocion_df2[841,4] <- 1
emocion_df2[870,5] <- 1
emocion_df2[885,4] <- 1
emocion_df2[903,5] <- 1
emocion_df2[905,5] <- 1
emocion_df2[914,5] <- 1
emocion_df2[918,5] <- 1
emocion_df2[936,4] <- 1
emocion_df2[949,5] <- 1
emocion_df2[963,5] <- 1
emocion_df2[966,4] <- 1
emocion_df2[991,4] <- 1
emocion_df2[993,4] <- 1
emocion_df2[994,4] <- 1
emocion_df2[1003,4] <- 1
emocion_df2[1017,4] <- 1
emocion_df2[1020,4] <- 1
emocion_df2[1021,4] <- 1
emocion_df2[1028,4] <- 1
emocion_df2[1056,5] <- 1
emocion_df2[1064,4] <- 1
emocion_df2[1073,4] <- 1
emocion_df2[1092,4] <- 1
emocion_df2[1095,4] <- 1
emocion_df2[1120,4] <- 1
emocion_df2[1128,4] <- 1
emocion_df2[1132,4] <- 1
emocion_df2[1134,4] <- 1
emocion_df2[1139,4] <- 1
emocion_df2[1144,4] <- 1
emocion_df2[1173,4] <- 1
emocion_df2[1175,4] <- 1
emocion_df2[1120,4] <- 1
emocion_df2[1203,4] <- 1
emocion_df2[1212,4] <- 1
emocion_df2[1258,4] <- 1
emocion_df2[1277,4] <- 1
emocion_df2[1292,4] <- 1
emocion_df2[1343,4] <- 1
emocion_df2[1354,4] <- 1
emocion_df2[1362,4] <- 1
emocion_df2[1376,4] <- 1
emocion_df2[1380,4] <- 1
emocion_df2[1399,4] <- 1
emocion_df2[1402,4] <- 1
emocion_df2[1407,4] <- 1
emocion_df2[1428,4] <- 1
emocion_df2[1431,4] <- 1
emocion_df2[1437,4] <- 1
emocion_df2[1460,4] <- 1
emocion_df2[1484,4] <- 1
emocion_df2[1495,4] <- 1
emocion_df2[1516,4] <- 1
emocion_df2[1535,4] <- 1
emocion_df2[1543,4] <- 1
emocion_df2[1552,5] <- 1
emocion_df2[1657,4] <- 1
emocion_df2[1695,4] <- 1
emocion_df2[1715,4] <- 1
emocion_df2[1724,4] <- 1
emocion_df2[1736,4] <- 1
emocion_df2[1746,4] <- 1
emocion_df2[1756,5] <- 1
emocion_df2[1781,4] <- 1
emocion_df2[1796,4] <- 1
emocion_df2[1828,5] <- 1
emocion_df2[1834,4] <- 1
emocion_df2[1882,5] <- 1
emocion_df2[1888,5] <- 1
emocion_df2[1901,4] <- 1
emocion_df2[1932,4] <- 1
emocion_df2[1946,4] <- 1
emocion_df2[1947,4] <- 1
emocion_df2[1969,4] <- 1
emocion_df2[1977,4] <- 1
emocion_df2[1983,4] <- 1
emocion_df2[2015,5] <- 1
emocion_df2[2021,4] <- 1
emocion_df2[2026,4] <- 1
emocion_df2[2029,4] <- 1
emocion_df2[2042,4] <- 1
emocion_df2[2057,4] <- 1
emocion_df2[2061,4] <- 1
emocion_df2[2072,4] <- 1
emocion_df2[2092,5] <- 1
emocion_df2[2096,4] <- 1
emocion_df2[2103,4] <- 1
emocion_df2[2105,5] <- 1
emocion_df2[2111,5] <- 1
emocion_df2[2134,5] <- 1
emocion_df2[2138,4] <- 1
emocion_df2[2145,4] <- 1
emocion_df2[2152,4] <- 1
emocion_df2[2165,4] <- 1
emocion_df2[2184,4] <- 1
emocion_df2[2188,4] <- 1
emocion_df2[2225,4] <- 1
emocion_df2[2227,4] <- 1
emocion_df2[2253,4] <- 1
emocion_df2[2275,4] <- 1
emocion_df2[2279,5] <- 1
emocion_df2[2225,4] <- 1
emocion_df2[2301,5] <- 1
emocion_df2[2305,5] <- 1
emocion_df2[2327,4] <- 1
emocion_df2[2336,4] <- 1
emocion_df2[2343,5] <- 1
emocion_df2[2360,4] <- 1
emocion_df2[2378,5] <- 1
emocion_df2[2382,5] <- 1
emocion_df2[2423,4] <- 1
emocion_df2[2429,4] <- 1
emocion_df2[2433,4] <- 1
emocion_df2[2453,5] <- 1
emocion_df2[2455,5] <- 1
emocion_df2[2462,4] <- 1
emocion_df2[2468,4] <- 1
emocion_df2[2471,4] <- 1
emocion_df2[2477,4] <- 1
emocion_df2[2481,4] <- 1
emocion_df2[2484,4] <- 1
emocion_df2[2495,4] <- 1
emocion_df2[2497,4] <- 1
emocion_df2[2499,4] <- 1
emocion_df2[2503,4] <- 1
emocion_df2[2511,4] <- 1
emocion_df2[2531,4] <- 1
emocion_df2[2536,4] <- 1
emocion_df2[2586,4] <- 1
emocion_df2[2592,4] <- 1
emocion_df2[2595,4] <- 1
emocion_df2[2629,5] <- 1
emocion_df2[2642,5] <- 1
emocion_df2[2647,5] <- 1
emocion_df2[2669,4] <- 1
emocion_df2[2673,4] <- 1
emocion_df2[2679,5] <- 1
emocion_df2[2685,4] <- 1
emocion_df2[2629,5] <- 1
emocion_df2[2690,5] <- 1
emocion_df2[2693,4] <- 1
emocion_df2[2694,4] <- 1
emocion_df2[2695,4] <- 1
emocion_df2[2696,4] <- 1
emocion_df2[2698,4] <- 1
emocion_df2[2707,4] <- 1
emocion_df2[2708,4] <- 1
emocion_df2[2714,4] <- 1
emocion_df2[2718,4] <- 1
emocion_df2[2743,4] <- 1
emocion_df2[2747,4] <- 1
emocion_df2[2748,4] <- 1
emocion_df2[2754,4] <- 1
emocion_df2[2762,4] <- 1
emocion_df2[2766,4] <- 1
emocion_df2[2778,4] <- 1
emocion_df2[2796,4] <- 1
emocion_df2[2805,4] <- 1
emocion_df2[2807,4] <- 1
emocion_df2[2824,5] <- 1
emocion_df2[2831,4] <- 1
emocion_df2[2832,4] <- 1
emocion_df2[2861,5] <- 1
emocion_df2[2874,5] <- 1
emocion_df2[2875,5] <- 1
emocion_df2[2878,4] <- 1
emocion_df2[2889,4] <- 1
emocion_df2[2890,4] <- 1
emocion_df2[2893,4] <- 1
emocion_df2[2895,4] <- 1
emocion_df2[2899,4] <- 1
emocion_df2[2902,4] <- 1
emocion_df2[2905,4] <- 1
emocion_df2[2909,4] <- 1
emocion_df2[2914,4] <- 1
emocion_df2[2921,4] <- 1
emocion_df2[2934,4] <- 1
emocion_df2[2938,4] <- 1
emocion_df2[2961,4] <- 1
emocion_df2[2975,4] <- 1
emocion_df2[2996,4] <- 1
emocion_df2[3039,4] <- 1
emocion_df2[3042,4] <- 1
emocion_df2[3044,4] <- 1
emocion_df2[3049,4] <- 1
emocion_df2[3054,4] <- 1
emocion_df2[3065,4] <- 1
emocion_df2[3069,5] <- 1
emocion_df2[3070,5] <- 1
emocion_df2[3086,4] <- 1
emocion_df2[3089,4] <- 1
emocion_df2[3099,5] <- 1
emocion_df2[3102,4] <- 1
emocion_df2[3110,4] <- 1
emocion_df2[3119,4] <- 1
emocion_df2[3136,4] <- 1
emocion_df2[3138,4] <- 1
emocion_df2[3145,4] <- 1
emocion_df2[3148,4] <- 1
emocion_df2[3169,4] <- 1
emocion_df2[3180,4] <- 1
emocion_df2[3185,4] <- 1
emocion_df2[3190,4] <- 1
emocion_df2[3208,5] <- 1
emocion_df2[3221,4] <- 1
emocion_df2[3232,4] <- 1
emocion_df2[3235,4] <- 1
emocion_df2[3261,4] <- 1
emocion_df2[3266,4] <- 1
emocion_df2[3272,4] <- 1
emocion_df2[3275,4] <- 1
emocion_df2[3280,4] <- 1
emocion_df2[3307,5] <- 1
emocion_df2[3314,5] <- 1
emocion_df2[3319,4] <- 1
emocion_df2[3323,4] <- 1
emocion_df2[3324,4] <- 1
emocion_df2[3343,5] <- 1
emocion_df2[3358,4] <- 1
emocion_df2[3360,4] <- 1
emocion_df2[3370,4] <- 1
emocion_df2[3376,4] <- 1
emocion_df2[3389,5] <- 1
emocion_df2[3393,4] <- 1
emocion_df2[3394,4] <- 1
emocion_df2[3422,4] <- 1
emocion_df2[3430,5] <- 1
emocion_df2[3461,4] <- 1
emocion_df2[3465,5] <- 1
emocion_df2[3468,4] <- 1
emocion_df2[3472,4] <- 1
emocion_df2[3475,4] <- 1
emocion_df2[3478,5] <- 1
emocion_df2[3484,4] <- 1
emocion_df2[3493,5] <- 1
emocion_df2[3501,4] <- 1
emocion_df2[3516,4] <- 1
emocion_df2[3519,4] <- 1
emocion_df2[3551,5] <- 1
emocion_df2[3555,4] <- 1
emocion_df2[3559,4] <- 1
emocion_df2[3582,4] <- 1
emocion_df2[3590,4] <- 1
emocion_df2[3594,4] <- 1
emocion_df2[3597,4] <- 1
emocion_df2[3616,5] <- 1
emocion_df2[3620,4] <- 1
emocion_df2[3626,4] <- 1
emocion_df2[3627,4] <- 1
emocion_df2[3635,4] <- 1
emocion_df2[3645,5] <- 1
emocion_df2[3647,4] <- 1
emocion_df2[3661,4] <- 1
emocion_df2[3664,5] <- 1
emocion_df2[3680,4] <- 1
emocion_df2[3683,4] <- 1
emocion_df2[3698,4] <- 1
emocion_df2[3699,4] <- 1
emocion_df2[3706,5] <- 1
emocion_df2[3720,4] <- 1
emocion_df2[3743,4] <- 1
emocion_df2[3761,4] <- 1
emocion_df2[3765,4] <- 1
emocion_df2[3820,4] <- 1
emocion_df2[3825,4] <- 1
emocion_df2[3838,4] <- 1
emocion_df2[3856,4] <- 1
emocion_df2[3863,4] <- 1
emocion_df2[3864,4] <- 1
emocion_df2[3866,4] <- 1
emocion_df2[3867,4] <- 1
emocion_df2[3875,4] <- 1
emocion_df2[3880,4] <- 1
emocion_df2[3888,4] <- 1
emocion_df2[3893,4] <- 1
emocion_df2[3907,5] <- 1
emocion_df2[3936,4] <- 1
emocion_df2[3941,4] <- 1
emocion_df2[3990,4] <- 1
emocion_df2[3993,4] <- 1
emocion_df2[3995,4] <- 1
emocion_df2[3997,4] <- 1
emocion_df2[4000,4] <- 1
emocion_df2[4008,4] <- 1
emocion_df2[4015,4] <- 1
emocion_df2[4016,4] <- 1
emocion_df2[4020,5] <- 1
emocion_df2[4028,4] <- 1
emocion_df2[4031,4] <- 1
emocion_df2[4033,4] <- 1
emocion_df2[4042,4] <- 1
emocion_df2[4110,4] <- 1
emocion_df2[4112,4] <- 1
emocion_df2[4120,4] <- 1
emocion_df2[4122,4] <- 1
emocion_df2[4125,4] <- 1
emocion_df2[4128,4] <- 1
emocion_df2[4147,4] <- 1
emocion_df2[4173,5] <- 1
emocion_df2[4177,4] <- 1
emocion_df2[4182,4] <- 1
emocion_df2[4189,4] <- 1
emocion_df2[4238,4] <- 1
emocion_df2[4248,4] <- 1
emocion_df2[4254,4] <- 1
emocion_df2[4266,4] <- 1
emocion_df2[4296,4] <- 1
emocion_df2[4304,4] <- 1
emocion_df2[4313,4] <- 1
emocion_df2[4318,4] <- 1
emocion_df2[4328,4] <- 1
emocion_df2[4341,4] <- 1
emocion_df2[4344,4] <- 1
emocion_df2[4345,4] <- 1
emocion_df2[4347,4] <- 1
emocion_df2[4351,4] <- 1
emocion_df2[4372,4] <- 1
emocion_df2[4380,4] <- 1
emocion_df2[4399,4] <- 1
emocion_df2[4408,4] <- 1
emocion_df2[4422,5] <- 1
emocion_df2[4453,4] <- 1
emocion_df2[4476,4] <- 1
emocion_df2[4498,4] <- 1
emocion_df2[4504,4] <- 1
emocion_df2[4508,5] <- 1
emocion_df2[4511,4] <- 1
emocion_df2[4521,4] <- 1
emocion_df2[4535,5] <- 1
emocion_df2[4536,4] <- 1
emocion_df2[4540,4] <- 1
emocion_df2[4543,4] <- 1
emocion_df2[4569,4] <- 1
emocion_df2[4587,5] <- 1
emocion_df2[4598,4] <- 1
emocion_df2[4599,4] <- 1
emocion_df2[4604,4] <- 1
emocion_df2[4608,4] <- 1
emocion_df2[4623,5] <- 1
emocion_df2[4627,5] <- 1
emocion_df2[4638,4] <- 1
emocion_df2[4646,5] <- 1
emocion_df2[4647,5] <- 1
emocion_df2[4667,5] <- 1
emocion_df2[4681,4] <- 1
emocion_df2[4693,4] <- 1
emocion_df2[4697,4] <- 1
emocion_df2[4701,4] <- 1
emocion_df2[4711,4] <- 1
emocion_df2[4720,4] <- 1
emocion_df2[4725,4] <- 1
emocion_df2[4731,4] <- 1
emocion_df2[4800,4] <- 1
emocion_df2[4807,4] <- 1
emocion_df2[4808,4] <- 1
emocion_df2[4817,4] <- 1
emocion_df2[4833,4] <- 1
emocion_df2[4841,4] <- 1
emocion_df2[4844,4] <- 1
emocion_df2[4848,4] <- 1
emocion_df2[4851,4] <- 1
emocion_df2[4853,4] <- 1
emocion_df2[4865,5] <- 1
emocion_df2[4870,4] <- 1
emocion_df2[4877,4] <- 1
emocion_df2[4892,4] <- 1
emocion_df2[4901,4] <- 1
emocion_df2[4911,4] <- 1
emocion_df2[4924,4] <- 1
emocion_df2[4931,4] <- 1
emocion_df2[4938,4] <- 1
emocion_df2[4947,4] <- 1
emocion_df2[4948,4] <- 1
emocion_df2[4957,4] <- 1
emocion_df2[4986,4] <- 1
emocion_df2[4997,4] <- 1
emocion_df2[5016,4] <- 1
emocion_df2[5023,4] <- 1
emocion_df2[5028,4] <- 1
emocion_df2[5033,4] <- 1
emocion_df2[5034,4] <- 1
emocion_df2[5043,4] <- 1
emocion_df2[5046,4] <- 1
emocion_df2[5051,4] <- 1
emocion_df2[5063,4] <- 1
emocion_df2[5077,4] <- 1
emocion_df2[5081,4] <- 1
emocion_df2[5085,4] <- 1
emocion_df2[5096,4] <- 1
emocion_df2[5104,4] <- 1
emocion_df2[5110,4] <- 1
emocion_df2[5124,5] <- 1
emocion_df2[5128,5] <- 1
emocion_df2[5144,5] <- 1
emocion_df2[5146,4] <- 1
emocion_df2[5150,4] <- 1
emocion_df2[5154,4] <- 1
emocion_df2[5166,4] <- 1
emocion_df2[5169,4] <- 1
emocion_df2[5178,4] <- 1
emocion_df2[5188,4] <- 1
emocion_df2[5192,4] <- 1
emocion_df2[5194,4] <- 1
emocion_df2[5223,4] <- 1
emocion_df2[5241,4] <- 1
emocion_df2[5242,4] <- 1
emocion_df2[5251,4] <- 1
emocion_df2[5281,4] <- 1
emocion_df2[5282,4] <- 1
emocion_df2[5287,4] <- 1
emocion_df2[5289,4] <- 1
emocion_df2[5294,4] <- 1
emocion_df2[5304,4] <- 1
emocion_df2[5311,4] <- 1
emocion_df2[5313,4] <- 1
emocion_df2[5323,5] <- 1
emocion_df2[5328,4] <- 1
emocion_df2[5336,4] <- 1
emocion_df2[5353,4] <- 1
emocion_df2[5362,4] <- 1
emocion_df2[5375,4] <- 1
emocion_df2[5376,4] <- 1
emocion_df2[5381,4] <- 1
emocion_df2[5384,4] <- 1
emocion_df2[5389,4] <- 1
emocion_df2[5398,4] <- 1
emocion_df2[5402,4] <- 1
emocion_df2[5428,4] <- 1
emocion_df2[5433,4] <- 1
emocion_df2[5437,4] <- 1
emocion_df2[5442,4] <- 1
emocion_df2[5448,4] <- 1
emocion_df2[5452,4] <- 1
emocion_df2[5468,4] <- 1
emocion_df2[5476,4] <- 1
emocion_df2[5482,4] <- 1
emocion_df2[5492,4] <- 1
emocion_df2[5499,4] <- 1
emocion_df2[5503,4] <- 1
emocion_df2[5507,4] <- 1
emocion_df2[5513,4] <- 1
emocion_df2[5517,4] <- 1
emocion_df2[5556,5] <- 1
emocion_df2[5562,4] <- 1
emocion_df2[5580,4] <- 1
emocion_df2[5582,4] <- 1
emocion_df2[5587,4] <- 1
emocion_df2[5597,4] <- 1
emocion_df2[5603,4] <- 1
emocion_df2[5607,4] <- 1
emocion_df2[5609,4] <- 1
emocion_df2[5611,4] <- 1
emocion_df2[5614,4] <- 1
emocion_df2[5615,4] <- 1
emocion_df2[5617,4] <- 1
emocion_df2[5623,4] <- 1
emocion_df2[5627,4] <- 1
emocion_df2[5636,4] <- 1
emocion_df2[5642,4] <- 1
emocion_df2[5672,4] <- 1
emocion_df2[5689,4] <- 1
emocion_df2[5695,4] <- 1
emocion_df2[5699,4] <- 1
emocion_df2[5703,4] <- 1
emocion_df2[5708,4] <- 1
emocion_df2[5712,4] <- 1
emocion_df2[5721,4] <- 1
emocion_df2[5722,4] <- 1
emocion_df2[5726,4] <- 1
emocion_df2[5731,5] <- 1
emocion_df2[5738,4] <- 1
emocion_df2[5745,5] <- 1
emocion_df2[5769,4] <- 1
emocion_df2[5779,4] <- 1
emocion_df2[5782,4] <- 1
emocion_df2[5786,4] <- 1
emocion_df2[5792,4] <- 1
emocion_df2[5802,4] <- 1
emocion_df2[5803,5] <- 1
emocion_df2[5813,5] <- 1
emocion_df2[5822,5] <- 1
emocion_df2[5824,5] <- 1
emocion_df2[5830,4] <- 1
emocion_df2[5858,4] <- 1
emocion_df2[5862,4] <- 1
emocion_df2[5909,4] <- 1
emocion_df2[5922,4] <- 1
emocion_df2[5925,4] <- 1
emocion_df2[5926,4] <- 1
emocion_df2[5957,4] <- 1
emocion_df2[5960,4] <- 1
emocion_df2[5963,4] <- 1
emocion_df2[5967,4] <- 1
emocion_df2[5970,4] <- 1
emocion_df2[5980,4] <- 1
emocion_df2[5995,5] <- 1
emocion_df2[6006,4] <- 1
emocion_df2[6016,4] <- 1
emocion_df2[6028,4] <- 1
emocion_df2[6045,4] <- 1
emocion_df2[6049,4] <- 1
emocion_df2[6058,4] <- 1
emocion_df2[6063,4] <- 1
emocion_df2[6066,4] <- 1
emocion_df2[6096,4] <- 1
emocion_df2[6099,5] <- 1
emocion_df2[6118,4] <- 1
emocion_df2[6123,4] <- 1
emocion_df2[6143,4] <- 1
emocion_df2[6147,4] <- 1
emocion_df2[6157,4] <- 1
emocion_df2[6174,4] <- 1
emocion_df2[6178,4] <- 1
emocion_df2[6184,4] <- 1
emocion_df2[6187,4] <- 1
emocion_df2[6194,4] <- 1
emocion_df2[6206,4] <- 1
emocion_df2[6241,4] <- 1
emocion_df2[6244,4] <- 1
emocion_df2[6257,4] <- 1
emocion_df2[6266,4] <- 1
emocion_df2[6366,4] <- 1
emocion_df2[6373,4] <- 1
emocion_df2[6389,4] <- 1
emocion_df2[6390,4] <- 1
emocion_df2[6396,4] <- 1
emocion_df2[6470,4] <- 1
emocion_df2[6476,4] <- 1
emocion_df2[6478,4] <- 1
emocion_df2[6488,4] <- 1
emocion_df2[6492,4] <- 1
emocion_df2[6494,5] <- 1
emocion_df2[6499,4] <- 1
emocion_df2[6504,5] <- 1
emocion_df2[6513,4] <- 1
emocion_df2[6516,4] <- 1
emocion_df2[6522,4] <- 1
emocion_df2[6527,4] <- 1
emocion_df2[6529,4] <- 1
emocion_df2[6535,4] <- 1
emocion_df2[6548,4] <- 1
emocion_df2[6554,4] <- 1
emocion_df2[6557,5] <- 1
emocion_df2[6577,4] <- 1
emocion_df2[6592,4] <- 1
emocion_df2[6610,4] <- 1
emocion_df2[6642,4] <- 1
emocion_df2[6645,4] <- 1
emocion_df2[6646,4] <- 1
emocion_df2[6648,4] <- 1
emocion_df2[6649,4] <- 1
emocion_df2[6659,4] <- 1
emocion_df2[6666,4] <- 1
emocion_df2[6679,4] <- 1
emocion_df2[6683,4] <- 1
emocion_df2[6688,4] <- 1
emocion_df2[6692,4] <- 1
emocion_df2[6709,4] <- 1
emocion_df2[6718,4] <- 1
emocion_df2[6722,4] <- 1
emocion_df2[6728,4] <- 1
emocion_df2[6735,4] <- 1
emocion_df2[6757,4] <- 1
emocion_df2[6795,4] <- 1
emocion_df2[6815,5] <- 1
emocion_df2[6853,4] <- 1
emocion_df2[6857,4] <- 1
emocion_df2[6864,4] <- 1
emocion_df2[6868,4] <- 1
emocion_df2[6869,4] <- 1
emocion_df2[6870,5] <- 1
emocion_df2[6895,4] <- 1
emocion_df2[6911,4] <- 1
emocion_df2[6914,4] <- 1
emocion_df2[6923,4] <- 1
emocion_df2[6924,4] <- 1
emocion_df2[6929,4] <- 1
emocion_df2[6930,4] <- 1
emocion_df2[6931,4] <- 1
emocion_df2[6935,4] <- 1
emocion_df2[6940,4] <- 1
emocion_df2[6944,4] <- 1
emocion_df2[6956,4] <- 1
emocion_df2[6976,4] <- 1
emocion_df2[6979,4] <- 1
emocion_df2[6991,4] <- 1
emocion_df2[7000,4] <- 1
emocion_df2[7002,4] <- 1
emocion_df2[7006,4] <- 1
emocion_df2[7013,4] <- 1
emocion_df2[7017,4] <- 1
emocion_df2[7019,4] <- 1
emocion_df2[7023,4] <- 1
emocion_df2[7039,4] <- 1
emocion_df2[7055,4] <- 1
emocion_df2[7059,4] <- 1
emocion_df2[7104,4] <- 1
emocion_df2[7111,4] <- 1
emocion_df2[7113,4] <- 1
emocion_df2[7119,4] <- 1
emocion_df2[7130,4] <- 1
emocion_df2[7140,4] <- 1
emocion_df2[7153,4] <- 1
emocion_df2[7163,4] <- 1
emocion_df2[7164,4] <- 1
emocion_df2[7166,4] <- 1
emocion_df2[7171,4] <- 1
emocion_df2[7174,4] <- 1
emocion_df2[7194,4] <- 1
emocion_df2[7203,4] <- 1
emocion_df2[7204,4] <- 1
emocion_df2[7212,4] <- 1
emocion_df2[7255,4] <- 1
emocion_df2[7273,4] <- 1
emocion_df2[7300,4] <- 1
emocion_df2[7303,4] <- 1
emocion_df2[7306,4] <- 1
emocion_df2[7306,4] <- 1
emocion_df2[7314,4] <- 1
emocion_df2[7316,4] <- 1
emocion_df2[7323,4] <- 1
emocion_df2[7326,4] <- 1
emocion_df2[7341,5] <- 1
emocion_df2[7345,4] <- 1
emocion_df2[7366,4] <- 1
emocion_df2[7372,4] <- 1
emocion_df2[7376,4] <- 1
emocion_df2[7387,4] <- 1
emocion_df2[7395,4] <- 1
emocion_df2[7402,4] <- 1
emocion_df2[7413,4] <- 1
emocion_df2[7416,4] <- 1
emocion_df2[7426,4] <- 1
emocion_df2[7434,4] <- 1
emocion_df2[7449,5] <- 1
emocion_df2[7465,4] <- 1
emocion_df2[7467,4] <- 1
emocion_df2[7476,4] <- 1
emocion_df2[7481,4] <- 1
emocion_df2[7496,4] <- 1
emocion_df2[7500,4] <- 1
emocion_df2[7503,4] <- 1
emocion_df2[7506,4] <- 1
emocion_df2[7511,4] <- 1
emocion_df2[7524,4] <- 1
emocion_df2[7552,4] <- 1
emocion_df2[7560,4] <- 1
emocion_df2[7568,5] <- 1
emocion_df2[7582,5] <- 1
emocion_df2[7584,5] <- 1
emocion_df2[7623,4] <- 1
emocion_df2[7631,4] <- 1
emocion_df2[7636,4] <- 1
emocion_df2[7652,4] <- 1
emocion_df2[7669,4] <- 1
emocion_df2[7693,4] <- 1
emocion_df2[7694,4] <- 1
emocion_df2[7696,4] <- 1
emocion_df2[7732,4] <- 1
emocion_df2[7755,4] <- 1
emocion_df2[7758,5] <- 1
emocion_df2[7766,4] <- 1
emocion_df2[7770,4] <- 1
emocion_df2[7805,4] <- 1
emocion_df2[7825,4] <- 1
emocion_df2[7829,4] <- 1
emocion_df2[7834,4] <- 1
emocion_df2[7847,5] <- 1
emocion_df2[7874,5] <- 1
emocion_df2[7886,4] <- 1
emocion_df2[7889,5] <- 1
emocion_df2[7905,4] <- 1
emocion_df2[7907,4] <- 1
emocion_df2[7923,4] <- 1
emocion_df2[7925,4] <- 1
emocion_df2[7930,4] <- 1
emocion_df2[7934,4] <- 1
emocion_df2[7937,4] <- 1
emocion_df2[7948,4] <- 1
emocion_df2[7953,4] <- 1
emocion_df2[7966,4] <- 1
emocion_df2[7978,4] <- 1
emocion_df2[8000,5] <- 1
emocion_df2[8009,4] <- 1
emocion_df2[8030,4] <- 1
emocion_df2[8033,4] <- 1
emocion_df2[8043,4] <- 1
emocion_df2[8061,4] <- 1
emocion_df2[8068,4] <- 1
emocion_df2[8072,4] <- 1
emocion_df2[8080,4] <- 1
emocion_df2[8084,4] <- 1
emocion_df2[8089,4] <- 1
emocion_df2[8093,4] <- 1
emocion_df2[8105,5] <- 1
emocion_df2[8123,4] <- 1
emocion_df2[8155,4] <- 1
emocion_df2[8186,4] <- 1
emocion_df2[8210,5] <- 1
emocion_df2[8231,4] <- 1
emocion_df2[8235,4] <- 1
emocion_df2[8237,4] <- 1
emocion_df2[8240,4] <- 1
emocion_df2[8249,4] <- 1
emocion_df2[8255,4] <- 1
emocion_df2[8348,4] <- 1
emocion_df2[8350,4] <- 1
emocion_df2[8362,4] <- 1
emocion_df2[8369,4] <- 1
emocion_df2[8372,4] <- 1
emocion_df2[8378,4] <- 1
emocion_df2[8389,4] <- 1
emocion_df2[8393,4] <- 1
emocion_df2[8398,4] <- 1
emocion_df2[8403,4] <- 1
emocion_df2[8411,4] <- 1
emocion_df2[8431,4] <- 1
emocion_df2[8448,4] <- 1
emocion_df2[8455,4] <- 1
emocion_df2[8468,4] <- 1
emocion_df2[8474,4] <- 1
emocion_df2[8509,4] <- 1
emocion_df2[8512,5] <- 1
emocion_df2[8520,4] <- 1
emocion_df2[8523,4] <- 1
emocion_df2[8530,4] <- 1
emocion_df2[8538,4] <- 1
emocion_df2[8541,4] <- 1
emocion_df2[8558,4] <- 1
emocion_df2[8561,4] <- 1
emocion_df2[8568,4] <- 1
emocion_df2[8575,4] <- 1
emocion_df2[8580,4] <- 1
emocion_df2[8583,4] <- 1
emocion_df2[8604,5] <- 1
emocion_df2[8618,4] <- 1
emocion_df2[8622,4] <- 1
emocion_df2[8638,4] <- 1
emocion_df2[8644,4] <- 1
emocion_df2[8647,4] <- 1
emocion_df2[8655,4] <- 1
emocion_df2[8659,4] <- 1
emocion_df2[8678,4] <- 1
emocion_df2[8688,4] <- 1
emocion_df2[8689,4] <- 1
emocion_df2[8697,4] <- 1
emocion_df2[8720,4] <- 1
emocion_df2[8745,4] <- 1
emocion_df2[8752,4] <- 1
emocion_df2[8767,4] <- 1
emocion_df2[8780,4] <- 1
emocion_df2[8784,4] <- 1
emocion_df2[8787,4] <- 1
emocion_df2[8790,4] <- 1
emocion_df2[8794,4] <- 1
emocion_df2[8798,4] <- 1
emocion_df2[8810,4] <- 1
emocion_df2[8818,4] <- 1
emocion_df2[8824,4] <- 1
emocion_df2[8831,4] <- 1
emocion_df2[8865,4] <- 1
emocion_df2[8868,4] <- 1
emocion_df2[8872,4] <- 1
emocion_df2[8886,4] <- 1
emocion_df2[8902,4] <- 1
emocion_df2[8917,5] <- 1
emocion_df2[8925,4] <- 1
emocion_df2[8929,5] <- 1
emocion_df2[8941,5] <- 1
emocion_df2[8947,4] <- 1
emocion_df2[8954,4] <- 1
emocion_df2[8972,4] <- 1
emocion_df2[8974,4] <- 1
emocion_df2[8981,4] <- 1
emocion_df2[8985,4] <- 1
emocion_df2[8994,4] <- 1
emocion_df2[9014,4] <- 1
emocion_df2[9015,4] <- 1
emocion_df2[9044,4] <- 1
emocion_df2[9045,4] <- 1
emocion_df2[9048,4] <- 1
emocion_df2[9057,4] <- 1
emocion_df2[9061,4] <- 1
emocion_df2[9065,4] <- 1
emocion_df2[9073,4] <- 1
emocion_df2[9081,4] <- 1
emocion_df2[9085,4] <- 1
emocion_df2[9100,4] <- 1
emocion_df2[9118,5] <- 1
emocion_df2[9120,4] <- 1
emocion_df2[9124,4] <- 1
emocion_df2[9135,4] <- 1
emocion_df2[9149,4] <- 1
emocion_df2[9153,4] <- 1
emocion_df2[9156,4] <- 1
emocion_df2[9166,4] <- 1
emocion_df2[9228,4] <- 1
emocion_df2[9248,4] <- 1
emocion_df2[9251,4] <- 1
emocion_df2[9253,4] <- 1
emocion_df2[9262,4] <- 1
emocion_df2[9267,4] <- 1
emocion_df2[9270,4] <- 1
emocion_df2[9283,4] <- 1
emocion_df2[9296,4] <- 1
emocion_df2[9297,4] <- 1
emocion_df2[9302,4] <- 1
emocion_df2[9315,4] <- 1
emocion_df2[9339,4] <- 1
emocion_df2[9352,4] <- 1
emocion_df2[9368,4] <- 1
emocion_df2[9374,4] <- 1
emocion_df2[9381,4] <- 1
emocion_df2[9395,4] <- 1
emocion_df2[9399,4] <- 1
emocion_df2[9409,4] <- 1
emocion_df2[9418,4] <- 1
emocion_df2[9423,4] <- 1
emocion_df2[9427,4] <- 1
emocion_df2[9430,4] <- 1
emocion_df2[9431,4] <- 1
emocion_df2[9487,4] <- 1
emocion_df2[9500,4] <- 1
emocion_df2[9546,4] <- 1
emocion_df2[9549,4] <- 1
emocion_df2[9554,4] <- 1
emocion_df2[9563,4] <- 1
emocion_df2[9571,4] <- 1
emocion_df2[9573,4] <- 1
emocion_df2[9577,4] <- 1
emocion_df2[9589,4] <- 1
emocion_df2[9599,4] <- 1
emocion_df2[9608,4] <- 1
emocion_df2[9618,4] <- 1
emocion_df2[9627,4] <- 1
emocion_df2[9631,4] <- 1
emocion_df2[9637,4] <- 1
emocion_df2[9642,4] <- 1
emocion_df2[9661,4] <- 1
emocion_df2[9664,4] <- 1
emocion_df2[9668,4] <- 1
emocion_df2[9671,4] <- 1
emocion_df2[9676,4] <- 1
emocion_df2[9685,4] <- 1
emocion_df2[9697,4] <- 1
emocion_df2[9698,4] <- 1
emocion_df2[9701,4] <- 1
emocion_df2[9733,4] <- 1
emocion_df2[9747,5] <- 1
emocion_df2[9753,5] <- 1
emocion_df2[9757,4] <- 1
emocion_df2[9760,4] <- 1
emocion_df2[9790,4] <- 1
emocion_df2[9803,4] <- 1
emocion_df2[9827,4] <- 1
emocion_df2[9836,4] <- 1
emocion_df2[9837,4] <- 1
emocion_df2[9844,4] <- 1
emocion_df2[9862,5] <- 1
emocion_df2[9865,4] <- 1
emocion_df2[9882,4] <- 1
emocion_df2[9895,4] <- 1
emocion_df2[9897,5] <- 1
emocion_df2[9931,4] <- 1
emocion_df2[9933,5] <- 1
emocion_df2[9940,4] <- 1
emocion_df2[9967,5] <- 1
emocion_df2[9983,4] <- 1
emocion_df2[9985,4] <- 1
emocion_df2[9989,4] <- 1
emocion_df2[9995,4] <- 1
emocion_df2[9996,4] <- 1
emocion_df2[10000,4] <- 1
emocion_df2[10003,4] <- 1
emocion_df2[10010,4] <- 1
emocion_df2[10015,4] <- 1
emocion_df2[10051,4] <- 1
emocion_df2[10057,4] <- 1
emocion_df2[10065,4] <- 1
emocion_df2[10069,4] <- 1
emocion_df2[10087,4] <- 1
emocion_df2[10118,4] <- 1
emocion_df2[10125,4] <- 1
emocion_df2[10129,4] <- 1
emocion_df2[10155,4] <- 1
emocion_df2[10159,4] <- 1
emocion_df2[10178,4] <- 1
emocion_df2[10183,4] <- 1
emocion_df2[10186,4] <- 1
emocion_df2[10188,4] <- 1
emocion_df2[10267,4] <- 1
emocion_df2[10308,4] <- 1
emocion_df2[10323,4] <- 1
emocion_df2[10325,4] <- 1
emocion_df2[10328,4] <- 1
emocion_df2[10335,4] <- 1
emocion_df2[10339,4] <- 1
emocion_df2[10343,4] <- 1
emocion_df2[10346,4] <- 1
emocion_df2[10349,4] <- 1
emocion_df2[10388,4] <- 1
emocion_df2[10471,4] <- 1
emocion_df2[10478,4] <- 1
emocion_df2[10509,4] <- 1
emocion_df2[10524,4] <- 1




# guarda el dataframe
write.csv(emocion_df2, file = "C:\\Users\\Admin\\Desktop\\tesis\\Tesis\\muestra_manual.csv")

# falsos positivos = 493
sum(emocion_df2$positive)

# falsos negativos = 516
sum(emocion_df2$negative)

# verdaderos positivos = 872
sum(emocion_df2$positivo)

# verdaderos negativos = 116
sum(emocion_df2$negativo)