tab = data.frame('Plantas'=c(1:400),
                'Tratamento'= rep(c("Round","Água"),200),
                'Tamanho'=runif(400, min=50, max=99),
                'Vagens'=rnorm(400,4),
                'Espessura'=runif(400, min=12, max=68),
                'Raiz'=rexp(400,0.4),
                'Folhas'=runif(400, min=10, max=568));

head(tab)

mean(tab$Vagens)
median(tab$Vagens)

library(dplyr)

library(dplyr)

descritiva <- tab %>%
  group_by(Tratamento) %>%
  summarize(
    sd_Tamanho = sd(Tamanho, na.rm = TRUE),
    mean_Tamanho = mean(Tamanho),
    median_Tamanho = median(Tamanho)
  )

print(descritiva, width = Inf)

summary(tab$Tamanho, discrete = FALSE)
summary(tab$Vagens, discrete = FALSE)
summary(tab$Espessura, discrete = FALSE)
summary(tab$Raiz, discrete = FALSE)

shapiro.test(tab$Tamanho)
shapiro.test(tab$Vagens)
shapiro.test(tab$Espessura)
shapiro.test(tab$Raiz)

install.packages("ggplot2")
install.packages("ggpubr")


library(ggplot2)
library(ggpubr)


ggplot(tab, aes(Tratamento, Tamanho))+
    ggtitle(" ")+
    scale_fill_manual(values=c("pink2", "green2"))+
    theme_minimal()+
    geom_boxplot(aes(fill=Tratamento), outlier.size=0.1);

ggplot(tab, aes(Tratamento, Vagens))+
    ggtitle(" ")+
    scale_fill_manual(values=c("purple3", "gold"))+
    theme_minimal()+
    geom_boxplot(aes(fill=Tratamento, outlier.size = 0.1));

plot(tab$Tamanho)
plot(tab$Vagens)
plot(tab$Espessura)
hist(tab$Tamanho)
hist(tab$Vagens)
